# ---- Client: route editing by dragging, sample lasso, undo, map overlays ----
# Loaded after the main script (ui.R); window.bindPinMarker is defined only here
# (ui.R binds pin markers with it as they are added).
#   Route editing (Batch review, Edit Samples; "route_edit" messages): the
#     proposed line can be dragged (the drop point becomes a via pin), the
#     start / end markers moved (Batch review), the pins dragged; a light
#     straight-line preview follows the drag and the server re-routes on drop
#     only. Batch review pins are local splices (the server ends each where
#     the route rejoins the base): the preview runs window_m along the path
#     each way (or to the neighbouring pin), and
#     the drop sends where the drag started (glat/glng). A click on a pin
#     opens its popup (official trips: wrong-way and restricted-turn rules,
#     review_pin_rule; a splice warning) and never re-routes; right-click on a
#     spliced stretch opens the leg menu (OSRM route, recorded path, straight
#     line: review_leg_mode). Right-click or double-click removes a pin.
#   Lasso (Batch review: the samples circled are sent as review_lasso_samples,
#     kept out of /match; Edit Samples: they join the selection for Ignore /
#     Discard / Exclude from snap, window.editLassoSelect in ui.R): a drag
#     draws a lasso while the tab's lasso mode is on (its button or L; Esc
#     leaves it; it stays on across lassos) or with Shift held. Lasso drags
#     are taken in the capture phase, so the map never pans and no marker drag
#     starts during one; a drag that starts on a pin, a route end or the
#     editable route line is never a lasso. Map dragging stays off while the
#     mode is on (wheel zoom works) and Leaflet's boxZoom (Shift + drag) is off.
#   Station pick (Batch review, rail trips; the pins by the start / end
#     selects): every rail station as a small marker (labels from zoom 13),
#     crosshair; a click on or within 300 m of one (the nearest) sends it as
#     review_station_pick and leaves the mode; Esc or the pin again cancels. The
#     map still pans (a drag is not a pick).
#   Ctrl+Z: route_undo (pins, moved ends and exclusions share one history).
#   Overlays "Bus infrastructure" (GeoJSON by map bounds from the server) and
#     "Router network" (OSRM /tile vector tiles of the selected profile, drawn
#     by a MapLibre GL layer of its own, so any base layer works).
ROUTE_EDIT_JS <- "
(function() {
  var RE = {scope: null, on: false, layer: null, ctrl: [], fixedEnds: false, path: [], cum: [], info: {}, legs: [],
            rules: false, labels: {}, windowM: 300, dragging: false};
  window.__routeEdit = RE;  // read by the browser tests
  function getMap() { var w = HTMLWidgets.find('#map'); return w && w.getMap(); }
  function mode() { return $('#mode input:checked').val(); }
  function prefix(scope) { return scope === 'edit' ? 'edit' : 'review'; }
  function send(name, value) { value.ts = Date.now(); Shiny.setInputValue(name, value, {priority: 'event'}); }

  // Straight segment nearest to a point among consecutive control points
  function nearestLeg(map, ll) {
    var p = map.latLngToLayerPoint(ll), best = 0, bestD = Infinity;
    for (var i = 0; i + 1 < RE.ctrl.length; i++) {
      var a = map.latLngToLayerPoint(RE.ctrl[i]), b = map.latLngToLayerPoint(RE.ctrl[i + 1]);
      var d = L.LineUtil.pointToSegmentDistance(p, a, b);
      if (d < bestD) { bestD = d; best = i; }
    }
    return best;
  }
  function previewStyle() { return {color: '#37474F', weight: 2, opacity: 0.8, dashArray: '6 6', interactive: false}; }

  // Distances along the path (metres) and points at them
  function pathCum(path) {
    var c = [0];
    for (var i = 1; i < path.length; i++) c.push(c[i - 1] + path[i - 1].distanceTo(path[i]));
    return c;
  }
  function projectOnPath(map, ll) {
    var p = map.latLngToLayerPoint(ll), best = 0, bestD = Infinity;
    for (var i = 0; i + 1 < RE.path.length; i++) {
      var a = map.latLngToLayerPoint(RE.path[i]), b = map.latLngToLayerPoint(RE.path[i + 1]);
      var cp = L.LineUtil.closestPointOnSegment(p, a, b), dd = cp.distanceTo(p), len = a.distanceTo(b);
      if (dd < bestD) { bestD = dd; best = RE.cum[i] + (len > 0 ? a.distanceTo(cp) / len : 0) * (RE.cum[i + 1] - RE.cum[i]); }
    }
    return best;
  }
  function pointAt(d) {
    var c = RE.cum, n = RE.path.length;
    if (d <= 0) return RE.path[0];
    if (d >= c[n - 1]) return RE.path[n - 1];
    for (var i = 0; i + 1 < n; i++) {
      if (c[i + 1] >= d) {
        var f = c[i + 1] > c[i] ? (d - c[i]) / (c[i + 1] - c[i]) : 0, a = RE.path[i], b = RE.path[i + 1];
        return L.latLng(a.lat + f * (b.lat - a.lat), a.lng + f * (b.lng - a.lng));
      }
    }
    return RE.path[n - 1];
  }
  // Splice anchors around distance d: window_m each way, or the neighbouring pin / route end
  function spliceAnchors(d) {
    var total = RE.cum[RE.cum.length - 1], lo = Math.max(0, d - RE.windowM), hi = Math.min(total, d + RE.windowM);
    RE.ctrl.forEach(function(c) {
      if (c.d === undefined) return;
      if (c.d <= d) lo = Math.max(lo, c.d); else hi = Math.min(hi, c.d);
    });
    return [pointAt(lo), pointAt(hi)];
  }
  function pinCtrl() { return RE.ctrl.filter(function(c) { return c.id !== 'start' && c.id !== 'end'; }); }
  function endIcon(kind) {
    return L.divIcon({className: 'route-end route-end-' + kind, html: '', iconSize: [16, 16], iconAnchor: [8, 8]});
  }

  // Edit Samples: while editing the route the sample markers (big pins over the
  // line) let the mouse through, so the line can be grabbed anywhere
  function samplesPassThrough(map, on) {
    if (!map) return;
    map.eachLayer(function(l) {
      if (l.options && l.options.group === 'edit_samples' && l._icon) l._icon.style.pointerEvents = on ? 'none' : '';
    });
  }
  function clearRouteEdit(map) {
    samplesPassThrough(map, false);
    RE.hit = null;
    if (RE.layer) RE.layer.clearLayers();
    RE.dragging = false;
    if (map) { syncDragging(map); map.doubleClickZoom.enable(); }
  }

  Shiny.addCustomMessageHandler('route_edit', function(msg) {
    var map = getMap();
    if (!map) return;
    if (!RE.layer) RE.layer = L.layerGroup().addTo(map);
    if (msg.scope !== RE.scope && !msg.on && RE.scope !== null) return;  // another tab's handles stay
    clearRouteEdit(map);
    RE.scope = msg.scope;
    RE.on = !!msg.on;
    if (!RE.on) return;
    map.doubleClickZoom.disable();
    if (RE.scope === 'edit') samplesPassThrough(map, true);
    RE.fixedEnds = !!msg.fixed_ends;
    RE.info = {};
    RE.ctrl = (msg.ctrl || []).map(function(c) {
      var ll = L.latLng(c.lat, c.lng);
      ll.id = String(c.id);
      RE.info[ll.id] = c;
      return ll;
    });
    var path = (msg.path || []).map(function(p) { return L.latLng(p[0], p[1]); });
    if (path.length < 2) return;
    RE.path = path;
    RE.cum = pathCum(path);
    RE.windowM = msg.window_m || 300;
    RE.rules = !!msg.rules;
    RE.labels = msg.labels || {};
    RE.legs = (msg.legs || []).map(function(l) {
      return {key: l.key, mode: l.mode, path: (l.path || []).map(function(p) { return L.latLng(p[0], p[1]); })};
    });
    var splices = RE.scope === 'review';
    if (splices) pinCtrl().forEach(function(c) { c.d = projectOnPath(map, c); });
    // A wide invisible line to grab the route anywhere
    var hit = L.polyline(path, {color: '#000', weight: 18, opacity: 0.01, className: 'route-hit', bubblingMouseEvents: false});
    hit.addTo(RE.layer);
    RE.hit = hit;
    hit.on('mousedown', function(e) {
      if (e.originalEvent.button !== 0) return;
      L.DomEvent.stop(e);
      RE.dragging = true;
      syncDragging(map);
      var start = e.containerPoint, moved = false, leg = nearestLeg(map, e.latlng);
      // Batch review: a local splice between anchors around the grab point
      var gd = splices ? projectOnPath(map, e.latlng) : null;
      var grab = splices ? pointAt(gd) : null;
      var ends = splices ? spliceAnchors(gd) : [RE.ctrl[leg], RE.ctrl[leg + 1]];
      var ghost = L.circleMarker(e.latlng, {radius: 6, color: '#fff', weight: 2, fillColor: '#37474F', fillOpacity: 1,
                                            interactive: false}).addTo(RE.layer);
      var preview = L.polyline([ends[0], e.latlng, ends[1]], previewStyle()).addTo(RE.layer);
      function move(ev) {
        if (!moved && ev.containerPoint.distanceTo(start) < 5) return;
        moved = true;
        ghost.setLatLng(ev.latlng);
        preview.setLatLngs([ends[0], ev.latlng, ends[1]]);
      }
      function up(ev) {
        map.off('mousemove', move);
        map.off('mouseup', up);
        $(document).off('mouseup.routeedit');
        RE.dragging = false;
        syncDragging(map);
        RE.layer.removeLayer(ghost);
        RE.layer.removeLayer(preview);
        if (moved && ev && ev.latlng) {
          var v = {kind: 'line', lat: ev.latlng.lat, lng: ev.latlng.lng};
          if (grab) { v.glat = grab.lat; v.glng = grab.lng; }
          send(prefix(RE.scope) + '_route_drag', v);
        }
      }
      map.on('mousemove', move);
      map.on('mouseup', up);
      $(document).on('mouseup.routeedit', function() { if (!moved) up(null); });
    });
    // Right-click on a spliced stretch: how that leg is drawn (Batch review)
    hit.on('contextmenu', function(e) {
      L.DomEvent.stop(e);
      if (!splices) return;
      var l = nearestSplicedLeg(map, e.latlng);
      L.popup({closeButton: false, className: 'leg-menu-popup', autoPanPadding: [20, 20]})
        .setLatLng(e.latlng).setContent(l ? legMenu(l) : '<div class=\"leg-menu-hint\">' + esc(RE.labels.leg_hint || '') + '</div>')
        .openOn(map);
    });
    // Draggable start / end (Batch review)
    if (!RE.fixedEnds) {
      ['start', 'end'].forEach(function(kind) {
        var ll = kind === 'start' ? RE.ctrl[0] : RE.ctrl[RE.ctrl.length - 1];
        var nb = kind === 'start' ? RE.ctrl[1] : RE.ctrl[RE.ctrl.length - 2];
        if (splices) {  // up to an anchor window_m along the path, or the nearest pin
          var total = RE.cum[RE.cum.length - 1], pd = pinCtrl().map(function(c) { return c.d; });
          nb = kind === 'start' ? pointAt(Math.min.apply(null, [RE.windowM].concat(pd))) :
            pointAt(Math.max.apply(null, [total - RE.windowM].concat(pd)));
        }
        var m = L.marker(ll, {draggable: true, icon: endIcon(kind), zIndexOffset: 1000, keyboard: false});
        m.options.routeEnd = kind;
        var preview = null;
        m.on('dragstart', function() { preview = L.polyline([nb, ll], previewStyle()).addTo(RE.layer); });
        m.on('drag', function() { if (preview) preview.setLatLngs([nb, m.getLatLng()]); });
        m.on('dragend', function() {
          if (preview) RE.layer.removeLayer(preview);
          var p = m.getLatLng();
          send(prefix(RE.scope) + '_route_drag', {kind: kind, lat: p.lat, lng: p.lng});
        });
        m.on('click dblclick', function(e) { L.DomEvent.stop(e); });
        m.addTo(RE.layer);
      });
    }
  });

  // Pin markers (Batch review: review_pins; Edit Samples: edit_pins): drag with a preview,
  // re-route on drop; right-click or double-click removes
  window.bindPinMarker = function(layer, pfx) {
    var pinId = String(layer.options.layerId).replace(/^pin_/, '');
    layer.off('dragstart drag dragend contextmenu dblclick click');
    var preview = null;
    function neighbours() {
      var c = RE.info[pinId];
      if (pfx === 'review_pin' && c && c.prev && c.next) return [L.latLng(c.prev.lat, c.prev.lng), L.latLng(c.next.lat, c.next.lng)];
      var k = -1;
      for (var i = 0; i < RE.ctrl.length; i++) if (RE.ctrl[i].id === pinId) k = i;
      return k > 0 && k + 1 < RE.ctrl.length ? [RE.ctrl[k - 1], RE.ctrl[k + 1]] : null;
    }
    layer.on('dragstart', function() {
      var nb = neighbours();
      if (nb && RE.layer) preview = L.polyline([nb[0], layer.getLatLng(), nb[1]], previewStyle()).addTo(RE.layer);
    });
    layer.on('drag', function() {
      var nb = neighbours();
      if (preview && nb) preview.setLatLngs([nb[0], layer.getLatLng(), nb[1]]);
    });
    layer.on('dragend', function() {
      if (preview && RE.layer) RE.layer.removeLayer(preview);
      preview = null;
      var ll = layer.getLatLng();
      send(pfx + '_moved', {id: pinId, lat: ll.lat, lng: ll.lng});
    });
    function remove(e) {
      L.DomEvent.stop(e);
      send(pfx + '_remove', {id: pinId});
    }
    layer.on('contextmenu', remove);
    layer.on('dblclick', remove);
    // Click: the pin's popup (rules on official trips, a splice warning); never re-routes
    layer.on('click', function(e) {
      L.DomEvent.stop(e);
      if (pfx !== 'review_pin') return;
      var html = pinPopup(pinId), map = getMap();
      if (html && map) L.popup({className: 'pin-popup', offset: [0, -30]}).setLatLng(layer.getLatLng()).setContent(html).openOn(map);
    });
  };

  // ---- Pin popup and leg menu (Batch review) ----
  function pinPopup(pinId) {
    var c = RE.info[pinId], t = RE.labels;
    if (!c || (!RE.rules && !c.warn)) return null;
    function group(rule, opts, cur) {
      return '<div class=\"btn-group btn-group-sm pin-rule-group\" role=\"group\">' + opts.map(function(o) {
        return '<button type=\"button\" class=\"btn ' + (o[0] === cur ? 'btn-primary' : 'btn-outline-secondary') +
          ' pin-rule\" data-pin=\"' + esc(pinId) + '\" data-rule=\"' + rule + '\" data-value=\"' + o[0] + '\">' + esc(o[1]) + '</button>';
      }).join('') + '</div>';
    }
    var html = '<div class=\"pin-rules\" data-pin=\"' + esc(pinId) + '\">';
    if (RE.rules) {
      html += '<div class=\"pin-rule-title\">' + esc(t.ww) + '</div>' +
        group('ww', [['no', t.ww_no], ['minor', t.ww_minor], ['all', t.ww_all]], c.ww || 'minor') +
        '<div class=\"pin-rule-title\">' + esc(t.turns) + '</div>' +
        group('turns', [['respect', t.turns_respect], ['ignore', t.turns_ignore]], c.turns || 'respect');
    }
    if (c.warn) html += '<div class=\"pin-rule-warn\">&#9888; ' + esc(t.warn) + '</div>';
    return html + '</div>';
  }
  function nearestSplicedLeg(map, ll) {
    var p = map.latLngToLayerPoint(ll), best = null, bestD = 15;
    RE.legs.forEach(function(l) {
      for (var i = 0; i + 1 < l.path.length; i++) {
        var d = L.LineUtil.pointToSegmentDistance(p, map.latLngToLayerPoint(l.path[i]), map.latLngToLayerPoint(l.path[i + 1]));
        if (d < bestD) { bestD = d; best = l; }
      }
    });
    return best;
  }
  function legMenu(l) {
    var t = RE.labels;
    return '<div class=\"leg-menu\">' + [['route', t.route], ['recorded', t.recorded], ['straight', t.straight]].map(function(o) {
      return '<button type=\"button\" class=\"leg-mode' + (o[0] === l.mode ? ' active' : '') + '\" data-leg=\"' + esc(l.key) +
        '\" data-mode=\"' + o[0] + '\">' + (o[0] === l.mode ? '&#10003; ' : '') + esc(o[1]) + '</button>';
    }).join('') + '</div>';
  }
  $(document).on('click', '.pin-rule', function() {
    var b = $(this), map = getMap();
    send('review_pin_rule', {id: String(b.data('pin')), rule: String(b.data('rule')), value: String(b.data('value'))});
    if (map) map.closePopup();
  });
  $(document).on('click', '.leg-mode', function() {
    var b = $(this), map = getMap();
    send('review_leg_mode', {key: String(b.attr('data-leg')), mode: String(b.data('mode'))});
    if (map) map.closePopup();
  });

  // ---- Lasso (Batch review and Edit Samples) ----
  var LASSO = {review: false, edit: false, drawing: false, applied: false, scope: null, shift: false, svg: null, pts: [],
               noClickUntil: 0};
  window.__lasso = LASSO;  // read by the browser tests
  function pointInPolygon(x, y, poly) {
    var inside = false;
    for (var i = 0, j = poly.length - 1; i < poly.length; j = i++) {
      var xi = poly[i].x, yi = poly[i].y, xj = poly[j].x, yj = poly[j].y;
      if (((yi > y) !== (yj > y)) && (x < (xj - xi) * (y - yi) / (yj - yi) + xi)) inside = !inside;
    }
    return inside;
  }
  function lassoScope() { var m = mode(); return m === 'BatchReview' ? 'review' : (m === 'EditSamples' ? 'edit' : null); }
  function lassoOn() { var sc = lassoScope(); return !!(sc && LASSO[sc]); }
  // Map dragging: off while the tab's lasso mode is on, a lasso is drawn or the
  // route is dragged; back on in the review tabs (elsewhere only if turned off here)
  function syncDragging(map) {
    map = map || getMap();
    if (!map) return;
    var off = lassoOn() || LASSO.drawing || RE.dragging;
    if (off) map.dragging.disable();
    else if (lassoScope() || LASSO.applied) map.dragging.enable();
    LASSO.applied = off;
    $('#map').toggleClass('lasso-mode', lassoOn());
    $('#edit_lasso_toggle').toggleClass('lasso-on btn-primary', LASSO.edit).toggleClass('btn-outline-secondary', !LASSO.edit);
  }
  window.syncMapDragging = syncDragging;
  Shiny.addCustomMessageHandler('lasso_mode', function(msg) {
    LASSO.review = !!msg.on;
    syncDragging();
  });
  function setEditLasso(on) {
    LASSO.edit = !!on;
    if (!LASSO.edit && window.editLassoOff) window.editLassoOff();  // turning it off clears the selection, as before
    syncDragging();
  }
  $(document).on('click', '#edit_lasso_toggle', function(e) { e.preventDefault(); setEditLasso(!LASSO.edit); });
  $(document).on('shiny:inputchanged', function(e) { if (e.name === 'mode') setTimeout(function() { syncDragging(); }, 0); });

  // A drag from here is not a lasso: a control, a popup, a route end, the editable route line, a pin
  function notLassoTarget(map, t) {
    if ($(t).closest('.leaflet-control, .leaflet-popup, .route-end, .route-hit').length) return true;
    var pin = false;
    map.eachLayer(function(l) {
      if (l._icon && (l._icon === t || l._icon.contains(t)) && l.options && /_pins$/.test(l.options.group || '')) pin = true;
    });
    return pin;
  }
  // The scope a mouse press starts a lasso in, or null
  function lassoStartScope(e) {
    if (e.button !== 0 || LASSO.drawing) return null;
    var sc = lassoScope(), map = getMap();
    if (!sc || !map || !(LASSO[sc] || e.shiftKey)) return null;
    return notLassoTarget(map, e.target) ? null : sc;
  }
  function finishLasso(cancel) {
    $(document).off('.lassodraw');
    if (LASSO.svg) LASSO.svg.remove();
    LASSO.svg = null;
    var map = getMap(), sc = LASSO.scope, pts = LASSO.pts;
    LASSO.drawing = false;
    LASSO.noClickUntil = Date.now() + 400;   // the click that ends a drag is not a map click
    syncDragging(map);
    if (cancel || pts.length < 3 || !map) return;
    if (sc === 'review') {
      var ids = [];
      map.eachLayer(function(layer) {
        if (!layer.options || layer.options.group !== 'review_samples' || !layer.options.layerId || !layer.getLatLng) return;
        var p = map.latLngToContainerPoint(layer.getLatLng());
        if (pointInPolygon(p.x, p.y, pts)) ids.push(String(layer.options.layerId));
      });
      if (ids.length) send('review_lasso_samples', {ids: ids, shift: LASSO.shift});
    } else if (sc === 'edit' && window.editLassoSelect) {
      window.editLassoSelect(pts);
    }
  }
  function startLasso(sc, e) {
    var map = getMap(), $m = $('#map'), off = $m.offset();
    LASSO.drawing = true;
    LASSO.scope = sc;
    LASSO.shift = !LASSO[sc];
    LASSO.pts = [{x: e.pageX - off.left, y: e.pageY - off.top}];
    syncDragging(map);
    if (LASSO.svg) LASSO.svg.remove();
    LASSO.svg = $('<svg class=\"selection-lasso\"><polyline points=\"\"></polyline></svg>').css({
      position: 'fixed', left: off.left + 'px', top: off.top + 'px', width: $m.width() + 'px', height: $m.height() + 'px'
    });
    $('body').append(LASSO.svg);
    $(document).on('mousemove.lassodraw', function(ev) {
      LASSO.pts.push({x: ev.pageX - off.left, y: ev.pageY - off.top});
      LASSO.svg.find('polyline').attr('points', LASSO.pts.map(function(p) { return p.x + ',' + p.y; }).join(' '));
    });
    $(document).on('mouseup.lassodraw', function() { finishLasso(false); });
  }
  // Capture phase on the map: Leaflet (map pan, marker drags) never sees a lasso drag
  function hookLasso(map) {
    var el = map.getContainer();
    if (el._lassoHook) return;
    el._lassoHook = true;
    map.boxZoom.disable();   // Shift + drag is the lasso
    el.addEventListener('pointerdown', function(e) {
      if (e.pointerType === 'mouse' && lassoStartScope(e)) e.stopPropagation();
    }, true);
    el.addEventListener('mousedown', function(e) {
      if (LASSO.drawing) { e.preventDefault(); e.stopPropagation(); return; }
      var sc = lassoStartScope(e);
      if (!sc) return;
      e.preventDefault();
      e.stopPropagation();
      startLasso(sc, e);
    }, true);
    el.addEventListener('click', function(e) {
      if (Date.now() < LASSO.noClickUntil) { e.preventDefault(); e.stopPropagation(); }
    }, true);
  }
  // Shift held over the map: the lasso cursor
  $(document).on('keydown keyup', function(e) {
    if (e.key === 'Shift') $('#map').toggleClass('lasso-shift', e.type === 'keydown' && !!lassoScope());
  });

  // ---- Station pick (Batch review, rail trips) ----
  var SP = {which: null, stations: null, lang: null, layer: null, down: null};
  window.__stationPick = SP;  // read by the browser tests
  function spLabels() {
    var map = getMap();
    if (!map || !SP.layer) return;
    var show = map.getZoom() >= 13, view = map.getBounds().pad(0.1), n = 0;
    SP.layer.eachLayer(function(m) {
      var on = show && n < 250 && view.contains(m.getLatLng());
      if (on) n++;
      if (on && !m.getTooltip()) {
        m.bindTooltip(m.options.stationLabel, {permanent: true, direction: 'right', offset: [5, 0], className: 'station-label',
                                               interactive: false});
      } else if (!on && m.getTooltip()) {
        m.unbindTooltip();
      }
    });
  }
  function spOff() {
    var map = getMap();
    SP.which = null;
    if (map) {
      map.off('zoomend moveend', spLabels);
      if (SP.layer) map.removeLayer(SP.layer);
    }
    SP.layer = null;
    $('#map').removeClass('station-pick');
    $('[data-station-pick]').removeClass('pick-on');
  }
  function spDraw() {
    var map = getMap(), s = SP.stations;
    if (!map || !s || SP.layer || !SP.which) return;
    SP.layer = L.layerGroup();
    for (var i = 0; i < s.key.length; i++) {
      L.circleMarker([s.lat[i], s.lng[i]], {radius: 4, color: '#4A148C', weight: 1.5, fillColor: '#fff', fillOpacity: 1,
                                             interactive: false, stationLabel: s.label[i]}).addTo(SP.layer);
    }
    SP.layer.addTo(map);
    spLabels();
    map.on('zoomend moveend', spLabels);
  }
  function spArm(which) {
    spOff();
    SP.which = which;
    $('#map').addClass('station-pick');
    $('[data-station-pick=\"' + which + '\"]').addClass('pick-on');
    if (SP.stations && SP.lang === window.APP_LANG) spDraw(); else send('review_station_list', {which: which});
  }
  $(document).on('click', '[data-station-pick]', function(e) {
    e.preventDefault();
    var w = $(this).attr('data-station-pick');
    if (SP.which === w) spOff(); else spArm(w);
  });
  Shiny.addCustomMessageHandler('rail_stations', function(msg) {
    SP.stations = msg;
    SP.lang = msg.lang;
    spDraw();
  });
  Shiny.addCustomMessageHandler('station_pick_off', function(msg) { spOff(); });
  // The detail re-renders: the armed pin keeps its highlight; another tab cancels
  $(document).on('shiny:value', function(e) {
    if (e.name === 'review_detail' && SP.which) {
      setTimeout(function() { $('[data-station-pick=\"' + SP.which + '\"]').addClass('pick-on'); }, 0);
    }
  });
  $(document).on('shiny:inputchanged', function(e) { if (e.name === 'mode' && SP.which) spOff(); });
  // Clicks on the map while armed: the nearest station within 300 m (not a pan)
  function hookStationPick(map) {
    var el = map.getContainer();
    if (el._stationPickHook) return;
    el._stationPickHook = true;
    el.addEventListener('mousedown', function(e) { SP.down = {x: e.clientX, y: e.clientY}; }, true);
    el.addEventListener('click', function(e) {
      if (!SP.which || !SP.stations || $(e.target).closest('.leaflet-control, .leaflet-popup').length) return;
      e.preventDefault();
      e.stopPropagation();
      if (SP.down && Math.abs(e.clientX - SP.down.x) + Math.abs(e.clientY - SP.down.y) > 6) return;
      var ll = map.mouseEventToLatLng(e), s = SP.stations, best = -1, bestD = Infinity;
      for (var i = 0; i < s.key.length; i++) {
        var d = ll.distanceTo([s.lat[i], s.lng[i]]);
        if (d < bestD) { bestD = d; best = i; }
      }
      if (best >= 0 && bestD <= 300) {
        send('review_station_pick', {which: SP.which, key: s.key[best]});
        spOff();
      } else {
        L.popup({closeButton: false, className: 'station-pick-hint'}).setLatLng(ll).setContent(esc(s.none || '')).openOn(map);
      }
    }, true);
  }

  // ---- Keys: Ctrl+Z undoes route edits; L toggles the lasso, Esc leaves it (or the station pick) ----
  $(document).on('keydown', function(e) {
    var m = mode();
    if (m !== 'BatchReview' && m !== 'EditSamples') return;
    if ($(e.target).is('input, textarea, select, [contenteditable]') || $('.modal.show').length) return;
    var key = String(e.key).toLowerCase();
    if ((e.ctrlKey || e.metaKey) && !e.shiftKey && key === 'z') {
      e.preventDefault();
      send('route_undo', {scope: m === 'EditSamples' ? 'edit' : 'review'});
    } else if (!e.ctrlKey && !e.metaKey && !e.altKey && key === 'l') {
      e.preventDefault();
      if (m === 'BatchReview') $('#review_lasso_toggle').click(); else setEditLasso(!LASSO.edit);
    } else if (key === 'escape') {
      if (SP.which) { spOff(); return; }
      if (LASSO.drawing) finishLasso(true);
      if (m === 'BatchReview' && LASSO.review) $('#review_lasso_toggle').click();
      else if (m === 'EditSamples' && LASSO.edit) setEditLasso(false);
    }
  });

  // ---- Edit Samples: samples kept out of the Snap routing look grey ----
  window.EDIT_EXCLUDED = [];
  function paintExcluded(map) {
    if (!map) return;
    map.eachLayer(function(layer) {
      if (layer.options && layer.options.group === 'edit_samples' && layer._icon) {
        $(layer._icon).toggleClass('sample-excluded', window.EDIT_EXCLUDED.indexOf(String(layer.options.layerId)) !== -1);
      }
    });
  }
  Shiny.addCustomMessageHandler('edit_excluded', function(msg) {
    window.EDIT_EXCLUDED = (msg.ids || []).map(String);
    paintExcluded(getMap());
  });

  // ---- Overlays ----
  var BUS = {req: null, layer: null, arrows: null};
  var ROUTER = {url: null, gl: null};
  var BUS_COLORS = {busway: '#7B1FA2', bus_only: '#7B1FA2', bus_lane: '#AB47BC', bus_ok: '#9575CD', blocked: '#757575'};
  function esc(s) { return String(s).replace(/[&<>\"]/g, function(c) { return {'&': '&amp;', '<': '&lt;', '>': '&gt;', '\"': '&quot;'}[c]; }); }
  function busPopup(p) {
    var rows = Object.keys(p.tags || {}).sort().map(function(k) {
      return '<tr><td>' + esc(k) + '</td><td>' + esc(p.tags[k]) + '</td></tr>';
    }).join('');
    return '<div class=\"bus-popup\"><b>' + esc(p.name || '') + '</b> <a href=\"https://www.openstreetmap.org/way/' + p.id +
      '\" target=\"_blank\" rel=\"noopener\">way ' + p.id + '</a><table>' + rows + '</table></div>';
  }
  function busArrows(map, features) {
    BUS.arrows.clearLayers();
    if (map.getZoom() < 15) return;
    features.forEach(function(f) {
      var ow = f.properties.oneway;
      if (!ow || f.properties.kind === 'blocked') return;
      var c = f.geometry.coordinates, acc = 0, next = 30;
      for (var i = 0; i + 1 < c.length; i++) {
        var a = L.latLng(c[i][1], c[i][0]), b = L.latLng(c[i + 1][1], c[i + 1][0]);
        var d = a.distanceTo(b);
        while (d > 0 && acc + d >= next) {
          var f2 = (next - acc) / d, ll = L.latLng(a.lat + f2 * (b.lat - a.lat), a.lng + f2 * (b.lng - a.lng));
          var pa = map.latLngToLayerPoint(a), pb = map.latLngToLayerPoint(b);
          var deg = Math.atan2(pb.y - pa.y, pb.x - pa.x) * 180 / Math.PI + (ow < 0 ? 180 : 0);
          L.marker(ll, {interactive: false, keyboard: false, icon: L.divIcon({className: 'bus-arrow', iconSize: [12, 12], iconAnchor: [6, 6],
            html: '<span style=\"transform: rotate(' + deg + 'deg); color: ' + BUS_COLORS[f.properties.kind] + '\">&#10148;</span>'})}).addTo(BUS.arrows);
          next += 150;
        }
        acc += d;
      }
    });
  }
  function busRequest(map) {
    var b = map.getBounds();
    send('bus_infra_req', {w: b.getWest(), s: b.getSouth(), e: b.getEast(), n: b.getNorth(), zoom: map.getZoom()});
  }
  Shiny.addCustomMessageHandler('bus_infra_data', function(msg) {
    var map = getMap();
    if (!map || !BUS.layer) return;
    BUS.layer.clearLayers();
    BUS.layer.addData(msg.features || []);
    busArrows(map, msg.features || []);
  });
  function routerStyle(url) {
    var color = ['interpolate', ['linear'], ['to-number', ['get', 'speed'], 0],
                 5, '#B71C1C', 20, '#F4511E', 35, '#FDD835', 50, '#7CB342', 80, '#2E7D32', 110, '#1565C0'];
    return {
      version: 8,
      glyphs: 'https://tiles.openfreemap.org/fonts/{fontstack}/{range}.pbf',
      sources: {osrm: {type: 'vector', tiles: [url], minzoom: 14, maxzoom: 18}},
      layers: [
        {id: 'speeds', type: 'line', source: 'osrm', 'source-layer': 'speeds', minzoom: 14,
         paint: {'line-color': color, 'line-width': 2.5, 'line-offset': 2,
                 'line-opacity': ['case', ['to-boolean', ['get', 'is_small']], 0.4, 0.9]}},
        {id: 'arrows', type: 'symbol', source: 'osrm', 'source-layer': 'speeds', minzoom: 15,
         layout: {'symbol-placement': 'line', 'symbol-spacing': 90, 'text-field': '\\u203A', 'text-size': 15,
                  'text-font': ['Noto Sans Regular'], 'text-keep-upright': false, 'text-rotation-alignment': 'map',
                  'text-allow-overlap': true, 'text-offset': [0, 0.15]},
         paint: {'text-color': color, 'text-halo-color': '#FFFFFF', 'text-halo-width': 1}}
      ]
    };
  }
  Shiny.addCustomMessageHandler('router_profile', function(msg) {
    ROUTER.url = msg.url;
    var gl = ROUTER.gl && ROUTER.gl.getMaplibreMap && ROUTER.gl.getMaplibreMap();
    if (gl && gl.getSource && gl.getSource('osrm')) gl.getSource('osrm').setTiles([msg.url]);
  });
  window.setupOverlays = function(map) {
    if (map._overlaysHook) return;
    map._overlaysHook = true;
    var lm = map.layerManager;
    var busGroup = lm.getLayerGroup('Bus infrastructure', true);
    var routerGroup = lm.getLayerGroup('Router network', true);
    map.on('overlayadd', function(e) {
      if (e.layer === busGroup) {
        if (!BUS.layer) {
          BUS.layer = L.geoJSON(null, {
            style: function(f) {
              var k = f.properties.kind;
              return {color: BUS_COLORS[k] || '#7B1FA2', weight: k === 'blocked' ? 2 : (k === 'bus_lane' || k === 'bus_ok' ? 3 : 4),
                      opacity: k === 'blocked' ? 0.8 : 0.9, dashArray: k === 'blocked' ? '5 5' : null};
            },
            bubblingMouseEvents: false,
            onEachFeature: function(f, l) { l.bindPopup(busPopup(f.properties), {maxWidth: 360}); }
          });
          BUS.arrows = L.layerGroup();
          busGroup.addLayer(BUS.layer);
          busGroup.addLayer(BUS.arrows);
        }
        busRequest(map);
      } else if (e.layer === routerGroup && !ROUTER.gl && ROUTER.url && typeof L.maplibreGL === 'function') {
        ROUTER.gl = L.maplibreGL({style: routerStyle(ROUTER.url), interactive: false, pane: 'overlayPane'});
        routerGroup.addLayer(ROUTER.gl);
      }
    });
    var timer = null;
    map.on('moveend', function() {
      if (!map.hasLayer(busGroup) || !BUS.layer) return;
      clearTimeout(timer);
      timer = setTimeout(function() { busRequest(map); }, 250);
    });
  };

  // Hook the map once it exists: overlays, excluded-sample paint after redraws
  function hook() {
    var map = getMap();
    if (!map) { setTimeout(hook, 500); return; }
    window.setupOverlays(map);
    hookLasso(map);
    hookStationPick(map);
    map.on('layeradd', function(e) {
      var l = e.layer;
      // The route's grab line stays above the lines the server redraws after a re-route
      if (RE.hit && l !== RE.hit && l instanceof L.Path) setTimeout(function() { if (RE.hit) RE.hit.bringToFront(); }, 0);
      if (l.options && l.options.group === 'edit_samples') {
        if (window.EDIT_EXCLUDED.length) setTimeout(function() { paintExcluded(map); }, 0);
        if (RE.on && RE.scope === 'edit') setTimeout(function() { if (l._icon) l._icon.style.pointerEvents = 'none'; }, 0);
      }
    });
  }
  $(document).on('shiny:connected', function() { setTimeout(hook, 600); });
})();
"
