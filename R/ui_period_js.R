# ---- Client: Period review (Edit > Period) ----
# "period_data" (server_period.R) brings every trip and visit of the period,
# each flagged for the two views (now: as Arc shows it; rec: as recorded).
# All drawing is here, on a canvas renderer (a month is hundreds of lines):
# the lines and visits of the chosen view on the map (only in Period mode),
# the list by day, the selected item's detail. A click on a line, a visit or
# a row selects it; Up / Down step through the list; a day's header zooms to
# that day. "Edit" on a trip of a routed mode (car, taxi, bus, rail, none)
# sends period_review (Batch review, that trip selected); on the rest, and
# with a neighbour, period_open (Edit samples: the item's start / end, or with
# its neighbour). A panel on the map (also in full screen) turns each mode on or
# off, asks "when was I here?" (a point and a radius: the visits and the trips
# passing by) and toggles full screen; the search above the list finds the
# visits to a place by name. The list then shows only the matches: when.
PERIOD_JS <- "
(function() {
  var P = {items: [], idx: {}, tz: undefined, view: 'now', sel: null, layer: null, renderer: null, shapes: {},
           off: {}, q: null, hits: null, picking: false, r: 100, qLayer: null, ctl: null, rsReopen: false, from: null, to: null,
           tab: 'list', gaps: [], taps: [], jae: null};
  window.__period = P;  // read by the browser tests
  function getMap() { var w = HTMLWidgets.find('#map'); return w && w.getMap(); }
  function mode() { return $('#mode input:checked').val(); }
  function lang() { return window.APP_LANG || 'pt'; }
  function i18nText(key) { var e = window.I18N && window.I18N[key]; return (e && e[lang()]) || key; }
  function esc(s) { return String(s).replace(/[&<>\"']/g, function(c) { return {'&': '&amp;', '<': '&lt;', '>': '&gt;', '\"': '&quot;', \"'\": '&#39;'}[c]; }); }
  // Date formatters are costly to build: one per language, kind and time zone (a year is thousands of rows)
  var FMT = {};
  function formatter(locale, kind, opts) {
    var key = locale + '|' + kind + '|' + (P.tz || '');
    if (!FMT[key]) {
      var o = Object.assign({}, opts);
      if (P.tz) o.timeZone = P.tz;
      FMT[key] = new Intl.DateTimeFormat(locale, o);
    }
    return FMT[key];
  }
  function fmt(ts, kind, opts) { return formatter(lang() === 'pt' ? 'pt-BR' : 'en-GB', kind, opts).format(new Date(ts * 1000)); }
  function hm(ts) { return fmt(ts, 'hm', {hour: '2-digit', minute: '2-digit', hourCycle: 'h23'}); }
  function dayKey(ts) { return formatter('en-CA', 'day', {year: 'numeric', month: '2-digit', day: '2-digit'}).format(new Date(ts * 1000)); }
  function dayLabel(ts) { return fmt(ts, 'dayLabel', {weekday: 'short', day: '2-digit', month: '2-digit'}); }
  function inView(it) { return P.view === 'now' ? it.now : it.rec; }
  function modeKey(it) { return it.k === 'v' ? 'visit' : it.k === 'j' ? 'tap' : (it.m === 'bogus' ? 'unknown' : it.m); }   // both are 'No mode'
  // In the view, its mode on, and a match of the place query (when there is one)
  function shown(it) { return inView(it) && !P.off[modeKey(it)] && (!P.hits || !!P.hits[it.id]); }
  // (the Jaé query of taps without a trip lists taps, not items)
  function list() { return (P.q && P.q.type === 'jae' && P.q.kind === 'tap' ? P.taps : P.items).filter(shown); }
  function viewList() { return P.items.filter(inView); }
  function byId(id) { return P.idx[id] || null; }
  function what(it) {
    if (it.k === 'v') return it.nm || i18nText('Visit');
    if (it.k === 'j') return 'Ja\\u00e9 \\u00b7 ' + it.lb;
    return i18nText(it.lb) + (it.km != null && it.km >= 0.05 ? ' \\u00b7 ' + it.km.toFixed(1) + ' km' : '');
  }
  function hiddenNow(it) { return it.rec && !it.now; }

  // on: the selected item; with a selection, the others fade
  function style(it, on) {
    var faded = !on && !!P.sel;
    if (it.k === 'v' || it.k === 'j') return {radius: on ? 9 : (it.k === 'j' ? 6 : 5), color: on ? '#000' : '#fff', weight: on ? 2.5 : 1.5,
                              fillColor: it.k === 'j' ? '#7B1FA2' : '#6D4C41', fillOpacity: faded ? 0.25 : 0.9, opacity: faded ? 0.3 : 1};
    return {color: it.c, weight: on ? 7 : 3, opacity: on ? 1 : (faded ? 0.18 : 0.75)};
  }
  function restyle() {
    Object.keys(P.shapes).forEach(function(id) { var it = byId(id); if (it) P.shapes[id].setStyle(style(it, id === P.sel)); });
    if (P.sel && P.shapes[P.sel] && P.shapes[P.sel].bringToFront) P.shapes[P.sel].bringToFront();
  }
  function tip(it) {
    return esc(it.ic + ' ' + what(it) + ' \\u00b7 ' + dayLabel(it.s) + ' ' + hm(it.s) + '\\u2013' + hm(it.e) +
               (it.g ? ' \\u00b7 GPX' : '') + (P.view === 'rec' && hiddenNow(it) ? ' \\u00b7 ' + i18nText('hidden in Arc') : ''));
  }

  function draw() {
    var map = getMap();
    if (!map) return;
    if (P.layer) map.removeLayer(P.layer);
    if (!P.renderer) P.renderer = L.canvas({padding: 0.5, tolerance: 4});
    P.layer = L.featureGroup();
    P.shapes = {};
    list().forEach(function(it) {
      var sh = null;
      if (it.k === 't' && it.p && it.p.length > 1) sh = L.polyline(it.p, Object.assign({renderer: P.renderer}, style(it, false)));
      else if ((it.k === 'v' || it.k === 'j') && it.ll) sh = L.circleMarker(it.ll, Object.assign({renderer: P.renderer}, style(it, false)));
      if (!sh) return;
      sh.bindTooltip(tip(it), {sticky: true});
      sh.on('click', function(e) {
        L.DomEvent.stopPropagation(e);
        if (P.picking) setPointQuery(e.latlng); else select(it.id, 'map');
      });
      sh.addTo(P.layer);
      P.shapes[it.id] = sh;
    });
    if (P.sel && !P.shapes[P.sel] && !(byId(P.sel) && shown(byId(P.sel)))) P.sel = null;
    if (mode() === 'Period') P.layer.addTo(map);
    restyle();
  }

  // ---- The list area: List, Places, Numbers or Sends (P.tab) ----
  function dur(sec) {
    var m = Math.round(sec / 60);
    if (m < 60) return m + ' min';
    var h = Math.floor(m / 60), d = Math.floor(h / 24);
    if (d >= 1) return d + ' d ' + (h % 24) + ' h';
    return h + ' h ' + (m % 60 < 10 ? '0' : '') + (m % 60);
  }
  function longDay(ts) { return fmt(ts, 'longDay', {day: '2-digit', month: '2-digit', year: '2-digit'}); }
  // Gaps: stretches of the view (all modes) with no item for GAP_MIN_S or more, incl. the period's edges
  var GAP_MIN_S = 600;
  function computeGaps() {
    P.gaps = [];
    Object.keys(P.idx).forEach(function(k) { if (k.indexOf('gap:') === 0) delete P.idx[k]; });
    if (!P.from) return;
    var items = viewList(), start = boundTs(P.from, false), end = Math.min(boundTs(P.to, true), Date.now() / 1000);
    var reach = start, prev = null;
    items.concat([{s: end, id: null, end: true}]).forEach(function(it) {
      if (it.s - reach >= GAP_MIN_S) {
        var g = {id: 'gap:' + reach, k: 'g', s: reach, e: it.s, a: prev && prev.id, b: it.end ? null : it.id, ic: '\\u26A0\\uFE0F'};
        g.dk = dayKey(g.s);
        P.gaps.push(g);
        P.idx[g.id] = g;
      }
      if (!it.end) { reach = Math.max(reach, it.e); prev = it; }
    });
  }
  // A local day's start / end as epoch s (the period's dates, in its time zone)
  function boundTs(day, end) {
    var guess = Date.parse(day + 'T' + (end ? '23:59:59' : '00:00:00') + 'Z') / 1000;
    var parts = formatter('en-CA', 'offset', {hour: '2-digit', hourCycle: 'h23', day: '2-digit'}).formatToParts(new Date(guess * 1000));
    var localH = Number((parts.find(function(p) { return p.type === 'hour'; }) || {}).value);
    var utcH = new Date(guess * 1000).getUTCHours(), off = localH - utcH;
    if (off > 12) off -= 24;
    if (off < -12) off += 24;
    return guess - off * 3600;
  }
  function gapsShown() { return P.tab === 'list' && !P.hits && !P.off.gap; }
  function listHtml() {
    var items = list(), rows = items, html = [], day = null, perDay = {};
    items.forEach(function(it) { perDay[it.dk] = (perDay[it.dk] || 0) + 1; });
    if (gapsShown() && P.gaps.length) rows = items.concat(P.gaps).sort(function(a, b) { return a.s - b.s || (a.k === 'g' ? -1 : 1); });
    rows.forEach(function(it) {
      var d = it.dk;
      if (d !== day) {
        day = d;
        html.push('<div class=\"period-day\" data-day=\"' + d + '\"><span>' + esc(dayLabel(it.s)) + '</span><span>' + (perDay[d] || 0) + '</span></div>');
      }
      var sel = it.id === P.sel ? ' selected' : '';
      if (it.k === 'g') {
        html.push('<div class=\"period-row pr-gap' + sel + '\" data-id=\"' + esc(it.id) + '\"><span>' + it.ic + '</span><span class=\"pr-time\">' +
                  hm(it.s) + '\\u2013' + hm(it.e) + '</span><span class=\"pr-what\">' + esc(i18nText('No data') + ' \\u00b7 ' + dur(it.e - it.s)) +
                  '</span><span></span></div>');
        return;
      }
      var tag = P.view === 'now' ? (it.g ? '<span class=\"pr-tag\">GPX</span>' : '<span></span>') :
        (hiddenNow(it) ? '<span class=\"pr-tag pr-replaced\">' + esc(i18nText('hidden in Arc')) + '</span>' : '<span></span>');
      html.push('<div class=\"period-row' + sel + '\" data-id=\"' + esc(it.id) + '\">' +
                '<span>' + esc(it.ic) + '</span><span class=\"pr-time\">' + hm(it.s) + '\\u2013' + hm(it.e) + '</span>' +
                '<span class=\"pr-what\" title=\"' + esc(what(it)) + '\">' + esc(what(it)) + '</span>' + tag + '</div>');
    });
    if (!items.length && P.from) html.push('<div class=\"review-empty\">' + esc(i18nText('No items in this view.')) + '</div>');
    return html.join('');
  }
  // Places: the visits of the view by place, by time spent; a click lists that place's visits
  function placeGroups() {
    var groups = {}, order = [];
    viewList().forEach(function(it) {
      if (it.k !== 'v') return;
      var key = it.pid ? 'id:' + it.pid : 'nm:' + (it.nm || '?');
      var g = groups[key];
      if (!g) { g = groups[key] = {key: key, pid: it.pid || null, name: it.nm || i18nText('Visit'), n: 0, secs: 0, first: it.s, last: it.s}; order.push(g); }
      g.n++;
      g.secs += it.e - it.s;
      if (it.s < g.first) g.first = it.s;
      if (it.s > g.last) g.last = it.s;
    });
    return order.sort(function(a, b) { return b.secs - a.secs; });
  }
  function placesHtml() {
    var gs = placeGroups();
    if (!gs.length) return '<div class=\"review-empty\">' + esc(i18nText('No items in this view.')) + '</div>';
    return '<table class=\"period-table\"><thead><tr><th>' + esc(i18nText('Place')) + '</th><th>' + esc(i18nText('visits')) + '</th><th>h</th><th>' +
      esc(i18nText('last')) + '</th></tr></thead><tbody>' +
      gs.slice(0, 400).map(function(g) {
        return '<tr class=\"pp-row\" data-place=\"' + esc(g.key) + '\" title=\"' + esc(longDay(g.first) + ' \\u2192 ' + longDay(g.last)) + '\"><td>' + esc(g.name) +
          '</td><td>' + g.n + '</td><td>' + (g.secs / 3600).toFixed(1) + '</td><td>' + esc(longDay(g.last)) + '</td></tr>';
      }).join('') + '</tbody></table>';
  }
  // Numbers: what is shown (view, modes, place query), by mode and by month
  function statsTables(items) {
    var byMode = {}, byMonth = {}, modes = [], months = [];
    items.forEach(function(it) {
      var k = modeKey(it), mo = it.dk.slice(0, 7);
      if (!byMode[k]) { byMode[k] = {it: it, n: 0, km: 0, secs: 0}; modes.push(k); }
      if (!byMonth[mo]) { byMonth[mo] = {trips: 0, km: 0, move: 0, visits: 0, stay: 0}; months.push(mo); }
      var a = byMode[k], b = byMonth[mo], secs = it.e - it.s;
      a.n++; a.secs += secs;
      if (it.k === 'v') { b.visits++; b.stay += secs; } else { a.km += it.km || 0; b.trips++; b.km += it.km || 0; b.move += secs; }
    });
    modes.sort(function(x, y) { return byMode[y].secs - byMode[x].secs; });
    months.sort();
    var t1 = '<table class=\"period-table\"><thead><tr><th>' + esc(i18nText('Mode')) + '</th><th>n</th><th>km</th><th>h</th></tr></thead><tbody>' +
      modes.map(function(k) {
        var a = byMode[k], lb = k === 'visit' ? i18nText('Visits') : i18nText(a.it.lb);
        return '<tr><td>' + esc(a.it.ic + ' ' + lb) + '</td><td>' + a.n + '</td><td>' + (k === 'visit' ? '' : a.km.toFixed(0)) + '</td><td>' +
          (a.secs / 3600).toFixed(1) + '</td></tr>';
      }).join('') + '</tbody></table>';
    var t2 = '<table class=\"period-table\"><thead><tr><th>' + esc(i18nText('Month')) + '</th><th>' + esc(i18nText('trips')) + '</th><th>km</th><th>' +
      esc(i18nText('h moving')) + '</th><th>' + esc(i18nText('visits')) + '</th></tr></thead><tbody>' +
      months.map(function(mo) {
        var b = byMonth[mo];
        return '<tr><td>' + mo.slice(5) + '/' + mo.slice(0, 4) + '</td><td>' + b.trips + '</td><td>' + b.km.toFixed(0) + '</td><td>' +
          (b.move / 3600).toFixed(1) + '</td><td>' + b.visits + '</td></tr>';
      }).join('') + '</tbody></table>';
    return t1 + t2;
  }
  function statsHtml() {
    var items = list();
    if (!items.length) return '<div class=\"review-empty\">' + esc(i18nText('No items in this view.')) + '</div>';
    return '<div class=\"period-export\">' + esc(i18nText('Export what is shown:')) +
      ' <a href=\"#\" data-pexp=\"geojson\">GeoJSON</a> \\u00b7 <a href=\"#\" data-pexp=\"kml\">KML</a> \\u00b7 <a href=\"#\" data-pexp=\"html\">' +
      esc(i18nText('summary (HTML)')) + '</a></div>' + statsTables(items);
  }
  function renderSummary() {
    var sum = document.getElementById('period_summary');
    $('#period_place_q').attr('placeholder', i18nText('Search a place by name'));
    if (!sum) return;
    if (!P.from) { sum.textContent = ''; return; }
    if (P.q) { sum.textContent = querySummary(list()); return; }
    var inView = viewList();
    var nT = inView.filter(function(it) { return it.k === 't'; }).length, nV = inView.length - nT;
    var extra = P.view === 'now' ? inView.filter(function(it) { return it.g; }).length + ' GPX' :
      inView.filter(hiddenNow).length + ' ' + i18nText('hidden in Arc');
    var noPath = inView.filter(function(it) { return it.k === 't' && !(it.p && it.p.length > 1); }).length;
    sum.textContent = nT + ' ' + i18nText('trips') + ' \\u00b7 ' + nV + ' ' + i18nText('visits') + ' \\u00b7 ' + extra +
      (P.gaps.length ? ' \\u00b7 ' + P.gaps.length + ' ' + i18nText('gaps') : '') +
      (noPath ? ' \\u00b7 ' + noPath + ' ' + i18nText('without samples in the local backup (fetch from iCloud)') : '');
  }
  function renderList() {
    var box = document.getElementById('period_list');
    if (!box) return;
    renderSummary();
    box.innerHTML = P.tab === 'places' ? placesHtml() : P.tab === 'stats' ? statsHtml() : P.tab === 'sends' ? sendsHtml() : listHtml();
    $(box).toggleClass('period-list-tall', P.tab !== 'list');
    renderDetail();
  }

  function renderDetail() {
    var box = document.getElementById('period_detail');
    if (!box) return;
    var it = P.sel && byId(P.sel);
    if (it && it.k === 'g') {
      box.innerHTML = '<b>' + it.ic + ' ' + esc(i18nText('No data')) + '</b><br>' +
        esc(dayLabel(it.s) + ' ' + hm(it.s) + ' \\u2192 ' + dayLabel(it.e) + ' ' + hm(it.e) + ' (' + dur(it.e - it.s) + ')') +
        '<div class=\"period-open-near\"><button type=\"button\" class=\"btn btn-outline-primary\" data-period-fill=\"visit\">' +
        esc(i18nText('Create a visit')) + '</button><button type=\"button\" class=\"btn btn-outline-primary\" data-period-fill=\"trip\">' +
        esc(i18nText('Create a trip')) + '</button></div>';
      return;
    }
    if (it && it.k === 's') {
      box.innerHTML = '<b>' + it.ic + ' ' + esc(it.type + ' \\u00b7 ' + statusText(it.status)) + '</b><br>' +
        esc(dayLabel(it.s) + ' ' + hm(it.s) + '\\u2013' + hm(it.e) + ' (' + dur(it.e - it.s) + ')') + '<br>' +
        (it.orig ? esc(i18nText('The original recording still shows in Arc there.')) + '<br>' : '') +
        '<span class=\"ps-file-name\">' + esc(it.file) + '</span>' +
        '<div class=\"period-open-near\"><button type=\"button\" class=\"btn btn-outline-primary\" data-period-open=\"track\">' +
        esc(i18nText('Edit')) + '</button><button type=\"button\" class=\"btn btn-outline-secondary\" data-period-resend=\"' + esc(it.file) + '\">' +
        esc(i18nText('Send the file again')) + '</button></div>';
      return;
    }
    if (it && it.k === 'j') {
      box.innerHTML = '<b>' + it.ic + ' ' + esc(what(it)) + '</b><br>' + esc(dayLabel(it.s) + ' ' + hm(it.s)) +
        '<br><button type=\"button\" class=\"btn btn-outline-primary\" data-period-fill=\"route\">' +
        esc(i18nText('Create the trip from this boarding')) + '</button>';
      return;
    }
    if (!it || !shown(it)) { box.innerHTML = ''; return; }
    var min = Math.round((it.e - it.s) / 60);
    var src = it.g ? i18nText('Source: GPX import') :
      (hiddenNow(it) ? i18nText('Source: recorded, hidden in Arc (a GPX import replaced it)') : i18nText('Source: recorded'));
    var lines = ['<b>' + esc(it.ic + ' ' + what(it)) + '</b>',
                 esc(dayLabel(it.s) + ' ' + hm(it.s) + '\\u2013' + hm(it.e) + ' (' + min + ' min)'), esc(src)];
    if (it.k === 't' && !(it.p && it.p.length > 1)) lines.push(esc(i18nText('No samples in the local backup.')));
    box.innerHTML = lines.join('<br>') +
      '<br><button type=\"button\" class=\"btn btn-outline-primary\" data-period-open=\"this\">' + esc(i18nText('Edit')) + '</button>' +
      '<div class=\"period-open-near\"><button type=\"button\" class=\"btn btn-outline-secondary\" data-period-open=\"prev\">' +
      '\\u2190 ' + esc(i18nText('with the item before')) + '</button><button type=\"button\" class=\"btn btn-outline-secondary\" data-period-open=\"next\">' +
      esc(i18nText('with the item after')) + ' \\u2192' + '</button></div>';
  }

  function fit(items) {
    var map = getMap(), b = L.latLngBounds([]);
    items.forEach(function(it) {
      if (it.k === 't' && it.p && it.p.length) it.p.forEach(function(q) { b.extend(q); });
      else if (it.ll) b.extend(it.ll);
    });
    if (map && b.isValid()) map.fitBounds(b, {padding: [30, 30], maxZoom: 17});
  }

  function select(id, from) {
    P.sel = id;
    restyle();
    var it = byId(id);
    $('#period_list .period-row').removeClass('selected');
    var row = $('#period_list .period-row').filter(function() { return this.getAttribute('data-id') === id; });
    row.addClass('selected');
    if (row[0] && from !== 'list') row[0].scrollIntoView({block: 'nearest'});
    sendTrackLayer(it);
    if (it && from !== 'map') fit(it.k === 'g' ? [byId(it.a), byId(it.b)].filter(Boolean) : [it]);
    renderDetail();
  }

  function step(dir) {
    var items = list();
    if (!items.length) return;
    var i = -1;
    for (var k = 0; k < items.length; k++) if (items[k].id === P.sel) i = k;
    var j = i < 0 ? (dir > 0 ? 0 : items.length - 1) : Math.min(items.length - 1, Math.max(0, i + dir));
    select(items[j].id, 'key');
  }

  function clearSel() {
    if (!P.sel) return;
    P.sel = null;
    restyle();
    $('#period_list .period-row').removeClass('selected');
    renderDetail();
  }
  // A click on the map away from the lines (Period mode) clears the selection
  function hookMap() {
    var map = getMap();
    if (!map || map._periodHook) return;
    map._periodHook = true;
    map.on('click', function(e) {
      if (mode() !== 'Period') return;
      if (P.picking) setPointQuery(e.latlng); else clearSel();
    });
  }

  // ---- Place queries: when was I here? ----
  // P.q: {type: 'name', text} (visits whose place matches) or {type: 'point',
  // ll, r} (visits within r metres, trips whose path passes within r); P.hits:
  // the matching ids (null: no query)
  function norm(t) { return String(t || '').toLowerCase().normalize('NFD').replace(/[\\u0300-\\u036f]/g, ''); }
  function metres(lat0) { var k = Math.cos(lat0 * Math.PI / 180); return function(q) { return [q[1] * 111320 * k, q[0] * 110540]; }; }
  function segDist(p, a, b) {
    var dx = b[0] - a[0], dy = b[1] - a[1], l2 = dx * dx + dy * dy;
    var t = l2 > 0 ? Math.max(0, Math.min(1, ((p[0] - a[0]) * dx + (p[1] - a[1]) * dy) / l2)) : 0;
    var x = a[0] + t * dx - p[0], y = a[1] + t * dy - p[1];
    return Math.sqrt(x * x + y * y);
  }
  function computeHits() {
    if (!P.q) { P.hits = null; return; }
    var hits = {};
    if (P.q.type === 'jae') {
      if (!P.jae) computeJae();
      hits = Object.assign({}, P.jae[P.q.kind]);
    } else if (P.q.type === 'place') {
      P.items.forEach(function(it) { if (it.k === 'v' && (P.q.pid ? it.pid === P.q.pid : it.nm === P.q.text)) hits[it.id] = true; });
    } else if (P.q.type === 'name') {
      var q = norm(P.q.text);
      P.items.forEach(function(it) { if (it.k === 'v' && it.nm && norm(it.nm).indexOf(q) >= 0) hits[it.id] = true; });
    } else {
      var m = metres(P.q.ll[0]), c = m(P.q.ll), r = P.q.r;
      P.items.forEach(function(it) {
        if (it.k === 'v' && it.ll) { if (segDist(c, m(it.ll), m(it.ll)) <= r) hits[it.id] = true; return; }
        if (!it.p) return;
        var prev = m(it.p[0]);
        for (var j = 1; j < it.p.length; j++) {
          var cur = m(it.p[j]);
          if (segDist(c, prev, cur) <= r) { hits[it.id] = true; return; }
          prev = cur;
        }
      });
    }
    P.hits = hits;
  }
  function querySummary(items) {
    if (P.q.type === 'jae') return '\\uD83C\\uDFAB ' + jaeLabel(P.q.kind) + ': ' + items.length;
    var v = items.filter(function(it) { return it.k === 'v'; }), t = items.length - v.length;
    var secs = v.reduce(function(a, it) { return a + (it.e - it.s); }, 0);
    var what = P.q.type === 'point' ? i18nText('within') + ' ' + P.q.r + ' m' : '\"' + P.q.text + '\"';
    var span = items.length ? ' \\u00b7 ' + dayLabel(items[0].s) + ' \\u2192 ' + dayLabel(items[items.length - 1].s) : '';
    return '\\uD83D\\uDCCD ' + what + ': ' + v.length + ' ' + i18nText('visits') + ' (' + (secs / 3600).toFixed(1) + ' h)' +
      (t ? ' \\u00b7 ' + t + ' ' + i18nText('trips passing by') : '') + span;
  }
  function drawQuery() {
    var map = getMap();
    if (!map) return;
    if (P.qLayer) { map.removeLayer(P.qLayer); P.qLayer = null; }
    if (P.q && P.q.type === 'point' && mode() === 'Period') {
      P.qLayer = L.circle(P.q.ll, {radius: P.q.r, color: '#C62828', weight: 2, fill: true, fillOpacity: 0.08, dashArray: '5 4',
                                   interactive: false}).addTo(map);
    }
  }
  function setQuery(q) {
    P.q = q;
    P.sel = null;
    computeHits();
    refresh();
    drawQuery();
    var items = list();
    if (q && q.type === 'point') { var map = getMap(); if (map && P.qLayer) map.fitBounds(P.qLayer.getBounds(), {padding: [60, 60], maxZoom: 17}); }
    else if (items.length) fit(items);
  }
  function setPointQuery(ll) { setPicking(false); setQuery({type: 'point', ll: [ll.lat, ll.lng], r: P.r}); }
  function setPicking(on) {
    P.picking = !!on;
    $('#map').toggleClass('draw-mode', P.picking);
    $('[data-pc=\"pick\"]').toggleClass('btn-primary', P.picking).toggleClass('btn-outline-secondary', !P.picking);
  }
  function refresh() { draw(); renderList(); renderControl(); }

  // ---- Jaé taps against the timeline ----
  // A tap belongs to a trip of its family (bus: bus; metro: metro / train;
  // tram: tram) that holds it (a minute of slack) or starts within 10 min after
  // it (waiting at the stop). Else, a trip of another mode holding it is a mode
  // mismatch, and none at all a tap without a trip. Bus / metro / tram trips
  // with no tap from 15 min before their start to their end: trips without a tap.
  var JAE_FAMILY = {bus: ['bus'], metro: ['metro', 'train'], tram: ['tram']};
  function tapsFromCols(c) {
    if (!c || !c.t) return [];
    return c.t.map(function(t, i) {
      var it = {id: 'tap:' + i, k: 'j', s: t, e: t, m: 'tap', p: undefined, prof: c.p[i], lb: c.lb[i] || 'Ja\\u00e9', ic: '\\uD83C\\uDFAB',
                c: '#7B1FA2', now: true, rec: true};
      if (c.lat[i] != null && c.lon[i] != null) it.ll = [c.lat[i], c.lon[i]];
      it.dk = dayKey(t);
      return it;
    });
  }
  function computeJae() {
    var trips = viewList().filter(function(it) { return it.k === 't'; });
    var res = {tap: {}, trip: {}, mode: {}, nTap: 0, nTrip: 0, nMode: 0};
    P.taps.forEach(function(tp) {
      var fam = JAE_FAMILY[tp.prof] || [], ok = false, other = null;
      trips.forEach(function(tr) {
        if (ok) return;
        var holds = tp.s >= tr.s - 60 && tp.s <= tr.e + 60, soon = tr.s >= tp.s && tr.s - tp.s <= 600;
        if (fam.indexOf(tr.m) >= 0 && (holds || soon)) ok = true;
        else if (holds && tp.s > tr.s + 60 && tp.s < tr.e - 60) other = tr;
      });
      if (ok) return;
      if (other) { res.mode[other.id] = true; res.nMode++; } else { res.tap[tp.id] = true; res.nTap++; }
    });
    trips.forEach(function(tr) {
      if (['bus', 'metro', 'tram'].indexOf(tr.m) < 0) return;
      var has = P.taps.some(function(tp) { return (JAE_FAMILY[tp.prof] || []).indexOf(tr.m) >= 0 && tp.s >= tr.s - 900 && tp.s <= tr.e; });
      if (!has) { res.trip[tr.id] = true; res.nTrip++; }
    });
    res.nMode = Object.keys(res.mode).length;
    P.jae = res;
  }
  function jaeLabel(kind) {
    return kind === 'tap' ? i18nText('taps without a trip') : kind === 'trip' ? i18nText('bus / metro / tram trips without a tap') :
      i18nText('trips whose mode differs from the tap');
  }

  // ---- Exports of what is shown (the simplified paths): GeoJSON, KML, an HTML summary ----
  function iso(ts) { return new Date(ts * 1000).toISOString(); }
  function download(name, mime, text) {
    var url = URL.createObjectURL(new Blob([text], {type: mime})), a = document.createElement('a');
    a.href = url;
    a.download = name;
    document.body.appendChild(a);
    a.click();
    setTimeout(function() { URL.revokeObjectURL(url); a.remove(); }, 1000);
  }
  function exportName(ext, pre) { return (pre || 'arc_') + P.from + '_' + P.to + '.' + ext; }
  function exportGeojson(items) {
    var feats = [];
    items.forEach(function(it) {
      var geom = it.k === 't' && it.p ? {type: 'LineString', coordinates: it.p.map(function(q) { return [q[1], q[0]]; })} :
        (it.ll ? {type: 'Point', coordinates: [it.ll[1], it.ll[0]]} : null);
      if (!geom) return;
      feats.push({type: 'Feature', geometry: geom, properties: {id: it.id, kind: it.k === 'v' ? 'visit' : 'trip', mode: it.k === 'v' ? 'visit' : it.m,
        place: it.nm || null, start: iso(it.s), end: iso(it.e), km: it.km == null ? null : it.km, gpx: it.g, in_arc_now: it.now}});
    });
    return JSON.stringify({type: 'FeatureCollection', features: feats});
  }
  function kmlColor(hex) { hex = (hex || '#757575').replace('#', ''); return 'ff' + hex.slice(4, 6) + hex.slice(2, 4) + hex.slice(0, 2); }
  function xmlEsc(t) { return String(t).replace(/[<>&]/g, function(c) { return {'<': '&lt;', '>': '&gt;', '&': '&amp;'}[c]; }); }
  function exportKml(items) {
    var pm = items.map(function(it) {
      var name = xmlEsc((it.k === 'v' ? (it.nm || i18nText('Visit')) : i18nText(it.lb)) + ' ' + dayLabel(it.s) + ' ' + hm(it.s));
      var ts = '<TimeSpan><begin>' + iso(it.s) + '</begin><end>' + iso(it.e) + '</end></TimeSpan>';
      if (it.k === 't' && it.p) {
        return '<Placemark><name>' + name + '</name>' + ts + '<Style><LineStyle><color>' + kmlColor(it.c) + '</color><width>3</width></LineStyle></Style>' +
          '<LineString><coordinates>' + it.p.map(function(q) { return q[1] + ',' + q[0]; }).join(' ') + '</coordinates></LineString></Placemark>';
      }
      if (it.ll) return '<Placemark><name>' + name + '</name>' + ts + '<Point><coordinates>' + it.ll[1] + ',' + it.ll[0] + '</coordinates></Point></Placemark>';
      return '';
    }).join('\\n');
    return '<?xml version=\"1.0\" encoding=\"UTF-8\"?>\\n<kml xmlns=\"http://www.opengis.net/kml/2.2\"><Document><name>Arc ' + P.from + ' ' + P.to +
      '</name>\\n' + pm + '\\n</Document></kml>';
  }
  function exportHtml(items) {
    var places = placeGroups().slice(0, 25).map(function(g) {
      return '<tr><td>' + esc(g.name) + '</td><td>' + g.n + '</td><td>' + (g.secs / 3600).toFixed(1) + '</td></tr>';
    }).join('');
    return '<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>Arc ' + P.from + ' \\u2013 ' + P.to + '</title><style>' +
      'body{font-family:system-ui,sans-serif;margin:24px;color:#222}table{border-collapse:collapse;margin:12px 0 24px}' +
      'td,th{border-bottom:1px solid #ddd;padding:3px 10px;text-align:right}td:first-child,th:first-child{text-align:left}</style></head><body>' +
      '<h1>Arc ' + P.from + ' \\u2013 ' + P.to + '</h1><p>' + esc(renderedSummary()) + '</p>' + statsTables(items) +
      '<h2>' + esc(i18nText('Places')) + '</h2><table><tr><th>' + esc(i18nText('Place')) + '</th><th>' + esc(i18nText('visits')) + '</th><th>h</th></tr>' +
      places + '</table></body></html>';
  }
  function renderedSummary() { var s = document.getElementById('period_summary'); return s ? s.textContent : ''; }
  $(document).on('click', '[data-pexp]', function(e) {
    e.preventDefault();
    var kind = this.getAttribute('data-pexp'), items = list();
    if (kind === 'geojson') download(exportName('geojson'), 'application/geo+json', exportGeojson(items));
    else if (kind === 'kml') download(exportName('kml'), 'application/vnd.google-earth.kml+xml', exportKml(items));
    else download(exportName('html', 'arc_summary_'), 'text/html', exportHtml(items));
  });
  $(document).on('change', 'input[name=\"period_tab\"]', function() {
    P.tab = this.value;
    if (P.tab === 'sends') requestSends(false);
    if (P.sel && byId(P.sel) && byId(P.sel).k === 's' && P.tab !== 'sends') { P.sel = null; sendTrackLayer(null); }
    renderList();
  });
  $(document).on('click', '#period_list .pp-row', function() {
    var key = this.getAttribute('data-place'), g = placeGroups().filter(function(x) { return x.key === key; })[0];
    if (!g) return;
    $('#period_tab_list').prop('checked', true);
    P.tab = 'list';
    setQuery({type: 'place', pid: g.pid, text: g.name});
  });
  $(document).on('click', '[data-period-fill]', function() {
    var it = P.sel && byId(P.sel);
    if (it && (it.k === 'g' || it.k === 'j')) Shiny.setInputValue('period_fill', {s: it.s, e: it.e, kind: this.getAttribute('data-period-fill'), ts: Date.now()}, {priority: 'event'});
  });
  // ---- Sends: the GPX files sent and what Arc shows of each track (utils_sends.R) ----
  var SENDS = null, sendOpen = null, sendLayer = null;
  var TYPE_ICON = {bus: '\\uD83D\\uDE8C', car: '\\uD83D\\uDE97', metro: '\\uD83D\\uDE87', taxi: '\\uD83D\\uDE95', tram: '\\uD83D\\uDE8A',
                   train: '\\uD83D\\uDE86', walking: '\\uD83D\\uDEB6', stationary: '\\u23F8'};
  function sendsFrom(m) {
    var f = m.files || {}, t = m.tracks || {}, files = [], byFile = {};
    (f.file || []).forEach(function(name, i) {
      var x = {file: name, sent: f.sent[i], copy: !!f.copy[i], n: f.n[i], nIn: f.n_in[i], nRep: f.n_rep[i], nMis: f.n_mis[i],
               nAbs: f.n_abs[i], nOrig: f.n_orig[i], tracks: []};
      files.push(x);
      byFile[name] = x;
    });
    Object.keys(P.idx).forEach(function(k) { if (k.indexOf('trk:') === 0) delete P.idx[k]; });
    (t.file || []).forEach(function(name, i) {
      var flat = t.p[i], pts = null;
      if (flat && flat.length >= 4) { pts = []; for (var j = 0; j + 1 < flat.length; j += 2) pts.push([flat[j], flat[j + 1]]); }
      var it = {id: 'trk:' + name + '|' + t.k[i], k: 's', file: name, s: t.t0[i], e: t.t1[i], type: t.type[i], status: t.status[i],
                orig: !!t.orig[i], p: pts, ic: TYPE_ICON[t.type[i]] || '\\u2022'};
      it.dk = dayKey(it.s);
      P.idx[it.id] = it;
      if (byFile[name]) byFile[name].tracks.push(it);
    });
    return files;
  }
  Shiny.addCustomMessageHandler('period_sends', function(msg) {
    SENDS = sendsFrom(msg);
    if (P.tab === 'sends') renderList();
  });
  function requestSends(force) {
    if (SENDS === 'loading' || (SENDS && !force)) return;
    SENDS = 'loading';
    Shiny.setInputValue('period_sends_req', {ts: Date.now()}, {priority: 'event'});
  }
  function statusText(st) {
    return st === 'in_arc' ? '\\u2713 ' + i18nText('in Arc') : st === 'replaced' ? i18nText('replaced by a later import') :
      st === 'absorbed' ? i18nText('absorbed by Arc (short leg)') : '\\u2717 ' + i18nText('not found in Arc');
  }
  function sendsHtml() {
    if (!SENDS || SENDS === 'loading') return '<div class=\"review-empty\">' + esc(i18nText('Loading...')) + '</div>';
    if (!SENDS.length) return '<div class=\"review-empty\">' + esc(i18nText('No GPX sent yet.')) + '</div>';
    var tot = {n: 0, mis: 0, orig: 0};
    SENDS.forEach(function(f) { tot.n += f.n || 0; tot.mis += f.nMis || 0; tot.orig += f.nOrig || 0; });
    var html = ['<div class=\"period-export\">' + SENDS.length + ' ' + esc(i18nText('files')) + ' \\u00b7 ' + tot.n + ' ' + esc(i18nText('tracks')) +
                ' \\u00b7 \\u2717 ' + tot.mis + ' ' + esc(i18nText('not found in Arc')) + ' \\u00b7 ' + tot.orig + ' ' + esc(i18nText('with the recording still visible')) +
                ' <a href=\"#\" data-psends=\"refresh\" title=\"' + esc(i18nText('Check again')) + '\">\\u21BB</a></div>'];
    SENDS.forEach(function(f) {
      var open = sendOpen === f.file, flags = f.copy ? '\\u2713' + f.nIn + (f.nMis ? ' \\u2717' + f.nMis : '') + (f.nOrig ? ' \\u26A0' + f.nOrig : '') :
        esc(i18nText('no local copy'));
      html.push('<div class=\"ps-file' + (open ? ' open' : '') + '\" data-send=\"' + esc(f.file) + '\"><span>' + (open ? '\\u25BE' : '\\u25B8') + ' ' +
                esc(longDay(f.sent) + ' ' + hm(f.sent)) + '</span><span class=\"ps-name\" title=\"' + esc(f.file) + '\">' +
                esc(f.file.replace(/^arc_(batch_)?/, '').replace(/\\.gpx$/, '')) + '</span><span class=\"ps-flags\">' + flags + '</span></div>');
      if (!open) return;
      f.tracks.forEach(function(it) {
        html.push('<div class=\"period-row ps-trk ps-' + it.status + (it.id === P.sel ? ' selected' : '') + '\" data-id=\"' + esc(it.id) + '\"><span>' + it.ic +
                  '</span><span class=\"pr-time\">' + esc(longDay(it.s) + ' ' + hm(it.s)) + '</span><span class=\"pr-what\">' + esc(statusText(it.status)) +
                  '</span><span>' + (it.orig ? '\\u26A0' : '') + '</span></div>');
      });
    });
    return html.join('');
  }
  function sendTrackLayer(it) {
    var map = getMap();
    if (!map) return;
    if (sendLayer) { map.removeLayer(sendLayer); sendLayer = null; }
    if (it && it.k === 's' && it.p && mode() === 'Period') {
      sendLayer = L.polyline(it.p, {color: '#C62828', weight: 5, opacity: 0.9, dashArray: '8 6', interactive: false}).addTo(map);
    }
  }
  $(document).on('click', '#period_list .ps-file', function() {
    var f = this.getAttribute('data-send');
    sendOpen = sendOpen === f ? null : f;
    renderList();
  });
  $(document).on('click', '[data-psends=\"refresh\"]', function(e) { e.preventDefault(); requestSends(true); renderList(); });
  $(document).on('click', '[data-period-resend]', function() {
    Shiny.setInputValue('period_resend', {file: this.getAttribute('data-period-resend'), ts: Date.now()}, {priority: 'event'});
  });


  // ---- The map panel: modes on / off, the place query, full screen ----
  function renderControl() {
    var map = getMap();
    if (!map) return;
    if (!P.ctl) {
      P.ctl = L.control({position: 'topright'});
      P.ctl.onAdd = function() {
        var d = L.DomUtil.create('div', 'leaflet-control period-ctl');
        L.DomEvent.disableClickPropagation(d);
        L.DomEvent.disableScrollPropagation(d);
        return d;
      };
    }
    var on = mode() === 'Period' && !!P.from;
    if (!on) { if (P.ctl._map) P.ctl.remove(); return; }
    if (!P.ctl._map) P.ctl.addTo(map);
    var counts = {}, info = {};
    viewList().forEach(function(it) {
      var k = modeKey(it);
      counts[k] = (counts[k] || 0) + 1;
      if (!info[k]) info[k] = it;
    });
    var keys = Object.keys(counts).sort(function(a, b) { return a === 'visit' ? -1 : b === 'visit' ? 1 : counts[b] - counts[a]; });
    var rows = keys.map(function(k) {
      var it = info[k], lb = k === 'visit' ? i18nText('Visits') : i18nText(it.lb);
      return '<label class=\"pc-mode\"><input type=\"checkbox\" data-pc-mode=\"' + esc(k) + '\"' + (P.off[k] ? '' : ' checked') + '> ' +
        '<span class=\"pc-sw\" style=\"background:' + esc(k === 'visit' ? '#6D4C41' : it.c) + '\"></span> ' + esc(it.ic + ' ' + lb) +
        ' <span class=\"pc-n\">' + counts[k] + '</span></label>';
    }).join('');
    if (P.gaps.length) {
      rows += '<label class=\"pc-mode\"><input type=\"checkbox\" data-pc-mode=\"gap\"' + (P.off.gap ? '' : ' checked') + '> ' +
        '<span class=\"pc-sw\" style=\"background:#F9A825\"></span> \\u26A0\\uFE0F ' + esc(i18nText('Gaps (no data)')) +
        ' <span class=\"pc-n\">' + P.gaps.length + '</span></label>';
    }
    var jaeRow = '';
    if (P.taps.length) {
      if (!P.jae) computeJae();
      jaeRow = '<div class=\"pc-jae\"><b>\\uD83C\\uDFAB Ja\\u00e9</b> ' + ['tap', 'trip', 'mode'].map(function(k) {
        var n = k === 'tap' ? P.jae.nTap : k === 'trip' ? P.jae.nTrip : P.jae.nMode;
        var on = P.q && P.q.type === 'jae' && P.q.kind === k;
        return '<a href=\"#\" data-pc-jae=\"' + k + '\" class=\"' + (on ? 'pc-on' : '') + '\">' + esc(jaeLabel(k)) + ' (' + n + ')</a>';
      }).join('<br>') + '</div>';
    }
    var fs = !!document.fullscreenElement;
    var radii = [50, 100, 250, 500].map(function(r) { return '<option value=\"' + r + '\"' + (r === P.r ? ' selected' : '') + '>' + r + ' m</option>'; }).join('');
    var c = P.ctl.getContainer();
    c.innerHTML =
      '<div class=\"pc-head\"><b>' + esc(i18nText('Modes')) + '</b> <a href=\"#\" data-pc=\"all\">' + esc(i18nText('all')) + '</a> \\u00b7 ' +
      '<a href=\"#\" data-pc=\"none\">' + esc(i18nText('none')) + '</a>' +
      '<button type=\"button\" class=\"btn btn-sm btn-outline-secondary pc-fs\" data-pc=\"fs\" title=\"' + esc(fs ? i18nText('Exit full screen') : i18nText('Full screen')) + '\">' +
      (fs ? '\\u2715' : '\\u26F6') + '</button></div>' +
      '<div class=\"pc-modes\">' + rows + '</div>' +
      '<div class=\"pc-place\"><button type=\"button\" class=\"btn btn-sm ' + (P.picking ? 'btn-primary' : 'btn-outline-secondary') + '\" data-pc=\"pick\" title=\"' +
      esc(i18nText('Click a point on the map')) + '\">\\uD83D\\uDCCD ' + esc(i18nText('When was I here?')) + '</button> ' +
      '<select data-pc=\"r\" class=\"form-select form-select-sm\">' + radii + '</select></div>' + jaeRow +
      (P.q ? '<div class=\"pc-q\">' + esc(querySummary(list())) + ' <a href=\"#\" data-pc=\"clear\">' + esc(i18nText('clear')) + '</a></div>' : '');
  }
  $(document).on('change', '[data-pc-mode]', function() {
    var k = this.getAttribute('data-pc-mode');
    if (this.checked) delete P.off[k]; else P.off[k] = true;
    draw();
    renderList();
  });
  $(document).on('click', '[data-pc]', function(e) {
    var what = this.getAttribute('data-pc');
    if (what === 'r') return;
    e.preventDefault();
    if (what === 'all') { P.off = {}; refresh(); }
    else if (what === 'none') { viewList().forEach(function(it) { P.off[modeKey(it)] = true; }); refresh(); }
    else if (what === 'pick') setPicking(!P.picking);
    else if (what === 'clear') { $('#period_place_q').val(''); setQuery(null); }
    else if (what === 'fs') toggleFullscreen();
  });
  $(document).on('click', '[data-pc-jae]', function(e) {
    e.preventDefault();
    var k = this.getAttribute('data-pc-jae');
    $('#period_place_q').val('');
    if (P.q && P.q.type === 'jae' && P.q.kind === k) setQuery(null); else setQuery({type: 'jae', kind: k});
  });
  $(document).on('change', '[data-pc=\"r\"]', function() {
    P.r = Number(this.value);
    if (P.q && P.q.type === 'point') setQuery({type: 'point', ll: P.q.ll, r: P.r});
  });
  var qTimer = null;
  $(document).on('input', '#period_place_q', function() {
    var t = this.value.trim();
    clearTimeout(qTimer);
    qTimer = setTimeout(function() { setQuery(t.length >= 2 ? {type: 'name', text: t} : null); }, 300);
  });

  // Full screen: the whole page (the list stays; the left panel folds with its
  // own toggle), the day timeline folded while it lasts
  function rightLayout() { return $('#right_sidebar').closest('.bslib-sidebar-layout'); }
  function toggleRight() { rightLayout().children('.collapse-toggle').trigger('click'); }
  function toggleFullscreen() {
    if (document.fullscreenElement) { document.exitFullscreen(); return; }
    var el = document.documentElement;
    if (el.requestFullscreen) el.requestFullscreen();
  }
  document.addEventListener('fullscreenchange', function() {
    var lay = rightLayout(), fs = !!document.fullscreenElement;
    if (fs && lay.length && !lay.hasClass('sidebar-collapsed')) { toggleRight(); P.rsReopen = true; }
    if (!fs && P.rsReopen) { toggleRight(); P.rsReopen = false; }
    setTimeout(function() { var m = getMap(); if (m) m.invalidateSize(); renderControl(); }, 450);
  });

  // The items from the message's columns (period_payload in utils_period.R)
  var VISIT = {label: 'Visit', icon: '\\uD83D\\uDCCD', color: '#6D4C41'};
  function fromCols(c) {
    if (!c || !c.id) return [];
    var n = c.id.length, out = new Array(n), styles = c.styles || {};
    for (var i = 0; i < n; i++) {
      var st = c.k[i] === 'v' ? VISIT : (styles[c.m[i]] || {});
      var it = {id: c.id[i], k: c.k[i], s: c.s[i], e: c.e[i], m: c.m[i], lb: st.label, ic: st.icon, c: st.color,
                g: !!c.g[i], now: !!c.now[i], rec: !!c.rec[i]};
      if (c.nm[i] != null) it.nm = c.nm[i];
      if (c.pid && c.pid[i] != null) it.pid = c.pid[i];
      if (c.lat[i] != null && c.lon[i] != null) it.ll = [c.lat[i], c.lon[i]];
      if (c.km[i] != null) it.km = c.km[i];
      var f = c.p[i];
      if (f && f.length >= 4) {
        var p = new Array(f.length / 2);
        for (var j = 0; j < p.length; j++) p[j] = [f[2 * j], f[2 * j + 1]];
        it.p = p;
      }
      out[i] = it;
    }
    return out;
  }
  Shiny.addCustomMessageHandler('period_data', function(msg) {
    P.items = fromCols(msg.cols);
    P.tz = msg.tz || undefined;
    P.idx = {};
    P.items.forEach(function(it) { it.dk = dayKey(it.s); P.idx[it.id] = it; });   // the local day, once; the index
    P.taps = tapsFromCols(msg.taps);
    P.taps.forEach(function(tp) { P.idx[tp.id] = tp; });
    P.jae = null;
    if (P.q && P.q.type === 'jae') P.q = null;
    P.from = msg.from;
    P.to = msg.to;
    P.sel = null;
    hookMap();
    computeHits();
    computeGaps();
    draw();
    renderList();
    renderControl();
    fit(list());
  });
  // (Shiny wants handlers of exactly one argument)
  Shiny.addCustomMessageHandler('period_autoload', function(msg) { $('#period_load').click(); });

  $(document).on('click', '[data-period-step]', function() {
    Shiny.setInputValue('period_step', {dir: Number(this.getAttribute('data-period-step')), ts: Date.now()}, {priority: 'event'});
  });
  $(document).on('change', 'input[name=\"period_view\"]', function() {
    P.view = this.value;
    computeGaps();
    P.jae = null;
    if (P.q && P.q.type === 'jae') computeHits();
    refresh();
  });
  $(document).on('click', '#period_list .period-row', function() { select(this.getAttribute('data-id'), 'list'); });
  $(document).on('click', '#period_list .period-day', function() {
    var d = this.getAttribute('data-day');
    fit(list().filter(function(it) { return it.dk === d; }));
  });
  // Edit: a trip of a routed mode (or without one) opens in Batch review,
  // routed there; the rest (walks, rides, visits) and an item with its
  // neighbour in the list (e.g. a walk and the car trip after it, to move the
  // boundary between them) in Edit samples
  var ROUTED = {car: 1, taxi: 1, bus: 1, metro: 1, train: 1, tram: 1, unknown: 1};
  $(document).on('click', '[data-period-open]', function() {
    var it = P.sel && byId(P.sel);
    if (!it) return;
    var s = it.s, e = it.e, how = this.getAttribute('data-period-open'), items = viewList();
    if ((how === 'this' && it.k === 't' && ROUTED[it.m]) || (how === 'track' && ROUTED[it.type])) {
      Shiny.setInputValue('period_review', {s: s, e: e, id: how === 'this' ? String(it.id) : '', ts: Date.now()}, {priority: 'event'});
      return;
    }
    if (how === 'track') { Shiny.setInputValue('period_open', {s: s, e: e, ts: Date.now()}, {priority: 'event'}); return; }
    var i = items.indexOf(it);
    if (how === 'prev' && i > 0) s = Math.min(s, items[i - 1].s);
    if (how === 'next' && i >= 0 && i + 1 < items.length) e = Math.max(e, items[i + 1].e);
    Shiny.setInputValue('period_open', {s: s, e: e, ts: Date.now()}, {priority: 'event'});
  });
  $(document).on('keydown', function(e) {
    if (mode() !== 'Period' || !P.items.length || e.altKey || e.ctrlKey || e.metaKey) return;
    if ($(e.target).is('input, textarea, select, [contenteditable]')) return;
    if (e.key === 'Escape') { if (P.picking) { setPicking(false); return; } clearSel(); return; }
    if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
      e.preventDefault();
      step(e.key === 'ArrowDown' ? 1 : -1);
    }
  });
  // The layer only in Period mode; the texts again after a language switch
  $(document).on('shiny:inputchanged', function(e) {
    if (e.name !== 'mode' || !P.layer) return;
    var map = getMap();
    if (!map) return;
    if (e.value === 'Period') P.layer.addTo(map); else map.removeLayer(P.layer);
    if (e.value !== 'Period') { setPicking(false); sendTrackLayer(null); }
    setTimeout(function() { renderControl(); drawQuery(); }, 0);
  });
  $(document).on('click', '.lang-toggle [data-lang]', function() { setTimeout(function() { renderList(); renderControl(); }, 0); });
})();
"
