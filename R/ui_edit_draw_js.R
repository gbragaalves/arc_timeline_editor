# ---- Client: freehand drawing in Edit Samples (✏️ Draw) ----
# The button arms one stroke: press and drag on the map draws a line (the map
# doesn't pan, no sample marker moves: the press is taken in the capture
# phase, like the lasso); on release the line goes to the server as
# edit_draw {path: [[lat, lng], ...]}, which places the chosen part's samples
# along it (server.R). Esc cancels; leaving Edit Samples disarms it.
EDIT_DRAW_JS <- "
(function() {
  var D = {on: false, drawing: false, pts: [], line: null, noClickUntil: 0};
  window.__editDraw = D;  // read by the browser tests
  function getMap() { var w = HTMLWidgets.find('#map'); return w && w.getMap(); }
  function mode() { return $('#mode input:checked').val(); }
  function setOn(on) {
    var map = getMap();
    D.on = !!on && mode() === 'EditSamples';
    $('#edit_draw_toggle').toggleClass('btn-primary', D.on).toggleClass('btn-outline-secondary', !D.on);
    $('#map').toggleClass('draw-mode', D.on);
    if (!map) return;
    if (D.on) {
      if (window.__lasso && window.__lasso.edit) $('#edit_lasso_toggle').click();
      map.dragging.disable();
    } else if (window.syncMapDragging) {
      window.syncMapDragging(map);
      if (!(window.__lasso && window.__lasso.edit)) map.dragging.enable();
    } else {
      map.dragging.enable();
    }
  }
  function finish(cancel) {
    $(document).off('.editdraw');
    var map = getMap(), pts = D.pts, line = D.line;
    D.drawing = false;
    D.pts = [];
    D.line = null;
    D.noClickUntil = Date.now() + 400;   // the click that ends the stroke is not a map click
    setOn(false);
    // the drawn line stays a moment, until the server draws the route
    if (line && map) setTimeout(function() { map.removeLayer(line); }, cancel ? 0 : 1500);
    if (cancel || pts.length < 2) return;
    Shiny.setInputValue('edit_draw', {path: pts.map(function(p) { return [p.lat, p.lng]; }), ts: Date.now()}, {priority: 'event'});
  }
  function hook(map) {
    var el = map.getContainer();
    if (el._drawHook) return;
    el._drawHook = true;
    el.addEventListener('pointerdown', function(e) {
      if (D.on && e.button === 0 && e.pointerType === 'mouse') e.stopPropagation();
    }, true);
    el.addEventListener('mousedown', function(e) {
      if (!D.on || D.drawing || e.button !== 0) return;
      if ($(e.target).closest('.leaflet-control').length) return;
      e.preventDefault();
      e.stopPropagation();
      D.drawing = true;
      var ll = map.mouseEventToLatLng(e);
      D.pts = [ll];
      D.line = L.polyline([ll], {color: '#6A1B9A', weight: 3, dashArray: '4 4', interactive: false}).addTo(map);
      $(document).on('mousemove.editdraw', function(ev) {
        var p = map.mouseEventToLatLng(ev.originalEvent || ev);
        var last = D.pts[D.pts.length - 1];
        if (map.latLngToContainerPoint(last).distanceTo(map.latLngToContainerPoint(p)) < 3) return;
        D.pts.push(p);
        D.line.addLatLng(p);
      });
      $(document).on('mouseup.editdraw', function() { finish(false); });
    }, true);
    el.addEventListener('click', function(e) {
      if (Date.now() < D.noClickUntil) { e.preventDefault(); e.stopPropagation(); }
    }, true);
  }
  $(document).on('click', '#edit_draw_toggle', function(e) {
    e.preventDefault();
    var map = getMap();
    if (map) hook(map);
    setOn(!D.on);
  });
  $(document).on('keydown', function(e) {
    if (e.key !== 'Escape' || !(D.on || D.drawing)) return;
    if (D.drawing) finish(true); else setOn(false);
  });
  $(document).on('shiny:inputchanged', function(e) {
    if (e.name === 'mode' && e.value !== 'EditSamples' && (D.on || D.drawing)) { if (D.drawing) finish(true); else setOn(false); }
  });
})();
"
