# ---- Client: the extra OpenFreeMap base maps (as in the COM Rio apps) ----
# window.addExtraBases(map, layerManager, attribution) is called by
# OPENFREEMAP_JS (server.R) once MapLibre has loaded. Each base is a MapLibre
# GL layer inside Leaflet (maplibre-gl-leaflet: its GL map exists only while
# the base is chosen):
#   OpenFreeMap Positron: the Positron style.
#   Rio Mosaico 2024: Bright with the IPP's 2024 orthophoto (Mosaico_2024,
#     15 cm) under its labels, only close in (from MapLibre zoom 17 = Leaflet
#     18, the COM Rio apps' lock: the ImageServer has no tile cache, each tile
#     is made on request) and only inside the city of Rio (window.RIO_CITY,
#     assets/rio_city.geojson): the IPP answers white outside it, so tiles go
#     through the "ipp2024" protocol: none outside the city (not requested),
#     cut to its limit where they cross it. A note on the map says when to
#     zoom in, or that the view is outside the city.
RIO_CITY_FILE <- file.path("assets", "rio_city.geojson")
# The city's limit for the page (window.RIO_CITY), "" without the file: no Rio Mosaico 2024
rio_city_js <- function(file = RIO_CITY_FILE) {
  if (!file.exists(file)) return("")
  paste0("window.RIO_CITY = ", paste(readLines(file, warn = FALSE), collapse = ""), ";")
}

MAP_BASES_JS <- "
(function() {
  var BRIGHT = 'https://tiles.openfreemap.org/styles/bright';
  var POSITRON = 'https://tiles.openfreemap.org/styles/positron';
  var ORTHO_GROUP = 'Rio Mosaico 2024';
  var ORTHO_MIN_ZOOM = 17;   // MapLibre zoom (Leaflet zoom - 1)
  var ORTHO_URL = 'https://pgeo3.rio.rj.gov.br/arcgis/rest/services/Imagens/Mosaico_2024/ImageServer/exportImage' +
    '?bboxSR=3857&imageSR=3857&size=256,256&format=jpgpng&f=image&bbox=';
  var ORTHO_ATTR = 'Mosaico 2024 &copy; <a href=\"https://pgeo3.rio.rj.gov.br/arcgis/rest/services/Imagens/Mosaico_2024/ImageServer\" target=\"_blank\">Instituto Pereira Passos (IPP/PCRJ)</a>, CC BY 4.0';
  var R = 6378137;

  function i18nText(key, lang) {
    var entry = window.I18N && window.I18N[key];
    return (entry && entry[lang]) || key;
  }

  // The city's rings in EPSG:3857, each with its box; the city's box in lon/lat
  var city = null;
  function cityShape() {
    if (city) return city;
    city = {rings: [], lonlat: [Infinity, Infinity, -Infinity, -Infinity]};
    ((window.RIO_CITY && window.RIO_CITY.features) || []).forEach(function(f) {
      var g = f.geometry;
      var polys = g.type === 'Polygon' ? [g.coordinates] : g.type === 'MultiPolygon' ? g.coordinates : [];
      polys.forEach(function(poly) { poly.forEach(function(ring) {
        var box = [Infinity, Infinity, -Infinity, -Infinity];
        var pts = ring.map(function(c) {
          var ll = city.lonlat;
          ll[0] = Math.min(ll[0], c[0]); ll[1] = Math.min(ll[1], c[1]); ll[2] = Math.max(ll[2], c[0]); ll[3] = Math.max(ll[3], c[1]);
          var p = [R * c[0] * Math.PI / 180, R * Math.log(Math.tan(Math.PI / 4 + c[1] * Math.PI / 360))];
          box[0] = Math.min(box[0], p[0]); box[1] = Math.min(box[1], p[1]); box[2] = Math.max(box[2], p[0]); box[3] = Math.max(box[3], p[1]);
          return p;
        });
        city.rings.push({pts: pts, box: box});
      }); });
    });
    return city;
  }

  // The city's outline as a path on a canvas of n px over the tile box b (EPSG:3857)
  function cityPath(ctx, b, n) {
    var sx = n / (b[2] - b[0]), sy = n / (b[3] - b[1]);
    ctx.beginPath();
    cityShape().rings.forEach(function(r) {
      if (r.box[0] > b[2] || r.box[2] < b[0] || r.box[1] > b[3] || r.box[3] < b[1]) return;
      r.pts.forEach(function(p, i) {
        var x = (p[0] - b[0]) * sx, y = (b[3] - p[1]) * sy;
        if (i) ctx.lineTo(x, y); else ctx.moveTo(x, y);
      });
      ctx.closePath();
    });
  }
  // How much of the tile is in the city (a 16 px test): 0 none, 1 all, 0.5 part
  function cityShare(b) {
    var c = new OffscreenCanvas(16, 16), ctx = c.getContext('2d');
    cityPath(ctx, b, 16);
    ctx.fill('evenodd');
    var a = ctx.getImageData(0, 0, 16, 16).data, none = true, all = true;
    for (var i = 3; i < a.length; i += 4) {
      if (a[i] > 0) none = false;
      if (a[i] < 255) all = false;
    }
    return none ? 0 : all ? 1 : 0.5;
  }

  var empty = null;   // a transparent PNG, made once
  function emptyTile() {
    if (empty) return Promise.resolve({data: empty.slice(0)});
    var c = new OffscreenCanvas(256, 256);
    c.getContext('2d');   // convertToBlob needs a context
    return c.convertToBlob({type: 'image/png'}).then(function(bl) { return bl.arrayBuffer(); }).then(function(buf) {
      empty = buf;
      return {data: buf.slice(0)};
    });
  }
  // ipp2024://<tile box in EPSG:3857>: outside the city an empty tile (no
  // request), inside it the IPP's, across its limit the IPP's cut to it
  function orthoTile(params, abort) {
    var b = params.url.replace(/^ipp2024:\\/\\//, '').split(',').map(Number);
    var share = b.length === 4 && b.every(isFinite) ? cityShare(b) : 0;
    if (share === 0) return emptyTile();
    return fetch(ORTHO_URL + b.join(','), {signal: abort.signal}).then(function(res) {
      if (!res.ok) throw new Error('IPP orthophoto: HTTP ' + res.status);
      if (share === 1) return res.arrayBuffer();
      return res.blob().then(createImageBitmap).then(function(img) {
        var c = new OffscreenCanvas(256, 256), ctx = c.getContext('2d');
        cityPath(ctx, b, 256);
        ctx.clip('evenodd');
        ctx.drawImage(img, 0, 0, 256, 256);
        return c.convertToBlob({type: 'image/png'}).then(function(bl) { return bl.arrayBuffer(); });
      });
    }).then(function(buf) { return {data: buf}; });
  }

  // The orthophoto in a GL map: under the first label layer (street names on top)
  function addOrtho(m) {
    if (m.getSource('rio-mosaico')) return;
    var first = (m.getStyle().layers || []).filter(function(l) { return l.type === 'symbol'; })[0];
    m.addSource('rio-mosaico', {type: 'raster', tiles: ['ipp2024://{bbox-epsg-3857}'], tileSize: 256,
                                minzoom: ORTHO_MIN_ZOOM, maxzoom: 20, bounds: cityShape().lonlat});
    m.addLayer({id: 'rio-mosaico', type: 'raster', source: 'rio-mosaico', minzoom: ORTHO_MIN_ZOOM}, first ? first.id : undefined);
  }

  var protocol = false;
  window.addExtraBases = function(map, lm, attr) {
    lm.getLayerGroup('OpenFreeMap Positron', true).addLayer(L.maplibreGL({style: POSITRON, attribution: attr}));
    if (!window.RIO_CITY || typeof OffscreenCanvas === 'undefined' || typeof createImageBitmap === 'undefined') return;
    if (!protocol) {
      maplibregl.addProtocol('ipp2024', orthoTile);
      protocol = true;
    }
    // The plugin's credit comes from the style's sources at load (before the
    // orthophoto's): the IPP's goes in and out with the base
    var ortho = L.maplibreGL({style: BRIGHT, attribution: attr});
    ortho.on('remove', function() { if (map.attributionControl) map.attributionControl.removeAttribution(ORTHO_ATTR); });
    ortho.on('add', function() {
      if (map.attributionControl) map.attributionControl.addAttribution(ORTHO_ATTR);
      var m = ortho.getMaplibreMap && ortho.getMaplibreMap();
      if (!m) return;
      if (m.isStyleLoaded()) addOrtho(m); else m.once('load', function() { addOrtho(m); });
    });
    var group = lm.getLayerGroup(ORTHO_GROUP, true);
    group.addLayer(ortho);

    // The note: zoom in (below Leaflet zoom 18), or outside the city
    var ll = cityShape().lonlat;
    var cityBounds = L.latLngBounds([ll[1], ll[0]], [ll[3], ll[2]]);
    var note = L.control({position: 'topright'});
    note.onAdd = function() {
      var d = L.DomUtil.create('div', 'leaflet-control map-base-note');
      d.style.display = 'none';
      return d;
    };
    note.addTo(map);
    function update() {
      var d = note.getContainer();
      var key = null;
      if (map.hasLayer(group)) {
        if (!map.getBounds().intersects(cityBounds)) key = 'Rio Mosaico 2024: only in the city of Rio';
        else if (map.getZoom() < ORTHO_MIN_ZOOM + 1) key = 'Rio Mosaico 2024: zoom in closer';
      }
      d.style.display = key ? '' : 'none';
      if (!key) return;
      var lang = window.APP_LANG || 'pt';
      // the texts, also for the translations check
      var txt = key === 'Rio Mosaico 2024: zoom in closer' ? i18nText('Rio Mosaico 2024: zoom in closer', lang) :
        i18nText('Rio Mosaico 2024: only in the city of Rio', lang);
      d.setAttribute('data-i18n', key);
      d.textContent = txt;
    }
    map.on('zoomend moveend baselayerchange', update);
  };
})();
"
