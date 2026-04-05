# ---- UI ----

help_icon <- function(text) {
  shiny::span(
    class = "help-tip", "?",
    shiny::span(class = "help-text", text)
  )
}

ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      .shiny-notification {
        position: fixed !important;
        top: 50% !important;
        left: 50% !important;
        transform: translate(-50%, -50%) !important;
        z-index: 99999 !important;
        max-width: 500px !important;
        font-size: 14px !important;
      }

      /* Crosshair cursor on Leaflet map */
      .leaflet-container {
        cursor: crosshair !important;
      }

      /* Custom context menu */
      .sample-context-menu {
        position: absolute;
        z-index: 10000;
        background: white;
        border: 1px solid #ccc;
        border-radius: 4px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.2);
        padding: 5px 0;
        min-width: 160px;
        display: none;
      }
      .sample-context-menu.show {
        display: block;
      }
      .sample-context-menu-item {
        padding: 8px 15px;
        cursor: pointer;
        font-size: 13px;
      }
      .sample-context-menu-item:hover {
        background: #f0f0f0;
      }
      .sample-context-menu-item.danger {
        color: #d9534f;
      }
      .sample-context-menu-separator {
        height: 1px;
        background: #e0e0e0;
        margin: 5px 0;
      }

      /* Selection lasso */
      .selection-lasso {
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        pointer-events: none;
        z-index: 1000;
      }
      .selection-lasso polyline {
        fill: rgba(255, 102, 0, 0.15);
        stroke: #ff6600;
        stroke-width: 2;
        stroke-dasharray: 5,5;
      }

      /* Compact inputs in the controls column */
      .shiny-split-layout > div {
        padding: 0 2px !important;
      }
      .shiny-split-layout .form-group {
        margin-bottom: 5px !important;
      }
      .shiny-split-layout .form-control {
        padding: 4px 8px !important;
        height: 30px !important;
        font-size: 12px !important;
      }
      .shiny-split-layout .selectize-input {
        padding: 4px 8px !important;
        min-height: 30px !important;
        font-size: 12px !important;
      }
      .btn-block + .btn-block {
        margin-top: 5px !important;
      }
      .col-sm-2 hr {
        margin: 8px 0 !important;
      }
      .col-sm-2 .form-group {
        margin-bottom: 8px !important;
      }

      /* Help tooltip icons */
      .help-tip {
        display: inline-block;
        width: 15px;
        height: 15px;
        line-height: 15px;
        text-align: center;
        font-size: 10px;
        font-weight: bold;
        color: #fff;
        background: #999;
        border-radius: 50%;
        cursor: help;
        margin-left: 4px;
        vertical-align: middle;
        position: relative;
      }
      .help-tip:hover {
        background: #666;
      }
      .help-tip .help-text {
        display: none;
        position: absolute;
        left: 50%;
        transform: translateX(-50%);
        bottom: 22px;
        width: 220px;
        padding: 8px 10px;
        background: #333;
        color: #fff;
        font-size: 11px;
        font-weight: normal;
        line-height: 1.4;
        border-radius: 4px;
        z-index: 10001;
        text-align: left;
        white-space: normal;
      }
      .help-tip:hover .help-text {
        display: block;
      }
    "))
  ),

  shiny::titlePanel("Arc Timeline Builder 2.0"),

  shiny::fluidRow(
    # Left column - controls (width = 2)
    shiny::column(
      width = 2,
      shiny::dateInput("data_trabalho", "Working date", Sys.Date()),
      shiny::uiOutput("toggles_pessoas"),
      shiny::radioButtons(
        "modo",
        "Edit mode",
        choices = c("Route" = "OSRM", "Visit" = "Visita", "Manual Route" = "Rota Manual", "Import File" = "Importar Arquivo", "Edit Samples" = "Editar Samples", "Edit Route" = "Editar Rota"),
        selected = "OSRM"
      ),
      shiny::hr(),
      # OSRM
      shiny::conditionalPanel(
        "input.modo == 'OSRM'",
        shiny::tags$div(
          shiny::tags$strong("Route"),
          help_icon("Click waypoints on the map, then calculate a snap-to-road route. Car/Foot/Bike/Bus use local OSRM (Docker ports 5000-5003). Metro/Train/Tram use Google Maps Directions API (requires GOOGLE_MAPS_API_KEY).")
        ),
        shiny::selectInput(
          "osrm_perfil", "Profile:",
          choices = c("Car" = "car", "Foot" = "foot", "Bike" = "bike", "Bus" = "bus",
                      "Metro" = "metro", "Train" = "train", "Tram/VLT" = "tram"),
          selected = "car", width = "100%"
        ),
        shiny::splitLayout(
          cellWidths = c("50%", "50%"),
          shiny::actionButton("desfazer_ponto_osrm", "Undo", class = "btn-sm"),
          shiny::actionButton("limpar_pontos_osrm", "Clear", class = "btn-sm")
        ),
        shiny::br(),
        shiny::actionButton("calcular_osrm", "Calculate route", class = "btn-primary btn-sm btn-block")
      ),
      # Visit
      shiny::conditionalPanel(
        "input.modo == 'Visita'",
        shiny::tags$div(
          shiny::tags$strong("Visit"),
          help_icon("Mark a stationary visit. Type a name from your frequent places (config_local.R) to auto-fill coordinates. Entry/exit define the stay duration.")
        ),
        shiny::textInput("visita_nome", "Visit name", "", width = "100%"),
        shiny::tags$small("Click the map to set the location."),
        shiny::tags$label("Entry:"),
        shiny::splitLayout(
          cellWidths = c("60%", "40%"),
          shiny::dateInput("visita_data_inicio", NULL, Sys.Date(), width = "100%"),
          shiny::textInput("visita_hora_inicio", NULL, "08:00", width = "100%")
        ),
        shiny::tags$label("Exit:"),
        shiny::splitLayout(
          cellWidths = c("60%", "40%"),
          shiny::dateInput("visita_data_fim", NULL, Sys.Date(), width = "100%"),
          shiny::textInput("visita_hora_fim", NULL, "09:00", width = "100%")
        ),
        shiny::actionButton("adicionar_visita", "Add visit", class = "btn-success btn-sm btn-block")
      ),
      # Manual route
      shiny::conditionalPanel(
        "input.modo == 'Rota Manual'",
        shiny::tags$div(
          shiny::tags$strong("Manual Route"),
          help_icon("Click points on the map to draw a freeform polyline route. Timestamps are evenly interpolated between start and end times.")
        ),
        shiny::tags$small("Click the map to set the points."),
        shiny::selectInput(
          "manual_activity_type", "Activity type:",
          choices = c(
            "Walking" = "walking", "Running" = "running", "Hiking" = "hiking",
            "Cycling" = "cycling", "Skateboard" = "skateboarding",
            "Inline skating" = "inline_skating",
            "Car" = "car", "Taxi" = "taxi", "Motorcycle" = "motorcycle",
            "Scooter" = "scooter", "Bus" = "bus",
            "Metro" = "metro", "Tram" = "tram", "Train" = "train",
            "Tuk tuk" = "tuktuk", "Songthaew" = "songthaew",
            "Cable car" = "cable_car", "Funicular" = "funicular",
            "Chairlift" = "chairlift", "Ski lift" = "ski_lift",
            "Boat" = "boat", "Kayaking" = "kayaking", "Rowing" = "rowing",
            "Surfing" = "surfing", "Swimming" = "swimming",
            "Airplane" = "airplane", "Hot air balloon" = "hot_air_balloon",
            "Tractor" = "tractor", "Horseback" = "horseback",
            "Wheelchair" = "wheelchair",
            "Skiing" = "skiing", "Snowboarding" = "snowboarding",
            "Golf" = "golf"
          ),
          selected = "car", width = "100%"
        ),
        shiny::tags$label("Start:"),
        shiny::splitLayout(
          cellWidths = c("60%", "40%"),
          shiny::dateInput("manual_data_inicio", NULL, Sys.Date(), width = "100%"),
          shiny::textInput("manual_hora_inicio", NULL, "08:00", width = "100%")
        ),
        shiny::tags$label("End:"),
        shiny::splitLayout(
          cellWidths = c("60%", "40%"),
          shiny::dateInput("manual_data_fim", NULL, Sys.Date(), width = "100%"),
          shiny::textInput("manual_hora_fim", NULL, "09:00", width = "100%")
        ),
        shiny::splitLayout(
          cellWidths = c("50%", "50%"),
          shiny::actionButton("desfazer_ponto_manual", "Undo", class = "btn-sm"),
          shiny::actionButton("limpar_pontos_manual", "Clear", class = "btn-sm")
        ),
        shiny::br(),
        shiny::actionButton("adicionar_manual", "Add route", class = "btn-success btn-sm btn-block")
      ),
      # Import file
      shiny::conditionalPanel(
        "input.modo == 'Importar Arquivo'",
        shiny::tags$div(
          shiny::tags$strong("Import File"),
          help_icon("Import a route from GeoJSON, GPX, KML, or GPKG. FlightRadar24 KML files are detected automatically and timestamps are extracted from the track.")
        ),
        shiny::fileInput(
          "arquivo_geo", NULL,
          accept = c(".geojson", ".json", ".gpx", ".kml", ".gpkg"),
          buttonLabel = "Browse...",
          placeholder = "GeoJSON/GPX/KML"
        ),
        shiny::radioButtons(
          "direcao_arquivo", NULL,
          choices = c("Normal" = "normal", "Reverse" = "inverter"),
          selected = "normal", inline = TRUE
        ),
        shiny::selectInput(
          "import_activity_type", "Activity type:",
          choices = c(
            "Walking" = "walking", "Running" = "running", "Hiking" = "hiking",
            "Cycling" = "cycling", "Skateboard" = "skateboarding",
            "Inline skating" = "inline_skating",
            "Car" = "car", "Taxi" = "taxi", "Motorcycle" = "motorcycle",
            "Scooter" = "scooter", "Bus" = "bus",
            "Metro" = "metro", "Tram" = "tram", "Train" = "train",
            "Tuk tuk" = "tuktuk", "Songthaew" = "songthaew",
            "Cable car" = "cable_car", "Funicular" = "funicular",
            "Chairlift" = "chairlift", "Ski lift" = "ski_lift",
            "Boat" = "boat", "Kayaking" = "kayaking", "Rowing" = "rowing",
            "Surfing" = "surfing", "Swimming" = "swimming",
            "Airplane" = "airplane", "Hot air balloon" = "hot_air_balloon",
            "Tractor" = "tractor", "Horseback" = "horseback",
            "Wheelchair" = "wheelchair",
            "Skiing" = "skiing", "Snowboarding" = "snowboarding",
            "Golf" = "golf"
          ),
          selected = "car", width = "100%"
        ),
        shiny::tags$label("Start:"),
        shiny::splitLayout(
          cellWidths = c("60%", "40%"),
          shiny::dateInput("import_data_inicio", NULL, Sys.Date(), width = "100%"),
          shiny::textInput("import_hora_inicio", NULL, "08:00", width = "100%")
        ),
        shiny::tags$label("End:"),
        shiny::splitLayout(
          cellWidths = c("60%", "40%"),
          shiny::dateInput("import_data_fim", NULL, Sys.Date(), width = "100%"),
          shiny::textInput("import_hora_fim", NULL, "09:00", width = "100%")
        ),
        shiny::actionButton("adicionar_import", "Add route", class = "btn-success btn-sm btn-block")
      ),
      # Edit Samples
      shiny::conditionalPanel(
        "input.modo == 'Editar Samples'",
        shiny::tags$div(
          shiny::tags$strong("Edit Samples"),
          help_icon("Load existing samples from weekly .json.gz backup files. Drag markers to fix GPS positions, snap to road via OSRM, ignore or discard bad points. Right-click a sample for more options.")
        ),
        shiny::tags$small("Load samples:"),
        shiny::splitLayout(
          cellWidths = c("60%", "40%"),
          shiny::dateInput("edit_samples_data_inicio", NULL, Sys.Date(), width = "100%"),
          shiny::textInput("edit_samples_hora_inicio", NULL, "00:00", width = "100%")
        ),
        shiny::splitLayout(
          cellWidths = c("60%", "40%"),
          shiny::dateInput("edit_samples_data_fim", NULL, Sys.Date(), width = "100%"),
          shiny::textInput("edit_samples_hora_fim", NULL, "23:59", width = "100%")
        ),
        shiny::actionButton("carregar_samples", "Load", class = "btn-primary btn-sm btn-block"),
        shiny::splitLayout(
          cellWidths = c("60%", "40%"),
          shiny::selectInput(
            "edit_osrm_perfil", NULL,
            choices = c("Car" = "car", "Foot" = "foot", "Bike" = "bike", "Bus" = "bus",
                        "Metro" = "metro", "Train" = "train", "Tram/VLT" = "tram"),
            selected = "car", width = "100%"
          ),
          shiny::actionButton("snap_to_road", "Snap", class = "btn-warning btn-sm", style = "margin-top: 0;",
                              title = "Snap all active samples to the nearest road via OSRM and generate virtual intermediate points along the route")
        ),
        shiny::tags$div(
          shiny::checkboxInput("modo_selecao", "Selection mode", FALSE),
          style = "display: inline-block; margin-bottom: 0;"
        ),
        help_icon("Draw a lasso on the map to select multiple samples at once for bulk ignore or discard."),
        shiny::splitLayout(
          cellWidths = c("50%", "50%"),
          shiny::actionButton("ignorar_selecionados", "Ignore", class = "btn-xs"),
          shiny::actionButton("descartar_selecionados", "Discard", class = "btn-xs btn-danger")
        ),
        shiny::br(),
        shiny::downloadButton("download_edit_arc", "Export (zip)", class = "btn-success btn-sm btn-block")
      ),
      # Edit Route
      shiny::conditionalPanel(
        "input.modo == 'Editar Rota'",
        shiny::tags$div(
          shiny::tags$strong("Edit Route"),
          help_icon("Select a trip from the timeline to edit its waypoints. Drag nodes to move, right-click to delete, or insert new waypoints. Average speed is preserved when you apply changes.")
        ),
        shiny::tags$small("Select a route from the timeline:"),
        shiny::selectInput(
          "rota_editar_selecionada", "Route:",
          choices = NULL, width = "100%"
        ),
        shiny::actionButton("carregar_rota_edit", "Load route", class = "btn-primary btn-sm btn-block"),
        shiny::hr(),
        shiny::tags$small("Drag: move nodes", style = "color: #666;"),
        shiny::br(),
        shiny::tags$small("Right-click: delete node", style = "color: #666;"),
        shiny::br(),
        shiny::checkboxInput("modo_inserir_waypoint", "Insert waypoint mode", FALSE),
        shiny::tags$small("Click the map to insert", style = "color: #888;"),
        shiny::hr(),
        shiny::checkboxInput("modo_selecao_nodes", "Selection mode (lasso)", FALSE),
        shiny::splitLayout(
          cellWidths = c("50%", "50%"),
          shiny::actionButton("deletar_nodes_selecionados", "Delete sel.", class = "btn-xs btn-danger"),
          shiny::actionButton("limpar_selecao_nodes", "Clear sel.", class = "btn-xs")
        ),
        shiny::hr(),
        shiny::splitLayout(
          cellWidths = c("50%", "50%"),
          shiny::actionButton("aplicar_edicoes_rota", "Apply", class = "btn-success btn-sm"),
          shiny::actionButton("cancelar_edicoes_rota", "Cancel", class = "btn-danger btn-sm")
        )
      ),
      shiny::hr(),
      shiny::actionButton("limpar_tudo", "Clear all", class = "btn-danger")
    ),

    # Center column - map (width = 8)
    shiny::column(
      width = 8,
      leaflet::leafletOutput("map", height = "700px")
    ),

    # Right column - timeline (width = 2)
    shiny::column(
      width = 2,
      shiny::h4(
        "Day timeline",
        help_icon("Timeline items for the current working date. Click Edit to change times, Delete to remove. Items are sorted by start time.")
      ),
      shiny::uiOutput("timeline_list"),
      shiny::hr(),
      shiny::downloadButton("download_arc", "Download Arc package (zip)", class = "btn-block"),
      help_icon("Export as a LocoKit2 JSON zip (items + samples + metadata), ready for Arc Editor import."),
      shiny::br(),
      shiny::downloadButton("download_gpx", "Download GPX (all samples)", class = "btn-block"),
      help_icon("Export all samples as a GPX track for use with other mapping tools.")
    )
  ),

  # Context menu for samples
  shiny::div(
    id = "sample-context-menu",
    class = "sample-context-menu",
    shiny::div(id = "ctx-propriedades", class = "sample-context-menu-item", "Properties"),
    shiny::div(class = "sample-context-menu-separator"),
    shiny::div(id = "ctx-ignorar", class = "sample-context-menu-item", "Ignore"),
    shiny::div(id = "ctx-inserir", class = "sample-context-menu-item", "Insert sample after"),
    shiny::div(class = "sample-context-menu-separator"),
    shiny::div(id = "ctx-descartar", class = "sample-context-menu-item danger", "Discard")
  ),

  # Context menu for route nodes
  shiny::div(
    id = "route-node-context-menu",
    class = "sample-context-menu",
    shiny::div(id = "ctx-node-props", class = "sample-context-menu-item", "Node properties"),
    shiny::div(class = "sample-context-menu-separator"),
    shiny::div(id = "ctx-delete-node", class = "sample-context-menu-item danger", "Delete node")
  ),

  # JavaScript for context menu
  shiny::tags$script(shiny::HTML("
    $(document).ready(function() {
      var currentSampleId = null;
      var $menu = $('#sample-context-menu');

      // Hide menu on outside click
      $(document).on('click', function() {
        $menu.removeClass('show');
      });

      // Prevent click propagation on the menu
      $menu.on('click', function(e) {
        e.stopPropagation();
      });

      // Handler for each menu option
      $('#ctx-propriedades').on('click', function() {
        if (currentSampleId) {
          Shiny.setInputValue('ctx_propriedades', {id: currentSampleId, ts: Date.now()});
        }
        $menu.removeClass('show');
      });

      $('#ctx-ignorar').on('click', function() {
        if (currentSampleId) {
          Shiny.setInputValue('ctx_ignorar', {id: currentSampleId, ts: Date.now()});
        }
        $menu.removeClass('show');
      });

      $('#ctx-inserir').on('click', function() {
        if (currentSampleId) {
          Shiny.setInputValue('ctx_inserir', {id: currentSampleId, ts: Date.now()});
        }
        $menu.removeClass('show');
      });

      $('#ctx-descartar').on('click', function() {
        if (currentSampleId) {
          Shiny.setInputValue('ctx_descartar', {id: currentSampleId, ts: Date.now()});
        }
        $menu.removeClass('show');
      });

      // Expose global function to show menu (called from Shiny)
      window.showSampleContextMenu = function(sampleId, x, y) {
        currentSampleId = sampleId;
        $menu.css({left: x + 'px', top: y + 'px'}).addClass('show');
      };

      // Handler for Shiny message to set up context menu
      Shiny.addCustomMessageHandler('setup_context_menu', function(msg) {
        setTimeout(function() {
          var widget = HTMLWidgets.find('#map');
          if (!widget) return;
          var map = widget.getMap();
          if (!map) return;

          // Add handlers on each Marker with layerId (Arc samples)
          map.eachLayer(function(layer) {
            if (layer.options && layer.options.layerId && layer.options.draggable) {
              var layerId = layer.options.layerId;

              // Remove old handlers
              layer.off('contextmenu');
              layer.off('click');
              layer.off('dragend');

              // Context menu (right click)
              layer.on('contextmenu', function(e) {
                L.DomEvent.stopPropagation(e);
                L.DomEvent.preventDefault(e);
                showSampleContextMenu(layerId, e.originalEvent.pageX, e.originalEvent.pageY);
              });

              // Click to select/deselect
              layer.on('click', function(e) {
                L.DomEvent.stopPropagation(e);
                var idx = selectedSampleIds.indexOf(layerId);
                if (idx === -1) {
                  selectedSampleIds.push(layerId);
                } else {
                  selectedSampleIds.splice(idx, 1);
                }
                Shiny.setInputValue('selected_samples', selectedSampleIds);
                updateSelectionVisual();
              });

              // Drag end - notify Shiny
              layer.on('dragend', function(e) {
                var newLatLng = layer.getLatLng();
                Shiny.setInputValue('map_marker_dragend', {
                  id: layerId,
                  lat: newLatLng.lat,
                  lng: newLatLng.lng,
                  ts: Date.now()
                });
              });
            }
          });
        }, 300);
      });

      // ---- Selection mode (lasso) ----
      var selectionMode = false;
      var lassoSvg = null;
      var lassoPoints = [];
      var selectedSampleIds = [];
      var mapInstance = null;

      // Listen to selection mode checkbox changes
      $(document).on('shiny:inputchanged', function(e) {
        if (e.name === 'modo_selecao') {
          selectionMode = e.value;

          // Get map instance
          var widget = HTMLWidgets.find('#map');
          if (widget) mapInstance = widget.getMap();

          if (mapInstance) {
            if (selectionMode) {
              // Disable map dragging
              mapInstance.dragging.disable();
              $('#map').css('cursor', 'crosshair');
            } else {
              // Re-enable map dragging
              mapInstance.dragging.enable();
              $('#map').css('cursor', '');
              // Clear selection on deactivation
              selectedSampleIds = [];
              Shiny.setInputValue('selected_samples', []);
              updateSelectionVisual();
            }
          }
        }
      });

      // Create lasso SVG
      function createLassoSvg() {
        if (lassoSvg) lassoSvg.remove();
        var mapEl = $('#map');
        var offset = mapEl.offset();
        lassoSvg = $('<svg class=\"selection-lasso\"><polyline points=\"\"></polyline></svg>');
        lassoSvg.css({
          position: 'fixed',
          left: offset.left + 'px',
          top: offset.top + 'px',
          width: mapEl.width() + 'px',
          height: mapEl.height() + 'px'
        });
        $('body').append(lassoSvg);
        return lassoSvg;
      }

      // Mouse down on the map
      $('#map').on('mousedown', function(e) {
        if (!selectionMode) return;
        if (e.button !== 0) return;

        e.preventDefault();
        e.stopPropagation();

        var mapOffset = $('#map').offset();
        lassoPoints = [];
        createLassoSvg();

        $(document).on('mousemove.lasso', function(e) {
          var x = e.pageX - mapOffset.left;
          var y = e.pageY - mapOffset.top;
          lassoPoints.push({x: x, y: y});

          var pointsStr = lassoPoints.map(function(p) { return p.x + ',' + p.y; }).join(' ');
          lassoSvg.find('polyline').attr('points', pointsStr);
        });

        $(document).on('mouseup.lasso', function(e) {
          $(document).off('mousemove.lasso mouseup.lasso');

          if (lassoPoints.length < 3) {
            if (lassoSvg) lassoSvg.remove();
            lassoSvg = null;
            return;
          }

          // Close the polygon
          lassoPoints.push(lassoPoints[0]);

          // Find samples inside the lasso
          if (!mapInstance) return;

          var mapOffset = $('#map').offset();

          mapInstance.eachLayer(function(layer) {
            if (layer.options && layer.options.layerId && layer.options.draggable) {
              var point = mapInstance.latLngToContainerPoint(layer.getLatLng());

              if (pointInPolygon(point.x, point.y, lassoPoints)) {
                var id = layer.options.layerId;
                if (selectedSampleIds.indexOf(id) === -1) {
                  selectedSampleIds.push(id);
                }
              }
            }
          });

          if (lassoSvg) lassoSvg.remove();
          lassoSvg = null;

          Shiny.setInputValue('selected_samples', selectedSampleIds);
          updateSelectionVisual();
        });
      });

      // Check if point is inside polygon (ray casting)
      function pointInPolygon(x, y, polygon) {
        var inside = false;
        for (var i = 0, j = polygon.length - 1; i < polygon.length; j = i++) {
          var xi = polygon[i].x, yi = polygon[i].y;
          var xj = polygon[j].x, yj = polygon[j].y;

          if (((yi > y) !== (yj > y)) && (x < (xj - xi) * (y - yi) / (yj - yi) + xi)) {
            inside = !inside;
          }
        }
        return inside;
      }

      // Update visual of selected items (Markers use _icon)
      function updateSelectionVisual() {
        if (!mapInstance) {
          var widget = HTMLWidgets.find('#map');
          if (widget) mapInstance = widget.getMap();
        }
        if (!mapInstance) return;

        mapInstance.eachLayer(function(layer) {
          if (layer.options && layer.options.layerId && layer._icon) {
            var id = layer.options.layerId;
            var isSelected = selectedSampleIds.indexOf(id) !== -1;
            if (isSelected) {
              // Selected: add color filter
              $(layer._icon).css('filter', 'hue-rotate(180deg) brightness(1.3)');
            } else {
              // Not selected: remove filter
              $(layer._icon).css('filter', '');
            }
          }
        });
      }

      // Handler to clear selection
      Shiny.addCustomMessageHandler('clear_selection', function(msg) {
        selectedSampleIds = [];
        Shiny.setInputValue('selected_samples', []);
        updateSelectionVisual();
      });

      // ---- Route editing ----
      var currentNodeId = null;
      var $nodeMenu = $('#route-node-context-menu');
      var routeEditMode = false;
      var insertWaypointMode = false;

      // Hide node menu on outside click
      $(document).on('click', function() {
        $nodeMenu.removeClass('show');
      });

      $nodeMenu.on('click', function(e) {
        e.stopPropagation();
      });

      // Handler to delete node
      $('#ctx-delete-node').on('click', function() {
        if (currentNodeId) {
          Shiny.setInputValue('ctx_delete_node', {id: currentNodeId, ts: Date.now()});
        }
        $nodeMenu.removeClass('show');
      });

      // Handler for node properties
      $('#ctx-node-props').on('click', function() {
        if (currentNodeId) {
          Shiny.setInputValue('ctx_node_props', {id: currentNodeId, ts: Date.now()});
        }
        $nodeMenu.removeClass('show');
      });

      // Global function to show route node context menu
      window.showRouteNodeContextMenu = function(nodeId, x, y) {
        currentNodeId = nodeId;
        $nodeMenu.css({left: x + 'px', top: y + 'px'}).addClass('show');
      };

      // Handler to set up route nodes (called from Shiny)
      Shiny.addCustomMessageHandler('setup_route_nodes', function(msg) {
        setTimeout(function() {
          var widget = HTMLWidgets.find('#map');
          if (!widget) return;
          var map = widget.getMap();
          if (!map) return;

          map.eachLayer(function(layer) {
            // Check if it's a draggable marker in the edit_route_nodes group
            if (layer.options && layer.options.layerId && layer.options.draggable) {
              var group = layer.options.group;
              if (group === 'edit_route_nodes') {
                var nodeId = layer.options.layerId;

                // Remove old handlers
                layer.off('contextmenu');
                layer.off('dragend');

                // Context menu
                layer.on('contextmenu', function(e) {
                  L.DomEvent.stopPropagation(e);
                  L.DomEvent.preventDefault(e);
                  showRouteNodeContextMenu(nodeId, e.originalEvent.pageX, e.originalEvent.pageY);
                });

                // Drag end
                layer.on('dragend', function(e) {
                  var newLatLng = layer.getLatLng();
                  Shiny.setInputValue('route_node_dragend', {
                    id: nodeId,
                    lat: newLatLng.lat,
                    lng: newLatLng.lng,
                    ts: Date.now()
                  });
                });
              }
            }
          });
        }, 500);  // Aumentado para garantir que markers estejam renderizados
      });

      // Listen to insert waypoint mode checkbox changes
      $(document).on('shiny:inputchanged', function(e) {
        if (e.name === 'modo_inserir_waypoint') {
          insertWaypointMode = e.value;

          var widget = HTMLWidgets.find('#map');
          if (!widget) return;
          var map = widget.getMap();
          if (!map) return;

          if (insertWaypointMode) {
            map.on('click', insertWaypointHandler);
            $('#map').css('cursor', 'crosshair');
          } else {
            map.off('click', insertWaypointHandler);
            $('#map').css('cursor', '');
          }
        }
      });

      function insertWaypointHandler(e) {
        Shiny.setInputValue('insert_waypoint_click', {
          lat: e.latlng.lat,
          lng: e.latlng.lng,
          ts: Date.now()
        });
      }

      // Handler to clear route edit state
      Shiny.addCustomMessageHandler('clear_route_edit', function(msg) {
        routeEditMode = false;
        insertWaypointMode = false;

        var widget = HTMLWidgets.find('#map');
        if (widget) {
          var map = widget.getMap();
          if (map) {
            map.off('click', insertWaypointHandler);
          }
        }
        $('#map').css('cursor', '');
      });

      // ---- Lasso selection for route nodes ----
      var nodeSelectionMode = false;
      var nodeLassoSvg = null;
      var nodeLassoPoints = [];
      var selectedNodeIds = [];
      var nodeMapInstance = null;

      // Listen to node selection mode checkbox changes
      $(document).on('shiny:inputchanged', function(e) {
        if (e.name === 'modo_selecao_nodes') {
          nodeSelectionMode = e.value;

          var widget = HTMLWidgets.find('#map');
          if (widget) nodeMapInstance = widget.getMap();

          if (nodeMapInstance) {
            if (nodeSelectionMode) {
              nodeMapInstance.dragging.disable();
              $('#map').css('cursor', 'crosshair');
            } else {
              nodeMapInstance.dragging.enable();
              $('#map').css('cursor', '');
              selectedNodeIds = [];
              Shiny.setInputValue('selected_route_nodes', []);
              updateNodeSelectionVisual();
            }
          }
        }
      });

      // Create lasso SVG for nodes
      function createNodeLassoSvg() {
        if (nodeLassoSvg) nodeLassoSvg.remove();
        var mapEl = $('#map');
        var offset = mapEl.offset();
        nodeLassoSvg = $('<svg class=\"selection-lasso\"><polyline points=\"\"></polyline></svg>');
        nodeLassoSvg.css({
          position: 'fixed',
          left: offset.left + 'px',
          top: offset.top + 'px',
          width: mapEl.width() + 'px',
          height: mapEl.height() + 'px'
        });
        $('body').append(nodeLassoSvg);
        return nodeLassoSvg;
      }

      // Mouse down on the map for node selection
      $('#map').on('mousedown.nodeselect', function(e) {
        if (!nodeSelectionMode) return;
        if (e.button !== 0) return;

        e.preventDefault();
        e.stopPropagation();

        var mapOffset = $('#map').offset();
        nodeLassoPoints = [];
        createNodeLassoSvg();

        $(document).on('mousemove.nodelasso', function(e) {
          var x = e.pageX - mapOffset.left;
          var y = e.pageY - mapOffset.top;
          nodeLassoPoints.push({x: x, y: y});

          var pointsStr = nodeLassoPoints.map(function(p) { return p.x + ',' + p.y; }).join(' ');
          nodeLassoSvg.find('polyline').attr('points', pointsStr);
        });

        $(document).on('mouseup.nodelasso', function(e) {
          $(document).off('mousemove.nodelasso mouseup.nodelasso');

          if (nodeLassoPoints.length < 3) {
            if (nodeLassoSvg) nodeLassoSvg.remove();
            nodeLassoSvg = null;
            return;
          }

          nodeLassoPoints.push(nodeLassoPoints[0]);

          if (!nodeMapInstance) return;

          nodeMapInstance.eachLayer(function(layer) {
            if (layer.options && layer.options.layerId && layer.options.draggable) {
              var group = layer.options.group;
              if (group === 'edit_route_nodes') {
                var point = nodeMapInstance.latLngToContainerPoint(layer.getLatLng());

                if (pointInPolygon(point.x, point.y, nodeLassoPoints)) {
                  var id = layer.options.layerId;
                  if (selectedNodeIds.indexOf(id) === -1) {
                    selectedNodeIds.push(id);
                  }
                }
              }
            }
          });

          if (nodeLassoSvg) nodeLassoSvg.remove();
          nodeLassoSvg = null;

          Shiny.setInputValue('selected_route_nodes', selectedNodeIds);
          updateNodeSelectionVisual();
        });
      });

      // Update visual of selected nodes
      function updateNodeSelectionVisual() {
        if (!nodeMapInstance) {
          var widget = HTMLWidgets.find('#map');
          if (widget) nodeMapInstance = widget.getMap();
        }
        if (!nodeMapInstance) return;

        nodeMapInstance.eachLayer(function(layer) {
          if (layer.options && layer.options.layerId && layer._icon) {
            var group = layer.options.group;
            if (group === 'edit_route_nodes') {
              var id = layer.options.layerId;
              var isSelected = selectedNodeIds.indexOf(id) !== -1;
              if (isSelected) {
                $(layer._icon).css('filter', 'brightness(1.5) drop-shadow(0 0 4px yellow)');
              } else {
                $(layer._icon).css('filter', '');
              }
            }
          }
        });
      }

      // Handler to clear selection de nos
      Shiny.addCustomMessageHandler('clear_node_selection', function(msg) {
        selectedNodeIds = [];
        Shiny.setInputValue('selected_route_nodes', []);
        updateNodeSelectionVisual();
      });
    });
  "))
)
