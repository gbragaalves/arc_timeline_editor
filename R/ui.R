# ---- UI ----

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

      /* Cruzinha no mapa Leaflet */
      .leaflet-container {
        cursor: crosshair !important;
      }

      /* Menu de contexto customizado */
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

      /* Laco de selecao */
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

      /* Inputs mais compactos na coluna de controles */
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
    "))
  ),

  shiny::titlePanel("Arc Timeline Builder 2.0"),

  shiny::fluidRow(
    # Coluna esquerda - controles (width = 2)
    shiny::column(
      width = 2,
      shiny::dateInput("data_trabalho", "Data principal", Sys.Date()),
      shiny::uiOutput("toggles_pessoas"),
      shiny::radioButtons(
        "modo",
        "Modo de edição",
        choices = c("OSRM", "Visita", "Rota Manual", "Importar Arquivo", "Editar Samples", "Editar Rota"),
        selected = "OSRM"
      ),
      shiny::hr(),
      # OSRM
      shiny::conditionalPanel(
        "input.modo == 'OSRM'",
        shiny::selectInput(
          "osrm_perfil", "Perfil:",
          choices = c("Carro" = "car", "A pé" = "foot", "Bike" = "bike", "Bus" = "bus"),
          selected = "car", width = "100%"
        ),
        shiny::splitLayout(
          cellWidths = c("50%", "50%"),
          shiny::actionButton("desfazer_ponto_osrm", "Desfazer", class = "btn-sm"),
          shiny::actionButton("limpar_pontos_osrm", "Limpar", class = "btn-sm")
        ),
        shiny::br(),
        shiny::actionButton("calcular_osrm", "Calcular rota OSRM", class = "btn-primary btn-sm btn-block")
      ),
      # Visita
      shiny::conditionalPanel(
        "input.modo == 'Visita'",
        shiny::textInput("visita_nome", "Nome da visita", "", width = "100%"),
        shiny::tags$small("Clique no mapa para marcar o local."),
        shiny::tags$label("Entrada:"),
        shiny::splitLayout(
          cellWidths = c("60%", "40%"),
          shiny::dateInput("visita_data_inicio", NULL, Sys.Date(), width = "100%"),
          shiny::textInput("visita_hora_inicio", NULL, "08:00", width = "100%")
        ),
        shiny::tags$label("Saida:"),
        shiny::splitLayout(
          cellWidths = c("60%", "40%"),
          shiny::dateInput("visita_data_fim", NULL, Sys.Date(), width = "100%"),
          shiny::textInput("visita_hora_fim", NULL, "09:00", width = "100%")
        ),
        shiny::actionButton("adicionar_visita", "Adicionar visita", class = "btn-success btn-sm btn-block")
      ),
      # Rota manual
      shiny::conditionalPanel(
        "input.modo == 'Rota Manual'",
        shiny::tags$small("Clique no mapa para definir os pontos."),
        shiny::selectInput(
          "manual_activity_type", "Tipo de atividade:",
          choices = c(
            "Carro" = "car", "Taxi" = "taxi", "A pe" = "walking",
            "Bus" = "bus", "Metro" = "metro", "Tram" = "tram",
            "Trem" = "train", "Bike" = "cycling", "Aviao" = "airplane",
            "Tuk tuk" = "tuk_tuk", "Teleferico" = "cable_car",
            "Funicular" = "funicular", "Skate" = "skateboarding"
          ),
          selected = "car", width = "100%"
        ),
        shiny::tags$label("Inicio:"),
        shiny::splitLayout(
          cellWidths = c("60%", "40%"),
          shiny::dateInput("manual_data_inicio", NULL, Sys.Date(), width = "100%"),
          shiny::textInput("manual_hora_inicio", NULL, "08:00", width = "100%")
        ),
        shiny::tags$label("Fim:"),
        shiny::splitLayout(
          cellWidths = c("60%", "40%"),
          shiny::dateInput("manual_data_fim", NULL, Sys.Date(), width = "100%"),
          shiny::textInput("manual_hora_fim", NULL, "09:00", width = "100%")
        ),
        shiny::splitLayout(
          cellWidths = c("50%", "50%"),
          shiny::actionButton("desfazer_ponto_manual", "Desfazer", class = "btn-sm"),
          shiny::actionButton("limpar_pontos_manual", "Limpar", class = "btn-sm")
        ),
        shiny::br(),
        shiny::actionButton("adicionar_manual", "Adicionar rota", class = "btn-success btn-sm btn-block")
      ),
      # Importar arquivo
      shiny::conditionalPanel(
        "input.modo == 'Importar Arquivo'",
        shiny::fileInput(
          "arquivo_geo", NULL,
          accept = c(".geojson", ".json", ".gpx", ".kml", ".gpkg"),
          buttonLabel = "Arquivo...",
          placeholder = "GeoJSON/GPX/KML"
        ),
        shiny::radioButtons(
          "direcao_arquivo", NULL,
          choices = c("Normal" = "normal", "Inverter" = "inverter"),
          selected = "normal", inline = TRUE
        ),
        shiny::selectInput(
          "import_activity_type", "Tipo de atividade:",
          choices = c(
            "Carro" = "car", "Taxi" = "taxi", "A pe" = "walking",
            "Bus" = "bus", "Metro" = "metro", "Tram" = "tram",
            "Trem" = "train", "Bike" = "cycling", "Aviao" = "airplane",
            "Tuk tuk" = "tuk_tuk", "Teleferico" = "cable_car",
            "Funicular" = "funicular", "Skate" = "skateboarding"
          ),
          selected = "car", width = "100%"
        ),
        shiny::tags$label("Inicio:"),
        shiny::splitLayout(
          cellWidths = c("60%", "40%"),
          shiny::dateInput("import_data_inicio", NULL, Sys.Date(), width = "100%"),
          shiny::textInput("import_hora_inicio", NULL, "08:00", width = "100%")
        ),
        shiny::tags$label("Fim:"),
        shiny::splitLayout(
          cellWidths = c("60%", "40%"),
          shiny::dateInput("import_data_fim", NULL, Sys.Date(), width = "100%"),
          shiny::textInput("import_hora_fim", NULL, "09:00", width = "100%")
        ),
        shiny::actionButton("adicionar_import", "Adicionar rota", class = "btn-success btn-sm btn-block")
      ),
      # Editar Samples
      shiny::conditionalPanel(
        "input.modo == 'Editar Samples'",
        shiny::tags$small("Carregar samples:"),
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
        shiny::actionButton("carregar_samples", "Carregar", class = "btn-primary btn-sm btn-block"),
        shiny::splitLayout(
          cellWidths = c("60%", "40%"),
          shiny::selectInput(
            "edit_osrm_perfil", NULL,
            choices = c("Carro" = "car", "A pé" = "foot", "Bike" = "bike", "Bus" = "bus"),
            selected = "car", width = "100%"
          ),
          shiny::actionButton("snap_to_road", "Snap", class = "btn-warning btn-sm", style = "margin-top: 0;")
        ),
        shiny::checkboxInput("modo_selecao", "Modo selecao", FALSE),
        shiny::splitLayout(
          cellWidths = c("50%", "50%"),
          shiny::actionButton("ignorar_selecionados", "Ignorar", class = "btn-xs"),
          shiny::actionButton("descartar_selecionados", "Descartar", class = "btn-xs btn-danger")
        ),
        shiny::br(),
        shiny::downloadButton("download_edit_arc", "Exportar (zip)", class = "btn-success btn-sm btn-block")
      ),
      # Editar Rota
      shiny::conditionalPanel(
        "input.modo == 'Editar Rota'",
        shiny::tags$small("Selecione uma rota da timeline:"),
        shiny::selectInput(
          "rota_editar_selecionada", "Rota:",
          choices = NULL, width = "100%"
        ),
        shiny::actionButton("carregar_rota_edit", "Carregar rota", class = "btn-primary btn-sm btn-block"),
        shiny::hr(),
        shiny::tags$small("Arrastar: mover nos", style = "color: #666;"),
        shiny::br(),
        shiny::tags$small("Clique direito: deletar no", style = "color: #666;"),
        shiny::br(),
        shiny::checkboxInput("modo_inserir_waypoint", "Modo inserir waypoint", FALSE),
        shiny::tags$small("Clique no mapa para inserir", style = "color: #888;"),
        shiny::hr(),
        shiny::checkboxInput("modo_selecao_nodes", "Modo selecao (laco)", FALSE),
        shiny::splitLayout(
          cellWidths = c("50%", "50%"),
          shiny::actionButton("deletar_nodes_selecionados", "Deletar sel.", class = "btn-xs btn-danger"),
          shiny::actionButton("limpar_selecao_nodes", "Limpar sel.", class = "btn-xs")
        ),
        shiny::hr(),
        shiny::splitLayout(
          cellWidths = c("50%", "50%"),
          shiny::actionButton("aplicar_edicoes_rota", "Aplicar", class = "btn-success btn-sm"),
          shiny::actionButton("cancelar_edicoes_rota", "Cancelar", class = "btn-danger btn-sm")
        )
      ),
      shiny::hr(),
      shiny::actionButton("limpar_tudo", "Limpar tudo", class = "btn-danger")
    ),

    # Coluna central - mapa (width = 8)
    shiny::column(
      width = 8,
      leaflet::leafletOutput("map", height = "700px")
    ),

    # Coluna direita - timeline (width = 2)
    shiny::column(
      width = 2,
      shiny::h4("Timeline do dia"),
      shiny::uiOutput("timeline_list"),
      shiny::hr(),
      shiny::downloadButton("download_arc", "Baixar pacote Arc (zip)", class = "btn-block"),
      shiny::br(),
      shiny::downloadButton("download_gpx", "Baixar GPX (todos samples)", class = "btn-block")
    )
  ),

  # Menu de contexto para samples
  shiny::div(
    id = "sample-context-menu",
    class = "sample-context-menu",
    shiny::div(id = "ctx-propriedades", class = "sample-context-menu-item", "Propriedades"),
    shiny::div(class = "sample-context-menu-separator"),
    shiny::div(id = "ctx-ignorar", class = "sample-context-menu-item", "Ignorar"),
    shiny::div(id = "ctx-inserir", class = "sample-context-menu-item", "Inserir sample apos"),
    shiny::div(class = "sample-context-menu-separator"),
    shiny::div(id = "ctx-descartar", class = "sample-context-menu-item danger", "Descartar")
  ),

  # Menu de contexto para nos de rota
  shiny::div(
    id = "route-node-context-menu",
    class = "sample-context-menu",
    shiny::div(id = "ctx-node-props", class = "sample-context-menu-item", "Propriedades do no"),
    shiny::div(class = "sample-context-menu-separator"),
    shiny::div(id = "ctx-delete-node", class = "sample-context-menu-item danger", "Deletar no")
  ),

  # JavaScript para menu de contexto
  shiny::tags$script(shiny::HTML("
    $(document).ready(function() {
      var currentSampleId = null;
      var $menu = $('#sample-context-menu');

      // Esconde menu ao clicar fora
      $(document).on('click', function() {
        $menu.removeClass('show');
      });

      // Previne propagacao do clique no menu
      $menu.on('click', function(e) {
        e.stopPropagation();
      });

      // Handler para cada opcao do menu
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

      // Expoe funcao global para mostrar menu (chamada do Shiny)
      window.showSampleContextMenu = function(sampleId, x, y) {
        currentSampleId = sampleId;
        $menu.css({left: x + 'px', top: y + 'px'}).addClass('show');
      };

      // Handler para mensagem do Shiny configurar menu de contexto
      Shiny.addCustomMessageHandler('setup_context_menu', function(msg) {
        setTimeout(function() {
          var widget = HTMLWidgets.find('#map');
          if (!widget) return;
          var map = widget.getMap();
          if (!map) return;

          // Adiciona handlers em cada Marker com layerId (Arc samples)
          map.eachLayer(function(layer) {
            if (layer.options && layer.options.layerId && layer.options.draggable) {
              var layerId = layer.options.layerId;

              // Remove handlers antigos
              layer.off('contextmenu');
              layer.off('click');
              layer.off('dragend');

              // Menu de contexto (clique direito)
              layer.on('contextmenu', function(e) {
                L.DomEvent.stopPropagation(e);
                L.DomEvent.preventDefault(e);
                showSampleContextMenu(layerId, e.originalEvent.pageX, e.originalEvent.pageY);
              });

              // Clique para selecionar/deselecionar
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

              // Drag end - notifica Shiny
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

      // ---- Modo de selecao por laco (lasso) ----
      var selectionMode = false;
      var lassoSvg = null;
      var lassoPoints = [];
      var selectedSampleIds = [];
      var mapInstance = null;

      // Escuta mudanca no checkbox de modo selecao
      $(document).on('shiny:inputchanged', function(e) {
        if (e.name === 'modo_selecao') {
          selectionMode = e.value;

          // Pega instancia do mapa
          var widget = HTMLWidgets.find('#map');
          if (widget) mapInstance = widget.getMap();

          if (mapInstance) {
            if (selectionMode) {
              // Desabilita drag do mapa
              mapInstance.dragging.disable();
              $('#map').css('cursor', 'crosshair');
            } else {
              // Reabilita drag do mapa
              mapInstance.dragging.enable();
              $('#map').css('cursor', '');
              // Limpa selecao ao desativar
              selectedSampleIds = [];
              Shiny.setInputValue('selected_samples', []);
              updateSelectionVisual();
            }
          }
        }
      });

      // Cria SVG do laco
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

      // Mouse down no mapa
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

          // Fecha o poligono
          lassoPoints.push(lassoPoints[0]);

          // Encontra samples dentro do laco
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

      // Verifica se ponto esta dentro do poligono (ray casting)
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

      // Atualiza visual dos selecionados (Markers usam _icon)
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
              // Selecionado: adiciona filtro de cor
              $(layer._icon).css('filter', 'hue-rotate(180deg) brightness(1.3)');
            } else {
              // Não selecionado: remove filtro
              $(layer._icon).css('filter', '');
            }
          }
        });
      }

      // Handler para limpar selecao
      Shiny.addCustomMessageHandler('clear_selection', function(msg) {
        selectedSampleIds = [];
        Shiny.setInputValue('selected_samples', []);
        updateSelectionVisual();
      });

      // ---- Edicao de rota ----
      var currentNodeId = null;
      var $nodeMenu = $('#route-node-context-menu');
      var routeEditMode = false;
      var insertWaypointMode = false;

      // Esconde menu de no ao clicar fora
      $(document).on('click', function() {
        $nodeMenu.removeClass('show');
      });

      $nodeMenu.on('click', function(e) {
        e.stopPropagation();
      });

      // Handler para deletar no
      $('#ctx-delete-node').on('click', function() {
        if (currentNodeId) {
          Shiny.setInputValue('ctx_delete_node', {id: currentNodeId, ts: Date.now()});
        }
        $nodeMenu.removeClass('show');
      });

      // Handler para propriedades do no
      $('#ctx-node-props').on('click', function() {
        if (currentNodeId) {
          Shiny.setInputValue('ctx_node_props', {id: currentNodeId, ts: Date.now()});
        }
        $nodeMenu.removeClass('show');
      });

      // Funcao global para mostrar menu de contexto de no
      window.showRouteNodeContextMenu = function(nodeId, x, y) {
        currentNodeId = nodeId;
        $nodeMenu.css({left: x + 'px', top: y + 'px'}).addClass('show');
      };

      // Handler para configurar nos de rota (chamado do Shiny)
      Shiny.addCustomMessageHandler('setup_route_nodes', function(msg) {
        setTimeout(function() {
          var widget = HTMLWidgets.find('#map');
          if (!widget) return;
          var map = widget.getMap();
          if (!map) return;

          map.eachLayer(function(layer) {
            // Verifica se e um marker draggable do grupo edit_route_nodes
            if (layer.options && layer.options.layerId && layer.options.draggable) {
              var group = layer.options.group;
              if (group === 'edit_route_nodes') {
                var nodeId = layer.options.layerId;

                // Remove handlers antigos
                layer.off('contextmenu');
                layer.off('dragend');

                // Menu de contexto
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

      // Escuta mudanca no checkbox de modo inserir waypoint
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

      // Handler para limpar estado de edicao de rota
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

      // ---- Selecao por laco para nos de rota ----
      var nodeSelectionMode = false;
      var nodeLassoSvg = null;
      var nodeLassoPoints = [];
      var selectedNodeIds = [];
      var nodeMapInstance = null;

      // Escuta mudanca no checkbox de modo selecao de nos
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

      // Cria SVG do laco para nos
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

      // Mouse down no mapa para selecao de nos
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

      // Atualiza visual dos nos selecionados
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

      // Handler para limpar selecao de nos
      Shiny.addCustomMessageHandler('clear_node_selection', function(msg) {
        selectedNodeIds = [];
        Shiny.setInputValue('selected_route_nodes', []);
        updateNodeSelectionVisual();
      });
    });
  "))
)
