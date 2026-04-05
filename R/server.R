# ---- Server ----

server <- function(input, output, session) {

  # Helper: modal for OSRM offline with instructions

  mostrar_modal_osrm_offline <- function(url_tentada) {
    shiny::showModal(
      shiny::modalDialog(
        title = "OSRM server is not running",
        shiny::tags$p(
          shiny::tags$strong("The OSRM server did not respond at:"),
          shiny::tags$code(url_tentada)
        ),
        shiny::tags$hr(),
        shiny::tags$p(shiny::tags$strong("To start OSRM, run in terminal:")),
        shiny::tags$pre(
          style = "background: #f5f5f5; padding: 10px; border-radius: 4px; overflow-x: auto;",
          "# If you use individual containers:\ndocker start osrm-car osrm-foot osrm-bike osrm-bus\n\n# Or if you use docker-compose:\ncd ~/location_history/osrm\ndocker-compose up -d"
        ),
        shiny::tags$hr(),
        shiny::tags$p(shiny::tags$strong("To check if it's running:")),
        shiny::tags$pre(
          style = "background: #f5f5f5; padding: 10px; border-radius: 4px;",
          "docker ps | grep osrm"
        ),
        shiny::tags$hr(),
        shiny::tags$p(
          shiny::tags$strong("Expected ports:"),
          shiny::tags$ul(
            shiny::tags$li("Car: ", shiny::tags$code("localhost:5000")),
            shiny::tags$li("Foot: ", shiny::tags$code("localhost:5001")),
            shiny::tags$li("Bike: ", shiny::tags$code("localhost:5002")),
            shiny::tags$li("Bus: ", shiny::tags$code("localhost:5003"))
          )
        ),
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      )
    )
  }

  # Reactive state
  pontos_temp <- shiny::reactiveVal(data.frame(lat = numeric(0), lng = numeric(0)))
  timeline <- shiny::reactiveVal(list())
  all_samples <- shiny::reactiveVal(list())
  edit_samples <- shiny::reactiveVal(list())  # Samples loaded for editing
  ignored_samples <- shiny::reactiveVal(character(0))  # IDs of ignored samples
  rota_osrm <- shiny::reactiveVal(NULL)  # OSRM route geometry (lon/lat matrix)
  samples_virtuais <- shiny::reactiveVal(list())  # Samples created from OSRM route (intermediate points)
  original_samples <- shiny::reactiveVal(list())  # Original samples before editing
  original_timeline_item_id <- shiny::reactiveVal(NULL)  # Original timelineItemId of samples

  # State for route editing
  route_edit_nodes <- shiny::reactiveVal(list())     # Editable route waypoints
  route_original_item <- shiny::reactiveVal(NULL)    # Original TimelineItem
  route_original_samples <- shiny::reactiveVal(list())  # Original route samples
  route_avg_speed <- shiny::reactiveVal(NULL)        # Average speed (m/s)

  # ---- Location History of other people ----
  # PESSOAS_CONFIG is loaded in app.R (global variable)
  pessoas_config <- tryCatch(
    get("PESSOAS_CONFIG", envir = globalenv()),
    error = function(e) list()
  )
  location_history_cache <- shiny::reactiveVal(list())  # Cache of loaded data

  # Dynamically generates toggles based on configuration
  output$toggles_pessoas <- shiny::renderUI({
    if (length(pessoas_config) == 0) return(NULL)

    checkboxes <- lapply(pessoas_config, function(p) {
      shiny::checkboxInput(
        inputId = paste0("lh_", p$id),
        label   = shiny::span(
          shiny::icon("map-marker-alt"),
          p$label,
          style = paste0("color:", p$cor, "; font-weight: 500;")
        ),
        value   = FALSE
      )
    })

    shiny::tagList(
      shiny::tags$small(
        shiny::tags$strong("Location History:"),
        style = "color: #666;"
      ),
      checkboxes,
      shiny::hr()
    )
  })

  # Loads Location History data when needed
  carregar_lh_pessoa <- function(pessoa_id) {
    cache <- location_history_cache()
    if (!is.null(cache[[pessoa_id]])) {
      return(cache[[pessoa_id]])
    }

    # Find person's config
    cfg <- NULL
    for (p in pessoas_config) {
      if (p$id == pessoa_id) {
        cfg <- p
        break
      }
    }
    if (is.null(cfg)) return(NULL)

    # Show loading notification
    shiny::showNotification(
      paste0("Loading Location History for ", cfg$label, "..."),
      id = "lh_loading", type = "message", duration = NULL
    )

    # Load the file
    dados <- carregar_location_history(cfg$arquivo)

    shiny::removeNotification("lh_loading")

    if (!is.null(dados)) {
      cache[[pessoa_id]] <- dados
      location_history_cache(cache)
      shiny::showNotification(
        paste0(cfg$label, ": ", nrow(dados), " records loaded"),
        type = "message", duration = 3
      )
    } else {
      shiny::showNotification(
        paste0("Failed to load data for ", cfg$label),
        type = "error"
      )
    }

    dados
  }

  # Update map when date or toggles change
  shiny::observe({
    data_atual <- input$data_trabalho
    if (is.null(data_atual)) return()

    # Clear previous layers
    proxy <- leaflet::leafletProxy("map")
    for (p in pessoas_config) {
      proxy <- proxy %>% leaflet::clearGroup(paste0("lh_", p$id))
    }

    # Draw each enabled person
    for (p in pessoas_config) {
      toggle_id <- paste0("lh_", p$id)
      if (!isTRUE(input[[toggle_id]])) next

      # Use RDS cache if available, otherwise load full JSON
      if (cache_existe(p$id)) {
        df <- carregar_lh_por_data(p$id, data_atual)
      } else {
        dados <- carregar_lh_pessoa(p$id)
        if (is.null(dados)) next
        df <- filtrar_location_history_por_data(dados, data_atual)
      }

      if (is.null(df) || nrow(df) == 0) next

      group_name <- paste0("lh_", p$id)

      # Draw visits as circles
      visitas <- df[df$tipo == "visit", ]
      if (nrow(visitas) > 0) {
        for (i in seq_len(nrow(visitas))) {
          v <- visitas[i, ]
          data_hora_ini <- formatar_data_hora_lh(v$start_time)
          data_hora_fim <- formatar_data_hora_lh(v$end_time)

          proxy <- proxy %>%
            leaflet::addCircleMarkers(
              lng = v$lon, lat = v$lat,
              radius = 10,
              color = p$cor, fillColor = p$cor, fillOpacity = 0.6,
              stroke = TRUE, weight = 2,
              group = group_name,
              label = paste0(p$label, ": ", data_hora_ini, " - ", data_hora_fim)
            )
        }
      }

      # Draw activities as lines with direction indicators
      atividades <- df[df$tipo == "activity", ]
      if (nrow(atividades) > 0) {
        for (i in seq_len(nrow(atividades))) {
          a <- atividades[i, ]
          data_hora_ini <- formatar_data_hora_lh(a$start_time)
          data_hora_fim <- formatar_data_hora_lh(a$end_time)
          label_texto <- paste0(p$label, ": ", data_hora_ini, " -> ", data_hora_fim)

          # Route line
          proxy <- proxy %>%
            leaflet::addPolylines(
              lng = c(a$lon, a$lon_end),
              lat = c(a$lat, a$lat_end),
              color = p$cor, weight = 3, opacity = 0.7,
              group = group_name,
              label = label_texto
            )

          # Start marker (small green circle)
          proxy <- proxy %>%
            leaflet::addCircleMarkers(
              lng = a$lon, lat = a$lat,
              radius = 4,
              color = "green", fillColor = "green", fillOpacity = 1,
              stroke = FALSE,
              group = group_name,
              label = paste0("Start: ", data_hora_ini)
            )

          # End marker (small red circle)
          proxy <- proxy %>%
            leaflet::addCircleMarkers(
              lng = a$lon_end, lat = a$lat_end,
              radius = 4,
              color = "red", fillColor = "red", fillOpacity = 1,
              stroke = FALSE,
              group = group_name,
              label = paste0("End: ", data_hora_fim)
            )
        }
      }

      # Check if any point is already visible on the current screen
      all_lats <- c(visitas$lat, atividades$lat, atividades$lat_end)
      all_lons <- c(visitas$lon, atividades$lon, atividades$lon_end)

      if (length(all_lats) > 0 && length(all_lons) > 0) {
        # Get current map bounds (isolate to avoid reactive loop with fitBounds)
        bounds <- shiny::isolate(input$map_bounds)

        # Only fitBounds if no point is visible
        algum_visivel <- FALSE
        if (!is.null(bounds)) {
          for (i in seq_along(all_lats)) {
            lat_i <- all_lats[i]
            lon_i <- all_lons[i]
            if (!is.na(lat_i) && !is.na(lon_i) &&
                lat_i >= bounds$south && lat_i <= bounds$north &&
                lon_i >= bounds$west && lon_i <= bounds$east) {
              algum_visivel <- TRUE
              break
            }
          }
        }

        if (!algum_visivel) {
          proxy <- proxy %>%
            leaflet::fitBounds(
              lng1 = min(all_lons, na.rm = TRUE) - 0.01,
              lat1 = min(all_lats, na.rm = TRUE) - 0.01,
              lng2 = max(all_lons, na.rm = TRUE) + 0.01,
              lat2 = max(all_lats, na.rm = TRUE) + 0.01
            )
        }
      }
    }
  })

  # Auto-format time inputs (only when they look complete)
  auto_format_time <- function(id) {
    shiny::observeEvent(input[[id]], {
      val <- input[[id]]
      if (is.null(val) || !nzchar(val)) return()

      # ONLY FORMAT if at least 3 characters typed (avoids formatting while typing)
      if (nchar(gsub("\\D", "", val)) < 3) return()

      fmt <- formatar_hora(val)
      if (!is.na(fmt) && !identical(fmt, val)) {
        shiny::updateTextInput(session, id, value = fmt)
      }
    }, ignoreInit = TRUE)
  }

  # Time fields that should be auto-formatted
  auto_format_time("visita_hora_inicio")
  auto_format_time("visita_hora_fim")
  auto_format_time("manual_hora_inicio")
  auto_format_time("manual_hora_fim")
  auto_format_time("import_hora_inicio")
  auto_format_time("import_hora_fim")
  auto_format_time("osrm_hora_inicio")
  auto_format_time("osrm_hora_fim")
  auto_format_time("edit_hora_inicio")
  auto_format_time("edit_hora_fim")
  auto_format_time("edit_samples_hora_inicio")
  auto_format_time("edit_samples_hora_fim")


  # Returns the local end of the LAST timeline activity (or NULL if it can't be calculated)
  get_last_end_local <- function() {
    tl <- timeline()
    if (length(tl) == 0) return(NULL)

    # get endDate as POSIXct (UTC), ignoring broken items
    ends_num <- vapply(tl, function(it) {
      date_str <- item_end_date(it) %||% NA_character_
      ts <- parse_timestamp_utc(date_str)
      if (is.null(ts) || is.na(ts[1])) {
        NA_real_
      } else {
        as.numeric(ts[1])  # seconds since epoch
      }
    }, numeric(1))

    # if all are NA, we can't determine the last end
    if (all(is.na(ends_num))) return(NULL)

    # get the index of the LARGEST valid endDate
    idx <- which.max(ends_num)
    if (length(idx) == 0 || is.na(idx) || idx < 1) return(NULL)

    # reconstruct POSIXct in UTC
    end_utc <- as.POSIXct(ends_num[idx], origin = "1970-01-01", tz = "UTC")

    end_utc
  }


  # When timeline changes, suggest next start = previous end + 1 min
  shiny::observeEvent(timeline(), {
    last_utc <- get_last_end_local()  # returns in UTC
    if (is.null(last_utc)) return()

    # Convert to user's system timezone
    last_local <- lubridate::with_tz(last_utc, tzone = Sys.timezone())

    next_local <- last_local + 60  # +1 minute

    data_next <- as.Date(next_local)
    hora_next <- format(next_local, "%H:%M:%S")

    # visit
    shiny::updateDateInput(session, "visita_data_inicio", value = data_next)
    shiny::updateTextInput(session, "visita_hora_inicio", value = hora_next)

    # manual route
    shiny::updateDateInput(session, "manual_data_inicio", value = data_next)
    shiny::updateTextInput(session, "manual_hora_inicio", value = hora_next)

    # import
    shiny::updateDateInput(session, "import_data_inicio", value = data_next)
    shiny::updateTextInput(session, "import_hora_inicio", value = hora_next)
  })


  # When the main date changes, replicate to other date fields
  shiny::observeEvent(input$data_trabalho, {
    d <- input$data_trabalho

    shiny::updateDateInput(session, "visita_data_inicio", value = d)
    shiny::updateDateInput(session, "visita_data_fim",    value = d)

    shiny::updateDateInput(session, "manual_data_inicio", value = d)
    shiny::updateDateInput(session, "manual_data_fim",    value = d)

    shiny::updateDateInput(session, "import_data_inicio", value = d)
    shiny::updateDateInput(session, "import_data_fim",    value = d)

    shiny::updateDateInput(session, "edit_samples_data_inicio", value = d)
    shiny::updateDateInput(session, "edit_samples_data_fim",    value = d)
  })


  # ---- Base map ----
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(maxZoom = 19)) |>
      leaflet::addTiles(
        group = "OSM",
        options = leaflet::tileOptions(maxZoom = 19)
      ) |>
      leaflet::addProviderTiles(
        "CartoDB.Positron",
        group = "CartoDB",
        options = leaflet::providerTileOptions(maxZoom = 19)
      ) |>
      leaflet::addProviderTiles(
        "Esri.WorldImagery",
        group = "Satelite",
        options = leaflet::providerTileOptions(maxZoom = 19)
      ) |>
      leaflet::addTiles(
        urlTemplate = paste0("https://tile.tracestrack.com/_/{z}/{x}/{y}.webp?key=", Sys.getenv("TRACESTRACK_KEY")),
        group = "Topo",
        options = leaflet::tileOptions(maxZoom = 19)
      ) |>
      leaflet::addLayersControl(
        baseGroups = c("OSM", "CartoDB", "Satelite", "Topo"),
        options = leaflet::layersControlOptions(collapsed = TRUE)
      ) |>
      leaflet::setView(lng = -43.2, lat = -22.9, zoom = 11)
  })

  # Helper: update points on the map
  atualizar_pontos_mapa <- function(auto_zoom = TRUE) {
    pts <- pontos_temp()
    proxy <- leaflet::leafletProxy("map")
    proxy <- proxy %>% leaflet::clearGroup("waypoints")
    if (nrow(pts) > 0) {
      proxy <- proxy %>%
        leaflet::addCircleMarkers(
          data = pts,
          lng = ~lng, lat = ~lat,
          radius = 6,
          color = "red",
          fillColor = "red",
          fillOpacity = 0.8,
          label = ~as.character(seq_len(nrow(pts))),
          group = "waypoints"
        )

      # fitBounds ONLY if auto_zoom = TRUE
      if (auto_zoom) {
        proxy <- proxy %>%
          leaflet::fitBounds(
            lng1 = min(pts$lng), lat1 = min(pts$lat),
            lng2 = max(pts$lng), lat2 = max(pts$lat)
          )
      }
    }
  }

  # Map click
  shiny::observeEvent(input$map_click, {
    click <- input$map_click
    if (is.null(click)) return()

    modo <- input$modo

    # Ignore clicks in Edit Samples mode (uses draggable markers)
    if (modo == "Editar Samples") return()

    # Ignore clicks in Import File mode (uses uploaded file)
    if (modo == "Importar Arquivo") return()

    pts <- pontos_temp()

    if (modo == "Visita") {
      # visit uses only 1 point: replace
      pts <- data.frame(lat = click$lat, lng = click$lng)
      pontos_temp(pts)
      atualizar_pontos_mapa(auto_zoom = TRUE)
    } else {
      # OSRM or Manual Route: add point
      pts <- rbind(pts, data.frame(lat = click$lat, lng = click$lng))
      pontos_temp(pts)
      atualizar_pontos_mapa(auto_zoom = FALSE)
    }
  })

  # Clear points (OSRM / manual) - WITHOUT auto_zoom
  shiny::observeEvent(input$limpar_pontos_osrm, {
    pontos_temp(data.frame(lat = numeric(0), lng = numeric(0)))
    atualizar_pontos_mapa(auto_zoom = FALSE)
  })
  shiny::observeEvent(input$limpar_pontos_manual, {
    pontos_temp(data.frame(lat = numeric(0), lng = numeric(0)))
    atualizar_pontos_mapa(auto_zoom = FALSE)
  })

  # Undo last point - WITHOUT auto_zoom
  shiny::observeEvent(input$desfazer_ponto_osrm, {
    pts <- pontos_temp()
    if (nrow(pts) > 0) {
      pts <- pts[-nrow(pts), , drop = FALSE]
      pontos_temp(pts)
      atualizar_pontos_mapa(auto_zoom = FALSE)
    }
  })
  shiny::observeEvent(input$desfazer_ponto_manual, {
    pts <- pontos_temp()
    if (nrow(pts) > 0) {
      pts <- pts[-nrow(pts), , drop = FALSE]
      pontos_temp(pts)
      atualizar_pontos_mapa(auto_zoom = FALSE)
    }
  })

  # ---- Visit: auto-fill coordinates from frequent place name ----

  shiny::observeEvent(input$visita_nome, {
    nome <- input$visita_nome
    if (nome %in% names(LOCAIS_FREQUENTES)) {
      loc <- LOCAIS_FREQUENTES[[nome]]
      pts <- data.frame(lat = loc$lat, lng = loc$lon)
      pontos_temp(pts)
      atualizar_pontos_mapa(auto_zoom = TRUE)
      leaflet::leafletProxy("map") %>%
        leaflet::addPopups(loc$lon, loc$lat, paste0("Place: ", nome), layerId = "visita_popup")
    } else {
      leaflet::leafletProxy("map") %>% leaflet::removePopup("visita_popup")
    }
  })

  # ---- Add visit ----

  shiny::observeEvent(input$adicionar_visita, {
    pts <- pontos_temp()
    if (nrow(pts) != 1) {
      shiny::showNotification("Select exactly 1 point on the map for the visit.", type = "error")
      return()
    }
    lat <- pts$lat[1]
    lng <- pts$lng[1]

    tz <- tz_from_coords(lat, lng)

    val <- validar_intervalo(
      data_ini = input$visita_data_inicio,
      hora_ini = input$visita_hora_inicio,
      data_fim = input$visita_data_fim,
      hora_fim = input$visita_hora_fim,
      tz = tz
    )
    if (is.null(val)) {
      shiny::showNotification("Invalid time for the visit.", type = "error")
      return()
    }

    # convert visit interval to UTC to check overlap
    inicio_utc <- lubridate::with_tz(val$inicio, "UTC")
    fim_utc    <- lubridate::with_tz(val$fim,    "UTC")

    if (has_overlap(timeline(), inicio_utc, fim_utc)) {
      shiny::showNotification("This interval overlaps an existing activity.", type = "error")
      return()
    }

    visita <- criar_timeline_item_visit(
      lat = lat,
      lon = lng,
      inicio_local = val$inicio,
      fim_local = val$fim,
      nome = if (nzchar(input$visita_nome)) input$visita_nome else NULL
    )


    item <- visita$item
    samples_novos <- visita$samples

    # description
    hora_ini_local <- format(val$inicio, "%H:%M")
    hora_fim_local <- format(val$fim, "%H:%M")
    nome_visita <- item$.place$name %||% item$visit$customTitle %||% "Visit"
    item$tipo <- "visita"
    item$descricao <- sprintf("📍 %s (%s - %s)", nome_visita, hora_ini_local, hora_fim_local)

    # Update timeline and samples
    tl <- timeline()
    tl[[length(tl) + 1L]] <- item
    timeline(tl)

    s <- all_samples()
    all_samples(c(s, samples_novos))

    # fitBounds on the visit point
    leaflet::leafletProxy("map") %>%
      leaflet::fitBounds(lng, lat, lng, lat)

    shiny::showNotification("Visit added.", type = "message")
  })

  # ---- Manual route ----

  shiny::observeEvent(input$adicionar_manual, {
    pts <- pontos_temp()
    if (nrow(pts) < 2) {
      shiny::showNotification("Set at least 2 points on the map for the manual route.", type = "error")
      return()
    }

    lat1 <- pts$lat[1]
    lng1 <- pts$lng[1]
    tz <- tz_from_coords(lat1, lng1)

    val <- validar_intervalo(
      data_ini = input$manual_data_inicio,
      hora_ini = input$manual_hora_inicio,
      data_fim = input$manual_data_fim,
      hora_fim = input$manual_hora_fim,
      tz = tz
    )
    if (is.null(val)) {
      shiny::showNotification("Invalid time for the manual route.", type = "error")
      return()
    }

    # Generate equally spaced UTC timestamps
    inicio_utc <- lubridate::with_tz(val$inicio, "UTC")
    fim_utc    <- lubridate::with_tz(val$fim,    "UTC")

    if (has_overlap(timeline(), inicio_utc, fim_utc)) {
      shiny::showNotification("This interval overlaps an existing activity.", type = "error")
      return()
    }


    # Interpolate coords to at least 100 points
    coords <- as.matrix(pts[, c("lng", "lat")])
    n0 <- nrow(coords)
    n_target <- max(n0, 100L)
    ts_seq <- seq(inicio_utc, fim_utc, length.out = n_target)

    t_interp <- seq(0, 1, length.out = n0)
    t_new <- seq(0, 1, length.out = n_target)

    lon_new <- suppressWarnings(approx(
      x = t_interp,
      y = as.numeric(coords[, 1]),
      xout = t_new,
      ties = "ordered"
    )$y)

    lat_new <- suppressWarnings(approx(
      x = t_interp,
      y = as.numeric(coords[, 2]),
      xout = t_new,
      ties = "ordered"
    )$y)


    coords_new <- cbind(lon_new, lat_new)

    manual_at <- input$manual_activity_type %||% "car"

    samples_novos <- criar_locomotion_samples(
      coords         = coords_new,
      timestamps_utc = ts_seq,
      accuracy       = 10,
      force_single_tz = TRUE,
      moving_state   = "moving",
      activity_type  = manual_at
    )
    sample_ids <- vapply(samples_novos, `[[`, "", "id")

    item <- criar_timeline_item_path(
      timestamps_utc = ts_seq,
      coords = coords_new,
      sample_ids = sample_ids,
      tipo = "rota_manual",
      descricao = sprintf("Manual route (%.1f km)",
                          geosphere::distVincentyEllipsoid(
                            coords_new[1, 2:1], coords_new[n_target, 2:1]
                          ) / 1000),
      activity_type = manual_at
    )

    tl <- timeline()
    tl[[length(tl) + 1L]] <- item
    timeline(tl)

    s <- all_samples()
    all_samples(c(s, samples_novos))

    # Draw route and zoom
    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("rota_atual") %>%
      leaflet::addPolylines(
        lng = coords_new[, 1],
        lat = coords_new[, 2],
        color = "red",
        weight = 4,
        opacity = 0.8,
        group = "rota_atual"
      ) %>%
      leaflet::fitBounds(
        lng1 = min(coords_new[, 1]),
        lat1 = min(coords_new[, 2]),
        lng2 = max(coords_new[, 1]),
        lat2 = max(coords_new[, 2])
      )

    shiny::showNotification("Manual route added.", type = "message")
  })

  # ---- OSRM: calculate route ----

  shiny::observeEvent(input$calcular_osrm, {
    pts <- pontos_temp()
    if (nrow(pts) < 2) {
      shiny::showNotification("Set at least 2 points on the map for the OSRM route.", type = "error")
      return()
    }

    perfil <- input$osrm_perfil

    # Perfis de transit usam Google Maps API ao invés de OSRM
    if (perfil %in% GOOGLE_TRANSIT_MODES) {
      if (!check_google_api()) {
        shiny::showNotification(
          "Google Maps API key não configurada ou inválida. Defina GOOGLE_MAPS_API_KEY no .Renviron.",
          type = "error", duration = 8
        )
        return()
      }
      transit_mode <- GOOGLE_TRANSIT_MODE_MAP[[perfil]]
      rota <- calcular_rota_google_transit(pts, transit_mode = transit_mode)
      if (is.null(rota)) {
        shiny::showNotification(
          sprintf("Nenhuma rota de %s encontrada entre esses pontos. A API não retornou rotas com esse modo de transporte.", perfil),
          type = "error", duration = 8
        )
        return()
      }
      # Aplicar suavização igual ao OSRM
      rota$coords <- suavizar_rota_osrm(rota$coords, tolerancia_m = 5, angulo_min = 2)
    } else {
      base_url <- OSRM_SERVERS[[perfil]]

      if (is.null(base_url) || !check_osrm_server(base_url)) {
        mostrar_modal_osrm_offline(base_url %||% "nao configurado")
        return()
      }

      rota <- calcular_rota_osrm(pts, perfil = perfil)
      if (is.null(rota)) {
        shiny::showNotification("Failed to calculate OSRM route.", type = "error")
        return()
      }
    }

    # Suggest time based on the last activity (if any)
    last_utc <- get_last_end_local()  # returns in UTC
    if (!is.null(last_utc)) {
      last_local <- lubridate::with_tz(last_utc, tzone = Sys.timezone())
      inicia_sugerido <- last_local + 60  # +1 minuto
    } else {
      data_ref <- input$data_trabalho
      inicia_sugerido <- lubridate::ymd_hms(
        paste(data_ref, "08:00:00"),
        tz = Sys.timezone()
      )
    }
    fim_sugerido <- inicia_sugerido + rota$duration_s


    shiny::showModal(
      shiny::modalDialog(
        title = "OSRM route times",
        shiny::dateInput("osrm_data_inicio", "Start date", as.Date(inicia_sugerido)),
        shiny::textInput("osrm_hora_inicio", "Start time", format(inicia_sugerido, "%H:%M:%S")),
        shiny::dateInput("osrm_data_fim", "End date", as.Date(fim_sugerido)),
        shiny::textInput("osrm_hora_fim", "End time", format(fim_sugerido, "%H:%M:%S")),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("recalcular_osrm_fim", "Recalculate arrival"),
          shiny::actionButton("confirmar_rota_osrm", "Confirm", class = "btn-primary")
        )
      )
    )

    auto_format_time("osrm_hora_inicio")
    auto_format_time("osrm_hora_fim")

    # Store route in session attribute
    session$userData$ultima_rota_osrm <- rota
    session$userData$ultima_rota_pts <- pts
    session$userData$ultima_rota_perfil <- perfil
  })

  shiny::observeEvent(input$recalcular_osrm_fim, {
    rota <- session$userData$ultima_rota_osrm
    if (is.null(rota)) {
      shiny::showNotification("No OSRM route calculated to recalculate the arrival time.", type = "error")
      return()
    }

    coords <- rota$coords
    if (is.data.frame(coords)) coords <- as.matrix(coords)
    if (!is.matrix(coords) || nrow(coords) < 1 || ncol(coords) < 2) {
      shiny::showNotification("OSRM route coordinates are in an unexpected format.", type = "error")
      return()
    }

    lat1 <- as.numeric(coords[1, 2])
    lng1 <- as.numeric(coords[1, 1])
    if (is.na(lat1) || is.na(lng1)) {
      shiny::showNotification("First point of the OSRM route is invalid.", type = "error")
      return()
    }

    tz <- tz_from_coords(lat1, lng1)

    # departure time (format again to be safe)
    hora_ini_fmt <- formatar_hora(input$osrm_hora_inicio)
    if (is.na(hora_ini_fmt)) {
      shiny::showNotification("Invalid departure time.", type = "error")
      return()
    }

    inicio_local <- suppressWarnings(
      lubridate::ymd_hms(
        paste(input$osrm_data_inicio, hora_ini_fmt),
        tz = tz
      )
    )
    if (is.na(inicio_local)) {
      shiny::showNotification("Could not parse the departure date/time.", type = "error")
      return()
    }

    # duration in seconds from OSRM
    dur_s <- as.numeric(rota$duration_s)
    if (is.na(dur_s)) {
      shiny::showNotification("Invalid OSRM route duration.", type = "error")
      return()
    }

    fim_local <- inicio_local + dur_s

    shiny::updateDateInput(session, "osrm_data_fim", value = as.Date(fim_local))
    shiny::updateTextInput(session, "osrm_hora_fim", value = format(fim_local, "%H:%M:%S"))
  })


  shiny::observeEvent(input$confirmar_rota_osrm, {
    tryCatch(
      {
        shiny::removeModal()

        rota <- session$userData$ultima_rota_osrm
        if (is.null(rota)) {
          shiny::showNotification("No OSRM route calculated.", type = "error")
          return()
        }

        coords <- rota$coords
        if (is.data.frame(coords)) {
          coords <- as.matrix(coords)
        }
        if (!is.matrix(coords) || ncol(coords) < 2) {
          shiny::showNotification("OSRM route coordinates are in an unexpected format.", type = "error")
          return()
        }
        coords <- suppressWarnings(apply(coords, 2, as.numeric))
        if (!is.matrix(coords)) {
          coords <- matrix(coords, ncol = 2)
        }

        n0 <- nrow(coords)
        if (is.null(n0) || is.na(n0) || n0 < 2) {
          shiny::showNotification("Invalid OSRM route (too few points).", type = "error")
          return()
        }

        lat1 <- coords[1, 2]
        lng1 <- coords[1, 1]

        if (is.na(lat1) || is.na(lng1)) {
          shiny::showNotification("First point of the OSRM route is invalid.", type = "error")
          return()
        }

        tz <- tz_from_coords(lat1, lng1)

        hora_ini_fmt <- formatar_hora(input$osrm_hora_inicio)
        hora_fim_fmt <- formatar_hora(input$osrm_hora_fim)

        if (is.na(hora_ini_fmt) || is.na(hora_fim_fmt)) {
          shiny::showNotification("Invalid times for OSRM route.", type = "error")
          return()
        }

        val <- validar_intervalo(
          data_ini = input$osrm_data_inicio,
          hora_ini = hora_ini_fmt,
          data_fim = input$osrm_data_fim,
          hora_fim = hora_fim_fmt,
          tz = tz
        )
        if (is.null(val)) {
          shiny::showNotification("Invalid time for OSRM route.", type = "error")
          return()
        }

        if (!inherits(val$inicio, "POSIXt") || !inherits(val$fim, "POSIXt")) {
          shiny::showNotification("OSRM route time interval could not be parsed.", type = "error")
          return()
        }

        inicio_utc <- lubridate::with_tz(val$inicio, "UTC")
        fim_utc    <- lubridate::with_tz(val$fim,    "UTC")

        if (is.na(inicio_utc) || is.na(fim_utc) || fim_utc <= inicio_utc) {
          shiny::showNotification("Invalid OSRM route interval (end before or equal to start).", type = "error")
          return()
        }

        if (has_overlap(timeline(), inicio_utc, fim_utc)) {
          shiny::showNotification("This interval overlaps an existing activity.", type = "error")
          return()
        }

        n_target <- max(n0, 100L)
        ts_seq <- seq(from = inicio_utc, to = fim_utc, length.out = n_target)
        if (any(is.na(ts_seq))) {
          shiny::showNotification("Failed to generate time sequence for OSRM route.", type = "error")
          return()
        }

        t_interp <- seq(0, 1, length.out = n0)
        t_new    <- seq(0, 1, length.out = n_target)

        lon_new <- suppressWarnings(approx(
          x = t_interp,
          y = coords[, 1],
          xout = t_new,
          ties = "ordered"
        )$y)

        lat_new <- suppressWarnings(approx(
          x = t_interp,
          y = coords[, 2],
          xout = t_new,
          ties = "ordered"
        )$y)

        if (any(is.na(lon_new)) || any(is.na(lat_new))) {
          shiny::showNotification("Failed to interpolate OSRM route coordinates.", type = "error")
          return()
        }

        # Map OSRM profile to activityType
        perfil <- session$userData$ultima_rota_perfil %||% "car"
        activity_map <- c(
          car   = "car",
          foot  = "walking",
          bike  = "cycling",
          bus   = "bus",
          metro = "metro",
          train = "train",
          tram  = "tram"
        )
        activity_type <- activity_map[[perfil]] %||% "car"

        coords_new <- cbind(lon_new, lat_new)

        samples_novos <- criar_locomotion_samples(
          coords         = coords_new,
          timestamps_utc = ts_seq,
          accuracy       = 10,
          force_single_tz = TRUE,
          moving_state   = "moving",
          activity_type  = activity_type
        )
        sample_ids <- vapply(samples_novos, `[[`, "", "id")

        item <- criar_timeline_item_path(
          timestamps_utc = ts_seq,
          coords         = coords_new,
          sample_ids     = sample_ids,
          tipo           = "rota_osrm",
          descricao      = sprintf("OSRM route (%s)", perfil),
          activity_type  = activity_type
        )

        tl <- timeline()
        tl[[length(tl) + 1L]] <- item
        timeline(tl)

        s <- all_samples()
        all_samples(c(s, samples_novos))

        leaflet::leafletProxy("map") %>%
          leaflet::clearGroup("rota_atual") %>%
          leaflet::clearGroup("waypoints") %>%
          leaflet::addPolylines(
            lng   = coords_new[, 1],
            lat   = coords_new[, 2],
            color = "blue",
            weight = 4,
            group = "rota_atual"
          ) %>%
          leaflet::fitBounds(
            lng1 = min(coords_new[, 1]),
            lat1 = min(coords_new[, 2]),
            lng2 = max(coords_new[, 1]),
            lat2 = max(coords_new[, 2])
          )

        # Clear temporary points after adding route
        pontos_temp(data.frame(lat = numeric(0), lng = numeric(0)))

        shiny::showNotification("OSRM route added.", type = "message")
      },
      error = function(e) {
        shiny::showNotification(
          paste("Error confirming OSRM route:", e$message),
          type = "error",
          duration = NULL
        )
      }
    )
  })



  # ---- Import file (GeoJSON/GPX/KML/GPKG) ----

  geometria_importada <- shiny::reactiveVal(NULL)
  fr24_enriquecido <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(input$arquivo_geo, {
    shiny::req(input$arquivo_geo)
    arquivo <- input$arquivo_geo$datapath
    ext <- tools::file_ext(arquivo)
    ext <- tolower(ext)

    leaflet::leafletProxy("map") %>% leaflet::clearGroup("importado")

    if (ext == "kml") {
      geo_rich <- parse_kml_rich(arquivo)
      if (!is.null(geo_rich) && "timestamp" %in% names(geo_rich)) {
        geometria_importada(geo_rich)
        fr24_enriquecido(TRUE)

        ts_gmt <- parse_timestamp_utc(geo_rich$timestamp)
        ord <- order(ts_gmt)
        ts_gmt <- ts_gmt[ord]

        coords <- cbind(
          geo_rich$lon[ord],
          geo_rich$lat[ord]
        )
        colnames(coords) <- c("lon", "lat")


        if (!is.null(ts_gmt) && !all(is.na(ts_gmt))) {
          ini <- ts_gmt[1]
          fim <- ts_gmt[length(ts_gmt)]
          shiny::updateDateInput(session, "import_data_inicio", value = as.Date(ini))
          shiny::updateTextInput(session, "import_hora_inicio", value = format(lubridate::with_tz(ini, Sys.timezone()), "%H:%M"))
          shiny::updateDateInput(session, "import_data_fim", value = as.Date(fim))
          shiny::updateTextInput(session, "import_hora_fim", value = format(lubridate::with_tz(fim, Sys.timezone()), "%H:%M"))
          shiny::showNotification("Times filled from KML file (UTC -> system local time).", type = "message")
        }

        leaflet::leafletProxy("map") %>%
          leaflet::addPolylines(
            lng = coords[, 1],
            lat = coords[, 2],
            color = "darkblue",
            weight = 3,
            group = "importado"
          ) %>%
          leaflet::addCircleMarkers(
            lng = coords[1, 1], lat = coords[1, 2],
            radius = 8, color = "green", fillColor = "green", fillOpacity = 1,
            group = "importado", label = "Start"
          ) %>%
          leaflet::addCircleMarkers(
            lng = coords[nrow(coords), 1], lat = coords[nrow(coords), 2],
            radius = 8, color = "red", fillColor = "red", fillOpacity = 1,
            group = "importado", label = "End"
          ) %>%
          leaflet::fitBounds(
            lng1 = min(coords[, 1]), lat1 = min(coords[, 2]),
            lng2 = max(coords[, 1]), lat2 = max(coords[, 2])
          )

        shiny::showNotification("Enriched KML (FR24) loaded. Green = start, Red = end.", type = "message")
        return()
      }
    }

    # Normal case: read with sf
    geo <- tryCatch(
      sf::st_read(arquivo, quiet = TRUE),
      error = function(e) NULL
    )
    if (is.null(geo)) {
      shiny::showNotification("Could not read file as geographic layer.", type = "error")
      return()
    }

    fr24_enriquecido(FALSE)

    if (!sf::st_is_longlat(geo)) {
      geo <- sf::st_transform(geo, 4326)
    }

    if (any(sf::st_geometry_type(geo) %in% c("MULTILINESTRING"))) {
      geo <- sf::st_cast(geo, "LINESTRING")
    }

    geometria_importada(geo)

    bbox <- sf::st_bbox(geo)

    # Extract first and last point for direction markers
    coords_all <- sf::st_coordinates(geo)
    pt_inicio <- coords_all[1, c("X", "Y")]
    pt_fim <- coords_all[nrow(coords_all), c("X", "Y")]

    leaflet::leafletProxy("map") %>%
      leaflet::addPolylines(
        data = geo,
        color = "purple",
        weight = 3,
        group = "importado"
      ) %>%
      leaflet::addCircleMarkers(
        lng = pt_inicio["X"], lat = pt_inicio["Y"],
        radius = 8, color = "green", fillColor = "green", fillOpacity = 1,
        group = "importado", label = "Start"
      ) %>%
      leaflet::addCircleMarkers(
        lng = pt_fim["X"], lat = pt_fim["Y"],
        radius = 8, color = "red", fillColor = "red", fillOpacity = 1,
        group = "importado", label = "End"
      ) %>%
      leaflet::fitBounds(
        lng1 = bbox["xmin"], lat1 = bbox["ymin"],
        lng2 = bbox["xmax"], lat2 = bbox["ymax"]
      )

    shiny::showNotification("Geographic file imported. Green = start, Red = end.", type = "message")
  })

  shiny::observeEvent(input$direcao_arquivo, {
    geo <- geometria_importada()
    if (is.null(geo)) return()

    leaflet::leafletProxy("map") %>% leaflet::clearGroup("importado")

    if (isTRUE(fr24_enriquecido())) {
      coords <- cbind(geo$lon, geo$lat)
      if (input$direcao_arquivo == "inverter") {
        coords <- coords[nrow(coords):1, , drop = FALSE]
      }
      leaflet::leafletProxy("map") %>%
        leaflet::addPolylines(
          lng = coords[, 1],
          lat = coords[, 2],
          color = "darkblue",
          weight = 3,
          group = "importado"
        ) %>%
        leaflet::addCircleMarkers(
          lng = coords[1, 1], lat = coords[1, 2],
          radius = 8, color = "green", fillColor = "green", fillOpacity = 1,
          group = "importado", label = "Start"
        ) %>%
        leaflet::addCircleMarkers(
          lng = coords[nrow(coords), 1], lat = coords[nrow(coords), 2],
          radius = 8, color = "red", fillColor = "red", fillOpacity = 1,
          group = "importado", label = "End"
        ) %>%
        leaflet::fitBounds(
          lng1 = min(coords[, 1]), lat1 = min(coords[, 2]),
          lng2 = max(coords[, 1]), lat2 = max(coords[, 2])
        )
    } else {
      bbox <- sf::st_bbox(geo)
      coords_all <- sf::st_coordinates(geo)

      # Consider inversion for markers
      if (input$direcao_arquivo == "inverter") {
        pt_inicio <- coords_all[nrow(coords_all), c("X", "Y")]
        pt_fim <- coords_all[1, c("X", "Y")]
      } else {
        pt_inicio <- coords_all[1, c("X", "Y")]
        pt_fim <- coords_all[nrow(coords_all), c("X", "Y")]
      }

      leaflet::leafletProxy("map") %>%
        leaflet::addPolylines(
          data = geo,
          color = "purple",
          weight = 3,
          group = "importado"
        ) %>%
        leaflet::addCircleMarkers(
          lng = pt_inicio["X"], lat = pt_inicio["Y"],
          radius = 8, color = "green", fillColor = "green", fillOpacity = 1,
          group = "importado", label = "Start"
        ) %>%
        leaflet::addCircleMarkers(
          lng = pt_fim["X"], lat = pt_fim["Y"],
          radius = 8, color = "red", fillColor = "red", fillOpacity = 1,
          group = "importado", label = "End"
        ) %>%
        leaflet::fitBounds(
          lng1 = bbox["xmin"], lat1 = bbox["ymin"],
          lng2 = bbox["xmax"], lat2 = bbox["ymax"]
        )
    }
  })

  shiny::observeEvent(input$adicionar_import, {
    geo <- geometria_importada()
    if (is.null(geo)) {
      shiny::showNotification("No file imported.", type = "error")
      return()
    }

    # -------------------------------------------------------------------
    # SPECIAL IMPORT: FR24 flight track already enriched
    # -------------------------------------------------------------------
    if (isTRUE(fr24_enriquecido())) {
      traj <- tryCatch(
        limpar_trajeto_fr24(
          df      = geo,
          ts_col  = "timestamp",
          lon_col = "lon",
          lat_col = "lat"
        ),
        error = function(e) {
          shiny::showNotification(
            paste("Error processing FR24 track:", e$message),
            type = "error",
            duration = NULL
          )
          return(NULL)
        }
      )

      if (is.null(traj)) return()

      ts_utc <- traj$ts_utc
      coords <- traj$coords

      distancia_km <- trajeto_distancia_km(coords)

      if (distancia_km <= 0 || distancia_km > 30000) {
        shiny::showNotification(
          sprintf(
            "Absurd distance detected for flight (%.1f km). Something is wrong with the track.",
            distancia_km
          ),
          type = "error",
          duration = NULL
        )
        return()
      }

      lat1 <- coords[1, "lat"]
      lon1 <- coords[1, "lon"]
      tz   <- tz_from_coords(lat1, lon1)
      sec_gmt <- as.integer(lubridate::with_tz(ts_utc[1], tz) - ts_utc[1])

      n_samples <- length(ts_utc)

      altitude_vec <- if ("altitude_m" %in% names(geo)) {
        alt <- geo$altitude_m
        alt_ts <- parse_timestamp_utc(geo$timestamp)
        ok <- !is.na(alt_ts) & !is.na(alt)
        if (any(ok)) {
          ord <- order(alt_ts[ok])
          alt[ok][ord]
        } else {
          NULL
        }
      } else {
        NULL
      }

      speed_vec <- if ("speed_mps" %in% names(geo)) {
        spd <- geo$speed_mps
        spd_ts <- parse_timestamp_utc(geo$timestamp)
        ok <- !is.na(spd_ts) & !is.na(spd)
        if (any(ok)) {
          ord <- order(spd_ts[ok])
          spd[ok][ord]
        } else {
          NULL
        }
      } else {
        NULL
      }

      heading_vec <- if ("heading" %in% names(geo)) {
        hdg <- geo$heading
        hdg_ts <- parse_timestamp_utc(geo$timestamp)
        ok <- !is.na(hdg_ts) & !is.na(hdg)
        if (any(ok)) {
          ord <- order(hdg_ts[ok])
          hdg[ok][ord]
        } else {
          NULL
        }
      } else {
        NULL
      }

      samples_novos <- criar_locomotion_samples(
        coords          = coords,
        timestamps_utc  = ts_utc,
        accuracy        = 5,
        altitude        = altitude_vec,
        speed           = speed_vec,
        heading         = heading_vec,
        force_single_tz = TRUE,
        moving_state    = "moving"
      )

      sample_ids <- vapply(samples_novos, `[[`, "", "id")

      item <- criar_timeline_item_path(
        timestamps_utc = ts_utc,
        coords         = coords,
        sample_ids     = sample_ids,
        tipo           = "voo_fr24",
        descricao      = sprintf("FR24 flight (%.1f km)", distancia_km),
        activity_type  = "airplane"
      )

      tl <- timeline()
      tl[[length(tl) + 1L]] <- item
      timeline(tl)

      s <- all_samples()
      all_samples(c(s, samples_novos))

      leaflet::leafletProxy("map") %>%
        leaflet::addPolylines(
          lng   = coords[, "lon"],
          lat   = coords[, "lat"],
          color = "purple",
          weight = 4,
          group = "rota_atual"
        ) %>%
        leaflet::fitBounds(
          lng1 = min(coords[, "lon"], na.rm = TRUE),
          lat1 = min(coords[, "lat"], na.rm = TRUE),
          lng2 = max(coords[, "lon"], na.rm = TRUE),
          lat2 = max(coords[, "lat"], na.rm = TRUE)
        )

      shiny::showNotification("FR24 flight added to timeline.", type = "message")
      return()
    }


    # Default case (sf): timestamps come from the form (local time)
    if (!inherits(geo, "sf")) {
      shiny::showNotification("Invalid imported geometry.", type = "error")
      return()
    }

    coords_list <- sf::st_coordinates(geo)
    if (nrow(coords_list) < 2) {
      shiny::showNotification("Geometry too short for route.", type = "error")
      return()
    }

    if (input$direcao_arquivo == "inverter") {
      coords_list <- coords_list[nrow(coords_list):1, , drop = FALSE]
    }

    lat1 <- coords_list[1, "Y"]
    lng1 <- coords_list[1, "X"]
    tz_origem <- tz_from_coords(lat1, lng1)

    latn <- coords_list[nrow(coords_list), "Y"]
    lngn <- coords_list[nrow(coords_list), "X"]
    tz_destino <- tz_from_coords(latn, lngn)
    if (is.na(tz_destino)) tz_destino <- tz_origem

    val <- validar_intervalo(
      data_ini = input$import_data_inicio,
      hora_ini = input$import_hora_inicio,
      data_fim = input$import_data_fim,
      hora_fim = input$import_hora_fim,
      tz = tz_origem
    )
    if (is.null(val)) {
      shiny::showNotification("Invalid time for imported route.", type = "error")
      return()
    }

    inicio_utc <- lubridate::with_tz(val$inicio, "UTC")
    fim_utc    <- lubridate::with_tz(val$fim,    "UTC")

    if (has_overlap(timeline(), inicio_utc, fim_utc)) {
      shiny::showNotification("This interval overlaps an existing activity.", type = "error")
      return()
    }

    n0 <- nrow(coords_list)
    n_target <- max(n0, 100L)

    ts_seq <- seq(inicio_utc, fim_utc, length.out = n_target)

    t0 <- seq(0, 1, length.out = n0)
    t_new <- seq(0, 1, length.out = n_target)

    lon_new <- approx(t0, coords_list[, "X"], xout = t_new)$y
    lat_new <- approx(t0, coords_list[, "Y"], xout = t_new)$y

    coords_new <- cbind(lon_new, lat_new)

    import_at <- input$import_activity_type %||% "car"

    samples_novos <- criar_locomotion_samples(
      coords         = coords_new,
      timestamps_utc = ts_seq,
      accuracy       = 10,
      force_single_tz = FALSE,
      moving_state   = "moving",
      activity_type  = import_at
    )
    sample_ids <- vapply(samples_novos, `[[`, "", "id")

    item <- criar_timeline_item_path(
      timestamps_utc = ts_seq,
      coords = coords_new,
      sample_ids = sample_ids,
      tipo = "rota_importada",
      descricao = "Route imported from file",
      activity_type = import_at
    )

    tl <- timeline()
    tl[[length(tl) + 1L]] <- item
    timeline(tl)

    s <- all_samples()
    all_samples(c(s, samples_novos))

    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("rota_atual") %>%
      leaflet::addPolylines(
        lng = coords_new[, 1],
        lat = coords_new[, 2],
        color = "purple",
        weight = 3,
        group = "rota_atual"
      ) %>%
      leaflet::fitBounds(
        lng1 = min(coords_new[, 1]), lat1 = min(coords_new[, 2]),
        lng2 = max(coords_new[, 1]), lat2 = max(coords_new[, 2])
      )

    shiny::showNotification("File route added.", type = "message")
  })

  # ---- Timeline UI ----

  output$timeline_list <- shiny::renderUI({
    tl <- timeline()
    if (length(tl) == 0) {
      return(shiny::tags$p("No items in the timeline yet."))
    }

    ord <- order(vapply(tl, function(it) item_start_date(it), character(1)))
    tl_ord <- tl[ord]

    shiny::tagList(
      lapply(seq_along(tl_ord), function(i) {
        it   <- tl_ord[[i]]
        tipo <- it$tipo %||% if (isTRUE(it$.isVisit)) "visita" else "rota"
        desc <- it$descricao %||% tipo

        ts_ini_utc <- parse_timestamp_utc(item_start_date(it))
        sec_ini    <- item_start_offset(it) %||% 0
        ts_ini_loc <- ts_ini_utc + sec_ini

        ts_fim_utc <- parse_timestamp_utc(item_end_date(it))
        sec_fim    <- item_end_offset(it) %||% sec_ini
        ts_fim_loc <- ts_fim_utc + sec_fim

        ini_str <- format(ts_ini_loc, "%Y-%m-%d %H:%M")
        fim_str <- format(ts_fim_loc, "%Y-%m-%d %H:%M")

        shiny::div(
          class = "card mb-2 p-2",
          shiny::strong(sprintf("[%s] %s", tipo, desc)),
          shiny::br(),
          shiny::tags$small(paste0(ini_str, " -> ", fim_str)),
          shiny::br(),
          shiny::actionButton(
            paste0("editar_", it$.internalId),
            "Edit",
            class = "btn-sm btn-warning"
          ),
          shiny::actionButton(
            paste0("deletar_", it$.internalId),
            "Delete",
            class = "btn-sm btn-danger"
          )
        )
      })
    )

  })

  # ---- Edit / delete items ----

  # Delete
  shiny::observe({
    tl <- timeline()
    if (length(tl) == 0) return()

    ids <- vapply(tl, `[[`, "", ".internalId")

    lapply(ids, function(id) {
      shiny::observeEvent(input[[paste0("deletar_", id)]], {
        tl_atual <- timeline()
        idx <- which(vapply(tl_atual, `[[`, "", ".internalId") == id)
        if (length(idx)) {
          it <- tl_atual[[idx]]
          tl_atual <- tl_atual[-idx]

          if (!is.null(it$samples)) {
            s <- all_samples()
            keep <- !vapply(s, function(x) x$id %in% it$samples, logical(1))
            all_samples(s[keep])
          }

          timeline(tl_atual)

          leaflet::leafletProxy("map") %>%
            leaflet::clearGroup("rota_atual") %>%
            leaflet::clearGroup("importado") %>%
            leaflet::clearGroup("waypoints")

          pontos_temp(data.frame(lat = numeric(0), lng = numeric(0)))

          shiny::showNotification("Item removed from timeline.", type = "message")
        }
      }, ignoreInit = TRUE)

    })
  })

  # Edit
  shiny::observe({
    tl <- timeline()
    if (length(tl) == 0) return()

    ids <- vapply(tl, `[[`, "", ".internalId")

    lapply(ids, function(id) {
      shiny::observeEvent(input[[paste0("editar_", id)]], {
        tl_atual <- timeline()
        idx <- which(vapply(tl_atual, `[[`, "", ".internalId") == id)
        if (!length(idx)) return()
        it <- tl_atual[[idx]]

        ts_utc <- parse_timestamp_utc(item_start_date(it))
        sec <- item_start_offset(it) %||% 0
        ts_local <- ts_utc + sec

        tsf_utc <- parse_timestamp_utc(item_end_date(it))
        secf <- item_end_offset(it) %||% sec
        tsf_local <- tsf_utc + secf

        shiny::showModal(
          shiny::modalDialog(
            title = "Edit item",
            shiny::dateInput("edit_data_inicio", "Start date", as.Date(ts_local)),
            shiny::textInput("edit_hora_inicio", "Start time", format(ts_local, "%H:%M")),
            shiny::dateInput("edit_data_fim", "End date", as.Date(tsf_local)),
            shiny::textInput("edit_hora_fim", "End time", format(tsf_local, "%H:%M")),
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton("salvar_edicao", "Save", class = "btn-primary")
            )
          )
        )

        session$userData$item_editando <- id
      }, ignoreInit = TRUE)
    })
  })

  shiny::observeEvent(input$salvar_edicao, {
    shiny::removeModal()
    id <- session$userData$item_editando
    if (is.null(id)) return()

    tl <- timeline()
    idx <- which(vapply(tl, `[[`, "", ".internalId") == id)
    if (!length(idx)) return()
    it <- tl[[idx]]

    start_sec <- item_start_offset(it) %||% 0
    end_sec   <- item_end_offset(it) %||% start_sec

    hora_ini_fmt <- formatar_hora(input$edit_hora_inicio)
    hora_fim_fmt <- formatar_hora(input$edit_hora_fim)

    local_ini <- lubridate::ymd_hms(paste(input$edit_data_inicio, hora_ini_fmt), tz = "UTC")
    local_fim <- lubridate::ymd_hms(paste(input$edit_data_fim, hora_fim_fmt), tz = "UTC")

    if (is.na(local_ini) || is.na(local_fim)) {
      shiny::showNotification("Invalid times.", type = "error")
      return()
    }

    novo_ini_utc <- local_ini - start_sec
    novo_fim_utc <- local_fim  - end_sec

    if (novo_fim_utc <= novo_ini_utc) {
      shiny::showNotification("End must be after start.", type = "error")
      return()
    }

    if (has_overlap(tl, novo_ini_utc, novo_fim_utc, ignore_id = id)) {
      shiny::showNotification("This interval overlaps an existing activity.", type = "error")
      return()
    }


    it <- set_item_start_date(it, format(novo_ini_utc, "%Y-%m-%dT%H:%M:%SZ"))
    it <- set_item_end_date(it, format(novo_fim_utc, "%Y-%m-%dT%H:%M:%SZ"))

    if (!is.null(it$samples) && length(it$samples)) {
      s <- all_samples()
      sample_ids <- it$samples
      n <- length(sample_ids)
      novos_ts <- seq(novo_ini_utc, novo_fim_utc, length.out = n)

      for (i in seq_len(n)) {
        sid <- sample_ids[i]
        idx_s <- which(vapply(s, `[[`, "", "id") == sid)
        if (length(idx_s)) {
          ts_utc <- novos_ts[i]
          lat <- s[[idx_s]]$latitude
          lon <- s[[idx_s]]$longitude
          tz <- tz_from_coords(lat, lon)
          if (is.na(tz)) tz <- "UTC"
          sec <- seconds_from_gmt(ts_utc, tz)

          s[[idx_s]]$date <- format(ts_utc, "%Y-%m-%dT%H:%M:%SZ")
          s[[idx_s]]$date <- s[[idx_s]]$date
          s[[idx_s]]$secondsFromGMT <- sec
        }
      }
      all_samples(s)
    }

    tl[[idx]] <- it
    timeline(tl)
    shiny::showNotification("Item updated.", type = "message")
  })

  # ---- Edit Samples mode ----

  # Helper: render samples on the map as draggable markers
  renderizar_edit_samples <- function() {
    samples <- edit_samples()
    virtuais <- samples_virtuais()
    ignored <- ignored_samples()
    proxy <- leaflet::leafletProxy("map")
    proxy <- proxy %>%
      leaflet::clearGroup("edit_samples") %>%
      leaflet::clearGroup("edit_virtuais") %>%
      leaflet::clearGroup("edit_rota")

    if (length(samples) == 0) return()

    # Filter samples with valid location
    samples <- Filter(function(s) {
      sample_has_coords(s)
    }, samples)

    if (length(samples) == 0) return()

    # Extract coordinates
    lats <- vapply(samples, function(s) as.numeric(s$latitude)[1], numeric(1))
    lngs <- vapply(samples, function(s) as.numeric(s$longitude)[1], numeric(1))
    ids <- vapply(samples, function(s) {
      id <- s$id
      if (is.null(id) || length(id) == 0) return(paste0("unknown_", runif(1)))
      as.character(id)[1]
    }, character(1))

    # Check if there is a stored OSRM route
    rota <- rota_osrm()
    if (!is.null(rota) && is.matrix(rota) && nrow(rota) >= 2) {
      # Draw the complete OSRM route (green)
      proxy <- proxy %>%
        leaflet::addPolylines(
          lng = rota[, 1],  # lon
          lat = rota[, 2],  # lat
          color = "#28a745",
          weight = 4,
          opacity = 0.8,
          group = "edit_rota"
        )
    } else {
      # No OSRM route: draw line connecting the samples
      proxy <- proxy %>%
        leaflet::addPolylines(
          lng = lngs,
          lat = lats,
          color = "blue",
          weight = 2,
          opacity = 0.6,
          group = "edit_rota"
        )
    }

    # ---- VIRTUAL SAMPLES (smaller orange dots) ----
    if (length(virtuais) > 0) {
      # Filter virtual samples with valid location
      virtuais <- Filter(function(s) {
        sample_has_coords(s)
      }, virtuais)
    }
    if (length(virtuais) > 0) {
      lats_v <- vapply(virtuais, function(s) as.numeric(s$latitude)[1], numeric(1))
      lngs_v <- vapply(virtuais, function(s) as.numeric(s$longitude)[1], numeric(1))

      proxy <- proxy %>%
        leaflet::addCircleMarkers(
          lng = lngs_v,
          lat = lats_v,
          radius = 4,
          color = "#ff8c00",  # laranja
          fillColor = "#ff8c00",
          fillOpacity = 0.8,
          stroke = FALSE,
          group = "edit_virtuais"
        )
    }

    # ---- ARC SAMPLES (draggable pins) ----
    for (i in seq_along(samples)) {
      is_ignored <- ids[i] %in% ignored
      proxy <- proxy %>%
        leaflet::addMarkers(
          lng = lngs[i],
          lat = lats[i],
          layerId = ids[i],
          group = "edit_samples",
          options = leaflet::markerOptions(draggable = TRUE)
        )
    }

    # Zoom to samples (includes virtual samples if they exist)
    all_lats <- lats
    all_lngs <- lngs
    if (length(virtuais) > 0) {
      all_lats <- c(all_lats, vapply(virtuais, function(s) as.numeric(s$latitude)[1], numeric(1)))
      all_lngs <- c(all_lngs, vapply(virtuais, function(s) as.numeric(s$longitude)[1], numeric(1)))
    }

    proxy %>%
      leaflet::fitBounds(
        lng1 = min(all_lngs), lat1 = min(all_lats),
        lng2 = max(all_lngs), lat2 = max(all_lats)
      )

    # Add right-click handler via JS
    session$sendCustomMessage("setup_context_menu", list())
  }

  # Load existing samples
  shiny::observeEvent(input$carregar_samples, {
    hora_ini_fmt <- formatar_hora(input$edit_samples_hora_inicio)
    hora_fim_fmt <- formatar_hora(input$edit_samples_hora_fim)

    if (is.na(hora_ini_fmt) || is.na(hora_fim_fmt)) {
      shiny::showNotification("Invalid times.", type = "error")
      return()
    }

    # Convert to UTC (assumes system local timezone)
    inicio_local <- lubridate::ymd_hms(
      paste(input$edit_samples_data_inicio, hora_ini_fmt),
      tz = Sys.timezone()
    )
    fim_local <- lubridate::ymd_hms(
      paste(input$edit_samples_data_fim, hora_fim_fmt),
      tz = Sys.timezone()
    )

    if (is.na(inicio_local) || is.na(fim_local)) {
      shiny::showNotification("Invalid date/time.", type = "error")
      return()
    }

    inicio_utc <- lubridate::with_tz(inicio_local, "UTC")
    fim_utc <- lubridate::with_tz(fim_local, "UTC")

    samples_raw <- carregar_samples_intervalo(inicio_utc, fim_utc)

    if (length(samples_raw) == 0) {
      shiny::showNotification("No samples found in the interval.", type = "warning")
      return()
    }

    # Filter samples with valid location
    samples <- Filter(function(s) {
      sample_has_coords(s)
    }, samples_raw)

    n_invalidos <- length(samples_raw) - length(samples)
    if (n_invalidos > 0) {
      shiny::showNotification(
        sprintf("%d samples skipped (no valid coordinates).", n_invalidos),
        type = "warning"
      )
    }

    if (length(samples) == 0) {
      shiny::showNotification("No samples with valid coordinates.", type = "error")
      return()
    }

    original_samples(samples)  # Keep copy of original samples for export
    # Store original timelineItemId (if it exists)
    if (length(samples) > 0 && !is.null(samples[[1]]$timelineItemId)) {
      original_timeline_item_id(samples[[1]]$timelineItemId)
    } else {
      original_timeline_item_id(NULL)
    }
    edit_samples(samples)
    rota_osrm(NULL)  # Clear OSRM route when loading new samples
    samples_virtuais(list())  # Clear virtual samples
    renderizar_edit_samples()

    shiny::showNotification(
      sprintf("%d samples loaded.", length(samples)),
      type = "message"
    )
  })

  # Capture marker drag
  shiny::observeEvent(input$map_marker_dragend, {
    event <- input$map_marker_dragend
    if (is.null(event)) return()

    sample_id <- event$id
    new_lat <- event$lat
    new_lng <- event$lng

    samples <- edit_samples()
    if (length(samples) == 0) return()

    # Find the sample by ID
    idx <- which(vapply(samples, function(s) {
      id <- s$id
      if (is.null(id) || length(id) == 0) return("")
      as.character(id)[1]
    }, character(1)) == sample_id)
    if (length(idx) == 0) return()

    # Update coordinates
    samples[[idx]]$latitude <- new_lat
    samples[[idx]]$longitude <- new_lng

    edit_samples(samples)
    rota_osrm(NULL)  # Clear OSRM route when manually moving marker
    samples_virtuais(list())  # Clear virtual samples

    # Redraw the line (without redrawing markers to keep drag)
    # Filter samples with valid location
    samples_validos <- Filter(function(s) {
      sample_has_coords(s)
    }, samples)
    if (length(samples_validos) == 0) return()

    lats <- vapply(samples_validos, function(s) as.numeric(s$latitude)[1], numeric(1))
    lngs <- vapply(samples_validos, function(s) as.numeric(s$longitude)[1], numeric(1))

    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("edit_rota") %>%
      leaflet::clearGroup("edit_virtuais") %>%
      leaflet::addPolylines(
        lng = lngs,
        lat = lats,
        color = "blue",
        weight = 2,
        opacity = 0.6,
        group = "edit_rota"
      )
  })

  # Snap-to-road OSRM route
  shiny::observeEvent(input$snap_to_road, {
    # Remove previous notifications
    shiny::removeNotification("snap_progress")

    tryCatch({
      shiny::showNotification("1/7 Starting...", id = "snap_progress", duration = NULL, type = "message")

      samples <- edit_samples()
      ignored <- ignored_samples()

      if (length(samples) == 0) {
        shiny::removeNotification("snap_progress")
        shiny::showNotification("No samples loaded. Click 'Load' first.", type = "error")
        return()
      }

      shiny::showNotification(sprintf("2/7 Filtering %d samples...", length(samples)), id = "snap_progress", duration = NULL, type = "message")

      # Filter ignored samples for snap-to-road
      ids <- character(length(samples))
      for (j in seq_along(samples)) {
        val <- samples[[j]]$id
        if (is.null(val)) val <- paste0("sample_", j)
        if (is.list(val)) val <- unlist(val)[1]
        ids[j] <- as.character(val)
      }
      samples_ativos <- samples[!ids %in% ignored]

      if (length(samples_ativos) < 2) {
        shiny::removeNotification("snap_progress")
        shiny::showNotification("Load at least 2 samples (not ignored) for snap-to-road.", type = "error")
        return()
      }

      perfil <- input$edit_osrm_perfil
      usa_google <- perfil %in% GOOGLE_TRANSIT_MODES

      if (usa_google) {
        if (!check_google_api()) {
          shiny::removeNotification("snap_progress")
          shiny::showNotification(
            "Google Maps API key não configurada ou inválida. Defina GOOGLE_MAPS_API_KEY no .Renviron.",
            type = "error", duration = 8
          )
          return()
        }
      } else {
        base_url <- OSRM_SERVERS[[perfil]]

        if (is.null(base_url)) {
          shiny::removeNotification("snap_progress")
          shiny::showNotification(sprintf("OSRM profile '%s' not configured.", perfil), type = "error")
          return()
        }

        shiny::showNotification("3/7 Checking OSRM...", id = "snap_progress", duration = NULL, type = "message")

        if (!check_osrm_server(base_url)) {
          shiny::removeNotification("snap_progress")
          mostrar_modal_osrm_offline(base_url)
          return()
        }
      }

      shiny::showNotification("4/7 Extracting coordinates...", id = "snap_progress", duration = NULL, type = "message")

      # Extract coordinates from active samples (with defensive conversion)
      n_ativos <- length(samples_ativos)
      lats <- numeric(n_ativos)
      lngs <- numeric(n_ativos)
      for (j in seq_len(n_ativos)) {
        lat_val <- samples_ativos[[j]]$latitude
        lng_val <- samples_ativos[[j]]$longitude
        if (is.list(lat_val)) lat_val <- unlist(lat_val)
        if (is.list(lng_val)) lng_val <- unlist(lng_val)
        lats[j] <- as.numeric(lat_val)[1]
        lngs[j] <- as.numeric(lng_val)[1]
      }

      # Limit to 100 waypoints (chunking if necessary)
      n <- length(samples_ativos)
      if (n > 100) {
        shiny::showNotification(
          sprintf("Too many samples (%d). Using only the first 100.", n),
          type = "warning"
        )
        lats <- lats[1:100]
        lngs <- lngs[1:100]
        samples_ativos <- samples_ativos[1:100]
        n <- 100
      }

      shiny::showNotification(sprintf("5/7 Calculating route with %d points...", n), id = "snap_progress", duration = NULL, type = "message")

      pts <- data.frame(lat = lats, lng = lngs)

      if (usa_google) {
        transit_mode <- GOOGLE_TRANSIT_MODE_MAP[[perfil]]
        rota <- calcular_rota_google_transit(pts, transit_mode = transit_mode)
        if (!is.null(rota)) {
          rota$coords <- suavizar_rota_osrm(rota$coords, tolerancia_m = 5, angulo_min = 2)
        }
      } else {
        rota <- calcular_rota_osrm(pts, perfil = perfil)
      }

      if (is.null(rota)) {
        shiny::removeNotification("snap_progress")
        shiny::showNotification(
          "Failed to calculate route. The server may not have been able to route between the points.",
          type = "error"
        )
        return()
      }

      if (is.null(rota$coords) || nrow(rota$coords) < 2) {
        shiny::removeNotification("snap_progress")
        shiny::showNotification("OSRM route returned no valid coordinates.", type = "error")
        return()
      }

      shiny::showNotification("6/7 Interpolating samples...", id = "snap_progress", duration = NULL, type = "message")

      coords_osrm <- rota$coords

      # Defensively convert to numeric matrix
      if (is.data.frame(coords_osrm)) {
        coords_osrm <- as.matrix(coords_osrm)
      }
      if (is.list(coords_osrm) && !is.matrix(coords_osrm)) {
        # If still a list, convert manually
        coords_osrm <- do.call(rbind, lapply(coords_osrm, function(x) as.numeric(unlist(x))))
      }
      # Ensure it is a numeric matrix
      storage.mode(coords_osrm) <- "double"

      # Calculate accumulated distance on the OSRM route
      n_osrm <- nrow(coords_osrm)
      if (is.null(n_osrm) || n_osrm < 2) {
        shiny::removeNotification("snap_progress")
        shiny::showNotification("OSRM route too short or invalid.", type = "error")
        return()
      }

      # Store the OSRM route geometry for display
      rota_osrm(coords_osrm)

      # Filter points with direction change (>2 degrees) or significant distance
      # Criteria: angular change OR distance > 50m from last included point
      pontos_relevantes <- 1L  # always include the first
      ultimo_incluido <- 1L

      for (i in 2:(n_osrm - 1)) {
        incluir <- FALSE

        # Criterion 1: direction change > 2 degrees
        b1 <- geosphere::bearing(
          c(coords_osrm[i - 1, 1], coords_osrm[i - 1, 2]),
          c(coords_osrm[i, 1], coords_osrm[i, 2])
        )
        b2 <- geosphere::bearing(
          c(coords_osrm[i, 1], coords_osrm[i, 2]),
          c(coords_osrm[i + 1, 1], coords_osrm[i + 1, 2])
        )
        diff_ang <- abs((b2 - b1 + 180) %% 360 - 180)
        if (diff_ang > 2) {
          incluir <- TRUE
        }

        # Criterion 2: distance > 50m from last included point
        if (!incluir) {
          dist_ultimo <- geosphere::distHaversine(
            c(coords_osrm[ultimo_incluido, 1], coords_osrm[ultimo_incluido, 2]),
            c(coords_osrm[i, 1], coords_osrm[i, 2])
          )
          if (dist_ultimo > 50) {
            incluir <- TRUE
          }
        }

        if (incluir) {
          pontos_relevantes <- c(pontos_relevantes, i)
          ultimo_incluido <- i
        }
      }
      pontos_relevantes <- c(pontos_relevantes, n_osrm)  # always include the last

      # Filtered coordinates for virtual samples
      coords_filtradas <- coords_osrm[pontos_relevantes, , drop = FALSE]
      n_filtrados <- nrow(coords_filtradas)

      dist_acum <- numeric(n_osrm)
      dist_acum[1] <- 0
      for (i in 2:n_osrm) {
        p1 <- c(coords_osrm[i - 1, 1], coords_osrm[i - 1, 2])
        p2 <- c(coords_osrm[i, 1], coords_osrm[i, 2])
        dist_acum[i] <- dist_acum[i - 1] + geosphere::distHaversine(p1, p2)
      }
      dist_total <- dist_acum[n_osrm]

      # For each active sample, interpolate position on the OSRM route
      # using time proportion - extract timestamps with for loop
      ts_vec <- numeric(n)
      for (j in seq_len(n)) {
        date_val <- samples_ativos[[j]]$date
        if (is.list(date_val)) date_val <- unlist(date_val)[1]
        ts_vec[j] <- as.numeric(as.POSIXct(as.character(date_val), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
      }

      t_min <- min(ts_vec)
      t_max <- max(ts_vec)
      t_range <- t_max - t_min
      if (t_range <= 0) t_range <- 1

      for (i in seq_len(n)) {
        # Time proportion (0 to 1)
        prop <- (ts_vec[i] - t_min) / t_range

        # Target distance on the OSRM route
        dist_alvo <- prop * dist_total

        # Find segment on the OSRM route
        seg_idx <- max(1, sum(dist_acum <= dist_alvo))
        if (seg_idx >= n_osrm) seg_idx <- n_osrm - 1

        # Interpolate within the segment
        d1 <- dist_acum[seg_idx]
        d2 <- dist_acum[seg_idx + 1]
        seg_len <- d2 - d1
        if (seg_len <= 0) seg_len <- 1

        frac <- (dist_alvo - d1) / seg_len
        frac <- max(0, min(1, frac))

        new_lon <- coords_osrm[seg_idx, 1] + frac * (coords_osrm[seg_idx + 1, 1] - coords_osrm[seg_idx, 1])
        new_lat <- coords_osrm[seg_idx, 2] + frac * (coords_osrm[seg_idx + 1, 2] - coords_osrm[seg_idx, 2])

        samples_ativos[[i]]$longitude <- new_lon
        samples_ativos[[i]]$latitude <- new_lat
      }

      shiny::showNotification("7/7 Finishing...", id = "snap_progress", duration = NULL, type = "message")

      # Recalculate bearings of active samples - uses for loop
      coords_new <- matrix(0, nrow = length(samples_ativos), ncol = 2)
      for (j in seq_along(samples_ativos)) {
        coords_new[j, 1] <- as.numeric(samples_ativos[[j]]$longitude)
        coords_new[j, 2] <- as.numeric(samples_ativos[[j]]$latitude)
      }
      bearings <- calcular_bearings(coords_new)

      for (i in seq_along(samples_ativos)) {
        samples_ativos[[i]]$course <- bearings[i]
      }

      # Update active samples back into the full list
      ids_ativos <- character(length(samples_ativos))
      for (j in seq_along(samples_ativos)) {
        val <- samples_ativos[[j]]$id
        if (is.null(val)) val <- paste0("sample_", j)
        if (is.list(val)) val <- unlist(val)[1]
        ids_ativos[j] <- as.character(val)
      }

      for (i in seq_along(samples)) {
        sid <- samples[[i]]$id
        if (is.null(sid)) next
        if (is.list(sid)) sid <- unlist(sid)[1]
        sid <- as.character(sid)
        idx_ativo <- which(ids_ativos == sid)
        if (length(idx_ativo) == 1) {
          samples[[i]] <- samples_ativos[[idx_ativo]]
        }
      }

      # ---- CREATE VIRTUAL SAMPLES from OSRM route ----
      # Calculate accumulated distance for filtered points
      dist_acum_filtrados <- numeric(n_filtrados)
      dist_acum_filtrados[1] <- 0
      for (i in 2:n_filtrados) {
        # Use original point indices to calculate distance
        idx_orig <- pontos_relevantes[i]
        dist_acum_filtrados[i] <- dist_acum[idx_orig]
      }

      # Get timelineItemId from the first sample (for virtual samples)
      ti_id_virtual <- NULL
      if (length(samples_ativos) > 0 && !is.null(samples_ativos[[1]]$timelineItemId)) {
        ti_id_virtual <- samples_ativos[[1]]$timelineItemId
      }

      # Create virtual samples interpolating timestamps
      novos_virtuais <- list()
      for (i in seq_len(n_filtrados)) {
        # Distance-based proportion
        prop <- if (dist_total > 0) dist_acum_filtrados[i] / dist_total else 0

        # Interpolate timestamp
        ts_virtual <- t_min + prop * t_range
        ts_utc <- as.POSIXct(ts_virtual, origin = "1970-01-01", tz = "UTC")
        date_str <- format(ts_utc, "%Y-%m-%dT%H:%M:%SZ")

        # Coordinates
        lon_v <- coords_filtradas[i, 1]
        lat_v <- coords_filtradas[i, 2]

        # Calculate bearing to the next point
        course_v <- 0
        if (i < n_filtrados) {
          course_v <- geosphere::bearing(
            c(coords_filtradas[i, 1], coords_filtradas[i, 2]),
            c(coords_filtradas[i + 1, 1], coords_filtradas[i + 1, 2])
          )
          if (is.na(course_v)) course_v <- 0
          if (course_v < 0) course_v <- course_v + 360
        } else if (i > 1) {
          # Last point: use bearing from previous
          course_v <- geosphere::bearing(
            c(coords_filtradas[i - 1, 1], coords_filtradas[i - 1, 2]),
            c(coords_filtradas[i, 1], coords_filtradas[i, 2])
          )
          if (is.na(course_v)) course_v <- 0
          if (course_v < 0) course_v <- course_v + 360
        }

        # Timezone lookup
        tz_v <- tz_from_coords(lat_v, lon_v)
        if (is.na(tz_v)) tz_v <- "UTC"
        sec_gmt_v <- seconds_from_gmt(ts_utc, tz_v)

        # Create virtual sample (LocoKit2 format)
        sample_v <- list(
          id = toupper(uuid::UUIDgenerate(use.time = TRUE)),
          source = "LocoKit2",
          sourceVersion = "9.0.0",
          date = date_str,
          secondsFromGMT = sec_gmt_v,
          lastSaved = date_str,
          latitude = lat_v,
          longitude = lon_v,
          altitude = 0,
          horizontalAccuracy = 10,
          verticalAccuracy = 10,
          speed = 0,
          course = course_v,
          movingState = 1L,
          recordingState = 2L,
          classifiedActivityType = activity_type_code("car"),
          confirmedActivityType = activity_type_code("car"),
          stepHz = 0,
          disabled = FALSE,
          .virtual = TRUE
        )

        if (!is.null(ti_id_virtual)) {
          sample_v$timelineItemId <- ti_id_virtual
        }

        novos_virtuais[[length(novos_virtuais) + 1L]] <- sample_v
      }

      # Store virtual samples
      samples_virtuais(novos_virtuais)

      edit_samples(samples)
      renderizar_edit_samples()

      shiny::removeNotification("snap_progress")
      shiny::showNotification(
        sprintf("Snap-to-road complete! %d Arc samples + %d virtual (%.1f km).",
                n, length(novos_virtuais), dist_total / 1000),
        type = "message"
      )
    }, error = function(e) {
      shiny::removeNotification("snap_progress")
      shiny::showNotification(
        paste("Snap-to-road error:", e$message),
        type = "error",
        duration = NULL
      )
    })
  })

  # ---- Context menu handlers ----

  # Properties: show modal with sample information
  shiny::observeEvent(input$ctx_propriedades, {
    req(input$ctx_propriedades)
    sample_id <- input$ctx_propriedades$id
    samples <- edit_samples()

    idx <- which(vapply(samples, function(s) s$id, character(1)) == sample_id)
    if (length(idx) == 0) return()

    s <- samples[[idx]]
    ts_utc <- as.POSIXct(s$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ts_local <- ts_utc + (s$secondsFromGMT %||% 0)

    lat <- s$latitude
    lon <- s$longitude
    speed <- s$speed %||% NA
    course <- s$course %||% NA
    alt <- s$altitude %||% NA

    shiny::showModal(
      shiny::modalDialog(
        title = "Sample Properties",
        shiny::tags$table(
          class = "table table-condensed",
          shiny::tags$tr(shiny::tags$td(shiny::strong("ID:")), shiny::tags$td(sample_id)),
          shiny::tags$tr(shiny::tags$td(shiny::strong("Date/Time (UTC):")), shiny::tags$td(format(ts_utc, "%Y-%m-%d %H:%M:%S"))),
          shiny::tags$tr(shiny::tags$td(shiny::strong("Date/Time (Local):")), shiny::tags$td(format(ts_local, "%Y-%m-%d %H:%M:%S"))),
          shiny::tags$tr(shiny::tags$td(shiny::strong("Latitude:")), shiny::tags$td(sprintf("%.8f", lat))),
          shiny::tags$tr(shiny::tags$td(shiny::strong("Longitude:")), shiny::tags$td(sprintf("%.8f", lon))),
          shiny::tags$tr(shiny::tags$td(shiny::strong("Speed:")), shiny::tags$td(if (is.na(speed)) "-" else sprintf("%.1f km/h", speed * 3.6))),
          shiny::tags$tr(shiny::tags$td(shiny::strong("Course:")), shiny::tags$td(if (is.na(course)) "-" else sprintf("%.1f degrees", course))),
          shiny::tags$tr(shiny::tags$td(shiny::strong("Altitude:")), shiny::tags$td(if (is.na(alt)) "-" else sprintf("%.1f m", alt)))
        ),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      )
    )
  })

  # Ignore: toggle sample ignored
  shiny::observeEvent(input$ctx_ignorar, {
    req(input$ctx_ignorar)
    sample_id <- input$ctx_ignorar$id
    ignored <- ignored_samples()

    if (sample_id %in% ignored) {
      # Remove from ignored list
      ignored <- ignored[ignored != sample_id]
      shiny::showNotification("Sample re-included.", type = "message")
    } else {
      # Add to ignored list
      ignored <- c(ignored, sample_id)
      shiny::showNotification("Sample ignored.", type = "message")
    }

    ignored_samples(ignored)
    renderizar_edit_samples()
  })

  # Discard: remove sample from list
  shiny::observeEvent(input$ctx_descartar, {
    req(input$ctx_descartar)
    sample_id <- input$ctx_descartar$id
    samples <- edit_samples()

    idx <- which(vapply(samples, function(s) s$id, character(1)) == sample_id)
    if (length(idx) == 0) return()

    samples <- samples[-idx]
    edit_samples(samples)

    # Also remove from ignored list if present
    ignored <- ignored_samples()
    ignored <- ignored[ignored != sample_id]
    ignored_samples(ignored)

    renderizar_edit_samples()
    shiny::showNotification("Sample discarded.", type = "message")
  })

  # Insert: create new sample after the selected one
  shiny::observeEvent(input$ctx_inserir, {
    req(input$ctx_inserir)
    sample_id <- input$ctx_inserir$id
    samples <- edit_samples()

    idx <- which(vapply(samples, function(s) s$id, character(1)) == sample_id)
    if (length(idx) == 0) return()

    # If it's the last sample, cannot insert after
    if (idx == length(samples)) {
      shiny::showNotification("Cannot insert after the last sample.", type = "warning")
      return()
    }

    s_atual <- samples[[idx]]
    s_prox <- samples[[idx + 1]]

    # Calculate average position and timestamp
    lat_novo <- (s_atual$latitude + s_prox$latitude) / 2
    lon_novo <- (s_atual$longitude + s_prox$longitude) / 2

    ts_atual <- as.POSIXct(s_atual$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ts_prox <- as.POSIXct(s_prox$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ts_novo <- ts_atual + as.numeric(difftime(ts_prox, ts_atual, units = "secs")) / 2

    # Create new sample copying structure from current
    novo_sample <- s_atual
    novo_sample$id <- uuid::UUIDgenerate(use.time = TRUE)
    novo_sample$date <- format(ts_novo, "%Y-%m-%dT%H:%M:%SZ")
    novo_sample$latitude <- lat_novo
    novo_sample$longitude <- lon_novo
    novo_sample$date <- novo_sample$date
    novo_sample$lastSaved <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

    # Insert at the correct position
    if (idx == length(samples)) {
      samples <- c(samples, list(novo_sample))
    } else {
      samples <- c(samples[1:idx], list(novo_sample), samples[(idx + 1):length(samples)])
    }

    edit_samples(samples)
    renderizar_edit_samples()
    shiny::showNotification("New sample inserted.", type = "message")
  })

  # ---- Bulk actions (selection) ----

  # Ignore selected
  shiny::observeEvent(input$ignorar_selecionados, {
    selected <- input$selected_samples
    if (is.null(selected) || length(selected) == 0) {
      shiny::showNotification("No samples selected.", type = "warning")
      return()
    }

    ignored <- ignored_samples()
    novos <- setdiff(selected, ignored)
    ignored <- c(ignored, novos)
    ignored_samples(ignored)

    # Clear selection
    session$sendCustomMessage("clear_selection", list())

    renderizar_edit_samples()
    shiny::showNotification(sprintf("%d samples ignored.", length(novos)), type = "message")
  })

  # Discard selected
  shiny::observeEvent(input$descartar_selecionados, {
    selected <- input$selected_samples
    if (is.null(selected) || length(selected) == 0) {
      shiny::showNotification("No samples selected.", type = "warning")
      return()
    }

    samples <- edit_samples()
    ids <- vapply(samples, function(s) s$id, character(1))
    samples <- samples[!ids %in% selected]
    edit_samples(samples)

    # Remove from ignored list as well
    ignored <- ignored_samples()
    ignored <- ignored[!ignored %in% selected]
    ignored_samples(ignored)

    # Clear selection
    session$sendCustomMessage("clear_selection", list())

    renderizar_edit_samples()
    shiny::showNotification(sprintf("%d samples discarded.", length(selected)), type = "message")
  })

  # Download corrected samples
  output$download_edit_arc <- shiny::downloadHandler(
    filename = function() {
      paste0("arc_edit_", format(Sys.Date(), "%Y%m%d"), ".zip")
    },
    content = function(file) {
      all_edit_samples <- edit_samples()
      ignored <- ignored_samples()

      if (length(all_edit_samples) == 0) {
        shiny::showNotification("No samples to export.", type = "error")
        return()
      }

      # Filter ignored samples
      ids <- vapply(all_edit_samples, sample_id, character(1))
      samples <- all_edit_samples[!ids %in% ignored]

      if (length(samples) == 0) {
        shiny::showNotification("All samples were ignored.", type = "error")
        return()
      }

      tmpdir <- tempfile("arc_edit_")
      dir.create(tmpdir)

      # Create LocoKit2 structure (places/, items/, samples/ required by ImportManager)
      sample_dir <- file.path(tmpdir, "samples")
      items_dir  <- file.path(tmpdir, "items")
      places_dir <- file.path(tmpdir, "places")
      dir.create(sample_dir, recursive = TRUE)
      dir.create(items_dir,  recursive = TRUE)
      dir.create(places_dir, recursive = TRUE)

      current_timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

      ti_id <- original_timeline_item_id()
      virtuais <- samples_virtuais()

      # ---- NEW SAMPLES ----
      samples_novos <- list()

      if (length(virtuais) > 0) {
        for (i in seq_along(virtuais)) {
          s <- virtuais[[i]]
          s$.virtual <- NULL
          s$lastSaved <- current_timestamp
          samples_novos[[length(samples_novos) + 1L]] <- s
        }
      } else {
        for (i in seq_along(samples)) {
          s <- samples[[i]]
          s$id <- toupper(uuid::UUIDgenerate(use.time = TRUE))
          s$lastSaved <- current_timestamp
          samples_novos[[length(samples_novos) + 1L]] <- s
        }
      }

      # Group by ISO week and generate .json.gz
      samples_by_week <- list()
      for (s in samples_novos) {
        ts <- tryCatch(
          as.POSIXct(s$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
          error = function(e) NULL
        )
        if (is.null(ts) || is.na(ts)) next
        week_key <- strftime(ts, "%G-W%V")
        if (!week_key %in% names(samples_by_week)) {
          samples_by_week[[week_key]] <- list()
        }
        samples_by_week[[week_key]][[length(samples_by_week[[week_key]]) + 1L]] <- s
      }

      for (week_key in names(samples_by_week)) {
        gz_path <- file.path(sample_dir, paste0(week_key, ".json.gz"))
        json_str <- jsonlite::toJSON(samples_by_week[[week_key]], auto_unbox = TRUE, pretty = TRUE, digits = NA)
        con <- gzfile(gz_path, "w")
        writeLines(json_str, con)
        close(con)
      }

      # ---- SAMPLES TO DELETE ----
      originais <- original_samples()
      samples_apagar <- list()
      for (i in seq_along(originais)) {
        s <- originais[[i]]
        if (!(sample_id(s) %in% ignored)) {
          s$deleted <- TRUE
          s$lastSaved <- current_timestamp
          samples_apagar[[length(samples_apagar) + 1L]] <- s
        }
      }

      if (length(samples_apagar) > 0) {
        # Group deletions by week as well
        apagar_by_week <- list()
        for (s in samples_apagar) {
          ts <- tryCatch(
            as.POSIXct(s$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
            error = function(e) NULL
          )
          if (is.null(ts) || is.na(ts)) next
          week_key <- strftime(ts, "%G-W%V")
          wk <- paste0(week_key, "_apagar")
          if (!wk %in% names(apagar_by_week)) {
            apagar_by_week[[wk]] <- list()
          }
          apagar_by_week[[wk]][[length(apagar_by_week[[wk]]) + 1L]] <- s
        }

        for (wk in names(apagar_by_week)) {
          gz_path <- file.path(sample_dir, paste0(wk, ".json.gz"))
          json_str <- jsonlite::toJSON(apagar_by_week[[wk]], auto_unbox = TRUE, pretty = TRUE, digits = NA)
          con <- gzfile(gz_path, "w")
          writeLines(json_str, con)
          close(con)
        }
      }

      # Empty files for dirs without content (iCloud drops empty dirs)
      jsonlite::write_json(list(), file.path(items_dir, "0.json"), auto_unbox = TRUE)
      jsonlite::write_json(list(), file.path(places_dir, "0.json"), auto_unbox = TRUE)

      # Metadata
      metadata <- list(
        exportId = toupper(uuid::UUIDgenerate(use.time = TRUE)),
        lastBackupDate = current_timestamp,
        sessionStartDate = current_timestamp,
        sessionFinishDate = current_timestamp,
        exportMode = "bucketed",
        exportType = "full",
        schemaVersion = "2.2.0",
        stats = list(itemCount = 0L, sampleCount = length(samples_novos), placeCount = 0L),
        itemsCompleted = TRUE,
        samplesCompleted = TRUE,
        placesCompleted = TRUE
      )
      jsonlite::write_json(metadata, file.path(tmpdir, "metadata.json"), auto_unbox = TRUE, pretty = TRUE)

      owd <- setwd(tmpdir)
      on.exit(setwd(owd), add = TRUE)

      # Zip directories and metadata to preserve folder structure
      files_to_zip <- c("metadata.json", "items", "samples", "places")
      files_to_zip <- files_to_zip[file.exists(files_to_zip)]
      zip::zipr(zipfile = file, files = files_to_zip)

      n_novos <- length(samples_novos)
      n_apagar <- length(samples_apagar)

      msg <- sprintf("%d new samples exported, %d marked for deletion",
                     n_novos, n_apagar)
      if (length(virtuais) > 0) {
        msg <- paste0(msg, " (OSRM route virtual samples)")
      }
      shiny::showNotification(msg, type = "message")
    }
  )

  # ---- Clear all ----

  shiny::observeEvent(input$limpar_tudo, {
    timeline(list())
    all_samples(list())
    edit_samples(list())
    original_samples(list())
    original_timeline_item_id(NULL)
    ignored_samples(character(0))
    rota_osrm(NULL)
    samples_virtuais(list())
    pontos_temp(data.frame(lat = numeric(0), lng = numeric(0)))
    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("waypoints") %>%
      leaflet::clearGroup("rota_atual") %>%
      leaflet::clearGroup("importado") %>%
      leaflet::clearGroup("edit_samples") %>%
      leaflet::clearGroup("edit_virtuais") %>%
      leaflet::clearGroup("edit_rota")
    shiny::showNotification("Timeline, samples and map cleared.", type = "message")
  })

  # ---- Download Arc ----

  output$download_arc <- shiny::downloadHandler(
    filename = function() {
      paste0("arc_import_", format(Sys.Date(), "%Y%m%d"), ".zip")
    },
    content = function(file) {
      tmpdir <- tempfile("arc_import_")
      dir.create(tmpdir)
      tl <- timeline()
      s <- all_samples()

      if (length(tl) == 0 || length(s) == 0) {
        shiny::showNotification("No items/samples to export.", type = "error")
        return()
      }

      exportar_arc_json(tl, s, tmpdir,
                        data_trabalho = input$data_trabalho)

      owd <- setwd(tmpdir)
      on.exit(setwd(owd), add = TRUE)

      # Zip directories and metadata to preserve folder structure
      files_to_zip <- c("metadata.json", "items", "samples", "places")
      files_to_zip <- files_to_zip[file.exists(files_to_zip)]
      zip::zipr(zipfile = file, files = files_to_zip)
    }
  )

  # ---- Download GPX ----

  output$download_gpx <- shiny::downloadHandler(
    filename = function() {
      paste0("samples_", format(Sys.Date(), "%Y%m%d"), ".gpx")
    },
    content = function(file) {
      s <- all_samples()
      if (length(s) == 0) {
        shiny::showNotification("No samples to export.", type = "error")
        return()
      }

      gpx_header <- '<?xml version="1.0" encoding="UTF-8"?>\n<gpx version="1.1" creator="ArcTimelineBuilder" xmlns="http://www.topografix.com/GPX/1/1">\n  <trk>\n    <name>Arc Samples</name>\n    <trkseg>\n'
      gpx_footer <- '    </trkseg>\n  </trk>\n</gpx>\n'

      trkpts <- vapply(s, function(x) {
        lat <- x$latitude
        lon <- x$longitude
        time <- x$date
        sprintf('      <trkpt lat="%.8f" lon="%.8f"><time>%s</time></trkpt>\n', lat, lon, time)
      }, character(1))

      cat(gpx_header, file = file)
      cat(trkpts, file = file, append = TRUE)
      cat(gpx_footer, file = file, append = TRUE)
    }
  )

  # ---- Route Editing ----

  # Update route selector when entering "Editar Rota" mode
  shiny::observe({
    if (input$modo != "Editar Rota") return()

    tl <- timeline()
    if (length(tl) == 0) {
      shiny::updateSelectInput(session, "rota_editar_selecionada",
                               choices = c("No routes" = ""),
                               selected = "")
      return()
    }

    # Filter routes only (not visits)
    rotas <- Filter(function(x) !isTRUE(x$.isVisit), tl)
    if (length(rotas) == 0) {
      shiny::updateSelectInput(session, "rota_editar_selecionada",
                               choices = c("No routes" = ""),
                               selected = "")
      return()
    }

    # Create options with name, mode and local time
    opcoes <- vapply(rotas, function(x) {
      nome <- x$descricao %||% "Route"
      modo <- x$activityType %||% "transport"
      hora <- tryCatch({
        # Parse UTC timestamp
        ts_utc <- lubridate::ymd_hms(item_start_date(x), tz = "UTC")
        # Convert to system local timezone
        ts_local <- lubridate::with_tz(ts_utc, Sys.timezone())
        format(ts_local, "%H:%M")
      }, error = function(e) "")
      paste0(hora, " [", modo, "] - ", nome)
    }, character(1))
    names(opcoes) <- vapply(rotas, `[[`, "", ".internalId")

    # Invert to have ID as value
    choices <- stats::setNames(names(opcoes), opcoes)

    shiny::updateSelectInput(session, "rota_editar_selecionada",
                             choices = choices,
                             selected = choices[1])
  })

  # Helper function to render editable route
  renderizar_rota_editavel <- function() {
    nodes <- route_edit_nodes()
    proxy <- leaflet::leafletProxy("map")

    proxy <- proxy %>%
      leaflet::clearGroup("edit_route") %>%
      leaflet::clearGroup("edit_route_nodes")

    if (length(nodes) == 0) return()

    # Extract coordinates to draw the polyline
    all_lngs <- vapply(nodes, `[[`, numeric(1), "lng")
    all_lats <- vapply(nodes, `[[`, numeric(1), "lat")

    # Draw the polyline connecting all nodes
    proxy <- proxy %>%
      leaflet::addPolylines(
        lng = all_lngs,
        lat = all_lats,
        color = "#28a745",
        weight = 4,
        opacity = 0.8,
        group = "edit_route"
      )

    # Draw nodes as Markers with divIcon (CSS circles that support drag)
    for (i in seq_along(nodes)) {
      node <- nodes[[i]]

      # Color based on node type
      fill_color <- if (i == 1) "#28a745"           # green - start
                    else if (i == length(nodes)) "#dc3545"  # red - end
                    else if (isTRUE(node$is_inserted)) "#fd7e14"  # orange - inserted
                    else if (isTRUE(node$is_modified)) "#007bff"  # blue - modified
                    else "#6c757d"  # gray - original

      # makeIcon with inline SVG for draggable circles
      icon <- leaflet::makeIcon(
        iconUrl = sprintf(
          "data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg' width='16' height='16'><circle cx='8' cy='8' r='6' fill='%s' stroke='white' stroke-width='2'/></svg>",
          gsub("#", "%%23", fill_color)  # URL-encode the #
        ),
        iconWidth = 16, iconHeight = 16,
        iconAnchorX = 8, iconAnchorY = 8
      )

      proxy <- proxy %>%
        leaflet::addMarkers(
          lng = node$lng,
          lat = node$lat,
          icon = icon,
          layerId = node$id,
          group = "edit_route_nodes",
          options = leaflet::markerOptions(
            draggable = TRUE
          ),
          label = sprintf("Node %d", i)
        )
    }

    # Adjust bounds
    proxy %>%
      leaflet::fitBounds(
        lng1 = min(all_lngs) - 0.005, lat1 = min(all_lats) - 0.005,
        lng2 = max(all_lngs) + 0.005, lat2 = max(all_lats) + 0.005
      )

    # Set up event handlers on nodes
    session$sendCustomMessage("setup_route_nodes", list())
  }

  # Handler: Load route for editing
  shiny::observeEvent(input$carregar_rota_edit, {
    item_id <- input$rota_editar_selecionada
    if (is.null(item_id) || item_id == "") {
      shiny::showNotification("Select a route.", type = "warning")
      return()
    }

    tl <- timeline()
    idx <- which(vapply(tl, `[[`, "", ".internalId") == item_id)
    if (length(idx) == 0) {
      shiny::showNotification("Route not found.", type = "error")
      return()
    }

    item <- tl[[idx]]
    route_original_item(item)

    # Find associated samples
    s <- all_samples()
    sample_ids <- item$samples %||% character(0)
    route_samples <- Filter(function(x) x$id %in% sample_ids, s)

    if (length(route_samples) < 2) {
      shiny::showNotification("Route must have at least 2 samples.", type = "error")
      return()
    }

    # Sort samples by timestamp
    route_samples <- route_samples[order(vapply(route_samples, function(x) x$date, ""))]
    route_original_samples(route_samples)

    # Calculate original metrics
    coords <- extract_coords_from_samples(route_samples)
    total_dist <- trajeto_distancia_km(coords) * 1000  # metros

    ts_start <- as.POSIXct(gsub("Z$", "", item_start_date(item)), tz = "UTC")
    ts_end <- as.POSIXct(gsub("Z$", "", item_end_date(item)), tz = "UTC")
    total_duration <- as.numeric(difftime(ts_end, ts_start, units = "secs"))

    # If duration is invalid, estimate based on default speed (30 km/h)
    if (total_duration <= 0 || is.na(total_duration)) {
      # 30 km/h = 8.33 m/s
      total_duration <- total_dist / 8.33
      if (total_duration < 60) total_duration <- 60  # minimum 1 minute
    }

    avg_speed <- total_dist / total_duration  # m/s

    # Limit maximum speed to 200 km/h (55.5 m/s)
    if (avg_speed > 55.5) avg_speed <- 55.5

    route_avg_speed(avg_speed)

    # Create nodes from ALL samples
    n_samples <- length(route_samples)
    nodes <- lapply(seq_len(n_samples), function(i) {
      smp <- route_samples[[i]]
      list(
        id = paste0("node_", i),
        lat = as.numeric(smp$latitude),
        lng = as.numeric(smp$longitude),
        sample_id = smp$id,
        is_original = TRUE,
        is_modified = FALSE,
        is_inserted = FALSE
      )
    })
    route_edit_nodes(nodes)

    renderizar_rota_editavel()

    shiny::showNotification(
      sprintf("Route loaded: %d nodes, %.1f km, avg speed %.1f km/h",
              n_samples, total_dist / 1000, avg_speed * 3.6),
      type = "message"
    )
  })

  # Handler: Drag node (dragend)
  shiny::observeEvent(input$route_node_dragend, {
    event <- input$route_node_dragend
    if (is.null(event)) return()

    node_id <- event$id
    new_lat <- event$lat
    new_lng <- event$lng

    nodes <- route_edit_nodes()
    node_idx <- which(vapply(nodes, `[[`, "", "id") == node_id)

    if (length(node_idx) == 0) return()

    # Update node position
    nodes[[node_idx]]$lat <- new_lat
    nodes[[node_idx]]$lng <- new_lng
    nodes[[node_idx]]$is_modified <- TRUE
    route_edit_nodes(nodes)

    # Re-render the route (straight line between nodes)
    renderizar_rota_editavel()
  })

  # Handler: Delete node (context menu)
  shiny::observeEvent(input$ctx_delete_node, {
    event <- input$ctx_delete_node
    if (is.null(event)) return()

    node_id <- event$id
    nodes <- route_edit_nodes()
    node_idx <- which(vapply(nodes, `[[`, "", "id") == node_id)

    if (length(node_idx) == 0) return()

    # Cannot delete first or last node
    if (node_idx == 1 || node_idx == length(nodes)) {
      shiny::showNotification("Cannot delete the first or last node.", type = "warning")
      return()
    }

    # Remove the node
    nodes <- nodes[-node_idx]
    route_edit_nodes(nodes)

    renderizar_rota_editavel()
    shiny::showNotification("Node deleted.", type = "message")
  })

  # Handler: Insert waypoint (map click)
  shiny::observeEvent(input$insert_waypoint_click, {
    if (!isTRUE(input$modo_inserir_waypoint)) return()

    event <- input$insert_waypoint_click
    if (is.null(event)) return()

    click_lat <- event$lat
    click_lng <- event$lng

    nodes <- route_edit_nodes()

    if (length(nodes) == 0) {
      shiny::showNotification("Load a route first.", type = "warning")
      return()
    }

    # Find closest segment (straight line between consecutive nodes)
    min_dist <- Inf
    insert_after <- 1

    for (i in seq_len(length(nodes) - 1)) {
      dist <- point_to_segment_distance(
        click_lng, click_lat,
        nodes[[i]]$lng, nodes[[i]]$lat,
        nodes[[i + 1]]$lng, nodes[[i + 1]]$lat
      )
      if (dist < min_dist) {
        min_dist <- dist
        insert_after <- i
      }
    }

    # Create new node
    new_node_id <- paste0("node_ins_", format(Sys.time(), "%H%M%S%OS3"))
    new_node <- list(
      id = new_node_id,
      lat = click_lat,
      lng = click_lng,
      sample_id = NULL,
      is_original = FALSE,
      is_inserted = TRUE,
      is_modified = FALSE
    )

    # Insert node at the correct position
    nodes <- append(nodes, list(new_node), after = insert_after)
    route_edit_nodes(nodes)

    renderizar_rota_editavel()
    shiny::showNotification("Waypoint inserted.", type = "message")
  })

  # Handler: Apply edits
  shiny::observeEvent(input$aplicar_edicoes_rota, {
    nodes <- route_edit_nodes()
    original_item <- route_original_item()
    avg_speed <- route_avg_speed()

    if (length(nodes) < 2 || is.null(original_item)) {
      shiny::showNotification("No route to apply.", type = "error")
      return()
    }

    # Extract coordinates from nodes
    all_lngs <- vapply(nodes, `[[`, numeric(1), "lng")
    all_lats <- vapply(nodes, `[[`, numeric(1), "lat")
    all_coords <- cbind(all_lngs, all_lats)
    colnames(all_coords) <- c("lon", "lat")

    n_route <- nrow(all_coords)

    # Original start time
    start_utc <- as.POSIXct(gsub("Z$", "", item_start_date(original_item)), tz = "UTC")

    # Recalculate times keeping average speed
    time_result <- recalcular_tempos_rota(all_coords, start_utc, avg_speed)

    # Use nodes directly as samples (we already have all points)
    n_target <- n_route
    ts_seq <- time_result$timestamps_utc

    lon_new <- all_lngs
    lat_new <- all_lats
    coords_new <- all_coords

    # Create new samples (preserve activity type from original route)
    edit_at <- original_item$activityType %||% "car"
    samples_novos <- criar_locomotion_samples(
      coords = coords_new,
      timestamps_utc = ts_seq,
      accuracy       = 10,
      force_single_tz = TRUE,
      moving_state = "moving",
      activity_type = edit_at
    )
    sample_ids <- vapply(samples_novos, `[[`, "", "id")

    # Update timeline
    tl <- timeline()
    idx <- which(vapply(tl, `[[`, "", ".internalId") == original_item$.internalId)

    if (length(idx) > 0) {
      item <- tl[[idx]]

      # Remove old samples
      s <- all_samples()
      old_sample_ids <- item$samples %||% character(0)
      s <- Filter(function(x) !x$id %in% old_sample_ids, s)

      # Update item
      item$samples <- sample_ids
      item <- set_item_end_date(item, format(time_result$end_time_utc, "%Y-%m-%dT%H:%M:%SZ"))

      # Update description
      new_dist_km <- time_result$total_distance_m / 1000
      item$descricao <- sprintf("Edited route (%.1f km)", new_dist_km)

      tl[[idx]] <- item
      timeline(tl)

      # Add new samples
      all_samples(c(s, samples_novos))

      # Draw updated route
      leaflet::leafletProxy("map") %>%
        leaflet::clearGroup("edit_route") %>%
        leaflet::clearGroup("edit_route_nodes") %>%
        leaflet::clearGroup("rota_atual") %>%
        leaflet::addPolylines(
          lng = lon_new,
          lat = lat_new,
          color = "blue",
          weight = 4,
          group = "rota_atual"
        )

      # Clear editing state
      route_edit_nodes(list())
      route_original_item(NULL)
      route_original_samples(list())
      route_avg_speed(NULL)

      # Deactivate insert waypoint mode
      shiny::updateCheckboxInput(session, "modo_inserir_waypoint", value = FALSE)
      session$sendCustomMessage("clear_route_edit", list())

      # Calculate actual average speed of the edited route
      recalc_speed_kmh <- if (time_result$total_duration_s > 0) {
        (time_result$total_distance_m / time_result$total_duration_s) * 3.6
      } else {
        avg_speed * 3.6
      }

      shiny::showNotification(
        sprintf("Route updated! %.1f km, duration %.0f min, avg speed %.1f km/h",
                new_dist_km,
                time_result$total_duration_s / 60,
                recalc_speed_kmh),
        type = "message"
      )
    }
  })

  # Handler: Cancel edits
  shiny::observeEvent(input$cancelar_edicoes_rota, {
    # Clear editing state
    route_edit_nodes(list())
    route_original_item(NULL)
    route_original_samples(list())
    route_avg_speed(NULL)

    # Clear map
    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("edit_route") %>%
      leaflet::clearGroup("edit_route_nodes")

    # Deactivate insert waypoint mode
    shiny::updateCheckboxInput(session, "modo_inserir_waypoint", value = FALSE)
    session$sendCustomMessage("clear_route_edit", list())

    shiny::showNotification("Edit cancelled.", type = "message")
  })

  # Handler: Delete selected nodes (via lasso)
  shiny::observeEvent(input$deletar_nodes_selecionados, {
    selected <- input$selected_route_nodes
    if (is.null(selected) || length(selected) == 0) {
      shiny::showNotification("No nodes selected.", type = "warning")
      return()
    }

    nodes <- route_edit_nodes()
    if (length(nodes) < 3) {
      shiny::showNotification("Route must have at least 2 nodes.", type = "warning")
      return()
    }

    # Get IDs of first and last node
    node_ids <- vapply(nodes, `[[`, "", "id")
    first_id <- node_ids[1]
    last_id <- node_ids[length(node_ids)]

    # Remove first and last from selected list
    to_delete <- setdiff(selected, c(first_id, last_id))

    if (length(to_delete) == 0) {
      shiny::showNotification("Cannot delete first/last node.", type = "warning")
      return()
    }

    # Check if at least 2 nodes will remain
    remaining <- length(nodes) - length(to_delete)
    if (remaining < 2) {
      shiny::showNotification("Route must have at least 2 nodes.", type = "warning")
      return()
    }

    # Remove selected nodes
    nodes <- Filter(function(n) !n$id %in% to_delete, nodes)
    route_edit_nodes(nodes)

    renderizar_rota_editavel()
    session$sendCustomMessage("clear_node_selection", list())
    shiny::showNotification(sprintf("%d node(s) deleted.", length(to_delete)), type = "message")
  })

  # Handler: Clear node selection
  shiny::observeEvent(input$limpar_selecao_nodes, {
    session$sendCustomMessage("clear_node_selection", list())
    shiny::showNotification("Selection cleared.", type = "message")
  })
}
