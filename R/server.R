# ---- Server ----

server <- function(input, output, session) {

  # Helper: modal for OSRM offline with instructions

  show_osrm_offline_modal <- function(attempted_url) {
    shiny::showModal(
      shiny::modalDialog(
        title = "OSRM server is not running",
        shiny::tags$p(
          shiny::tags$strong("The OSRM server did not respond at:"),
          shiny::tags$code(attempted_url)
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
  temp_points <- shiny::reactiveVal(data.frame(lat = numeric(0), lng = numeric(0)))
  timeline <- shiny::reactiveVal(list())
  all_samples <- shiny::reactiveVal(list())
  edit_samples <- shiny::reactiveVal(list())  # Samples loaded for editing
  ignored_samples <- shiny::reactiveVal(character(0))  # IDs of ignored samples
  osrm_route <- shiny::reactiveVal(NULL)  # OSRM route geometry (lon/lat matrix)
  virtual_samples <- shiny::reactiveVal(list())  # Samples created from OSRM route (intermediate points)
  original_samples <- shiny::reactiveVal(list())  # Original samples before editing
  original_timeline_item_id <- shiny::reactiveVal(NULL)  # Original timelineItemId of samples

  # State for route editing
  route_edit_nodes <- shiny::reactiveVal(list())     # Editable route waypoints
  route_original_item <- shiny::reactiveVal(NULL)    # Original TimelineItem
  route_original_samples <- shiny::reactiveVal(list())  # Original route samples
  route_avg_speed <- shiny::reactiveVal(NULL)        # Average speed (m/s)

  # ---- Location History of other people ----
  # PEOPLE_CONFIG is loaded in app.R (global variable)
  people_config <- tryCatch(
    get("PEOPLE_CONFIG", envir = globalenv()),
    error = function(e) list()
  )
  location_history_cache <- shiny::reactiveVal(list())  # Cache of loaded data

  # Dynamically generates toggles based on configuration
  output$toggles_people <- shiny::renderUI({
    if (length(people_config) == 0) return(NULL)

    checkboxes <- lapply(people_config, function(p) {
      shiny::checkboxInput(
        inputId = paste0("lh_", p$id),
        label   = shiny::span(
          shiny::icon("map-marker-alt"),
          p$label,
          style = paste0("color:", p$color, "; font-weight: 500;")
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
  load_lh_person <- function(person_id) {
    cache <- location_history_cache()
    if (!is.null(cache[[person_id]])) {
      return(cache[[person_id]])
    }

    # Find person's config
    cfg <- NULL
    for (p in people_config) {
      if (p$id == person_id) {
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
    lh_data <- load_location_history(cfg$file)

    shiny::removeNotification("lh_loading")

    if (!is.null(lh_data)) {
      cache[[person_id]] <- lh_data
      location_history_cache(cache)
      shiny::showNotification(
        paste0(cfg$label, ": ", nrow(lh_data), " records loaded"),
        type = "message", duration = 3
      )
    } else {
      shiny::showNotification(
        paste0("Failed to load data for ", cfg$label),
        type = "error"
      )
    }

    lh_data
  }

  # Update map when date or toggles change
  shiny::observe({
    current_date <- input$work_date
    if (is.null(current_date)) return()

    # Clear previous layers
    proxy <- leaflet::leafletProxy("map")
    for (p in people_config) {
      proxy <- proxy %>% leaflet::clearGroup(paste0("lh_", p$id))
    }

    # Draw each enabled person
    for (p in people_config) {
      toggle_id <- paste0("lh_", p$id)
      if (!isTRUE(input[[toggle_id]])) next

      # Use RDS cache if available, otherwise load full JSON
      if (cache_exists(p$id)) {
        df <- load_lh_by_date(p$id, current_date)
      } else {
        lh_data <- load_lh_person(p$id)
        if (is.null(lh_data)) next
        df <- filter_location_history_by_date(lh_data, current_date)
      }

      if (is.null(df) || nrow(df) == 0) next

      group_name <- paste0("lh_", p$id)

      # Draw visits as circles
      visits <- df[df$type == "visit", ]
      if (nrow(visits) > 0) {
        for (i in seq_len(nrow(visits))) {
          v <- visits[i, ]
          datetime_start <- format_datetime_lh(v$start_time)
          datetime_end <- format_datetime_lh(v$end_time)

          proxy <- proxy %>%
            leaflet::addCircleMarkers(
              lng = v$lon, lat = v$lat,
              radius = 10,
              color = p$color, fillColor = p$color, fillOpacity = 0.6,
              stroke = TRUE, weight = 2,
              group = group_name,
              label = paste0(p$label, ": ", datetime_start, " - ", datetime_end)
            )
        }
      }

      # Draw activities as lines with direction indicators
      activities <- df[df$type == "activity", ]
      if (nrow(activities) > 0) {
        for (i in seq_len(nrow(activities))) {
          a <- activities[i, ]
          datetime_start <- format_datetime_lh(a$start_time)
          datetime_end <- format_datetime_lh(a$end_time)
          label_text <- paste0(p$label, ": ", datetime_start, " -> ", datetime_end)

          # Route line
          proxy <- proxy %>%
            leaflet::addPolylines(
              lng = c(a$lon, a$lon_end),
              lat = c(a$lat, a$lat_end),
              color = p$color, weight = 3, opacity = 0.7,
              group = group_name,
              label = label_text
            )

          # Start marker (small green circle)
          proxy <- proxy %>%
            leaflet::addCircleMarkers(
              lng = a$lon, lat = a$lat,
              radius = 4,
              color = "green", fillColor = "green", fillOpacity = 1,
              stroke = FALSE,
              group = group_name,
              label = paste0("Start: ", datetime_start)
            )

          # End marker (small red circle)
          proxy <- proxy %>%
            leaflet::addCircleMarkers(
              lng = a$lon_end, lat = a$lat_end,
              radius = 4,
              color = "red", fillColor = "red", fillOpacity = 1,
              stroke = FALSE,
              group = group_name,
              label = paste0("End: ", datetime_end)
            )
        }
      }

      # Check if any point is already visible on the current screen
      all_lats <- c(visits$lat, activities$lat, activities$lat_end)
      all_lons <- c(visits$lon, activities$lon, activities$lon_end)

      if (length(all_lats) > 0 && length(all_lons) > 0) {
        # Get current map bounds (isolate to avoid reactive loop with fitBounds)
        bounds <- shiny::isolate(input$map_bounds)

        # Only fitBounds if no point is visible
        any_visible <- FALSE
        if (!is.null(bounds)) {
          for (i in seq_along(all_lats)) {
            lat_i <- all_lats[i]
            lon_i <- all_lons[i]
            if (!is.na(lat_i) && !is.na(lon_i) &&
                lat_i >= bounds$south && lat_i <= bounds$north &&
                lon_i >= bounds$west && lon_i <= bounds$east) {
              any_visible <- TRUE
              break
            }
          }
        }

        if (!any_visible) {
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

  # Auto-format time inputs with debounce (gives time to type seconds)
  auto_format_time <- function(id) {
    val_reactive <- shiny::reactive({ input[[id]] })
    val_debounced <- shiny::debounce(val_reactive, millis = 1500)

    shiny::observeEvent(val_debounced(), {
      val <- val_debounced()
      if (is.null(val) || !nzchar(val)) return()

      # ONLY FORMAT if at least 3 characters typed (avoids formatting while typing)
      if (nchar(gsub("\\D", "", val)) < 3) return()

      fmt <- format_time(val)
      if (!is.na(fmt) && !identical(fmt, val)) {
        shiny::updateTextInput(session, id, value = fmt)
      }
    }, ignoreInit = TRUE)
  }

  # Time fields that should be auto-formatted
  auto_format_time("visit_time_start")
  auto_format_time("visit_time_end")
  auto_format_time("manual_time_start")
  auto_format_time("manual_time_end")
  auto_format_time("import_time_start")
  auto_format_time("import_time_end")
  auto_format_time("osrm_time_start")
  auto_format_time("osrm_time_end")
  auto_format_time("edit_time_start")
  auto_format_time("edit_time_end")
  auto_format_time("edit_samples_time_start")
  auto_format_time("edit_samples_time_end")


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

    next_date <- as.Date(next_local)
    next_time <- format(next_local, "%H:%M:%S")

    # visit
    shiny::updateDateInput(session, "visit_date_start", value = next_date)
    shiny::updateTextInput(session, "visit_time_start", value = next_time)

    # manual route
    shiny::updateDateInput(session, "manual_date_start", value = next_date)
    shiny::updateTextInput(session, "manual_time_start", value = next_time)

    # import
    shiny::updateDateInput(session, "import_date_start", value = next_date)
    shiny::updateTextInput(session, "import_time_start", value = next_time)
  })


  # When the main date changes, replicate to other date fields
  shiny::observeEvent(input$work_date, {
    d <- input$work_date

    shiny::updateDateInput(session, "visit_date_start", value = d)
    shiny::updateDateInput(session, "visit_date_end",    value = d)

    shiny::updateDateInput(session, "manual_date_start", value = d)
    shiny::updateDateInput(session, "manual_date_end",    value = d)

    shiny::updateDateInput(session, "import_date_start", value = d)
    shiny::updateDateInput(session, "import_date_end",    value = d)

    shiny::updateDateInput(session, "edit_samples_date_start", value = d)
    shiny::updateDateInput(session, "edit_samples_date_end",    value = d)
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
  update_map_points <- function(auto_zoom = TRUE) {
    pts <- temp_points()
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

    current_mode <- input$mode

    # Ignore clicks in Edit Samples mode (uses draggable markers)
    if (current_mode == "EditSamples") return()

    # Ignore clicks in Import File mode (uses uploaded file)
    if (current_mode == "Import") return()

    pts <- temp_points()

    if (current_mode == "Visit") {
      # visit uses only 1 point: replace
      pts <- data.frame(lat = click$lat, lng = click$lng)
      temp_points(pts)
      update_map_points(auto_zoom = TRUE)
    } else {
      # OSRM or Manual Route: add point
      pts <- rbind(pts, data.frame(lat = click$lat, lng = click$lng))
      temp_points(pts)
      update_map_points(auto_zoom = FALSE)
    }
  })

  # Clear points (OSRM / manual) - WITHOUT auto_zoom
  shiny::observeEvent(input$clear_points_osrm, {
    temp_points(data.frame(lat = numeric(0), lng = numeric(0)))
    update_map_points(auto_zoom = FALSE)
  })
  shiny::observeEvent(input$clear_points_manual, {
    temp_points(data.frame(lat = numeric(0), lng = numeric(0)))
    update_map_points(auto_zoom = FALSE)
  })

  # Undo last point - WITHOUT auto_zoom
  shiny::observeEvent(input$undo_point_osrm, {
    pts <- temp_points()
    if (nrow(pts) > 0) {
      pts <- pts[-nrow(pts), , drop = FALSE]
      temp_points(pts)
      update_map_points(auto_zoom = FALSE)
    }
  })
  shiny::observeEvent(input$undo_point_manual, {
    pts <- temp_points()
    if (nrow(pts) > 0) {
      pts <- pts[-nrow(pts), , drop = FALSE]
      temp_points(pts)
      update_map_points(auto_zoom = FALSE)
    }
  })

  # ---- Visit: auto-fill coordinates from frequent place name ----

  shiny::observeEvent(input$visit_name, {
    label <- input$visit_name
    if (label %in% names(FREQUENT_PLACES)) {
      loc <- FREQUENT_PLACES[[label]]
      pts <- data.frame(lat = loc$lat, lng = loc$lon)
      temp_points(pts)
      update_map_points(auto_zoom = TRUE)
      leaflet::leafletProxy("map") %>%
        leaflet::addPopups(loc$lon, loc$lat, paste0("Place: ", label), layerId = "visit_popup")
    } else {
      leaflet::leafletProxy("map") %>% leaflet::removePopup("visit_popup")
    }
  })

  # ---- Add visit ----

  shiny::observeEvent(input$add_visit, {
    pts <- temp_points()
    if (nrow(pts) != 1) {
      shiny::showNotification("Select exactly 1 point on the map for the visit.", type = "error")
      return()
    }
    lat <- pts$lat[1]
    lng <- pts$lng[1]

    tz <- tz_from_coords(lat, lng)

    val <- validate_interval(
      date_start = input$visit_date_start,
      time_start = input$visit_time_start,
      date_end = input$visit_date_end,
      time_end = input$visit_time_end,
      tz = tz
    )
    if (is.null(val)) {
      shiny::showNotification("Invalid time for the visit.", type = "error")
      return()
    }

    # convert visit interval to UTC to check overlap
    start_utc <- lubridate::with_tz(val$start, "UTC")
    end_utc    <- lubridate::with_tz(val$end,    "UTC")

    if (has_overlap(timeline(), start_utc, end_utc)) {
      shiny::showNotification("This interval overlaps an existing activity.", type = "error")
      return()
    }

    visit_result <- create_timeline_item_visit(
      lat = lat,
      lon = lng,
      start_local = val$start,
      end_local = val$end,
      name = if (nzchar(input$visit_name)) input$visit_name else NULL
    )


    item <- visit_result$item
    new_samples <- visit_result$samples

    # description
    start_time_local <- format(val$start, "%H:%M")
    end_time_local <- format(val$end, "%H:%M")
    visit_label <- item$.place$name %||% item$visit$customTitle %||% "Visit"
    item$type <- "visit"
    item$description <- sprintf("📍 %s (%s - %s)", visit_label, start_time_local, end_time_local)

    # Update timeline and samples
    tl <- timeline()
    tl[[length(tl) + 1L]] <- item
    timeline(tl)

    s <- all_samples()
    all_samples(c(s, new_samples))

    # fitBounds on the visit point
    leaflet::leafletProxy("map") %>%
      leaflet::fitBounds(lng, lat, lng, lat)

    shiny::showNotification("Visit added.", type = "message")
  })

  # ---- Manual route ----

  shiny::observeEvent(input$add_manual, {
    pts <- temp_points()
    if (nrow(pts) < 2) {
      shiny::showNotification("Set at least 2 points on the map for the manual route.", type = "error")
      return()
    }

    lat1 <- pts$lat[1]
    lng1 <- pts$lng[1]
    tz <- tz_from_coords(lat1, lng1)

    val <- validate_interval(
      date_start = input$manual_date_start,
      time_start = input$manual_time_start,
      date_end = input$manual_date_end,
      time_end = input$manual_time_end,
      tz = tz
    )
    if (is.null(val)) {
      shiny::showNotification("Invalid time for the manual route.", type = "error")
      return()
    }

    # Generate equally spaced UTC timestamps
    start_utc <- lubridate::with_tz(val$start, "UTC")
    end_utc    <- lubridate::with_tz(val$end,    "UTC")

    if (has_overlap(timeline(), start_utc, end_utc)) {
      shiny::showNotification("This interval overlaps an existing activity.", type = "error")
      return()
    }


    # Interpolate coords to at least 100 points
    coords <- as.matrix(pts[, c("lng", "lat")])
    n0 <- nrow(coords)
    n_target <- max(n0, 100L)
    ts_seq <- seq(start_utc, end_utc, length.out = n_target)

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

    new_samples <- create_locomotion_samples(
      coords         = coords_new,
      timestamps_utc = ts_seq,
      accuracy       = 10,
      force_single_tz = TRUE,
      moving_state   = "moving",
      activity_type  = manual_at
    )
    sample_ids <- vapply(new_samples, `[[`, "", "id")

    item <- create_timeline_item_path(
      timestamps_utc = ts_seq,
      coords = coords_new,
      sample_ids = sample_ids,
      type = "manual_route",
      description = sprintf("Manual route (%.1f km)",
                          geosphere::distVincentyEllipsoid(
                            coords_new[1, 2:1], coords_new[n_target, 2:1]
                          ) / 1000),
      activity_type = manual_at
    )

    tl <- timeline()
    tl[[length(tl) + 1L]] <- item
    timeline(tl)

    s <- all_samples()
    all_samples(c(s, new_samples))

    # Draw route and zoom
    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("current_route") %>%
      leaflet::addPolylines(
        lng = coords_new[, 1],
        lat = coords_new[, 2],
        color = "red",
        weight = 4,
        opacity = 0.8,
        group = "current_route"
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

  shiny::observeEvent(input$calculate_osrm, {
    pts <- temp_points()
    if (nrow(pts) < 2) {
      shiny::showNotification("Set at least 2 points on the map for the OSRM route.", type = "error")
      return()
    }

    profile <- input$osrm_profile

    # Transit profiles use Google Maps API instead of OSRM
    if (profile %in% GOOGLE_TRANSIT_MODES) {
      if (!check_google_api()) {
        shiny::showNotification(
          "Google Maps API key not configured or invalid. Set GOOGLE_MAPS_API_KEY in your .Renviron.",
          type = "error", duration = 8
        )
        return()
      }
      transit_mode <- GOOGLE_TRANSIT_MODE_MAP[[profile]]
      route <- calculate_google_transit_route(pts, transit_mode = transit_mode)
      if (is.null(route)) {
        shiny::showNotification(
          sprintf("No %s route found between these points. The API did not return routes with this transport mode.", profile),
          type = "error", duration = 8
        )
        return()
      }
      # Apply same smoothing as OSRM
      route$coords <- smooth_osrm_route(route$coords, tolerance_m = 5, angle_min = 2)
    } else {
      base_url <- OSRM_SERVERS[[profile]]

      if (is.null(base_url) || !check_osrm_server(base_url)) {
        show_osrm_offline_modal(base_url %||% "not configured")
        return()
      }

      route <- calculate_osrm_route(pts, profile = profile)
      if (is.null(route)) {
        shiny::showNotification("Failed to calculate OSRM route.", type = "error")
        return()
      }
    }

    # Suggest time based on the last activity (if any)
    last_utc <- get_last_end_local()  # returns in UTC
    if (!is.null(last_utc)) {
      last_local <- lubridate::with_tz(last_utc, tzone = Sys.timezone())
      suggested_start <- last_local + 60  # +1 minute
    } else {
      data_ref <- input$work_date
      suggested_start <- lubridate::ymd_hms(
        paste(data_ref, "08:00:00"),
        tz = Sys.timezone()
      )
    }
    suggested_end <- suggested_start + route$duration_s


    shiny::showModal(
      shiny::modalDialog(
        title = "OSRM route times",
        shiny::dateInput("osrm_date_start", "Start date", as.Date(suggested_start)),
        shiny::textInput("osrm_time_start", "Start time", format(suggested_start, "%H:%M:%S")),
        shiny::dateInput("osrm_date_end", "End date", as.Date(suggested_end)),
        shiny::textInput("osrm_time_end", "End time", format(suggested_end, "%H:%M:%S")),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("recalculate_osrm_end", "Recalculate arrival"),
          shiny::actionButton("confirm_osrm_route", "Confirm", class = "btn-primary")
        )
      )
    )

    auto_format_time("osrm_time_start")
    auto_format_time("osrm_time_end")

    # Store route in session attribute
    session$userData$last_osrm_route <- route
    session$userData$last_route_pts <- pts
    session$userData$last_route_profile <- profile
  })

  shiny::observeEvent(input$recalculate_osrm_end, {
    route <- session$userData$last_osrm_route
    if (is.null(route)) {
      shiny::showNotification("No OSRM route calculated to recalculate the arrival time.", type = "error")
      return()
    }

    coords <- route$coords
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
    start_time_fmt <- format_time(input$osrm_time_start)
    if (is.na(start_time_fmt)) {
      shiny::showNotification("Invalid departure time.", type = "error")
      return()
    }

    start_local <- suppressWarnings(
      lubridate::ymd_hms(
        paste(input$osrm_date_start, start_time_fmt),
        tz = tz
      )
    )
    if (is.na(start_local)) {
      shiny::showNotification("Could not parse the departure date/time.", type = "error")
      return()
    }

    # duration in seconds from OSRM
    dur_s <- as.numeric(route$duration_s)
    if (is.na(dur_s)) {
      shiny::showNotification("Invalid OSRM route duration.", type = "error")
      return()
    }

    end_local <- start_local + dur_s

    shiny::updateDateInput(session, "osrm_date_end", value = as.Date(end_local))
    shiny::updateTextInput(session, "osrm_time_end", value = format(end_local, "%H:%M:%S"))
  })


  shiny::observeEvent(input$confirm_osrm_route, {
    tryCatch(
      {
        shiny::removeModal()

        route <- session$userData$last_osrm_route
        if (is.null(route)) {
          shiny::showNotification("No OSRM route calculated.", type = "error")
          return()
        }

        coords <- route$coords
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

        start_time_fmt <- format_time(input$osrm_time_start)
        end_time_fmt <- format_time(input$osrm_time_end)

        if (is.na(start_time_fmt) || is.na(end_time_fmt)) {
          shiny::showNotification("Invalid times for OSRM route.", type = "error")
          return()
        }

        val <- validate_interval(
          date_start = input$osrm_date_start,
          time_start = start_time_fmt,
          date_end = input$osrm_date_end,
          time_end = end_time_fmt,
          tz = tz
        )
        if (is.null(val)) {
          shiny::showNotification("Invalid time for OSRM route.", type = "error")
          return()
        }

        if (!inherits(val$start, "POSIXt") || !inherits(val$end, "POSIXt")) {
          shiny::showNotification("OSRM route time interval could not be parsed.", type = "error")
          return()
        }

        start_utc <- lubridate::with_tz(val$start, "UTC")
        end_utc    <- lubridate::with_tz(val$end,    "UTC")

        if (is.na(start_utc) || is.na(end_utc) || end_utc <= start_utc) {
          shiny::showNotification("Invalid OSRM route interval (end before or equal to start).", type = "error")
          return()
        }

        if (has_overlap(timeline(), start_utc, end_utc)) {
          shiny::showNotification("This interval overlaps an existing activity.", type = "error")
          return()
        }

        n_target <- max(n0, 100L)
        ts_seq <- seq(from = start_utc, to = end_utc, length.out = n_target)
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
        profile <- session$userData$last_route_profile %||% "car"
        activity_map <- c(
          car   = "car",
          foot  = "walking",
          bike  = "cycling",
          bus   = "bus",
          metro = "metro",
          train = "train",
          tram  = "tram"
        )
        activity_type <- activity_map[[profile]] %||% "car"

        coords_new <- cbind(lon_new, lat_new)

        new_samples <- create_locomotion_samples(
          coords         = coords_new,
          timestamps_utc = ts_seq,
          accuracy       = 10,
          force_single_tz = TRUE,
          moving_state   = "moving",
          activity_type  = activity_type
        )
        sample_ids <- vapply(new_samples, `[[`, "", "id")

        item <- create_timeline_item_path(
          timestamps_utc = ts_seq,
          coords         = coords_new,
          sample_ids     = sample_ids,
          type           = "osrm_route",
          description    = sprintf("OSRM route (%s)", profile),
          activity_type  = activity_type
        )

        tl <- timeline()
        tl[[length(tl) + 1L]] <- item
        timeline(tl)

        s <- all_samples()
        all_samples(c(s, new_samples))

        leaflet::leafletProxy("map") %>%
          leaflet::clearGroup("current_route") %>%
          leaflet::clearGroup("waypoints") %>%
          leaflet::addPolylines(
            lng   = coords_new[, 1],
            lat   = coords_new[, 2],
            color = "blue",
            weight = 4,
            group = "current_route"
          ) %>%
          leaflet::fitBounds(
            lng1 = min(coords_new[, 1]),
            lat1 = min(coords_new[, 2]),
            lng2 = max(coords_new[, 1]),
            lat2 = max(coords_new[, 2])
          )

        # Clear temporary points after adding route
        temp_points(data.frame(lat = numeric(0), lng = numeric(0)))

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

  imported_geometry <- shiny::reactiveVal(NULL)
  fr24_enriched <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(input$geo_file, {
    shiny::req(input$geo_file)
    file_path <- input$geo_file$datapath
    ext <- tools::file_ext(file_path)
    ext <- tolower(ext)

    leaflet::leafletProxy("map") %>% leaflet::clearGroup("imported")

    if (ext == "kml") {
      geo_rich <- parse_kml_rich(file_path)
      if (!is.null(geo_rich) && "timestamp" %in% names(geo_rich)) {
        imported_geometry(geo_rich)
        fr24_enriched(TRUE)

        ts_gmt <- parse_timestamp_utc(geo_rich$timestamp)
        ord <- order(ts_gmt)
        ts_gmt <- ts_gmt[ord]

        coords <- cbind(
          geo_rich$lon[ord],
          geo_rich$lat[ord]
        )
        colnames(coords) <- c("lon", "lat")


        if (!is.null(ts_gmt) && !all(is.na(ts_gmt))) {
          start_ts <- ts_gmt[1]
          end_ts <- ts_gmt[length(ts_gmt)]
          shiny::updateDateInput(session, "import_date_start", value = as.Date(start_ts))
          shiny::updateTextInput(session, "import_time_start", value = format(lubridate::with_tz(start_ts, Sys.timezone()), "%H:%M"))
          shiny::updateDateInput(session, "import_date_end", value = as.Date(end_ts))
          shiny::updateTextInput(session, "import_time_end", value = format(lubridate::with_tz(end_ts, Sys.timezone()), "%H:%M"))
          shiny::showNotification("Times filled from KML file (UTC -> system local time).", type = "message")
        }

        leaflet::leafletProxy("map") %>%
          leaflet::addPolylines(
            lng = coords[, 1],
            lat = coords[, 2],
            color = "darkblue",
            weight = 3,
            group = "imported"
          ) %>%
          leaflet::addCircleMarkers(
            lng = coords[1, 1], lat = coords[1, 2],
            radius = 8, color = "green", fillColor = "green", fillOpacity = 1,
            group = "imported", label = "Start"
          ) %>%
          leaflet::addCircleMarkers(
            lng = coords[nrow(coords), 1], lat = coords[nrow(coords), 2],
            radius = 8, color = "red", fillColor = "red", fillOpacity = 1,
            group = "imported", label = "End"
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
      sf::st_read(file_path, quiet = TRUE),
      error = function(e) NULL
    )
    if (is.null(geo)) {
      shiny::showNotification("Could not read file as geographic layer.", type = "error")
      return()
    }

    fr24_enriched(FALSE)

    if (!sf::st_is_longlat(geo)) {
      geo <- sf::st_transform(geo, 4326)
    }

    if (any(sf::st_geometry_type(geo) %in% c("MULTILINESTRING"))) {
      geo <- sf::st_cast(geo, "LINESTRING")
    }

    imported_geometry(geo)

    bbox <- sf::st_bbox(geo)

    # Extract first and last point for direction markers
    coords_all <- sf::st_coordinates(geo)
    pt_start <- coords_all[1, c("X", "Y")]
    pt_end <- coords_all[nrow(coords_all), c("X", "Y")]

    leaflet::leafletProxy("map") %>%
      leaflet::addPolylines(
        data = geo,
        color = "purple",
        weight = 3,
        group = "imported"
      ) %>%
      leaflet::addCircleMarkers(
        lng = pt_start["X"], lat = pt_start["Y"],
        radius = 8, color = "green", fillColor = "green", fillOpacity = 1,
        group = "imported", label = "Start"
      ) %>%
      leaflet::addCircleMarkers(
        lng = pt_end["X"], lat = pt_end["Y"],
        radius = 8, color = "red", fillColor = "red", fillOpacity = 1,
        group = "imported", label = "End"
      ) %>%
      leaflet::fitBounds(
        lng1 = bbox["xmin"], lat1 = bbox["ymin"],
        lng2 = bbox["xmax"], lat2 = bbox["ymax"]
      )

    shiny::showNotification("Geographic file imported. Green = start, Red = end.", type = "message")
  })

  shiny::observeEvent(input$file_direction, {
    geo <- imported_geometry()
    if (is.null(geo)) return()

    leaflet::leafletProxy("map") %>% leaflet::clearGroup("imported")

    if (isTRUE(fr24_enriched())) {
      coords <- cbind(geo$lon, geo$lat)
      if (input$file_direction == "reverse") {
        coords <- coords[nrow(coords):1, , drop = FALSE]
      }
      leaflet::leafletProxy("map") %>%
        leaflet::addPolylines(
          lng = coords[, 1],
          lat = coords[, 2],
          color = "darkblue",
          weight = 3,
          group = "imported"
        ) %>%
        leaflet::addCircleMarkers(
          lng = coords[1, 1], lat = coords[1, 2],
          radius = 8, color = "green", fillColor = "green", fillOpacity = 1,
          group = "imported", label = "Start"
        ) %>%
        leaflet::addCircleMarkers(
          lng = coords[nrow(coords), 1], lat = coords[nrow(coords), 2],
          radius = 8, color = "red", fillColor = "red", fillOpacity = 1,
          group = "imported", label = "End"
        ) %>%
        leaflet::fitBounds(
          lng1 = min(coords[, 1]), lat1 = min(coords[, 2]),
          lng2 = max(coords[, 1]), lat2 = max(coords[, 2])
        )
    } else {
      bbox <- sf::st_bbox(geo)
      coords_all <- sf::st_coordinates(geo)

      # Consider inversion for markers
      if (input$file_direction == "reverse") {
        pt_start <- coords_all[nrow(coords_all), c("X", "Y")]
        pt_end <- coords_all[1, c("X", "Y")]
      } else {
        pt_start <- coords_all[1, c("X", "Y")]
        pt_end <- coords_all[nrow(coords_all), c("X", "Y")]
      }

      leaflet::leafletProxy("map") %>%
        leaflet::addPolylines(
          data = geo,
          color = "purple",
          weight = 3,
          group = "imported"
        ) %>%
        leaflet::addCircleMarkers(
          lng = pt_start["X"], lat = pt_start["Y"],
          radius = 8, color = "green", fillColor = "green", fillOpacity = 1,
          group = "imported", label = "Start"
        ) %>%
        leaflet::addCircleMarkers(
          lng = pt_end["X"], lat = pt_end["Y"],
          radius = 8, color = "red", fillColor = "red", fillOpacity = 1,
          group = "imported", label = "End"
        ) %>%
        leaflet::fitBounds(
          lng1 = bbox["xmin"], lat1 = bbox["ymin"],
          lng2 = bbox["xmax"], lat2 = bbox["ymax"]
        )
    }
  })

  shiny::observeEvent(input$add_import, {
    geo <- imported_geometry()
    if (is.null(geo)) {
      shiny::showNotification("No file imported.", type = "error")
      return()
    }

    # -------------------------------------------------------------------
    # SPECIAL IMPORT: FR24 flight track already enriched
    # -------------------------------------------------------------------
    if (isTRUE(fr24_enriched())) {
      traj <- tryCatch(
        clean_fr24_track(
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

      distancia_km <- track_distance_km(coords)

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

      new_samples <- create_locomotion_samples(
        coords          = coords,
        timestamps_utc  = ts_utc,
        accuracy        = 5,
        altitude        = altitude_vec,
        speed           = speed_vec,
        heading         = heading_vec,
        force_single_tz = TRUE,
        moving_state    = "moving"
      )

      sample_ids <- vapply(new_samples, `[[`, "", "id")

      item <- create_timeline_item_path(
        timestamps_utc = ts_utc,
        coords         = coords,
        sample_ids     = sample_ids,
        type           = "voo_fr24",
        description    = sprintf("FR24 flight (%.1f km)", distancia_km),
        activity_type  = "airplane"
      )

      tl <- timeline()
      tl[[length(tl) + 1L]] <- item
      timeline(tl)

      s <- all_samples()
      all_samples(c(s, new_samples))

      leaflet::leafletProxy("map") %>%
        leaflet::addPolylines(
          lng   = coords[, "lon"],
          lat   = coords[, "lat"],
          color = "purple",
          weight = 4,
          group = "current_route"
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

    if (input$file_direction == "reverse") {
      coords_list <- coords_list[nrow(coords_list):1, , drop = FALSE]
    }

    lat1 <- coords_list[1, "Y"]
    lng1 <- coords_list[1, "X"]
    tz_origin <- tz_from_coords(lat1, lng1)

    latn <- coords_list[nrow(coords_list), "Y"]
    lngn <- coords_list[nrow(coords_list), "X"]
    tz_dest <- tz_from_coords(latn, lngn)
    if (is.na(tz_dest)) tz_dest <- tz_origin

    val <- validate_interval(
      date_start = input$import_date_start,
      time_start = input$import_time_start,
      date_end = input$import_date_end,
      time_end = input$import_time_end,
      tz = tz_origin
    )
    if (is.null(val)) {
      shiny::showNotification("Invalid time for imported route.", type = "error")
      return()
    }

    start_utc <- lubridate::with_tz(val$start, "UTC")
    end_utc    <- lubridate::with_tz(val$end,    "UTC")

    if (has_overlap(timeline(), start_utc, end_utc)) {
      shiny::showNotification("This interval overlaps an existing activity.", type = "error")
      return()
    }

    n0 <- nrow(coords_list)
    n_target <- max(n0, 100L)

    ts_seq <- seq(start_utc, end_utc, length.out = n_target)

    t0 <- seq(0, 1, length.out = n0)
    t_new <- seq(0, 1, length.out = n_target)

    lon_new <- approx(t0, coords_list[, "X"], xout = t_new)$y
    lat_new <- approx(t0, coords_list[, "Y"], xout = t_new)$y

    coords_new <- cbind(lon_new, lat_new)

    import_at <- input$import_activity_type %||% "car"

    new_samples <- create_locomotion_samples(
      coords         = coords_new,
      timestamps_utc = ts_seq,
      accuracy       = 10,
      force_single_tz = FALSE,
      moving_state   = "moving",
      activity_type  = import_at
    )
    sample_ids <- vapply(new_samples, `[[`, "", "id")

    item <- create_timeline_item_path(
      timestamps_utc = ts_seq,
      coords = coords_new,
      sample_ids = sample_ids,
      type = "imported_route",
      description = "Route imported from file",
      activity_type = import_at
    )

    tl <- timeline()
    tl[[length(tl) + 1L]] <- item
    timeline(tl)

    s <- all_samples()
    all_samples(c(s, new_samples))

    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("current_route") %>%
      leaflet::addPolylines(
        lng = coords_new[, 1],
        lat = coords_new[, 2],
        color = "purple",
        weight = 3,
        group = "current_route"
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
        item_type <- it$type %||% if (isTRUE(it$.isVisit)) "visit" else "route"
        desc <- it$description %||% item_type

        ts_start_utc <- parse_timestamp_utc(item_start_date(it))
        sec_start    <- item_start_offset(it) %||% 0
        ts_start_loc <- ts_start_utc + sec_start

        ts_end_utc <- parse_timestamp_utc(item_end_date(it))
        sec_end    <- item_end_offset(it) %||% sec_start
        ts_end_loc <- ts_end_utc + sec_end

        start_str <- format(ts_start_loc, "%Y-%m-%d %H:%M")
        end_str <- format(ts_end_loc, "%Y-%m-%d %H:%M")

        shiny::div(
          class = "card mb-2 p-2",
          shiny::strong(sprintf("[%s] %s", item_type, desc)),
          shiny::br(),
          shiny::tags$small(paste0(start_str, " -> ", end_str)),
          shiny::br(),
          shiny::actionButton(
            paste0("edit_", it$.internalId),
            "Edit",
            class = "btn-sm btn-warning"
          ),
          shiny::actionButton(
            paste0("delete_", it$.internalId),
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
      shiny::observeEvent(input[[paste0("delete_", id)]], {
        tl_current <- timeline()
        idx <- which(vapply(tl_current, `[[`, "", ".internalId") == id)
        if (length(idx)) {
          it <- tl_current[[idx]]
          tl_current <- tl_current[-idx]

          if (!is.null(it$samples)) {
            s <- all_samples()
            keep <- !vapply(s, function(x) x$id %in% it$samples, logical(1))
            all_samples(s[keep])
          }

          timeline(tl_current)

          leaflet::leafletProxy("map") %>%
            leaflet::clearGroup("current_route") %>%
            leaflet::clearGroup("imported") %>%
            leaflet::clearGroup("waypoints")

          temp_points(data.frame(lat = numeric(0), lng = numeric(0)))

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
      shiny::observeEvent(input[[paste0("edit_", id)]], {
        tl_current <- timeline()
        idx <- which(vapply(tl_current, `[[`, "", ".internalId") == id)
        if (!length(idx)) return()
        it <- tl_current[[idx]]

        ts_utc <- parse_timestamp_utc(item_start_date(it))
        sec <- item_start_offset(it) %||% 0
        ts_local <- ts_utc + sec

        tsf_utc <- parse_timestamp_utc(item_end_date(it))
        secf <- item_end_offset(it) %||% sec
        tsf_local <- tsf_utc + secf

        shiny::showModal(
          shiny::modalDialog(
            title = "Edit item",
            shiny::dateInput("edit_date_start", "Start date", as.Date(ts_local)),
            shiny::textInput("edit_time_start", "Start time", format(ts_local, "%H:%M")),
            shiny::dateInput("edit_date_end", "End date", as.Date(tsf_local)),
            shiny::textInput("edit_time_end", "End time", format(tsf_local, "%H:%M")),
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton("save_edit", "Save", class = "btn-primary")
            )
          )
        )

        session$userData$editing_item <- id
      }, ignoreInit = TRUE)
    })
  })

  shiny::observeEvent(input$save_edit, {
    shiny::removeModal()
    id <- session$userData$editing_item
    if (is.null(id)) return()

    tl <- timeline()
    idx <- which(vapply(tl, `[[`, "", ".internalId") == id)
    if (!length(idx)) return()
    it <- tl[[idx]]

    start_sec <- item_start_offset(it) %||% 0
    end_sec   <- item_end_offset(it) %||% start_sec

    start_time_fmt <- format_time(input$edit_time_start)
    end_time_fmt <- format_time(input$edit_time_end)

    local_start <- lubridate::ymd_hms(paste(input$edit_date_start, start_time_fmt), tz = "UTC")
    local_end <- lubridate::ymd_hms(paste(input$edit_date_end, end_time_fmt), tz = "UTC")

    if (is.na(local_start) || is.na(local_end)) {
      shiny::showNotification("Invalid times.", type = "error")
      return()
    }

    new_start_utc <- local_start - start_sec
    new_end_utc <- local_end  - end_sec

    if (new_end_utc <= new_start_utc) {
      shiny::showNotification("End must be after start.", type = "error")
      return()
    }

    if (has_overlap(tl, new_start_utc, new_end_utc, ignore_id = id)) {
      shiny::showNotification("This interval overlaps an existing activity.", type = "error")
      return()
    }


    it <- set_item_start_date(it, format(new_start_utc, "%Y-%m-%dT%H:%M:%SZ"))
    it <- set_item_end_date(it, format(new_end_utc, "%Y-%m-%dT%H:%M:%SZ"))

    if (!is.null(it$samples) && length(it$samples)) {
      s <- all_samples()
      sample_ids <- it$samples
      n <- length(sample_ids)
      new_ids_ts <- seq(new_start_utc, new_end_utc, length.out = n)

      for (i in seq_len(n)) {
        sid <- sample_ids[i]
        idx_s <- which(vapply(s, `[[`, "", "id") == sid)
        if (length(idx_s)) {
          ts_utc <- new_ids_ts[i]
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
  render_edit_samples <- function() {
    samples <- edit_samples()
    virtual <- virtual_samples()
    ignored <- ignored_samples()
    proxy <- leaflet::leafletProxy("map")
    proxy <- proxy %>%
      leaflet::clearGroup("edit_samples") %>%
      leaflet::clearGroup("edit_virtual") %>%
      leaflet::clearGroup("edit_path")

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
    route <- osrm_route()
    if (!is.null(route) && is.matrix(route) && nrow(route) >= 2) {
      # Draw the complete OSRM route (green)
      proxy <- proxy %>%
        leaflet::addPolylines(
          lng = route[, 1],  # lon
          lat = route[, 2],  # lat
          color = "#28a745",
          weight = 4,
          opacity = 0.8,
          group = "edit_path"
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
          group = "edit_path"
        )
    }

    # ---- VIRTUAL SAMPLES (smaller orange dots) ----
    if (length(virtual) > 0) {
      # Filter virtual samples with valid location
      virtual <- Filter(function(s) {
        sample_has_coords(s)
      }, virtual)
    }
    if (length(virtual) > 0) {
      lats_v <- vapply(virtual, function(s) as.numeric(s$latitude)[1], numeric(1))
      lngs_v <- vapply(virtual, function(s) as.numeric(s$longitude)[1], numeric(1))

      proxy <- proxy %>%
        leaflet::addCircleMarkers(
          lng = lngs_v,
          lat = lats_v,
          radius = 4,
          color = "#ff8c00",  # orange
          fillColor = "#ff8c00",
          fillOpacity = 0.8,
          stroke = FALSE,
          group = "edit_virtual"
        )
    }

    # ---- ARC SAMPLES (draggable pins with timestamp popup) ----
    for (i in seq_along(samples)) {
      is_ignored <- ids[i] %in% ignored
      # Format timestamp for popup (UTC -> local)
      ts_str <- samples[[i]]$date
      popup_text <- ""
      if (!is.null(ts_str) && nzchar(ts_str)) {
        ts_utc <- tryCatch(
          as.POSIXct(ts_str, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
          error = function(e) NA
        )
        if (!is.na(ts_utc)) {
          ts_local <- lubridate::with_tz(ts_utc, Sys.timezone())
          popup_text <- format(ts_local, "%H:%M:%S")
        }
      }
      proxy <- proxy %>%
        leaflet::addMarkers(
          lng = lngs[i],
          lat = lats[i],
          layerId = ids[i],
          group = "edit_samples",
          popup = popup_text,
          options = leaflet::markerOptions(draggable = TRUE)
        )
    }

    # Zoom to samples (includes virtual samples if they exist)
    all_lats <- lats
    all_lngs <- lngs
    if (length(virtual) > 0) {
      all_lats <- c(all_lats, vapply(virtual, function(s) as.numeric(s$latitude)[1], numeric(1)))
      all_lngs <- c(all_lngs, vapply(virtual, function(s) as.numeric(s$longitude)[1], numeric(1)))
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
  shiny::observeEvent(input$load_samples, {
    start_time_fmt <- format_time(input$edit_samples_time_start)
    end_time_fmt <- format_time(input$edit_samples_time_end)

    if (is.na(start_time_fmt) || is.na(end_time_fmt)) {
      shiny::showNotification("Invalid times.", type = "error")
      return()
    }

    # Convert to UTC (assumes system local timezone)
    start_local <- lubridate::ymd_hms(
      paste(input$edit_samples_date_start, start_time_fmt),
      tz = Sys.timezone()
    )
    end_local <- lubridate::ymd_hms(
      paste(input$edit_samples_date_end, end_time_fmt),
      tz = Sys.timezone()
    )

    if (is.na(start_local) || is.na(end_local)) {
      shiny::showNotification("Invalid date/time.", type = "error")
      return()
    }

    start_utc <- lubridate::with_tz(start_local, "UTC")
    end_utc <- lubridate::with_tz(end_local, "UTC")

    samples_raw <- load_samples_interval(start_utc, end_utc)

    if (length(samples_raw) == 0) {
      shiny::showNotification("No samples found in the interval.", type = "warning")
      return()
    }

    # Filter samples with valid location
    samples <- Filter(function(s) {
      sample_has_coords(s)
    }, samples_raw)

    n_invalid <- length(samples_raw) - length(samples)
    if (n_invalid > 0) {
      shiny::showNotification(
        sprintf("%d samples skipped (no valid coordinates).", n_invalid),
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
    osrm_route(NULL)  # Clear OSRM route when loading new samples
    virtual_samples(list())  # Clear virtual samples
    render_edit_samples()

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
    osrm_route(NULL)  # Clear OSRM route when manually moving marker
    virtual_samples(list())  # Clear virtual samples

    # Redraw the line (without redrawing markers to keep drag)
    # Filter samples with valid location
    valid_samples <- Filter(function(s) {
      sample_has_coords(s)
    }, samples)
    if (length(valid_samples) == 0) return()

    lats <- vapply(valid_samples, function(s) as.numeric(s$latitude)[1], numeric(1))
    lngs <- vapply(valid_samples, function(s) as.numeric(s$longitude)[1], numeric(1))

    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("edit_path") %>%
      leaflet::clearGroup("edit_virtual") %>%
      leaflet::addPolylines(
        lng = lngs,
        lat = lats,
        color = "blue",
        weight = 2,
        opacity = 0.6,
        group = "edit_path"
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
      active_samples <- samples[!ids %in% ignored]

      if (length(active_samples) < 2) {
        shiny::removeNotification("snap_progress")
        shiny::showNotification("Load at least 2 samples (not ignored) for snap-to-road.", type = "error")
        return()
      }

      profile <- input$edit_osrm_profile
      uses_google <- profile %in% GOOGLE_TRANSIT_MODES

      if (uses_google) {
        if (!check_google_api()) {
          shiny::removeNotification("snap_progress")
          shiny::showNotification(
            "Google Maps API key not configured or invalid. Set GOOGLE_MAPS_API_KEY in your .Renviron.",
            type = "error", duration = 8
          )
          return()
        }
      } else {
        base_url <- OSRM_SERVERS[[profile]]

        if (is.null(base_url)) {
          shiny::removeNotification("snap_progress")
          shiny::showNotification(sprintf("OSRM profile '%s' not configured.", profile), type = "error")
          return()
        }

        shiny::showNotification("3/7 Checking OSRM...", id = "snap_progress", duration = NULL, type = "message")

        if (!check_osrm_server(base_url)) {
          shiny::removeNotification("snap_progress")
          show_osrm_offline_modal(base_url)
          return()
        }
      }

      shiny::showNotification("4/7 Extracting coordinates...", id = "snap_progress", duration = NULL, type = "message")

      # Extract coordinates from active samples (with defensive conversion)
      n_active <- length(active_samples)
      lats <- numeric(n_active)
      lngs <- numeric(n_active)
      for (j in seq_len(n_active)) {
        lat_val <- active_samples[[j]]$latitude
        lng_val <- active_samples[[j]]$longitude
        if (is.list(lat_val)) lat_val <- unlist(lat_val)
        if (is.list(lng_val)) lng_val <- unlist(lng_val)
        lats[j] <- as.numeric(lat_val)[1]
        lngs[j] <- as.numeric(lng_val)[1]
      }

      # Limit to 100 waypoints (chunking if necessary)
      n <- length(active_samples)
      if (n > 100) {
        shiny::showNotification(
          sprintf("Too many samples (%d). Using only the first 100.", n),
          type = "warning"
        )
        lats <- lats[1:100]
        lngs <- lngs[1:100]
        active_samples <- active_samples[1:100]
        n <- 100
      }

      shiny::showNotification(sprintf("5/7 Calculating route with %d points...", n), id = "snap_progress", duration = NULL, type = "message")

      pts <- data.frame(lat = lats, lng = lngs)

      if (uses_google) {
        transit_mode <- GOOGLE_TRANSIT_MODE_MAP[[profile]]
        route <- calculate_google_transit_route(pts, transit_mode = transit_mode)
        if (!is.null(route)) {
          route$coords <- smooth_osrm_route(route$coords, tolerance_m = 5, angle_min = 2)
        }
      } else {
        route <- calculate_osrm_route(pts, profile = profile)
      }

      if (is.null(route)) {
        shiny::removeNotification("snap_progress")
        shiny::showNotification(
          "Failed to calculate route. The server may not have been able to route between the points.",
          type = "error"
        )
        return()
      }

      if (is.null(route$coords) || nrow(route$coords) < 2) {
        shiny::removeNotification("snap_progress")
        shiny::showNotification("OSRM route returned no valid coordinates.", type = "error")
        return()
      }

      shiny::showNotification("6/7 Interpolating samples...", id = "snap_progress", duration = NULL, type = "message")

      osrm_coords <- route$coords

      # Defensively convert to numeric matrix
      if (is.data.frame(osrm_coords)) {
        osrm_coords <- as.matrix(osrm_coords)
      }
      if (is.list(osrm_coords) && !is.matrix(osrm_coords)) {
        # If still a list, convert manually
        osrm_coords <- do.call(rbind, lapply(osrm_coords, function(x) as.numeric(unlist(x))))
      }
      # Ensure it is a numeric matrix
      storage.mode(osrm_coords) <- "double"

      # Calculate accumulated distance on the OSRM route
      n_osrm <- nrow(osrm_coords)
      if (is.null(n_osrm) || n_osrm < 2) {
        shiny::removeNotification("snap_progress")
        shiny::showNotification("OSRM route too short or invalid.", type = "error")
        return()
      }

      # Store the OSRM route geometry for display
      osrm_route(osrm_coords)

      # Filter points with direction change (>2 degrees) or significant distance
      # Criteria: angular change OR distance > 50m from last included point
      relevant_points <- 1L  # always include the first
      last_included <- 1L

      for (i in 2:(n_osrm - 1)) {
        should_include <- FALSE

        # Criterion 1: direction change > 2 degrees
        b1 <- geosphere::bearing(
          c(osrm_coords[i - 1, 1], osrm_coords[i - 1, 2]),
          c(osrm_coords[i, 1], osrm_coords[i, 2])
        )
        b2 <- geosphere::bearing(
          c(osrm_coords[i, 1], osrm_coords[i, 2]),
          c(osrm_coords[i + 1, 1], osrm_coords[i + 1, 2])
        )
        diff_ang <- abs((b2 - b1 + 180) %% 360 - 180)
        if (diff_ang > 2) {
          should_include <- TRUE
        }

        # Criterion 2: distance > 50m from last included point
        if (!should_include) {
          dist_last <- geosphere::distHaversine(
            c(osrm_coords[last_included, 1], osrm_coords[last_included, 2]),
            c(osrm_coords[i, 1], osrm_coords[i, 2])
          )
          if (dist_last > 50) {
            should_include <- TRUE
          }
        }

        if (should_include) {
          relevant_points <- c(relevant_points, i)
          last_included <- i
        }
      }
      relevant_points <- c(relevant_points, n_osrm)  # always include the last

      # Filtered coordinates for virtual samples
      filtered_coords <- osrm_coords[relevant_points, , drop = FALSE]
      n_filtered <- nrow(filtered_coords)

      cumulative_dist <- numeric(n_osrm)
      cumulative_dist[1] <- 0
      for (i in 2:n_osrm) {
        p1 <- c(osrm_coords[i - 1, 1], osrm_coords[i - 1, 2])
        p2 <- c(osrm_coords[i, 1], osrm_coords[i, 2])
        cumulative_dist[i] <- cumulative_dist[i - 1] + geosphere::distHaversine(p1, p2)
      }
      total_dist <- cumulative_dist[n_osrm]

      # For each active sample, interpolate position on the OSRM route
      # using time proportion - extract timestamps with for loop
      ts_vec <- numeric(n)
      for (j in seq_len(n)) {
        date_val <- active_samples[[j]]$date
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
        target_dist <- prop * total_dist

        # Find segment on the OSRM route
        seg_idx <- max(1, sum(cumulative_dist <= target_dist))
        if (seg_idx >= n_osrm) seg_idx <- n_osrm - 1

        # Interpolate within the segment
        d1 <- cumulative_dist[seg_idx]
        d2 <- cumulative_dist[seg_idx + 1]
        seg_len <- d2 - d1
        if (seg_len <= 0) seg_len <- 1

        frac <- (target_dist - d1) / seg_len
        frac <- max(0, min(1, frac))

        new_lon <- osrm_coords[seg_idx, 1] + frac * (osrm_coords[seg_idx + 1, 1] - osrm_coords[seg_idx, 1])
        new_lat <- osrm_coords[seg_idx, 2] + frac * (osrm_coords[seg_idx + 1, 2] - osrm_coords[seg_idx, 2])

        active_samples[[i]]$longitude <- new_lon
        active_samples[[i]]$latitude <- new_lat
      }

      shiny::showNotification("7/7 Finishing...", id = "snap_progress", duration = NULL, type = "message")

      # Recalculate bearings of active samples - uses for loop
      coords_new <- matrix(0, nrow = length(active_samples), ncol = 2)
      for (j in seq_along(active_samples)) {
        coords_new[j, 1] <- as.numeric(active_samples[[j]]$longitude)
        coords_new[j, 2] <- as.numeric(active_samples[[j]]$latitude)
      }
      bearings <- calculate_bearings(coords_new)

      for (i in seq_along(active_samples)) {
        active_samples[[i]]$course <- bearings[i]
      }

      # Update active samples back into the full list
      active_ids <- character(length(active_samples))
      for (j in seq_along(active_samples)) {
        val <- active_samples[[j]]$id
        if (is.null(val)) val <- paste0("sample_", j)
        if (is.list(val)) val <- unlist(val)[1]
        active_ids[j] <- as.character(val)
      }

      for (i in seq_along(samples)) {
        sid <- samples[[i]]$id
        if (is.null(sid)) next
        if (is.list(sid)) sid <- unlist(sid)[1]
        sid <- as.character(sid)
        active_idx <- which(active_ids == sid)
        if (length(active_idx) == 1) {
          samples[[i]] <- active_samples[[active_idx]]
        }
      }

      # ---- CREATE VIRTUAL SAMPLES from OSRM route ----
      # Calculate accumulated distance for filtered points
      cumulative_dist_filtered <- numeric(n_filtered)
      cumulative_dist_filtered[1] <- 0
      for (i in 2:n_filtered) {
        # Use original point indices to calculate distance
        idx_orig <- relevant_points[i]
        cumulative_dist_filtered[i] <- cumulative_dist[idx_orig]
      }

      # Get timelineItemId from the first sample (for virtual samples)
      ti_id_virtual <- NULL
      if (length(active_samples) > 0 && !is.null(active_samples[[1]]$timelineItemId)) {
        ti_id_virtual <- active_samples[[1]]$timelineItemId
      }

      # Create virtual samples interpolating timestamps
      new_virtual_samples <- list()
      for (i in seq_len(n_filtered)) {
        # Distance-based proportion
        prop <- if (total_dist > 0) cumulative_dist_filtered[i] / total_dist else 0

        # Interpolate timestamp
        ts_virtual <- t_min + prop * t_range
        ts_utc <- as.POSIXct(ts_virtual, origin = "1970-01-01", tz = "UTC")
        date_str <- format(ts_utc, "%Y-%m-%dT%H:%M:%SZ")

        # Coordinates
        lon_v <- filtered_coords[i, 1]
        lat_v <- filtered_coords[i, 2]

        # Calculate bearing to the next point
        course_v <- 0
        if (i < n_filtered) {
          course_v <- geosphere::bearing(
            c(filtered_coords[i, 1], filtered_coords[i, 2]),
            c(filtered_coords[i + 1, 1], filtered_coords[i + 1, 2])
          )
          if (is.na(course_v)) course_v <- 0
          if (course_v < 0) course_v <- course_v + 360
        } else if (i > 1) {
          # Last point: use bearing from previous
          course_v <- geosphere::bearing(
            c(filtered_coords[i - 1, 1], filtered_coords[i - 1, 2]),
            c(filtered_coords[i, 1], filtered_coords[i, 2])
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

        new_virtual_samples[[length(new_virtual_samples) + 1L]] <- sample_v
      }

      # Store virtual samples
      virtual_samples(new_virtual_samples)

      edit_samples(samples)
      render_edit_samples()

      shiny::removeNotification("snap_progress")
      shiny::showNotification(
        sprintf("Snap-to-road complete! %d Arc samples + %d virtual (%.1f km).",
                n, length(new_virtual_samples), total_dist / 1000),
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
  shiny::observeEvent(input$ctx_properties, {
    req(input$ctx_properties)
    sample_id <- input$ctx_properties$id
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
  shiny::observeEvent(input$ctx_ignore, {
    req(input$ctx_ignore)
    sample_id <- input$ctx_ignore$id
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
    render_edit_samples()
  })

  # Discard: remove sample from list
  shiny::observeEvent(input$ctx_discard, {
    req(input$ctx_discard)
    sample_id <- input$ctx_discard$id
    samples <- edit_samples()

    idx <- which(vapply(samples, function(s) s$id, character(1)) == sample_id)
    if (length(idx) == 0) return()

    samples <- samples[-idx]
    edit_samples(samples)

    # Also remove from ignored list if present
    ignored <- ignored_samples()
    ignored <- ignored[ignored != sample_id]
    ignored_samples(ignored)

    render_edit_samples()
    shiny::showNotification("Sample discarded.", type = "message")
  })

  # Insert: create new sample after the selected one
  shiny::observeEvent(input$ctx_insert, {
    req(input$ctx_insert)
    sample_id <- input$ctx_insert$id
    samples <- edit_samples()

    idx <- which(vapply(samples, function(s) s$id, character(1)) == sample_id)
    if (length(idx) == 0) return()

    # If it's the last sample, cannot insert after
    if (idx == length(samples)) {
      shiny::showNotification("Cannot insert after the last sample.", type = "warning")
      return()
    }

    s_current <- samples[[idx]]
    s_next <- samples[[idx + 1]]

    # Calculate average position and timestamp
    lat_new <- (s_current$latitude + s_next$latitude) / 2
    lon_new <- (s_current$longitude + s_next$longitude) / 2

    ts_current <- as.POSIXct(s_current$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ts_next <- as.POSIXct(s_next$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ts_new <- ts_current + as.numeric(difftime(ts_next, ts_current, units = "secs")) / 2

    # Create new sample copying structure from current
    new_sample <- s_current
    new_sample$id <- uuid::UUIDgenerate(use.time = TRUE)
    new_sample$date <- format(ts_new, "%Y-%m-%dT%H:%M:%SZ")
    new_sample$latitude <- lat_new
    new_sample$longitude <- lon_new
    new_sample$date <- new_sample$date
    new_sample$lastSaved <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

    # Insert at the correct position
    if (idx == length(samples)) {
      samples <- c(samples, list(new_sample))
    } else {
      samples <- c(samples[1:idx], list(new_sample), samples[(idx + 1):length(samples)])
    }

    edit_samples(samples)
    render_edit_samples()
    shiny::showNotification("New sample inserted.", type = "message")
  })

  # ---- Bulk actions (selection) ----

  # Ignore selected
  shiny::observeEvent(input$ignore_selected, {
    selected <- input$selected_samples
    if (is.null(selected) || length(selected) == 0) {
      shiny::showNotification("No samples selected.", type = "warning")
      return()
    }

    ignored <- ignored_samples()
    new_ids <- setdiff(selected, ignored)
    ignored <- c(ignored, new_ids)
    ignored_samples(ignored)

    # Clear selection
    session$sendCustomMessage("clear_selection", list())

    render_edit_samples()
    shiny::showNotification(sprintf("%d samples ignored.", length(new_ids)), type = "message")
  })

  # Discard selected
  shiny::observeEvent(input$discard_selected, {
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

    render_edit_samples()
    shiny::showNotification(sprintf("%d samples discarded.", length(selected)), type = "message")
  })

  # ---- Download corrected samples as LocoKit2 zip ----
  # Exports edited/snapped samples as a LocoKit2 import package.
  #
  # Reuses the original timeline item ID (from original_timeline_item_id())
  # so that Arc sees an item with an existing ID.
  #
  # Known limitation (LocoKit2 ImportManager):
  # The ImportManager uses INSERT OR IGNORE for all data types.
  # Items/samples with existing IDs are silently skipped, never updated.
  # The corrected route is added as a NEW item alongside the original.
  # There is no way to replace or remove existing data via import.
  output$download_edit_arc <- shiny::downloadHandler(
    filename = function() {
      "Import.zip"
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
      virtual <- virtual_samples()

      # ---- NEW SAMPLES ----
      new_samples <- list()

      if (length(virtual) > 0) {
        for (i in seq_along(virtual)) {
          s <- virtual[[i]]
          s$.virtual <- NULL
          s$lastSaved <- current_timestamp
          new_samples[[length(new_samples) + 1L]] <- s
        }
      } else {
        for (i in seq_along(samples)) {
          s <- samples[[i]]
          s$id <- toupper(uuid::UUIDgenerate(use.time = TRUE))
          s$lastSaved <- current_timestamp
          new_samples[[length(new_samples) + 1L]] <- s
        }
      }

      # Reuse original item ID (so Arc updates in-place) or generate new
      item_id <- if (!is.null(ti_id) && nzchar(ti_id)) ti_id else toupper(uuid::UUIDgenerate(use.time = TRUE))
      for (i in seq_along(new_samples)) {
        new_samples[[i]]$timelineItemId <- item_id
      }

      # Group by ISO week
      samples_by_week <- list()
      for (s in new_samples) {
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

      originals <- original_samples()

      # Write all weekly files
      for (week_key in names(samples_by_week)) {
        gz_path <- file.path(sample_dir, paste0(week_key, ".json.gz"))
        json_str <- jsonlite::toJSON(samples_by_week[[week_key]], auto_unbox = TRUE, pretty = TRUE, digits = NA)
        con <- gzfile(gz_path, "w")
        writeLines(json_str, con)
        close(con)
      }

      # ---- TIMELINE ITEMS ----
      # Determine start/end from new samples
      ts_new_ids <- vapply(new_samples, function(s) {
        as.numeric(as.POSIXct(s$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
      }, numeric(1))
      start_date_str <- format(as.POSIXct(min(ts_new_ids), origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
      end_date_str   <- format(as.POSIXct(max(ts_new_ids), origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

      # Activity type from original samples
      at_code <- originals[[1]]$confirmedActivityType %||%
                 originals[[1]]$classifiedActivityType %||% 5L

      # Edited item — reuses original ID so Arc updates in-place via lastSaved
      new_item <- list(
        base = list(
          id              = item_id,
          source          = "LocoKit2",
          sourceVersion   = "9.0.0",
          isVisit         = FALSE,
          startDate       = start_date_str,
          endDate         = end_date_str,
          lastSaved       = current_timestamp,
          deleted         = FALSE,
          disabled        = FALSE,
          locked          = FALSE,
          samplesChanged  = FALSE,
          stepCount       = 0L,
          activeEnergyBurned = 0
        ),
        trip = list(
          itemId                 = item_id,
          classifiedActivityType = at_code,
          confirmedActivityType  = at_code,
          uncertainActivityType  = FALSE,
          distance               = 0,
          speed                  = 0,
          lastSaved              = current_timestamp
        )
      )

      items_export <- list(new_item)

      # Write items grouped by month
      month_key <- substr(start_date_str, 1, 7)
      items_path <- file.path(items_dir, paste0(month_key, ".json"))
      jsonlite::write_json(items_export, items_path, auto_unbox = TRUE, pretty = TRUE, digits = NA)

      # Empty places file (iCloud drops empty dirs)
      jsonlite::write_json(list(), file.path(places_dir, "0.json"), auto_unbox = TRUE)

      # Metadata
      metadata <- list(
        exportId = toupper(uuid::UUIDgenerate(use.time = TRUE)),
        lastBackupDate = current_timestamp,
        sessionStartDate = current_timestamp,
        sessionFinishDate = current_timestamp,
        exportMode = "bucketed",
        exportType = "incremental",
        schemaVersion = "2.2.0",
        stats = list(
          itemCount   = length(items_export),
          sampleCount = length(new_samples),
          placeCount  = 0L
        ),
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

      msg <- sprintf("%d new samples exported, %d items",
                     length(new_samples), length(items_export))
      if (length(virtual) > 0) {
        msg <- paste0(msg, " (OSRM route virtual samples)")
      }
      shiny::showNotification(msg, type = "message")
    }
  )

  # ---- Clear all ----

  shiny::observeEvent(input$clear_all, {
    timeline(list())
    all_samples(list())
    edit_samples(list())
    original_samples(list())
    original_timeline_item_id(NULL)
    ignored_samples(character(0))
    osrm_route(NULL)
    virtual_samples(list())
    temp_points(data.frame(lat = numeric(0), lng = numeric(0)))
    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("waypoints") %>%
      leaflet::clearGroup("current_route") %>%
      leaflet::clearGroup("imported") %>%
      leaflet::clearGroup("edit_samples") %>%
      leaflet::clearGroup("edit_virtual") %>%
      leaflet::clearGroup("edit_path")
    shiny::showNotification("Timeline, samples and map cleared.", type = "message")
  })

  # ---- Download Arc ----

  output$download_arc <- shiny::downloadHandler(
    filename = function() {
      "Import.zip"
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

      export_arc_json(tl, s, tmpdir,
                        work_date = input$work_date)

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

  # Update route selector when entering "EditRoute" mode
  shiny::observe({
    if (input$mode != "EditRoute") return()

    tl <- timeline()
    if (length(tl) == 0) {
      shiny::updateSelectInput(session, "selected_route",
                               choices = c("No routes" = ""),
                               selected = "")
      return()
    }

    # Filter routes only (not visits)
    routes <- Filter(function(x) !isTRUE(x$.isVisit), tl)
    if (length(routes) == 0) {
      shiny::updateSelectInput(session, "selected_route",
                               choices = c("No routes" = ""),
                               selected = "")
      return()
    }

    # Create options with name, mode and local time
    options_list <- vapply(routes, function(x) {
      label <- x$description %||% "Route"
      current_mode <- x$activityType %||% "transport"
      time_str <- tryCatch({
        # Parse UTC timestamp
        ts_utc <- lubridate::ymd_hms(item_start_date(x), tz = "UTC")
        # Convert to system local timezone
        ts_local <- lubridate::with_tz(ts_utc, Sys.timezone())
        format(ts_local, "%H:%M")
      }, error = function(e) "")
      paste0(time_str, " [", current_mode, "] - ", label)
    }, character(1))
    names(options_list) <- vapply(routes, `[[`, "", ".internalId")

    # Invert to have ID as value
    choices <- stats::setNames(names(options_list), options_list)

    shiny::updateSelectInput(session, "selected_route",
                             choices = choices,
                             selected = choices[1])
  })

  # Helper function to render editable route
  render_editable_route <- function() {
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
  shiny::observeEvent(input$load_route, {
    item_id <- input$selected_route
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
    total_dist <- track_distance_km(coords) * 1000  # metros

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

    render_editable_route()

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
    render_editable_route()
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

    render_editable_route()
    shiny::showNotification("Node deleted.", type = "message")
  })

  # Handler: Insert waypoint (map click)
  shiny::observeEvent(input$insert_waypoint_click, {
    if (!isTRUE(input$insert_waypoint_mode)) return()

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

    render_editable_route()
    shiny::showNotification("Waypoint inserted.", type = "message")
  })

  # Handler: Apply edits
  shiny::observeEvent(input$apply_route_edits, {
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
    time_result <- recalculate_route_times(all_coords, start_utc, avg_speed)

    # Use nodes directly as samples (we already have all points)
    n_target <- n_route
    ts_seq <- time_result$timestamps_utc

    lon_new <- all_lngs
    lat_new <- all_lats
    coords_new <- all_coords

    # Create new samples (preserve activity type from original route)
    edit_at <- original_item$activityType %||% "car"
    new_samples <- create_locomotion_samples(
      coords = coords_new,
      timestamps_utc = ts_seq,
      accuracy       = 10,
      force_single_tz = TRUE,
      moving_state = "moving",
      activity_type = edit_at
    )
    sample_ids <- vapply(new_samples, `[[`, "", "id")

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
      item$description <- sprintf("Edited route (%.1f km)", new_dist_km)

      tl[[idx]] <- item
      timeline(tl)

      # Add new samples
      all_samples(c(s, new_samples))

      # Draw updated route
      leaflet::leafletProxy("map") %>%
        leaflet::clearGroup("edit_route") %>%
        leaflet::clearGroup("edit_route_nodes") %>%
        leaflet::clearGroup("current_route") %>%
        leaflet::addPolylines(
          lng = lon_new,
          lat = lat_new,
          color = "blue",
          weight = 4,
          group = "current_route"
        )

      # Clear editing state
      route_edit_nodes(list())
      route_original_item(NULL)
      route_original_samples(list())
      route_avg_speed(NULL)

      # Deactivate insert waypoint mode
      shiny::updateCheckboxInput(session, "insert_waypoint_mode", value = FALSE)
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
  shiny::observeEvent(input$cancel_route_edits, {
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
    shiny::updateCheckboxInput(session, "insert_waypoint_mode", value = FALSE)
    session$sendCustomMessage("clear_route_edit", list())

    shiny::showNotification("Edit cancelled.", type = "message")
  })

  # Handler: Delete selected nodes (via lasso)
  shiny::observeEvent(input$delete_selected_nodes, {
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

    render_editable_route()
    session$sendCustomMessage("clear_node_selection", list())
    shiny::showNotification(sprintf("%d node(s) deleted.", length(to_delete)), type = "message")
  })

  # Handler: Clear node selection
  shiny::observeEvent(input$clear_node_selection, {
    session$sendCustomMessage("clear_node_selection", list())
    shiny::showNotification("Selection cleared.", type = "message")
  })
}
