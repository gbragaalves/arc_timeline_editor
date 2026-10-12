# ---- Server ----

# ---- OpenFreeMap base layer (MapLibre GL inside Leaflet; scripts loaded in ui.R) ----
# Fills the "OpenFreeMap" base group; switches to OSM if MapLibre is missing,
# WebGL fails, or the style doesn't load within 10 s. Shiny may run onRender
# before the CDN scripts finish loading, so wait for them first.
OPENFREEMAP_JS <- "
function(el, x) {
  var map = this;
  var lm = map.layerManager;
  var ofm = lm.getLayerGroup('OpenFreeMap', true);
  var done = false;
  function fallback(reason) {
    if (done) return;
    done = true;
    console.warn('[map] OpenFreeMap unavailable, using OSM:', reason);
    try { ofm.clearLayers(); } catch (e) {}  // a half-initialised GL layer may throw
    if (map.hasLayer(ofm)) {
      try { map.removeLayer(ofm); } catch (e) {}
      map.addLayer(lm.getLayerGroup('OSM', true));
    }
  }
  function start() {
    try {
      var gl = L.maplibreGL({
        style: 'https://tiles.openfreemap.org/styles/liberty',
        attribution: '<a href=\"https://openfreemap.org\" target=\"_blank\">OpenFreeMap</a> &copy; <a href=\"https://www.openmaptiles.org/\" target=\"_blank\">OpenMapTiles</a> Data from <a href=\"https://www.openstreetmap.org/copyright\" target=\"_blank\">OpenStreetMap</a>'
      });
      ofm.addLayer(gl);
      var glMap = gl.getMaplibreMap && gl.getMaplibreMap();
      if (!glMap) { fallback('no MapLibre map'); return; }
      glMap.on('load', function() { done = true; });
      glMap.on('error', function(e) { fallback(e && e.error ? e.error.message : 'style error'); });
    } catch (err) {
      fallback(err && err.message);
    }
  }
  var waited = 0;
  (function waitForMapLibre() {
    if (done) return;
    if (typeof L.maplibreGL === 'function' && typeof maplibregl !== 'undefined') return start();
    if (waited >= 8000) return fallback('MapLibre not loaded');
    waited += 100;
    setTimeout(waitForMapLibre, 100);
  })();
  setTimeout(function() { fallback('timeout'); }, 15000);
  // Translate the layers control / zoom titles now that they exist
  if (window.applyI18n) window.applyI18n(window.APP_LANG);
}
"
# MAP_RASTER_BASE (test-only): no OpenFreeMap layer, only the translations
MAP_I18N_JS <- "function(el, x) { if (window.applyI18n) window.applyI18n(window.APP_LANG); }"

# ---- Service status (top-bar dots), cached per R process ----
.service_status <- new.env(parent = emptyenv())

check_services <- function(force = FALSE) {
  if (!force && !is.null(.service_status$value)) return(.service_status$value)
  if (force) .icloud_state$available <- NULL  # re-run the rclone check

  safe <- function(expr) isTRUE(tryCatch(expr, error = function(e) FALSE))
  value <- list(
    osrm   = vapply(osrm_enabled_profiles(), function(p) safe(check_osrm_server(OSRM_SERVERS[[p]], p)), logical(1)),
    google = safe(check_google_api()),
    icloud = safe(icloud_available())
  )
  .service_status$value <- value
  value
}

server <- function(input, output, session) {

  # Helper: modal for OSRM offline with instructions

  show_osrm_offline_modal <- function(attempted_url) {
    if (osrm_maintenance_notify()) return(invisible())
    shiny::showModal(
      shiny::modalDialog(
        title = tr("OSRM server is not running"),
        shiny::tags$p(
          shiny::tags$strong(tr("The OSRM server did not respond at:")),
          shiny::tags$code(attempted_url)
        ),
        shiny::tags$hr(),
        shiny::tags$p(shiny::tags$strong(tr("To start OSRM, run in terminal:"))),
        shiny::tags$pre(
          style = "background: #f5f5f5; padding: 10px; border-radius: 4px; overflow-x: auto;",
          paste0(
            "# ", tr("If you use individual containers:"), "\ndocker start osrm-car osrm-foot osrm-bus osrm-official osrm-official-noturn\n\n",
            "# ", tr("Or if you use docker-compose:"), "\ncd path/to/osrm\ndocker-compose up -d"
          )
        ),
        shiny::tags$hr(),
        shiny::tags$p(shiny::tags$strong(tr("To check if it's running:"))),
        shiny::tags$pre(
          style = "background: #f5f5f5; padding: 10px; border-radius: 4px;",
          "docker ps | grep osrm"
        ),
        shiny::tags$hr(),
        shiny::tags$p(
          shiny::tags$strong(tr("Expected ports:")),
          shiny::tags$ul(lapply(osrm_enabled_profiles(), function(p) {
            label <- names(PROFILE_CHOICES)[match(p, PROFILE_CHOICES)]
            shiny::tags$li(if (is.na(label)) p else tr(label), ": ",
                           shiny::tags$code(sub("^https?://", "", OSRM_SERVERS[[p]])))
          }))
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
  edit_pins <- shiny::reactiveVal(NULL)    # Edit Samples control pins: data.frame(id, lon, lat, after)
  edit_pin_mode <- shiny::reactiveVal(FALSE)
  edit_excluded <- shiny::reactiveVal(character(0))  # sample ids kept out of the Snap routing (still placed)
  edit_hist <- new.env(parent = emptyenv())          # undo (Ctrl+Z) of pins and exclusions
  virtual_samples <- shiny::reactiveVal(list())  # Samples created from OSRM route (intermediate points)
  original_samples <- shiny::reactiveVal(list())  # Original samples before editing
  original_timeline_item_id <- shiny::reactiveVal(NULL)  # Original timelineItemId of samples

  # State for route editing
  route_edit_nodes <- shiny::reactiveVal(list())     # Editable route waypoints
  route_original_item <- shiny::reactiveVal(NULL)    # Original TimelineItem
  route_original_samples <- shiny::reactiveVal(list())  # Original route samples
  route_avg_speed <- shiny::reactiveVal(NULL)        # Average speed (m/s)

  # Jaé fare card boardings (JAE_CSV_PATH in config_local.R; empty if not set)
  jae_boardings <- read_jae_boardings()
  jae_used <- shiny::reactiveVal(character(0))  # jae_ids already turned into routes
  jae_day <- shiny::reactive(jae_on_date(jae_boardings, input$work_date))

  # ---- Language (PT | EN toggle in the top bar) ----
  # input$app_lang comes from the browser (remembered in localStorage);
  # APP_LANGUAGE (config_local.R, default "pt") until it arrives.
  current_lang <- shiny::reactive({
    lang <- input$app_lang
    if (is.character(lang) && length(lang) == 1 && lang %in% I18N_LANGS) lang else APP_LANGUAGE
  })

  # Translate a key into the current language (reactive: outputs re-render on toggle)
  tr <- function(key) tr_lang(key, current_lang())

  # Select choices can't be swapped client-side: re-label them, keeping values
  shiny::observeEvent(current_lang(), {
    lang <- current_lang()
    for (id in c("osrm_profile", "edit_osrm_profile")) {
      shiny::updateSelectInput(session, id, choices = tr_choices(profile_choices(), lang),
                               selected = shiny::isolate(input[[id]]) %||% "car")
    }
    for (id in c("manual_activity_type", "import_activity_type")) {
      shiny::updateSelectInput(session, id, choices = tr_choices(ACTIVITY_CHOICES, lang),
                               selected = shiny::isolate(input[[id]]) %||% "car")
    }
  }, ignoreInit = TRUE)

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


  # ---- Top bar: service status dots ----
  # First check is triggered by the browser after load; clicking re-checks.
  service_status <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$refresh_status, {
    service_status(check_services(force = isTRUE(input$refresh_status$force)))
  })

  # OSRM maintenance window (MAINTENANCE, see osrm_maintenance): polled every
  # 30 s; while it lasts (and when it ends) the OSRM servers are re-checked
  osrm_maint <- shiny::reactivePoll(30000, session, function() {
    f <- OSRM_MAINTENANCE_FILE
    if (file.exists(f)) paste(file.mtime(f), as.numeric(Sys.time()) %/% 30) else ""
  }, function() osrm_maintenance())
  shiny::observeEvent(osrm_maint(), {
    st <- shiny::isolate(service_status())
    if (is.null(st)) return()
    up <- function(p) isTRUE(tryCatch(check_osrm_server(OSRM_SERVERS[[p]], p), error = function(e) FALSE))
    st$osrm <- vapply(osrm_enabled_profiles(), up, logical(1))
    service_status(st)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # During maintenance an OSRM-dependent action shows a notice instead of an
  # error: TRUE when it did (the caller stops)
  osrm_maintenance_notify <- function() {
    m <- osrm_maintenance()
    if (is.null(m)) return(FALSE)
    shiny::showNotification(osrm_maintenance_notice(m, current_lang()), type = "warning", duration = 10)
    TRUE
  }
  session$userData$osrm_maintenance_notify <- osrm_maintenance_notify

  output$status_dots <- shiny::renderUI({
    st <- service_status()
    maint <- osrm_maint()
    dot <- function(ok, label, tip, warn = FALSE, maint = FALSE) {
      bslib::tooltip(
        shiny::span(class = paste("status-dot", if (isTRUE(maint)) "maint" else if (isTRUE(warn)) "warn" else if (isTRUE(ok)) "ok"),
                    shiny::span(class = "dot"), label),
        tip,
        placement = "bottom"
      )
    }

    if (is.null(st)) {
      checking <- tr("Checking...")
      return(shiny::tagList(
        dot(FALSE, "OSRM", checking), dot(FALSE, "Google", checking), dot(FALSE, "iCloud", checking)
      ))
    }

    profile_labels <- names(OSRM_PROFILE_LABELS)[match(names(st$osrm), OSRM_PROFILE_LABELS)]
    osrm_detail <- paste(
      sprintf("%s %s", vapply(profile_labels, tr, character(1)), ifelse(st$osrm, "✓", "✗")),
      collapse = ", "
    )
    refresh <- tr("Click to re-check.")
    # Data refresh (last_refresh.json): base date and my edits; orange when the last run FAILED
    rs <- osrm_refresh_status()
    data_txt <- osrm_refresh_text(rs, current_lang())
    # iCloud cleanup (tools/icloud_cleanup.R): reconnect, notes package waiting for Restore
    cs <- icloud_cleanup_status()
    cleanup_txt <- icloud_cleanup_text(cs, current_lang())
    maint_txt <- osrm_maintenance_text(maint, current_lang())
    shiny::tagList(
      dot(any(st$osrm), "OSRM",
          paste0(if (nzchar(maint_txt)) paste0(maint_txt, ". "), tr("OSRM local routing:"), " ", osrm_detail, ". ",
                 if (nzchar(data_txt)) paste0(data_txt, ". "), refresh),
          warn = osrm_refresh_failed(rs), maint = !is.null(maint)),
      dot(st$google, "Google",
          paste0(tr("Google Maps Directions API (metro/train/tram):"), " ",
                 if (st$google) tr("key OK") else tr("key missing or invalid"), ". ", refresh)),
      dot(st$icloud, "iCloud",
          paste0(sprintf(tr("iCloud Drive via rclone (remote '%s'):"), ICLOUD_REMOTE), " ",
                 if (st$icloud) tr("available") else tr("not available"), ". ",
                 if (nzchar(cleanup_txt)) paste0(cleanup_txt, if (endsWith(cleanup_txt, ":")) " " else ". "), refresh),
          warn = icloud_cleanup_warn(cs))
    )
  })

  # ---- Base map ----
  # OpenFreeMap (vector, via MapLibre GL) is the default base layer; its group
  # is filled client-side by OPENFREEMAP_JS, which falls back to OSM on failure.
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
        group = "Satellite",
        options = leaflet::providerTileOptions(maxZoom = 19)
      ) |>
      leaflet::addTiles(
        urlTemplate = paste0("https://tile.tracestrack.com/_/{z}/{x}/{y}.webp?key=", Sys.getenv("TRACESTRACK_KEY")),
        group = "Topo",
        options = leaflet::tileOptions(maxZoom = 19)
      ) |>
      # First base group is the one shown by default
      leaflet::addLayersControl(
        baseGroups = c(if (!MAP_RASTER_BASE) "OpenFreeMap", "OSM", "CartoDB", i18n_key("Satellite"), "Topo"),  # labels translated client-side
        overlayGroups = c("Bus infrastructure", "Router network"),  # filled client-side when switched on
        options = leaflet::layersControlOptions(collapsed = TRUE)
      ) |>
      leaflet::hideGroup(c("Bus infrastructure", "Router network")) |>
      leaflet::setView(lng = -43.2, lat = -22.9, zoom = 11) |>
      htmlwidgets::onRender(if (MAP_RASTER_BASE) MAP_I18N_JS else OPENFREEMAP_JS)
  })

  # ---- Map overlays: bus infrastructure (by bounds), router network (OSRM tiles) ----
  shiny::observeEvent(input$bus_infra_req, {
    r <- input$bus_infra_req
    feats <- tryCatch(bus_infra_in_bounds(as.numeric(r$w), as.numeric(r$s), as.numeric(r$e), as.numeric(r$n), as.numeric(r$zoom)),
                      error = function(e) list())
    session$sendCustomMessage("bus_infra_data", list(features = feats))
  })
  # The router network follows the profile of the selected trip (Batch review),
  # the Snap profile (Edit Samples) or the car default
  shiny::observe({
    sel <- input$review_profile
    snap <- input$edit_osrm_profile
    md <- input$mode
    profile <- if (identical(md, "BatchReview") && isTRUE(sel %in% c("car", "official", "bus"))) {
      sel
    } else if (identical(md, "EditSamples") && isTRUE(snap %in% c("car", "official", "bus"))) {
      snap
    } else {
      ROAD_PROFILE_FOR_CAR
    }
    session$sendCustomMessage("router_profile", list(profile = profile, url = router_tile_url(profile)))
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

    # Edit Samples uses draggable markers; pins come from dragging the route (not clicks)
    if (current_mode == "EditSamples") return()

    # Ignore clicks in Import File mode (uses uploaded file)
    if (current_mode == "Import") return()

    # Batch review uses its own markers (right-click on the samples)
    if (current_mode == "BatchReview") return()

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

  # ---- Jaé boardings: map layer, list in the Route pane, start a route ----

  # Markers for the work date in their own group (only cleared here);
  # boardings already turned into routes are grey
  shiny::observe({
    day <- jae_day()
    used <- jae_used()
    lang <- current_lang()
    proxy <- leaflet::leafletProxy("map") %>% leaflet::clearGroup("jae")
    day <- day[!is.na(day$latitude) & !is.na(day$longitude), , drop = FALSE]
    if (nrow(day) == 0) return()
    fill <- ifelse(day$jae_id %in% used, JAE_USED_COLOR, unname(JAE_MODE_COLORS[day$modo]))
    proxy %>%
      leaflet::addCircleMarkers(
        lng = day$longitude, lat = day$latitude,
        radius = 7, weight = 2, color = "white", opacity = 1,
        fillColor = ifelse(is.na(fill), JAE_USED_COLOR, fill), fillOpacity = 0.95,
        popup = jae_popup(day, lang),
        label = jae_local_time(day$datetime_utc),
        group = "jae",
        # A click opens the popup without adding a route point
        options = leaflet::pathOptions(bubblingMouseEvents = FALSE)
      )
  })

  jae_mode_name <- function(modo) {
    switch(modo, "Ônibus" = tr("Bus"), "Metrô" = tr("Metro"), modo)
  }

  output$jae_list <- shiny::renderUI({
    day <- jae_day()
    if (nrow(day) == 0) return(NULL)
    used <- jae_used()
    rows <- lapply(seq_len(nrow(day)), function(i) {
      b <- day[i, ]
      is_used <- b$jae_id %in% used
      has_xy <- !is.na(b$latitude) && !is.na(b$longitude)
      col <- unname(JAE_MODE_COLORS[b$modo])
      shiny::div(
        class = paste("jae-row", if (is_used) "jae-used"),
        shiny::span(class = "jae-time", jae_local_time(b$datetime_utc)),
        shiny::span(
          class = "jae-head",
          shiny::span(class = "jae-mode", style = sprintf("background: %s;", if (is.na(col)) JAE_USED_COLOR else col),
                      jae_mode_name(b$modo)),
          if (!has_xy) shiny::span(class = "jae-noloc", tr("no location"))
        ),
        shiny::tags$button(
          type = "button", class = "btn btn-outline-primary jae-start", `data-jae-start` = b$jae_id,
          disabled = if (is_used) NA,
          if (is_used) paste("✓", tr("Route added")) else tr("Start route here")
        ),
        shiny::span(class = "jae-label", b$label)
      )
    })
    shiny::div(
      class = "pane-section jae-box",
      shiny::div(class = "pane-title", title = tr("Fare card tap-ins on this date. A route started here takes the boarding's place, time, profile and description."),
                 tr("Jaé boardings")),
      shiny::div(class = "jae-list", rows)
    )
  })

  shiny::observeEvent(input$jae_start, {
    b <- jae_boardings[jae_boardings$jae_id == input$jae_start, , drop = FALSE]
    if (nrow(b) != 1 || b$jae_id %in% jae_used()) return()

    # The hidden mode radio and the Create segment are synced client-side
    if (!identical(input$mode, "OSRM")) session$sendCustomMessage("jae_route_mode", list())

    if (!is.na(b$latitude) && !is.na(b$longitude)) {
      temp_points(data.frame(lat = b$latitude, lng = b$longitude))
      update_map_points(auto_zoom = TRUE)
    } else {
      # No coordinates: the next map click becomes the first point
      temp_points(data.frame(lat = numeric(0), lng = numeric(0)))
      update_map_points(auto_zoom = FALSE)
      shiny::showNotification(tr("Click the boarding location on the map"), type = "message")
    }
    if (!is.na(b$profile)) shiny::updateSelectInput(session, "osrm_profile", selected = b$profile)
    session$userData$jae_pending <- b
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
        leaflet::addPopups(loc$lon, loc$lat, paste0(tr("Place"), ": ", label), layerId = "visit_popup")
    } else {
      leaflet::leafletProxy("map") %>% leaflet::removePopup("visit_popup")
    }
  })

  # ---- Add visit ----

  shiny::observeEvent(input$add_visit, {
    pts <- temp_points()
    if (nrow(pts) != 1) {
      shiny::showNotification(tr("Select exactly 1 point on the map for the visit."), type = "error")
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
      shiny::showNotification(tr("Invalid time for the visit."), type = "error")
      return()
    }

    # convert visit interval to UTC to check overlap
    start_utc <- lubridate::with_tz(val$start, "UTC")
    end_utc    <- lubridate::with_tz(val$end,    "UTC")

    if (has_overlap(timeline(), start_utc, end_utc)) {
      shiny::showNotification(tr("This interval overlaps an existing activity."), type = "error")
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
    visit_label <- item$.place$name %||% item$visit$customTitle %||% tr("Visit")
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

    shiny::showNotification(tr("Visit added."), type = "message")
  })

  # ---- Manual route ----

  shiny::observeEvent(input$add_manual, {
    pts <- temp_points()
    if (nrow(pts) < 2) {
      shiny::showNotification(tr("Set at least 2 points on the map for the manual route."), type = "error")
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
      shiny::showNotification(tr("Invalid time for the manual route."), type = "error")
      return()
    }

    # Generate equally spaced UTC timestamps
    start_utc <- lubridate::with_tz(val$start, "UTC")
    end_utc    <- lubridate::with_tz(val$end,    "UTC")

    if (has_overlap(timeline(), start_utc, end_utc)) {
      shiny::showNotification(tr("This interval overlaps an existing activity."), type = "error")
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
      description = sprintf(tr("Manual route (%.1f km)"),
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

    shiny::showNotification(tr("Manual route added."), type = "message")
  })

  # ---- OSRM: calculate route ----

  shiny::observeEvent(input$calculate_osrm, {
    pts <- temp_points()
    if (nrow(pts) < 2) {
      shiny::showNotification(tr("Set at least 2 points on the map for the OSRM route."), type = "error")
      return()
    }

    profile <- input$osrm_profile

    # Suggest time based on the last activity (if any); also the reference
    # time for transit routing
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
    # Route started from a Jaé boarding: its tap-in time
    jae <- session$userData$jae_pending
    if (!is.null(jae)) suggested_start <- lubridate::with_tz(jae$datetime_utc, tzone = Sys.timezone())

    # Transit profiles use Google Maps API instead of OSRM
    if (profile %in% GOOGLE_TRANSIT_MODES) {
      if (!check_google_api()) {
        shiny::showNotification(
          tr("Google Maps API key not configured or invalid. Set GOOGLE_MAPS_API_KEY in your .Renviron."),
          type = "error", duration = 8
        )
        return()
      }
      transit_mode <- GOOGLE_TRANSIT_MODE_MAP[[profile]]
      route <- calculate_google_transit_route(pts, transit_mode = transit_mode,
                                              ref_time_utc = lubridate::with_tz(suggested_start, "UTC"))
      if (is.null(route)) {
        shiny::showNotification(
          sprintf(tr("No %s route found between these points. The API did not return routes with this transport mode."), profile),
          type = "error", duration = 8
        )
        return()
      }
      # Apply same smoothing as OSRM
      route$coords <- smooth_osrm_route(route$coords, tolerance_m = 5, angle_min = 2)
    } else {
      base_url <- OSRM_SERVERS[[profile]]

      if (is.null(base_url) || !check_osrm_server(base_url)) {
        show_osrm_offline_modal(base_url %||% tr("not configured"))
        return()
      }

      route <- calculate_osrm_route(pts, profile = profile)
      if (is.null(route)) {
        if (!osrm_maintenance_notify()) shiny::showNotification(tr("Failed to calculate OSRM route."), type = "error")
        return()
      }
    }

    suggested_end <- suggested_start + route$duration_s
    # as.Date() gives a POSIXct's UTC date: a boarding after 21:00 local needs its
    # local date (other routes keep the UTC default)
    date_tz <- if (!is.null(jae)) Sys.timezone() else "UTC"


    shiny::showModal(
      shiny::modalDialog(
        title = tr("OSRM route times"),
        date_input("osrm_date_start", tr("Start date"), as.Date(suggested_start, tz = date_tz), lang = current_lang()),
        shiny::textInput("osrm_time_start", tr("Start time"), format(suggested_start, "%H:%M:%S")),
        date_input("osrm_date_end", tr("End date"), as.Date(suggested_end, tz = date_tz), lang = current_lang()),
        shiny::textInput("osrm_time_end", tr("End time"), format(suggested_end, "%H:%M:%S")),
        footer = shiny::tagList(
          shiny::modalButton(tr("Cancel")),
          shiny::actionButton("recalculate_osrm_end", tr("Recalculate arrival")),
          shiny::actionButton("confirm_osrm_route", tr("Confirm"), class = "btn-primary")
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
      shiny::showNotification(tr("No OSRM route calculated to recalculate the arrival time."), type = "error")
      return()
    }

    coords <- route$coords
    if (is.data.frame(coords)) coords <- as.matrix(coords)
    if (!is.matrix(coords) || nrow(coords) < 1 || ncol(coords) < 2) {
      shiny::showNotification(tr("OSRM route coordinates are in an unexpected format."), type = "error")
      return()
    }

    lat1 <- as.numeric(coords[1, 2])
    lng1 <- as.numeric(coords[1, 1])
    if (is.na(lat1) || is.na(lng1)) {
      shiny::showNotification(tr("First point of the OSRM route is invalid."), type = "error")
      return()
    }

    tz <- tz_from_coords(lat1, lng1)

    # departure time (format again to be safe)
    start_time_fmt <- format_time(input$osrm_time_start)
    if (is.na(start_time_fmt)) {
      shiny::showNotification(tr("Invalid departure time."), type = "error")
      return()
    }

    start_local <- suppressWarnings(
      lubridate::ymd_hms(
        paste(input$osrm_date_start, start_time_fmt),
        tz = tz
      )
    )
    if (is.na(start_local)) {
      shiny::showNotification(tr("Could not parse the departure date/time."), type = "error")
      return()
    }

    # duration in seconds from OSRM
    dur_s <- as.numeric(route$duration_s)
    if (is.na(dur_s)) {
      shiny::showNotification(tr("Invalid OSRM route duration."), type = "error")
      return()
    }

    end_local <- start_local + dur_s

    # Local date for a route started from a Jaé boarding (see calculate_osrm)
    date_tz <- if (!is.null(session$userData$jae_pending)) tz else "UTC"
    shiny::updateDateInput(session, "osrm_date_end", value = as.Date(end_local, tz = date_tz))
    shiny::updateTextInput(session, "osrm_time_end", value = format(end_local, "%H:%M:%S"))
  })


  shiny::observeEvent(input$confirm_osrm_route, {
    tryCatch(
      {
        shiny::removeModal()

        route <- session$userData$last_osrm_route
        if (is.null(route)) {
          shiny::showNotification(tr("No OSRM route calculated."), type = "error")
          return()
        }

        coords <- route$coords
        if (is.data.frame(coords)) {
          coords <- as.matrix(coords)
        }
        if (!is.matrix(coords) || ncol(coords) < 2) {
          shiny::showNotification(tr("OSRM route coordinates are in an unexpected format."), type = "error")
          return()
        }
        coords <- suppressWarnings(apply(coords, 2, as.numeric))
        if (!is.matrix(coords)) {
          coords <- matrix(coords, ncol = 2)
        }

        n0 <- nrow(coords)
        if (is.null(n0) || is.na(n0) || n0 < 2) {
          shiny::showNotification(tr("Invalid OSRM route (too few points)."), type = "error")
          return()
        }

        lat1 <- coords[1, 2]
        lng1 <- coords[1, 1]

        if (is.na(lat1) || is.na(lng1)) {
          shiny::showNotification(tr("First point of the OSRM route is invalid."), type = "error")
          return()
        }

        tz <- tz_from_coords(lat1, lng1)

        start_time_fmt <- format_time(input$osrm_time_start)
        end_time_fmt <- format_time(input$osrm_time_end)

        if (is.na(start_time_fmt) || is.na(end_time_fmt)) {
          shiny::showNotification(tr("Invalid times for OSRM route."), type = "error")
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
          shiny::showNotification(tr("Invalid time for OSRM route."), type = "error")
          return()
        }

        if (!inherits(val$start, "POSIXt") || !inherits(val$end, "POSIXt")) {
          shiny::showNotification(tr("OSRM route time interval could not be parsed."), type = "error")
          return()
        }

        start_utc <- lubridate::with_tz(val$start, "UTC")
        end_utc    <- lubridate::with_tz(val$end,    "UTC")

        if (is.na(start_utc) || is.na(end_utc) || end_utc <= start_utc) {
          shiny::showNotification(tr("Invalid OSRM route interval (end before or equal to start)."), type = "error")
          return()
        }

        if (has_overlap(timeline(), start_utc, end_utc)) {
          shiny::showNotification(tr("This interval overlaps an existing activity."), type = "error")
          return()
        }

        n_target <- max(n0, 100L)
        ts_seq <- seq(from = start_utc, to = end_utc, length.out = n_target)
        if (any(is.na(ts_seq))) {
          shiny::showNotification(tr("Failed to generate time sequence for OSRM route."), type = "error")
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
          shiny::showNotification(tr("Failed to interpolate OSRM route coordinates."), type = "error")
          return()
        }

        # Map OSRM profile to activityType
        profile <- session$userData$last_route_profile %||% "car"
        activity_map <- c(
          car   = "car",
          foot  = "walking",
          bike  = "cycling",
          bus   = "bus",
          official = "car",
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

        # Route started from a Jaé boarding: its label is the description
        jae <- session$userData$jae_pending

        item <- create_timeline_item_path(
          timestamps_utc = ts_seq,
          coords         = coords_new,
          sample_ids     = sample_ids,
          type           = "osrm_route",
          description    = if (!is.null(jae)) jae$label else sprintf(tr("OSRM route (%s)"), profile),
          activity_type  = activity_type
        )

        tl <- timeline()
        tl[[length(tl) + 1L]] <- item
        timeline(tl)

        s <- all_samples()
        all_samples(c(s, new_samples))

        if (!is.null(jae)) {
          jae_used(unique(c(jae_used(), jae$jae_id)))
          session$userData$jae_pending <- NULL
        }

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

        shiny::showNotification(tr("OSRM route added."), type = "message")
      },
      error = function(e) {
        shiny::showNotification(
          paste(tr("Error confirming OSRM route:"), e$message),
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
          shiny::showNotification(tr("Times filled from KML file (UTC -> system local time)."), type = "message")
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
            group = "imported", label = tr("Start")
          ) %>%
          leaflet::addCircleMarkers(
            lng = coords[nrow(coords), 1], lat = coords[nrow(coords), 2],
            radius = 8, color = "red", fillColor = "red", fillOpacity = 1,
            group = "imported", label = tr("End")
          ) %>%
          leaflet::fitBounds(
            lng1 = min(coords[, 1]), lat1 = min(coords[, 2]),
            lng2 = max(coords[, 1]), lat2 = max(coords[, 2])
          )

        shiny::showNotification(tr("Enriched KML (FR24) loaded. Green = start, Red = end."), type = "message")
        return()
      }
    }

    # Normal case: read with sf
    geo <- tryCatch(
      sf::st_read(file_path, quiet = TRUE),
      error = function(e) NULL
    )
    if (is.null(geo)) {
      shiny::showNotification(tr("Could not read file as geographic layer."), type = "error")
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
        group = "imported", label = tr("Start")
      ) %>%
      leaflet::addCircleMarkers(
        lng = pt_end["X"], lat = pt_end["Y"],
        radius = 8, color = "red", fillColor = "red", fillOpacity = 1,
        group = "imported", label = tr("End")
      ) %>%
      leaflet::fitBounds(
        lng1 = bbox["xmin"], lat1 = bbox["ymin"],
        lng2 = bbox["xmax"], lat2 = bbox["ymax"]
      )

    shiny::showNotification(tr("Geographic file imported. Green = start, Red = end."), type = "message")
  })

  shiny::observeEvent(input$file_direction, {
    geo <- imported_geometry()
    if (is.null(geo)) return()

    leaflet::leafletProxy("map") %>% leaflet::clearGroup("imported")

    if (isTRUE(fr24_enriched())) {
      coords <- cbind(geo$lon, geo$lat)
      if (input$file_direction == "reverse") {
        coords <- coords[rev(seq_len(nrow(coords))), , drop = FALSE]
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
          group = "imported", label = tr("Start")
        ) %>%
        leaflet::addCircleMarkers(
          lng = coords[nrow(coords), 1], lat = coords[nrow(coords), 2],
          radius = 8, color = "red", fillColor = "red", fillOpacity = 1,
          group = "imported", label = tr("End")
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
          group = "imported", label = tr("Start")
        ) %>%
        leaflet::addCircleMarkers(
          lng = pt_end["X"], lat = pt_end["Y"],
          radius = 8, color = "red", fillColor = "red", fillOpacity = 1,
          group = "imported", label = tr("End")
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
      shiny::showNotification(tr("No file imported."), type = "error")
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
            paste(tr("Error processing FR24 track:"), e$message),
            type = "error",
            duration = NULL
          )
          return(NULL)
        }
      )

      if (is.null(traj)) return()

      ts_utc <- traj$ts_utc
      coords <- traj$coords

      distance_km <- track_distance_km(coords)

      if (distance_km <= 0 || distance_km > 30000) {
        shiny::showNotification(
          sprintf(
            tr("Absurd distance detected for flight (%.1f km). Something is wrong with the track."),
            distance_km
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
        description    = sprintf(tr("FR24 flight (%.1f km)"), distance_km),
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

      # Arc keeps its own recording of the flight: replacing it goes through GPX
      hint <- if (icloud_available()) {
        tr("Flight added. To replace the Arc data: Export → Send timeline GPX to iCloud, then in Arc use Import GPX File.")
      } else {
        tr("Flight added. To replace the Arc data: Export → Download timeline GPX, then in Arc use Import GPX File.")
      }
      shiny::showNotification(hint, type = "message", duration = 15)
      return()
    }


    # Default case (sf): timestamps come from the form (local time)
    if (!inherits(geo, "sf")) {
      shiny::showNotification(tr("Invalid imported geometry."), type = "error")
      return()
    }

    coords_list <- sf::st_coordinates(geo)
    if (nrow(coords_list) < 2) {
      shiny::showNotification(tr("Geometry too short for route."), type = "error")
      return()
    }

    if (input$file_direction == "reverse") {
      coords_list <- coords_list[rev(seq_len(nrow(coords_list))), , drop = FALSE]
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
      shiny::showNotification(tr("Invalid time for imported route."), type = "error")
      return()
    }

    start_utc <- lubridate::with_tz(val$start, "UTC")
    end_utc    <- lubridate::with_tz(val$end,    "UTC")

    if (has_overlap(timeline(), start_utc, end_utc)) {
      shiny::showNotification(tr("This interval overlaps an existing activity."), type = "error")
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
      description = tr("Route imported from file"),
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

    shiny::showNotification(tr("File route added."), type = "message")
  })

  # ---- Timeline UI ----

  # Display labels (translation keys) for timeline item types
  ITEM_TYPE_LABELS <- c(
    visit = i18n_key("visit"), route = i18n_key("route"),
    manual_route = i18n_key("manual route"), osrm_route = i18n_key("OSRM route"),
    imported_route = i18n_key("imported route"), voo_fr24 = i18n_key("FR24 flight")
  )

  output$timeline_list <- shiny::renderUI({
    tl <- timeline()
    if (length(tl) == 0) {
      return(shiny::tags$p(tr("No items in the timeline yet.")))
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

        start_str <- fmt_datetime(ts_start_loc, current_lang())
        end_str <- fmt_datetime(ts_end_loc, current_lang())
        type_key <- unname(ITEM_TYPE_LABELS[item_type])
        type_label <- if (is.na(type_key)) item_type else tr(type_key)

        shiny::div(
          class = "card mb-2 p-2",
          shiny::strong(sprintf("[%s] %s", type_label, desc)),
          shiny::br(),
          shiny::tags$small(paste0(start_str, " -> ", end_str)),
          shiny::br(),
          shiny::actionButton(
            paste0("edit_", it$.internalId),
            tr("Edit"),
            class = "btn-sm btn-warning"
          ),
          shiny::actionButton(
            paste0("delete_", it$.internalId),
            tr("Delete"),
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

          shiny::showNotification(tr("Item removed from timeline."), type = "message")
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
            title = tr("Edit item"),
            date_input("edit_date_start", tr("Start date"), as.Date(ts_local), lang = current_lang()),
            shiny::textInput("edit_time_start", tr("Start time"), format(ts_local, "%H:%M")),
            date_input("edit_date_end", tr("End date"), as.Date(tsf_local), lang = current_lang()),
            shiny::textInput("edit_time_end", tr("End time"), format(tsf_local, "%H:%M")),
            footer = shiny::tagList(
              shiny::modalButton(tr("Cancel")),
              shiny::actionButton("save_edit", tr("Save"), class = "btn-primary")
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
      shiny::showNotification(tr("Invalid times."), type = "error")
      return()
    }

    new_start_utc <- local_start - start_sec
    new_end_utc <- local_end  - end_sec

    if (new_end_utc <= new_start_utc) {
      shiny::showNotification(tr("End must be after start."), type = "error")
      return()
    }

    if (has_overlap(tl, new_start_utc, new_end_utc, ignore_id = id)) {
      shiny::showNotification(tr("This interval overlaps an existing activity."), type = "error")
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
    shiny::showNotification(tr("Item updated."), type = "message")
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
  # ---- Edit Samples: control pins (Snap routes through them) ----
  set_edit_pin_mode <- function(on) {
    edit_pin_mode(isTRUE(on))
    session$sendCustomMessage("pin_mode", list(on = isTRUE(on), scope = "edit"))
  }
  # Pin and exclusion changes, with an undo history (Ctrl+Z). They apply at the
  # next Snap (which rewrites the samples, so it isn't re-run by itself).
  set_edit_route <- function(pins = shiny::isolate(edit_pins()), excl = shiny::isolate(edit_excluded()), record = TRUE) {
    if (record) {
      h <- edit_hist$stack %||% list()
      h[[length(h) + 1L]] <- list(pins = shiny::isolate(edit_pins()), excl = shiny::isolate(edit_excluded()))
      edit_hist$stack <- utils::tail(h, 30)
    }
    edit_pins(if (is.null(pins) || nrow(pins) == 0) NULL else pins)
    edit_excluded(unique(as.character(excl)))
  }
  shiny::observeEvent(input$edit_pin, {
    if (identical(input$edit_pin$action, "toggle")) {
      set_edit_pin_mode(!isTRUE(edit_pin_mode()))
      if (isTRUE(edit_pin_mode())) {
        shiny::showNotification(tr("Route editing: drag the line to add a via pin; Snap routes through the pins (P to leave)."),
                                duration = 5)
      }
    } else if (identical(input$edit_pin$action, "clear")) {
      set_edit_pin_mode(FALSE)
      set_edit_route(pins = NULL)
    } else if (identical(input$edit_pin$action, "exclude")) {
      selected <- input$selected_samples
      if (length(selected) == 0) {
        shiny::showNotification(tr("No samples selected."), type = "warning")
        return()
      }
      set_edit_route(excl = c(shiny::isolate(edit_excluded()), unlist(selected)))
      session$sendCustomMessage("clear_selection", list())
      shiny::showNotification(sprintf(tr("%d samples kept out of the Snap routing (still placed on the route)."),
                                      length(selected)), duration = 5)
    } else if (identical(input$edit_pin$action, "clear_excl")) {
      set_edit_route(excl = character(0))
    }
  })
  shiny::observeEvent(input$edit_route_drag, {
    ev <- input$edit_route_drag
    if (!identical(input$mode, "EditSamples") || !isTRUE(edit_pin_mode()) || !identical(ev$kind, "line")) return()
    pins <- edit_pins() %||% data.frame(id = character(0), lon = numeric(0), lat = numeric(0), after = character(0))
    new_id <- as.character(max(c(0L, suppressWarnings(as.integer(pins$id))), na.rm = TRUE) + 1L)
    set_edit_route(pins = rbind(pins, data.frame(id = new_id, lon = as.numeric(ev$lng), lat = as.numeric(ev$lat),
                                                 after = NA_character_)))
  })
  shiny::observeEvent(input$edit_pin_moved, {
    pins <- edit_pins()
    k <- if (is.null(pins)) NA else match(as.character(input$edit_pin_moved$id), pins$id)
    if (is.na(k)) return()
    pins$lon[k] <- as.numeric(input$edit_pin_moved$lng)
    pins$lat[k] <- as.numeric(input$edit_pin_moved$lat)
    set_edit_route(pins = pins)
  })
  shiny::observeEvent(input$edit_pin_remove, {
    pins <- edit_pins()
    k <- if (is.null(pins)) NA else match(as.character(input$edit_pin_remove$id), pins$id)
    if (is.na(k)) return()
    set_edit_route(pins = pins[-k, , drop = FALSE])
  })
  shiny::observeEvent(input$route_undo, {
    if (!identical(input$route_undo$scope, "edit") || !identical(input$mode, "EditSamples")) return()
    h <- edit_hist$stack %||% list()
    if (!length(h)) {
      shiny::showNotification(tr("Nothing to undo."), duration = 3)
      return()
    }
    last <- h[[length(h)]]
    edit_hist$stack <- h[-length(h)]
    set_edit_route(pins = last$pins, excl = last$excl, record = FALSE)
  })
  # Excluded samples look grey with an outline; the route handles follow the pins
  shiny::observe({
    excl <- edit_excluded()
    input$mode
    session$sendCustomMessage("edit_excluded", list(ids = as.list(excl)))
  })
  shiny::observe({
    pins <- edit_pins()
    on <- edit_pin_mode()
    route <- osrm_route()
    if (!identical(input$mode, "EditSamples") || !isTRUE(on)) {
      session$sendCustomMessage("route_edit", list(scope = "edit", on = FALSE))
      return()
    }
    s <- tryCatch(shiny::isolate(edit_samples_df()), error = function(e) NULL)
    if (is.null(s) || nrow(s) < 2) return()
    line <- if (!is.null(route) && nrow(route) >= 2) route[, 1:2, drop = FALSE] else cbind(s$lon, s$lat)
    op <- if (!is.null(pins) && nrow(pins)) order_pins(s, pins) else NULL
    ctrl <- c(list(list(id = "start", lat = s$lat[1], lng = s$lon[1])),
              lapply(seq_len(nrow(op %||% data.frame())), function(k) list(id = as.character(op$id[k]), lat = op$lat[k], lng = op$lon[k])),
              list(list(id = "end", lat = s$lat[nrow(s)], lng = s$lon[nrow(s)])))
    session$sendCustomMessage("route_edit", list(scope = "edit", on = TRUE, fixed_ends = TRUE, ctrl = ctrl,
                                                 path = lapply(seq_len(nrow(line)), function(k) c(line[k, 2], line[k, 1]))))
  })
  # Leaving Edit Samples leaves pin mode (Batch review has its own)
  shiny::observeEvent(input$mode, {
    if (!identical(input$mode, "EditSamples") && isTRUE(edit_pin_mode())) set_edit_pin_mode(FALSE)
  })
  # Samples (lon, lat, t) of the loaded, not ignored, samples, by time
  edit_samples_df <- function() {
    smp <- shiny::isolate(edit_samples())
    ids <- vapply(seq_along(smp), function(j) as.character(unlist(smp[[j]]$id %||% paste0("sample_", j)))[1], "")
    smp <- smp[!ids %in% shiny::isolate(ignored_samples())]
    if (length(smp) == 0) return(NULL)
    num <- function(v) as.numeric(unlist(v))[1]
    d <- data.frame(lon = vapply(smp, function(x) num(x$longitude), 0), lat = vapply(smp, function(x) num(x$latitude), 0),
                    t = vapply(smp, function(x) as.numeric(parse_timestamp_utc(as.character(unlist(x$date))[1])), 0))
    d[order(d$t), ]
  }
  shiny::observe({
    pins <- edit_pins()
    mode <- input$mode
    proxy <- leaflet::clearGroup(leaflet::leafletProxy("map"), "edit_pins")
    if (!identical(mode, "EditSamples") || is.null(pins) || nrow(pins) == 0) return()
    s <- tryCatch(edit_samples_df(), error = function(e) NULL)
    ord <- if (!is.null(s) && nrow(s) >= 2) order_pins(s, pins)$id else pins$id
    leaflet::addMarkers(
      proxy, lng = pins$lon, lat = pins$lat, layerId = paste0("pin_", pins$id), group = "edit_pins",
      label = as.character(match(pins$id, ord)),
      labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "top", offset = c(-15, -18), className = "pin-label"),
      options = leaflet::markerOptions(draggable = TRUE)
    )
  })

  shiny::observeEvent(input$load_samples, {
    edit_pins(NULL)
    start_time_fmt <- format_time(input$edit_samples_time_start)
    end_time_fmt <- format_time(input$edit_samples_time_end)

    if (is.na(start_time_fmt) || is.na(end_time_fmt)) {
      shiny::showNotification(tr("Invalid times."), type = "error")
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
      shiny::showNotification(tr("Invalid date/time."), type = "error")
      return()
    }

    start_utc <- lubridate::with_tz(start_local, "UTC")
    end_utc <- lubridate::with_tz(end_local, "UTC")

    # Refresh the weekly files from iCloud first; on failure keep going with
    # whatever is already on disk
    if (isTRUE(input$fetch_icloud) && icloud_available()) {
      fetch <- shiny::withProgress(message = tr("Fetching from iCloud..."), value = 0.3, {
        icloud_fetch_weeks(weeks_for_interval(start_utc, end_utc), lang = current_lang())
      })
      if (isTRUE(fetch$ok)) {
        shiny::showNotification(fetch$message, type = "message")
      } else {
        shiny::showNotification(paste(tr("iCloud fetch failed, using local files."), fetch$message),
                                type = "warning", duration = 10)
      }
    }

    samples_raw <- load_samples_interval(start_utc, end_utc)

    if (length(samples_raw) == 0) {
      shiny::showNotification(tr("No samples found in the interval."), type = "warning")
      return()
    }

    # Filter samples with valid location
    samples <- Filter(function(s) {
      sample_has_coords(s)
    }, samples_raw)

    n_invalid <- length(samples_raw) - length(samples)
    if (n_invalid > 0) {
      shiny::showNotification(
        sprintf(tr("%d samples skipped (no valid coordinates)."), n_invalid),
        type = "warning"
      )
    }

    if (length(samples) == 0) {
      shiny::showNotification(tr("No samples with valid coordinates."), type = "error")
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
    bslib::accordion_panel_open("edit_samples_steps", "clean")  # next step

    shiny::showNotification(
      sprintf(tr("%d samples loaded."), length(samples)),
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
      shiny::showNotification(tr("1/7 Starting..."), id = "snap_progress", duration = NULL, type = "message")

      samples <- edit_samples()
      ignored <- ignored_samples()

      if (length(samples) == 0) {
        shiny::removeNotification("snap_progress")
        shiny::showNotification(tr("No samples loaded. Click 'Load' first."), type = "error")
        return()
      }

      shiny::showNotification(sprintf(tr("2/7 Filtering %d samples..."), length(samples)), id = "snap_progress", duration = NULL, type = "message")

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
        shiny::showNotification(tr("Load at least 2 samples (not ignored) for snap-to-road."), type = "error")
        return()
      }

      profile <- input$edit_osrm_profile
      uses_google <- profile %in% GOOGLE_TRANSIT_MODES

      if (uses_google) {
        if (!check_google_api()) {
          shiny::removeNotification("snap_progress")
          shiny::showNotification(
            tr("Google Maps API key not configured or invalid. Set GOOGLE_MAPS_API_KEY in your .Renviron."),
            type = "error", duration = 8
          )
          return()
        }
      } else {
        base_url <- OSRM_SERVERS[[profile]]

        if (is.null(base_url)) {
          shiny::removeNotification("snap_progress")
          shiny::showNotification(sprintf(tr("OSRM profile '%s' not configured."), profile), type = "error")
          return()
        }

        shiny::showNotification(tr("3/7 Checking OSRM..."), id = "snap_progress", duration = NULL, type = "message")

        if (!check_osrm_server(base_url)) {
          shiny::removeNotification("snap_progress")
          show_osrm_offline_modal(base_url)
          return()
        }
      }

      shiny::showNotification(tr("4/7 Extracting coordinates..."), id = "snap_progress", duration = NULL, type = "message")

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

      # Limit to 100 waypoints (chunking if necessary).
      # Google transit routes origin -> destination only, so no cap there
      # (it would cut off the real destination).
      n <- length(active_samples)
      if (uses_google && n > 2) {
        shiny::showNotification(
          sprintf(tr("Google transit: only the first and last active samples are used (%d intermediate ignored)."), n - 2),
          type = "warning", duration = 8
        )
      }
      # Control pins (OSRM only): route start -> pins -> end, samples placed by projection.
      # Samples excluded by lasso aren't route waypoints; they are placed by projection too.
      pins_now <- edit_pins()
      use_pins <- !uses_google && !is.null(pins_now) && nrow(pins_now) > 0
      if (n > 100 && !uses_google && !use_pins) {
        shiny::showNotification(
          sprintf(tr("Too many samples (%d). Using only the first 100."), n),
          type = "warning"
        )
        lats <- lats[1:100]
        lngs <- lngs[1:100]
        active_samples <- active_samples[1:100]
        n <- 100
      }

      shiny::showNotification(sprintf(tr("5/7 Calculating route with %d points..."), n), id = "snap_progress", duration = NULL, type = "message")

      excl_now <- !uses_google & vapply(active_samples, function(x) as.character(unlist(x$id))[1] %in% edit_excluded(), logical(1))
      if (all(excl_now[c(1, length(excl_now))] == FALSE) && sum(!excl_now) >= 2) {
        lats_route <- lats[!excl_now]
        lngs_route <- lngs[!excl_now]
      } else {
        excl_now[] <- FALSE
        lats_route <- lats
        lngs_route <- lngs
      }
      pts <- data.frame(lat = lats_route, lng = lngs_route)

      if (uses_google) {
        transit_mode <- GOOGLE_TRANSIT_MODE_MAP[[profile]]
        # Reference time: the first active sample (same weekday/time of day)
        first_date <- active_samples[[1]]$date
        if (is.list(first_date)) first_date <- unlist(first_date)
        ref_time_utc <- parse_timestamp_utc(as.character(first_date %||% NA_character_)[1])
        route <- calculate_google_transit_route(pts, transit_mode = transit_mode, ref_time_utc = ref_time_utc)
        if (!is.null(route)) {
          route$coords <- smooth_osrm_route(route$coords, tolerance_m = 5, angle_min = 2)
        }
      } else if (use_pins) {
        s_pin <- edit_samples_df()
        op <- order_pins(s_pin, pins_now)
        ends <- s_pin[c(1, nrow(s_pin)), ]
        r <- osrm_route_via(rbind(c(ends$lon[1], ends$lat[1]), cbind(op$lon, op$lat), c(ends$lon[2], ends$lat[2])), profile,
                            bearings = c(NA, op$bearing, NA), radiuses = c(NA, rep(REVIEW_THRESHOLDS$pins$radius_m, nrow(op)), NA))
        route <- if (is.null(r)) NULL else list(coords = r$coords, distance_m = r$distance_m)
      } else {
        route <- calculate_osrm_route(pts, profile = profile)
      }

      if (is.null(route)) {
        shiny::removeNotification("snap_progress")
        shiny::showNotification(
          tr("Failed to calculate route. The server may not have been able to route between the points."),
          type = "error"
        )
        return()
      }

      if (is.null(route$coords) || nrow(route$coords) < 2) {
        shiny::removeNotification("snap_progress")
        shiny::showNotification(tr("OSRM route returned no valid coordinates."), type = "error")
        return()
      }

      shiny::showNotification(tr("6/7 Interpolating samples..."), id = "snap_progress", duration = NULL, type = "message")

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
        shiny::showNotification(tr("OSRM route too short or invalid."), type = "error")
        return()
      }

      # Store the OSRM route geometry for display
      osrm_route(osrm_coords)

      # Points with a direction change (>2 degrees) or > 50 m from the last kept one
      relevant_points <- path_key_points(osrm_coords, angle_deg = 2, max_gap_m = 50)

      # Filtered coordinates for virtual samples
      filtered_coords <- osrm_coords[relevant_points, , drop = FALSE]
      n_filtered <- nrow(filtered_coords)

      cumulative_dist <- path_cumulative_m(osrm_coords)
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

      # Position on the OSRM route at the sample's time proportion (0 to 1)
      new_xy <- if (use_pins || any(excl_now)) {
        # Through pins (or with excluded samples): each sample at its projection on the route (outliers by time)
        o <- order(ts_vec)
        xy <- matrix(NA_real_, n, 2)
        xy[o, ] <- snap_positions_to_route(osrm_coords, data.frame(lon = lngs[o], lat = lats[o], t = ts_vec[o]))
        xy
      } else {
        path_points_at(osrm_coords, cumulative_dist, (ts_vec - t_min) / t_range * total_dist)
      }
      for (i in seq_len(n)) {
        active_samples[[i]]$longitude <- unname(new_xy[i, 1])
        active_samples[[i]]$latitude <- unname(new_xy[i, 2])
      }

      shiny::showNotification(tr("7/7 Finishing..."), id = "snap_progress", duration = NULL, type = "message")

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
      bslib::accordion_panel_open("edit_samples_steps", "export")  # next step

      shiny::removeNotification("snap_progress")
      shiny::showNotification(
        sprintf(tr("Snap-to-road complete! %d Arc samples + %d virtual (%.1f km)."),
                n, length(new_virtual_samples), total_dist / 1000),
        type = "message"
      )
    }, error = function(e) {
      shiny::removeNotification("snap_progress")
      shiny::showNotification(
        paste(tr("Snap-to-road error:"), e$message),
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
        title = tr("Sample Properties"),
        shiny::tags$table(
          class = "table table-condensed",
          shiny::tags$tr(shiny::tags$td(shiny::strong("ID:")), shiny::tags$td(sample_id)),
          shiny::tags$tr(shiny::tags$td(shiny::strong(tr("Date/Time (UTC):"))), shiny::tags$td(fmt_datetime(ts_utc, current_lang(), seconds = TRUE))),
          shiny::tags$tr(shiny::tags$td(shiny::strong(tr("Date/Time (Local):"))), shiny::tags$td(fmt_datetime(ts_local, current_lang(), seconds = TRUE))),
          shiny::tags$tr(shiny::tags$td(shiny::strong(tr("Latitude:"))), shiny::tags$td(sprintf("%.8f", lat))),
          shiny::tags$tr(shiny::tags$td(shiny::strong(tr("Longitude:"))), shiny::tags$td(sprintf("%.8f", lon))),
          shiny::tags$tr(shiny::tags$td(shiny::strong(tr("Speed:"))), shiny::tags$td(if (is.na(speed)) "-" else sprintf("%.1f km/h", speed * 3.6))),
          shiny::tags$tr(shiny::tags$td(shiny::strong(tr("Course:"))), shiny::tags$td(if (is.na(course)) "-" else sprintf(tr("%.1f degrees"), course))),
          shiny::tags$tr(shiny::tags$td(shiny::strong(tr("Altitude:"))), shiny::tags$td(if (is.na(alt)) "-" else sprintf("%.1f m", alt)))
        ),
        easyClose = TRUE,
        footer = shiny::modalButton(tr("Close"))
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
      shiny::showNotification(tr("Sample re-included."), type = "message")
    } else {
      # Add to ignored list
      ignored <- c(ignored, sample_id)
      shiny::showNotification(tr("Sample ignored."), type = "message")
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
    shiny::showNotification(tr("Sample discarded."), type = "message")
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
      shiny::showNotification(tr("Cannot insert after the last sample."), type = "warning")
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
    new_sample$id <- toupper(uuid::UUIDgenerate(use.time = TRUE))
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
    shiny::showNotification(tr("New sample inserted."), type = "message")
  })

  # ---- Bulk actions (selection) ----

  # Ignore selected
  shiny::observeEvent(input$ignore_selected, {
    selected <- input$selected_samples
    if (is.null(selected) || length(selected) == 0) {
      shiny::showNotification(tr("No samples selected."), type = "warning")
      return()
    }

    ignored <- ignored_samples()
    new_ids <- setdiff(selected, ignored)
    ignored <- c(ignored, new_ids)
    ignored_samples(ignored)

    # Clear selection
    session$sendCustomMessage("clear_selection", list())

    render_edit_samples()
    shiny::showNotification(sprintf(tr("%d samples ignored."), length(new_ids)), type = "message")
  })

  # Discard selected
  shiny::observeEvent(input$discard_selected, {
    selected <- input$selected_samples
    if (is.null(selected) || length(selected) == 0) {
      shiny::showNotification(tr("No samples selected."), type = "warning")
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
    shiny::showNotification(sprintf(tr("%d samples discarded."), length(selected)), type = "message")
  })

  # Edit export package (item + samples) from the current Edit Samples state.
  # Same sample selection for the zip and GPX exports. NULL if nothing to export.
  edit_export_package <- function(mode, now_utc) {
    samples <- select_edit_export_samples(edit_samples(), ignored_samples(), virtual_samples())
    if (length(samples) == 0) return(NULL)
    build_edit_package(samples, original_samples(), original_timeline_item_id(), mode, now_utc)
  }

  # ---- Download corrected samples as LocoKit2 zip ----
  # Exports edited/snapped samples (virtual samples if any, otherwise the
  # non-ignored edit samples) as a LocoKit2 import package. Samples always get
  # new IDs and the activity type of the original samples.
  #
  # The LocoKit2 ImportManager uses INSERT OR IGNORE for all data types, so
  # existing items/samples can never be updated or removed via import.
  # Replacing a bad route works via GPX instead (Arc → Import GPX File), see
  # download_edit_gpx / send_edit_gpx_icloud.
  # Export modes (input$edit_export_mode, built by build_edit_package()):
  # - additive (default): new item ID, enabled. Added alongside the original.
  # - replace_disabled: new item ID, item and samples disabled. Imports fine
  #   but stays hidden: Arc has no UI to enable it (tested 2026-10-10).
  # - reuse_id: original item ID, enabled. Does not work yet: the item is
  #   skipped and its samples end up orphaned until the importer supports upsert.
  output$download_edit_arc <- shiny::downloadHandler(
    filename = function() {
      "Import.zip"
    },
    content = function(file) {
      if (length(edit_samples()) == 0) {
        shiny::showNotification(tr("No samples to export."), type = "error")
        return()
      }

      mode <- input$edit_export_mode %||% "additive"
      current_timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      pkg <- edit_export_package(mode, current_timestamp)

      if (is.null(pkg)) {
        shiny::showNotification(tr("All samples were ignored."), type = "error")
        return()
      }

      new_item <- pkg$item
      new_samples <- pkg$samples

      tmpdir <- tempfile("arc_edit_")
      dir.create(tmpdir)
      write_edit_package_dir(pkg, tmpdir)

      owd <- setwd(tmpdir)
      on.exit(setwd(owd), add = TRUE)

      # Zip directories and metadata to preserve folder structure
      files_to_zip <- c("metadata.json", "items", "samples", "places")
      files_to_zip <- files_to_zip[file.exists(files_to_zip)]
      zip::zipr(zipfile = file, files = files_to_zip)

      state <- if (isTRUE(new_item$base$disabled)) tr("disabled") else tr("enabled")
      if (identical(new_item$base$id, original_timeline_item_id())) {
        state <- paste0(state, ", ", tr("original ID"))
      }
      msg <- sprintf(tr("Exported %d item (%s) + %d samples"),
                     1L, state, length(new_samples))
      if (length(virtual_samples()) > 0) {
        msg <- paste0(msg, " ", tr("(OSRM route virtual samples)"))
      }
      shiny::showNotification(msg, type = "message")
    }
  )

  # ---- Download corrected samples as GPX ----
  # Same samples as the zip export, as a GPX 1.1 track. Primary route-correction
  # path: Arc → Import GPX File disables and hides the overlapping data
  # (verified 2026-10-10, reversible in Arc).
  output$download_edit_gpx <- shiny::downloadHandler(
    filename = function() {
      now_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      pkg <- edit_export_package(input$edit_export_mode %||% "additive", now_utc)
      if (is.null(pkg)) return("arc_edit.gpx")
      edit_gpx_filename(pkg)
    },
    content = function(file) {
      now_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      pkg <- edit_export_package(input$edit_export_mode %||% "additive", now_utc)

      if (is.null(pkg)) {
        shiny::showNotification(tr("No samples to export."), type = "error")
        return()
      }

      writeLines(build_edit_gpx(pkg, now_utc), file)
      n_points <- sum(vapply(pkg$samples, sample_has_coords, logical(1)))
      shiny::showNotification(sprintf(tr("Exported GPX with %d points"), n_points), type = "message")
    }
  )

  # ---- Send to iCloud (rclone) ----
  # Same packages as the downloads, pushed straight to iCloud Drive.
  # Import packages replace the contents of <Arc dir>/Import (rclone sync).
  import_sent_message <- function(n_items, n_samples) {
    sprintf(
      tr("Sent to iCloud: %s/Import (%d %s, %d samples). On the iPhone: Arc → Backup & Restore → Restore from Backup → %s/Import"),
      ICLOUD_ARC_DIR, n_items, if (n_items == 1) tr("item") else tr("items"), n_samples, ICLOUD_ARC_DIR
    )
  }

  # Edit Samples: corrected route as an import package
  shiny::observeEvent(input$send_edit_icloud, {
    if (!icloud_available()) {
      shiny::showNotification(tr("iCloud (rclone) is not available."), type = "error")
      return()
    }
    if (length(edit_samples()) == 0) {
      shiny::showNotification(tr("No samples to export."), type = "error")
      return()
    }

    now_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    pkg <- edit_export_package(input$edit_export_mode %||% "additive", now_utc)

    if (is.null(pkg)) {
      shiny::showNotification(tr("All samples were ignored."), type = "error")
      return()
    }

    tmpdir <- tempfile("arc_edit_")
    dir.create(tmpdir)
    write_edit_package_dir(pkg, tmpdir)

    res <- shiny::withProgress(message = tr("Sending to iCloud..."), value = 0.3, {
      icloud_push_import(tmpdir, lang = current_lang())
    })
    unlink(tmpdir, recursive = TRUE)

    if (!isTRUE(res$ok)) {
      shiny::showNotification(res$message, type = "error", duration = NULL)
      return()
    }
    shiny::showNotification(import_sent_message(1L, length(pkg$samples)), type = "message", duration = 15)
  })

  # Edit Samples: corrected route as GPX (Arc → Import GPX File)
  shiny::observeEvent(input$send_edit_gpx_icloud, {
    if (!icloud_available()) {
      shiny::showNotification(tr("iCloud (rclone) is not available."), type = "error")
      return()
    }

    now_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    pkg <- edit_export_package(input$edit_export_mode %||% "additive", now_utc)

    if (is.null(pkg)) {
      shiny::showNotification(tr("No samples to export."), type = "error")
      return()
    }

    gpx_name <- edit_gpx_filename(pkg)
    gpx_path <- file.path(tempfile("arc_gpx_"), gpx_name)
    dir.create(dirname(gpx_path))
    writeLines(build_edit_gpx(pkg, now_utc), gpx_path)

    res <- shiny::withProgress(message = tr("Sending GPX to iCloud..."), value = 0.3, {
      icloud_push_file(gpx_path, subdir = "GPX", lang = current_lang())
    })
    unlink(dirname(gpx_path), recursive = TRUE)

    if (!isTRUE(res$ok)) {
      shiny::showNotification(res$message, type = "error", duration = NULL)
      return()
    }
    shiny::showNotification(
      sprintf(tr("GPX sent to iCloud: %s/GPX/%s. On the iPhone: Arc → timeline → Import GPX File"),
              ICLOUD_ARC_DIR, gpx_name),
      type = "message", duration = 15
    )
  })

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
    shiny::showNotification(tr("Timeline, samples and map cleared."), type = "message")
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
        shiny::showNotification(tr("No items/samples to export."), type = "error")
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

  # Send the Arc package (timeline + samples) to iCloud Import
  shiny::observeEvent(input$send_arc_icloud, {
    if (!icloud_available()) {
      shiny::showNotification(tr("iCloud (rclone) is not available."), type = "error")
      return()
    }

    tl <- timeline()
    s <- all_samples()

    if (length(tl) == 0 || length(s) == 0) {
      shiny::showNotification(tr("No items/samples to export."), type = "error")
      return()
    }

    tmpdir <- tempfile("arc_import_")
    dir.create(tmpdir)
    export_arc_json(tl, s, tmpdir,
                      work_date = input$work_date)

    res <- shiny::withProgress(message = tr("Sending to iCloud..."), value = 0.3, {
      icloud_push_import(tmpdir, lang = current_lang())
    })
    unlink(tmpdir, recursive = TRUE)

    if (!isTRUE(res$ok)) {
      shiny::showNotification(res$message, type = "error", duration = NULL)
      return()
    }
    shiny::showNotification(import_sent_message(length(tl), length(s)), type = "message", duration = 15)
  })

  # ---- Timeline GPX (Arc → Import GPX File) ----
  # Trip items of the day timeline, one <trk> per item (visits skipped). Arc's
  # Import GPX File disables/hides the overlapping data, so this replaces any
  # recorded trip (e.g. a badly recorded flight with its FR24 track).
  timeline_gpx_filename_or_default <- function() {
    tracks <- gpx_tracks(timeline(), all_samples())
    if (length(tracks) == 0) return("arc_timeline.gpx")
    timeline_gpx_filename(input$work_date %||% Sys.Date(), tracks)
  }

  timeline_gpx_exported_message <- function(tracks) {
    n_trips <- length(tracks)
    n_points <- sum(vapply(tracks, function(t) length(t$samples), integer(1)))
    sprintf(tr("Exported timeline GPX: %d %s, %d points"),
            n_trips, if (n_trips == 1) tr("trip") else tr("trips"), n_points)
  }

  output$download_timeline_gpx <- shiny::downloadHandler(
    filename = function() {
      timeline_gpx_filename_or_default()
    },
    content = function(file) {
      tl <- timeline()
      s <- all_samples()
      tracks <- gpx_tracks(tl, s)

      if (length(tracks) == 0) {
        shiny::showNotification(tr("No trips in the timeline to export."), type = "error")
        return()
      }

      writeLines(build_gpx(tl, s), file)
      shiny::showNotification(timeline_gpx_exported_message(tracks), type = "message")
    }
  )

  shiny::observeEvent(input$send_timeline_gpx_icloud, {
    if (!icloud_available()) {
      shiny::showNotification(tr("iCloud (rclone) is not available."), type = "error")
      return()
    }

    tl <- timeline()
    s <- all_samples()
    tracks <- gpx_tracks(tl, s)

    if (length(tracks) == 0) {
      shiny::showNotification(tr("No trips in the timeline to export."), type = "error")
      return()
    }

    gpx_name <- timeline_gpx_filename(input$work_date %||% Sys.Date(), tracks)
    gpx_path <- file.path(tempfile("arc_gpx_"), gpx_name)
    dir.create(dirname(gpx_path))
    writeLines(build_gpx(tl, s), gpx_path)

    res <- shiny::withProgress(message = tr("Sending GPX to iCloud..."), value = 0.3, {
      icloud_push_file(gpx_path, subdir = "GPX", lang = current_lang())
    })
    unlink(dirname(gpx_path), recursive = TRUE)

    if (!isTRUE(res$ok)) {
      shiny::showNotification(res$message, type = "error", duration = NULL)
      return()
    }
    shiny::showNotification(
      sprintf(tr("GPX sent to iCloud: %s/GPX/%s. On the iPhone: Arc → timeline → Import GPX File"),
              ICLOUD_ARC_DIR, gpx_name),
      type = "message", duration = 15
    )
  })

  # ---- Download GPX (all samples, ⚙ menu) ----
  # Same format as the timeline GPX, but visits (without <type>) and samples not
  # linked to any item are kept too.

  output$download_gpx <- shiny::downloadHandler(
    filename = function() {
      paste0("samples_", format(Sys.Date(), "%Y%m%d"), ".gpx")
    },
    content = function(file) {
      s <- all_samples()
      if (length(s) == 0) {
        shiny::showNotification(tr("No samples to export."), type = "error")
        return()
      }

      writeLines(build_gpx(timeline(), s, include_all = TRUE), file)
    }
  )

  # ---- Route Editing ----

  # Update route selector when entering "EditRoute" mode
  shiny::observe({
    if (input$mode != "EditRoute") return()

    tl <- timeline()
    if (length(tl) == 0) {
      shiny::updateSelectInput(session, "selected_route",
                               choices = stats::setNames("", tr("No routes")),
                               selected = "")
      return()
    }

    # Filter routes only (not visits)
    routes <- Filter(function(x) !isTRUE(x$.isVisit), tl)
    if (length(routes) == 0) {
      shiny::updateSelectInput(session, "selected_route",
                               choices = stats::setNames("", tr("No routes")),
                               selected = "")
      return()
    }

    # Create options with name, mode and local time
    options_list <- vapply(routes, function(x) {
      label <- x$description %||% tr("Route")
      mode_code <- x$activityType %||% "transport"
      mode_label <- names(ACTIVITY_CHOICES)[match(mode_code, ACTIVITY_CHOICES)]
      current_mode <- if (is.na(mode_label)) mode_code else tr(mode_label)
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
  render_editable_route <- function(auto_zoom = FALSE) {
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
          label = sprintf(tr("Node %d"), i)
        )
    }

    # Adjust bounds only on initial load
    if (auto_zoom) {
      proxy %>%
        leaflet::fitBounds(
          lng1 = min(all_lngs) - 0.005, lat1 = min(all_lats) - 0.005,
          lng2 = max(all_lngs) + 0.005, lat2 = max(all_lats) + 0.005
        )
    }

    # Set up event handlers on nodes
    session$sendCustomMessage("setup_route_nodes", list())
  }

  # Handler: Load route for editing
  shiny::observeEvent(input$load_route, {
    item_id <- input$selected_route
    if (is.null(item_id) || item_id == "") {
      shiny::showNotification(tr("Select a route."), type = "warning")
      return()
    }

    tl <- timeline()
    idx <- which(vapply(tl, `[[`, "", ".internalId") == item_id)
    if (length(idx) == 0) {
      shiny::showNotification(tr("Route not found."), type = "error")
      return()
    }

    item <- tl[[idx]]
    route_original_item(item)

    # Find associated samples
    s <- all_samples()
    sample_ids <- item$samples %||% character(0)
    route_samples <- Filter(function(x) x$id %in% sample_ids, s)

    if (length(route_samples) < 2) {
      shiny::showNotification(tr("Route must have at least 2 samples."), type = "error")
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

    render_editable_route(auto_zoom = TRUE)
    bslib::accordion_panel_open("edit_route_steps", "edit")  # next step

    shiny::showNotification(
      sprintf(tr("Route loaded: %d nodes, %.1f km, avg speed %.1f km/h"),
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
      shiny::showNotification(tr("Cannot delete the first or last node."), type = "warning")
      return()
    }

    # Remove the node
    nodes <- nodes[-node_idx]
    route_edit_nodes(nodes)

    render_editable_route()
    shiny::showNotification(tr("Node deleted."), type = "message")
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
      shiny::showNotification(tr("Load a route first."), type = "warning")
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
    shiny::showNotification(tr("Waypoint inserted."), type = "message")
  })

  # Handler: Apply edits
  shiny::observeEvent(input$apply_route_edits, {
    nodes <- route_edit_nodes()
    original_item <- route_original_item()
    avg_speed <- route_avg_speed()

    if (length(nodes) < 2 || is.null(original_item)) {
      shiny::showNotification(tr("No route to apply."), type = "error")
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
      item$description <- sprintf(tr("Edited route (%.1f km)"), new_dist_km)

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
        sprintf(tr("Route updated! %.1f km, duration %.0f min, avg speed %.1f km/h"),
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

    shiny::showNotification(tr("Edit cancelled."), type = "message")
  })

  # Handler: Delete selected nodes (via lasso)
  shiny::observeEvent(input$delete_selected_nodes, {
    selected <- input$selected_route_nodes
    if (is.null(selected) || length(selected) == 0) {
      shiny::showNotification(tr("No nodes selected."), type = "warning")
      return()
    }

    nodes <- route_edit_nodes()
    if (length(nodes) < 3) {
      shiny::showNotification(tr("Route must have at least 2 nodes."), type = "warning")
      return()
    }

    # Get IDs of first and last node
    node_ids <- vapply(nodes, `[[`, "", "id")
    first_id <- node_ids[1]
    last_id <- node_ids[length(node_ids)]

    # Remove first and last from selected list
    to_delete <- setdiff(selected, c(first_id, last_id))

    if (length(to_delete) == 0) {
      shiny::showNotification(tr("Cannot delete first/last node."), type = "warning")
      return()
    }

    # Check if at least 2 nodes will remain
    remaining <- length(nodes) - length(to_delete)
    if (remaining < 2) {
      shiny::showNotification(tr("Route must have at least 2 nodes."), type = "warning")
      return()
    }

    # Remove selected nodes
    nodes <- Filter(function(n) !n$id %in% to_delete, nodes)
    route_edit_nodes(nodes)

    render_editable_route()
    session$sendCustomMessage("clear_node_selection", list())
    shiny::showNotification(sprintf(tr("%d node(s) deleted."), length(to_delete)), type = "message")
  })

  # Handler: Clear node selection
  shiny::observeEvent(input$clear_node_selection, {
    session$sendCustomMessage("clear_node_selection", list())
    shiny::showNotification(tr("Selection cleared."), type = "message")
  })

  # ---- Batch review (R/server_review.R) ----
  review_server(input, output, session, tr, current_lang)

  # Download links can sit in collapsed panels, other tabs or the ⚙ menu:
  # keep their URLs set even while hidden
  for (id in c("download_arc", "download_gpx", "download_timeline_gpx", "download_edit_arc", "download_edit_gpx")) {
    shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
  }
}
