# ---- UI ----
# Layout: compact top bar + collapsible left sidebar (Create / Edit / Export
# tabs) + full-size map + collapsible right sidebar (day timeline). The page
# itself never scrolls; only sidebar contents (e.g. the timeline list) do.
#
# input$mode keeps its original values (OSRM, Visit, Manual, Import,
# EditSamples, EditRoute): it is a hidden radio input driven by the tabs and
# segmented controls (see the navigation script at the end).

README_URL <- "https://github.com/gbragaalves/arc_timeline_editor"
# Test-only (headless runs, set by the test launcher): OSM raster as the default
# base layer, without MapLibre/OpenFreeMap, whose WebGL is rendered in software
# there and is very slow. Real use keeps OpenFreeMap.
MAP_RASTER_BASE <- isTRUE(get0("MAP_RASTER_BASE", ifnotfound = FALSE))

# OpenFreeMap base layer: MapLibre GL + its Leaflet plugin, pinned CDN versions.
# Attached after leafletOutput() so the scripts load (in order) after leaflet.js.
MAPLIBRE_DEPS <- list(
  htmltools::htmlDependency(
    "maplibre-gl", "5.24.0",
    src = c(href = "https://cdn.jsdelivr.net/npm/maplibre-gl@5.24.0/dist"),
    script = "maplibre-gl.js",
    stylesheet = "maplibre-gl.css"
  ),
  htmltools::htmlDependency(
    "maplibre-gl-leaflet", "0.1.4",
    src = c(href = "https://cdn.jsdelivr.net/npm/@maplibre/maplibre-gl-leaflet@0.1.4"),
    script = "leaflet-maplibre-gl.js"
  )
)


PROFILE_CHOICES <- c(
  "Car" = "car", "Official" = "official", "Foot" = "foot", "Bike" = "bike", "Bus" = "bus",
  "Metro" = "metro", "Train" = "train", "Tram/VLT" = "tram"
)

# Profile select choices: OSRM profiles not in OSRM_ENABLED_PROFILES are left out
profile_choices <- function(choices = PROFILE_CHOICES) {
  choices[!choices %in% names(OSRM_SERVERS) | choices %in% osrm_enabled_profiles()]
}

# Batch review: OSRM profile of a road trip
REVIEW_PROFILE_CHOICES <- c("Car" = "car", "Official" = "official", "Bus" = "bus")

# Batch review: activities, sort order (labels are translation keys)
REVIEW_ACTIVITY_CHOICES <- c(
  "Metro" = "metro", "Train" = "train", "Tram" = "tram",
  "Car" = "car", "Taxi" = "taxi", "Bus" = "bus"
)
REVIEW_SORT_CHOICES <- c("By date" = "date", "By severity" = "severity")

ACTIVITY_CHOICES <- c(
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
)

# Small ⓘ icon with a Bootstrap tooltip (text is a translation key)
help_icon <- function(text) {
  bslib::tooltip(
    shiny::span(class = "help-icon", "ⓘ"),
    t_(text),
    placement = "auto"
  )
}

# "Label [date] [time]" on one row
date_time_row <- function(label, date_id, time_id, time_value) {
  shiny::div(
    class = "dt-row",
    shiny::span(class = "dt-label", t_(label)),
    # Picker right-aligned to the date field, so it doesn't spread over the time column
    htmltools::tagQuery(date_input(date_id, NULL, Sys.Date(), width = "100%"))$
      find("input")$addAttrs(`data-date-orientation` = "right")$allTags(),
    shiny::textInput(time_id, NULL, time_value, width = "100%")
  )
}

# Segmented control (plain radios, not a Shiny input): drives input$mode
mode_segments <- function(name, choices, selected) {
  shiny::div(
    class = "btn-group btn-group-sm w-100 mode-segments", role = "group",
    lapply(seq_along(choices), function(i) {
      value <- unname(choices[i])
      id <- paste0("seg_", value)
      shiny::tagList(
        shiny::tags$input(
          type = "radio", class = "btn-check", name = name, id = id, value = value,
          autocomplete = "off", checked = if (identical(value, selected)) NA
        ),
        shiny::tags$label(class = "btn btn-outline-primary", `for` = id, t_(names(choices)[i]))
      )
    })
  )
}

# Short hint line with a help tooltip, shown at the top of a mode
mode_hint <- function(hint, help) {
  shiny::div(class = "mode-hint", t_(hint), help_icon(help))
}

# Action buttons pinned to the bottom of the sidebar
mode_actions <- function(...) {
  shiny::div(class = "mode-actions", ...)
}

APP_CSS <- "
  /* Notifications: bottom-right, compact (progress ones too) */
  #shiny-notification-panel {
    width: 380px;
    max-width: calc(100vw - 24px);
    right: 8px;
    bottom: 8px;
  }
  .shiny-notification { font-size: 0.85rem; }

  /* Top bar */
  .app-topbar {
    display: flex;
    align-items: center;
    gap: 16px;
    height: 44px;
    flex: 0 0 auto;
    padding: 0 12px;
    border-bottom: 1px solid var(--bs-border-color);
    background: var(--bs-body-bg);
  }
  .app-name { font-weight: 600; white-space: nowrap; }
  .topbar-date { display: flex; align-items: center; gap: 6px; }
  .topbar-date .shiny-input-container { margin: 0 !important; }
  .topbar-date .form-control { padding: 0.15rem 0.45rem; font-size: 0.82rem; }
  .topbar-label { font-size: 0.8rem; color: var(--bs-secondary-color); white-space: nowrap; }
  .topbar-right { margin-left: auto; display: flex; align-items: center; gap: 14px; }
  .status-dots { display: flex; gap: 10px; cursor: pointer; font-size: 0.78rem; user-select: none; }
  .status-dot { display: inline-flex; align-items: center; gap: 4px; }
  .status-dot .dot { width: 9px; height: 9px; border-radius: 50%; background: #adb5bd; }
  .status-dot.ok .dot { background: #198754; }
  .status-dot.warn .dot { background: #fd7e14; }
  .app-topbar .dropdown-menu { font-size: 0.85rem; }
  .lang-toggle .btn { padding: 0.1rem 0.45rem; font-size: 0.75rem; }

  /* Sidebars: compact controls */
  .bslib-sidebar-layout > .sidebar { font-size: 0.84rem; }
  .sidebar .shiny-input-container { margin-bottom: 0.4rem; }
  .sidebar .shiny-input-container:not(.shiny-input-container-inline) { width: 100%; }
  .sidebar .form-control, .sidebar .selectize-input {
    font-size: 0.82rem;
    padding: 0.2rem 0.45rem;
    min-height: 30px;
  }
  .sidebar .selectize-dropdown { font-size: 0.82rem; }
  .sidebar .control-label { margin-bottom: 0.1rem; }
  .sidebar .checkbox, .sidebar .radio { margin-bottom: 0.1rem; }
  .sidebar .shiny-options-group .radio { margin-bottom: 0; }
  .sidebar .nav-underline { margin-bottom: 0.5rem; gap: 1rem; }
  .sidebar .nav-underline .nav-link { padding: 0.3rem 0.1rem; }
  .sidebar .accordion-button { padding: 0.45rem 0.6rem; font-size: 0.84rem; }
  .sidebar .accordion-body { padding: 0.5rem 0.6rem; }
  .subsection-toggle {
    padding: 0.1rem 0;
    font-size: 0.8rem;
    font-weight: 600;
    text-decoration: none;
    color: var(--bs-body-color);
  }
  .subsection-toggle::before {
    content: '\\25B8';
    display: inline-block;
    margin-right: 4px;
    transition: transform 0.2s;
  }
  .subsection-toggle:not(.collapsed)::before { transform: rotate(90deg); }
  .subsection-body { padding: 0.3rem 0 0 0.2rem; }
  .mode-segments { margin-bottom: 0.5rem; }
  .mode-segments .btn { font-size: 0.78rem; padding: 0.25rem 0.3rem; }
  .mode-hint { font-size: 0.78rem; color: var(--bs-secondary-color); margin-bottom: 0.4rem; }
  .pane-section {
    border: 1px solid var(--bs-border-color);
    border-radius: 0.375rem;
    padding: 0.5rem 0.6rem;
    margin-bottom: 0.5rem;
  }
  .pane-title { font-weight: 600; font-size: 0.82rem; margin-bottom: 0.35rem; }
  .mode-actions {
    position: sticky;
    bottom: 0;
    z-index: 2;
    display: flex;
    flex-wrap: wrap;
    gap: 4px;
    padding: 0.4rem 0 0.2rem;
    background: var(--_sidebar-bg, var(--bs-body-bg));
  }
  .mode-actions > * { flex: 1 1 auto; }
  .dt-row {
    display: grid;
    grid-template-columns: 46px 1fr 72px;
    gap: 4px;
    align-items: center;
    margin-bottom: 0.3rem;
  }
  .dt-row .shiny-input-container { margin-bottom: 0 !important; }
  .dt-label { font-size: 0.78rem; color: var(--bs-secondary-color); }
  .btn-row { display: flex; gap: 4px; margin-bottom: 0.35rem; }
  .btn-row > * { flex: 1 1 0; }
  .btn-row .shiny-input-container { margin-bottom: 0 !important; }
  .help-icon { margin-left: 4px; color: var(--bs-secondary-color); cursor: help; font-size: 0.95em; }
  .timeline-title { font-weight: 600; font-size: 0.9rem; }
  .timeline-scroll { flex: 1 1 0; min-height: 0; overflow-y: auto; }

  /* Batch review: only the trip list scrolls (height set by fitReviewList) */
  .range-row {
    display: grid;
    grid-template-columns: auto 1fr auto 1fr;
    gap: 4px;
    align-items: center;
    margin-bottom: 0.3rem;
  }
  .range-row .shiny-input-container { margin-bottom: 0 !important; }
  .review-toolbar { display: flex; flex-wrap: nowrap; gap: 4px; align-items: center; justify-content: space-between; }
  .review-toolbar .shiny-input-container { margin-bottom: 0 !important; }
  .review-toolbar .shiny-options-group { white-space: nowrap; }
  .review-toolbar .form-check-inline { margin-right: 0.3rem; padding-left: 1.3em; font-size: 0.74rem; }
  .review-toolbar .form-check-inline .form-check-input { margin-left: -1.3em; }
  .review-toolbar .selectize-input, .review-toolbar select { font-size: 0.74rem; min-height: 0; padding: 2px 6px; }
  .review-list {
    overflow-y: auto;
    border: 1px solid var(--bs-border-color);
    border-radius: 0.375rem;
    margin: 0.3rem 0;
    height: 160px;
  }
  .review-row {
    display: grid;
    grid-template-columns: 14px 16px 34px 64px 30px 34px minmax(0, 1fr);
    gap: 2px;
    align-items: center;
    padding: 2px 2px;
    font-size: 0.74rem;
    white-space: nowrap;
    cursor: pointer;
    border-bottom: 1px solid var(--bs-border-color-translucent);
  }
  .review-row:hover { background: var(--bs-tertiary-bg); }
  .review-row.selected { background: var(--bs-primary-bg-subtle); }
  .review-row input { margin: 0; }
  .review-row .rv-num { text-align: right; font-variant-numeric: tabular-nums; }
  .review-row .rv-badge {
    overflow: hidden;
    text-overflow: ellipsis;
    font-size: 0.64rem;
    padding: 0 2px;
    border-radius: 3px;
    justify-self: start;
    max-width: 100%;
  }
  .review-empty { padding: 6px; font-size: 0.76rem; color: var(--bs-secondary-color); }
  .review-summary { font-size: 0.76rem; margin-top: 0.3rem; }
  .review-detail { font-size: 0.76rem; margin-bottom: 0.3rem; }
  .review-detail .rv-title { font-weight: 600; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
  .review-detail .rv-title .rv-stations { font-weight: 400; }
  .review-detail .rv-title-row { display: flex; align-items: center; gap: 6px; }
  .review-detail .rv-title-row .rv-title { flex: 1 1 auto; min-width: 0; }
  .review-detail .review-profile { flex: 0 0 auto; font-weight: 400; }
  .review-profile .selectize-input { line-height: 1.2; vertical-align: top; }
  .review-detail .rv-jae { display: flex; flex-wrap: wrap; align-items: center; column-gap: 6px; }
  .review-detail .rv-jae > .rv-clamp { flex: 1 1 auto; min-width: 0; }
  .review-detail .rv-jae.rv-split-on { flex-wrap: nowrap; }
  .review-detail .rv-jae .btn.rv-split-cancel { padding: 0 7px; font-size: 0.8rem; line-height: 1.15; }
  .review-detail .rv-jae .btn { flex: 0 0 auto; margin-left: auto; font-size: 0.7rem; padding: 0 6px; line-height: 1.35; }
  .review-detail .rv-split-row { display: flex; align-items: center; justify-content: space-between; gap: 6px; }
  .review-detail .rv-hint { color: var(--bs-primary-text-emphasis); min-width: 0; }
  .review-detail .rv-split-row .shiny-input-container, .review-detail .rv-split-after .shiny-input-container {
    margin-bottom: 0 !important; width: auto;
  }
  .review-detail .rv-split-osrm .checkbox, .review-detail .rv-split-osrm .form-check { margin: 0; min-height: 0; white-space: nowrap; }
  .review-detail .rv-split-after .form-check-inline, .review-detail .rv-split-after .radio-inline { margin-right: 0.45rem; }
  .review-detail .rv-segs { display: flex; flex-wrap: wrap; gap: 2px 6px; }
  .review-detail .rv-split-stop .shiny-input-container { flex: 1 1 auto; min-width: 0; }
  .review-detail .rv-split-stop input { font-size: 0.8rem; padding: 0 0.4rem; min-height: 0; height: 1.6rem; }
  .review-notes-opts { display: flex; column-gap: 10px; font-size: 0.8rem; }
  .review-notes-opts .shiny-input-container { margin-bottom: 0.25rem !important; width: auto; white-space: nowrap; }
  .review-detail .rv-pin-btn { flex: 0 0 auto; font-size: 0.72rem; padding: 0 5px; line-height: 1.35; }
  .review-detail .rv-pin-amb { display: flex; align-items: center; gap: 6px; }
  .review-detail .rv-pin-amb select { width: auto; font-size: 0.72rem; padding: 0 1.6rem 0 0.4rem; min-height: 0; }
  .leaflet-container.lasso-mode, .leaflet-container.lasso-mode .leaflet-interactive { cursor: crosshair; }
  .leaflet-interactive.route-hit { cursor: grab; }
  .route-end { border-radius: 50%; border: 2px solid #fff; box-shadow: 0 0 0 1px rgba(0,0,0,0.35); cursor: grab; }
  .route-end-start { background: #2E7D32; }
  .route-end-end { background: #C62828; }
  .leaflet-marker-icon.sample-excluded { filter: grayscale(1) opacity(0.55); outline: 2px solid #37474F; outline-offset: -2px; border-radius: 50%; }
  .bus-arrow span { display: inline-block; font-size: 11px; line-height: 12px; text-shadow: 0 0 2px #fff; }
  .bus-popup table { font-size: 0.75rem; margin-top: 4px; }
  .bus-popup td { padding: 0 6px 0 0; vertical-align: top; }
  .bus-popup td:first-child { color: #616161; white-space: nowrap; }
  .leaflet-tooltip.pin-label { font-weight: 700; padding: 0 4px; }
  .review-detail .rv-seg { border-left: 4px solid; padding-left: 3px; white-space: nowrap; }
  .review-detail .rv-warn { color: var(--bs-danger-text-emphasis); }
  .review-detail .rv-clamp { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
  .review-end {
    display: grid;
    grid-template-columns: 34px minmax(0, 1fr) 52px;
    gap: 2px 3px;
    align-items: center;
    margin-top: 2px;
  }
  .review-end .shiny-input-container, .review-profile .shiny-input-container { margin-bottom: 0 !important; }
  .review-end .selectize-input, .review-toolbar .selectize-input, .review-profile .selectize-input {
    font-size: 0.72rem;
    min-height: 0;
    padding: 1px 18px 1px 4px;
    white-space: nowrap;
    overflow: hidden;
  }
  .review-end .selectize-input > .item, .review-toolbar .selectize-input > .item, .review-profile .selectize-input > .item {
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    max-width: 100%;
  }
  .review-end .btn-group { grid-column: 2 / 4; }
  .review-end .btn-group .btn { padding: 0 6px; font-size: 0.66rem; line-height: 1.25; }
  .review-actions .btn { white-space: nowrap; font-size: 0.76rem; padding: 0.2rem 0.3rem; }
  .review-keep { font-size: 0.74rem; }
  .review-row .rv-badge.rv-detour, .review-detail .rv-detour { background: #FFE0B2; color: #BF360C; }
  .review-row .rv-badge.rv-fragment, .review-detail .rv-fragment {
    background: #ECEFF1; color: #455A64; border: 1px dashed #90A4AE;
  }
  .review-detail .rv-detour, .review-detail .rv-fragment {
    font-size: 0.66rem; padding: 0 3px; border-radius: 3px; margin-right: 4px;
  }
  /* Jaé boardings (Route pane): time | mode (+ no location) | button, label below (wraps) */
  .jae-box { padding: 0.35rem 0.5rem; }
  .jae-box .pane-title { margin-bottom: 0.2rem; cursor: help; }
  .jae-list { max-height: 168px; overflow-y: auto; }
  .jae-row {
    display: grid;
    grid-template-columns: 34px minmax(0, 1fr) auto;
    column-gap: 4px;
    align-items: center;
    padding: 2px 0;
    font-size: 0.74rem;
    border-bottom: 1px solid var(--bs-border-color-translucent);
  }
  .jae-row:last-child { border-bottom: 0; }
  .jae-time { font-variant-numeric: tabular-nums; font-weight: 600; }
  .jae-head { display: flex; gap: 4px; align-items: center; min-width: 0; }
  .jae-mode { color: #fff; font-size: 0.64rem; padding: 0 4px; border-radius: 3px; white-space: nowrap; }
  .jae-noloc { font-size: 0.66rem; color: var(--bs-secondary-color); font-style: italic; white-space: nowrap; }
  .jae-start { font-size: 0.66rem; padding: 0 6px; line-height: 1.4; white-space: nowrap; }
  .jae-label { grid-column: 2 / 4; overflow-wrap: anywhere; line-height: 1.3; color: var(--bs-secondary-color); }
  .jae-row.jae-used .jae-time, .jae-row.jae-used .jae-head { opacity: 0.55; }
  .jae-row.jae-used .jae-start { border-color: transparent; color: var(--bs-success-text-emphasis); opacity: 1; }
  .review-cands { display: flex; width: 100%; margin-top: 2px; }
  .review-cands .btn { flex: 1 1 auto; min-width: 0; padding: 0 4px; font-size: 0.68rem; line-height: 1.35;
                       white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
  .review-bulk {
    display: grid;
    grid-template-columns: minmax(0, 1fr) auto;
    gap: 4px;
    align-items: center;
    margin-top: 0.3rem;
    line-height: 1.15;
  }
  #right_sidebar > .sidebar-content { height: 100%; overflow: hidden; }

  /* A collapsed sidebar's toggle sits over the map corner: move the Leaflet
     controls below it */
  .bslib-sidebar-layout.sidebar-collapsed:not(.sidebar-right) #map .leaflet-top.leaflet-left,
  .bslib-sidebar-layout.sidebar-right.sidebar-collapsed #map .leaflet-top.leaflet-right {
    top: 38px;
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
"

# ---- Mode panes ----

osrm_pane <- shiny::tagList(
  mode_hint(
    "Click waypoints on the map.",
    "Click waypoints on the map, then calculate a snap-to-road route. Car/Official/Foot/Bus use local OSRM (Docker ports 5000-5005). Metro/Train/Tram use Google Maps Directions API (requires GOOGLE_MAPS_API_KEY)."
  ),
  shiny::selectInput("osrm_profile", t_("Profile:"), choices = tr_choices(profile_choices()), selected = "car", width = "100%"),
  shiny::uiOutput("jae_list"),
  mode_actions(
    shiny::actionButton("undo_point_osrm", t_("Undo"), class = "btn-sm btn-outline-secondary"),
    shiny::actionButton("clear_points_osrm", t_("Clear"), class = "btn-sm btn-outline-secondary"),
    bslib::input_task_button("calculate_osrm", t_("Calculate route"), label_busy = t_("Processing..."), class = "btn-sm w-100")
  )
)

visit_pane <- shiny::tagList(
  mode_hint(
    "Click the map to set the location.",
    "Mark a stationary visit. Type a name from your frequent places (config_local.R) to auto-fill coordinates. Entry/exit define the stay duration."
  ),
  shiny::textInput("visit_name", t_("Visit name"), "", width = "100%"),
  date_time_row("Entry", "visit_date_start", "visit_time_start", "08:00"),
  date_time_row("Exit", "visit_date_end", "visit_time_end", "09:00"),
  mode_actions(
    shiny::actionButton("add_visit", t_("Add visit"), class = "btn-sm btn-success w-100")
  )
)

manual_pane <- shiny::tagList(
  mode_hint(
    "Click the map to set the points.",
    "Click points on the map to draw a freeform polyline route. Timestamps are evenly interpolated between start and end times."
  ),
  shiny::selectInput("manual_activity_type", t_("Activity type:"), choices = tr_choices(ACTIVITY_CHOICES), selected = "car", width = "100%"),
  date_time_row("Start", "manual_date_start", "manual_time_start", "08:00"),
  date_time_row("End", "manual_date_end", "manual_time_end", "09:00"),
  mode_actions(
    shiny::actionButton("undo_point_manual", t_("Undo"), class = "btn-sm btn-outline-secondary"),
    shiny::actionButton("clear_points_manual", t_("Clear"), class = "btn-sm btn-outline-secondary"),
    shiny::actionButton("add_manual", t_("Add route"), class = "btn-sm btn-success w-100")
  )
)

import_pane <- shiny::tagList(
  mode_hint(
    "GeoJSON, GPX, KML or GPKG.",
    "Import a route from GeoJSON, GPX, KML, or GPKG. FlightRadar24 KML files are detected automatically and timestamps are extracted from the track."
  ),
  shiny::fileInput(
    "geo_file", NULL,
    accept = c(".geojson", ".json", ".gpx", ".kml", ".gpkg"),
    buttonLabel = t_("Browse..."),
    placeholder = "GeoJSON/GPX/KML",
    width = "100%"
  ),
  shiny::radioButtons(
    "file_direction", NULL,
    choiceNames = list(t_("Normal"), t_("Reverse")),
    choiceValues = c("normal", "reverse"),
    selected = "normal", inline = TRUE
  ),
  shiny::selectInput("import_activity_type", t_("Activity type:"), choices = tr_choices(ACTIVITY_CHOICES), selected = "car", width = "100%"),
  date_time_row("Start", "import_date_start", "import_time_start", "08:00"),
  date_time_row("End", "import_date_end", "import_time_end", "09:00"),
  mode_actions(
    shiny::actionButton("add_import", t_("Add route"), class = "btn-sm btn-success w-100")
  )
)

edit_samples_pane <- shiny::tagList(
  mode_hint(
    "Fix existing samples from the weekly backups.",
    "Load existing samples from weekly .json.gz backup files. Drag markers to fix GPS positions, snap to road via OSRM, ignore or discard bad points. Right-click a sample for more options."
  ),
  bslib::accordion(
    id = "edit_samples_steps",
    multiple = FALSE,
    open = "load",
    bslib::accordion_panel(
      shiny::tagList("1 · ", t_("Load")), value = "load",
      date_time_row("Start", "edit_samples_date_start", "edit_samples_time_start", "00:00"),
      date_time_row("End", "edit_samples_date_end", "edit_samples_time_end", "23:59"),
      if (icloud_available()) {
        shiny::checkboxInput("fetch_icloud", t_("Fetch weeks from iCloud first"), value = TRUE)
      },
      bslib::input_task_button("load_samples", t_("Load"), label_busy = t_("Processing..."), class = "btn-sm w-100")
    ),
    bslib::accordion_panel(
      shiny::tagList("2 · ", t_("Clean & snap")), value = "clean",
      shiny::div(
        class = "btn-row",
        shiny::selectInput("edit_osrm_profile", NULL, choices = tr_choices(profile_choices()), selected = "car", width = "100%"),
        bslib::input_task_button(
          "snap_to_road", t_("Snap"), label_busy = t_("Processing..."), type = "warning", class = "btn-sm",
          title = tr_lang("Snap all active samples to the nearest road via OSRM and generate virtual intermediate points along the route"),
          `data-i18n-title` = "Snap all active samples to the nearest road via OSRM and generate virtual intermediate points along the route"
        )
      ),
      shiny::div(
        class = "btn-row",
        shiny::tags$button(id = "edit_pin_toggle", type = "button", `data-edit-pin` = "toggle",
                           class = "btn btn-sm btn-outline-secondary",
                           `data-i18n-title` = "Edit the route (P): drag the line to add a via pin, drag a pin to move it; right-click or double-click a pin to remove it; Ctrl+Z undoes. Snap routes through the pins.",
                           title = tr_lang("Edit the route (P): drag the line to add a via pin, drag a pin to move it; right-click or double-click a pin to remove it; Ctrl+Z undoes. Snap routes through the pins."),
                           "\U0001F4CD ", t_("Pins")),
        shiny::tags$button(type = "button", `data-edit-pin` = "clear", class = "btn btn-sm btn-outline-secondary", t_("Clear pins"))
      ),
      shiny::div(
        class = "d-flex align-items-center",
        shiny::checkboxInput("selection_mode", t_("Selection mode"), FALSE),
        help_icon("Draw a lasso on the map to select multiple samples at once for bulk ignore, discard or exclusion from the Snap routing.")
      ),
      shiny::div(
        class = "btn-row",
        shiny::actionButton("ignore_selected", t_("Ignore"), class = "btn-sm btn-outline-secondary"),
        shiny::actionButton("discard_selected", t_("Discard"), class = "btn-sm btn-outline-danger")
      ),
      shiny::div(
        class = "btn-row",
        shiny::tags$button(type = "button", `data-edit-pin` = "exclude", class = "btn btn-sm btn-outline-secondary",
                           `data-i18n-title` = "Keep the selected samples out of the Snap routing (GPS jitter, e.g. stopped at a light); they are still placed on the route. Ctrl+Z undoes.",
                           title = tr_lang("Keep the selected samples out of the Snap routing (GPS jitter, e.g. stopped at a light); they are still placed on the route. Ctrl+Z undoes."),
                           t_("Exclude from snap")),
        shiny::tags$button(type = "button", `data-edit-pin` = "clear_excl", class = "btn btn-sm btn-outline-secondary", t_("Clear exclusions"))
      )
    ),
    bslib::accordion_panel(
      shiny::tagList("3 · ", t_("Export")), value = "export",
      shiny::div(
        class = "mode-hint",
        t_("Corrected route as GPX"),
        help_icon("Recommended for replacing a bad route. In Arc, Import GPX File reads the activity type from the GPX, then disables and hides the overlapping data (reversible in Arc).")
      ),
      if (icloud_available()) {
        shiny::div(
          class = "mb-1",
          bslib::input_task_button("send_edit_gpx_icloud", t_("Send GPX to iCloud"), label_busy = t_("Sending..."), class = "btn-sm w-100")
        )
      },
      shiny::downloadButton("download_edit_gpx", t_("Export GPX"), class = "btn-sm btn-outline-secondary w-100 mb-2"),
      # Collapsed sub-section. A plain .collapse, not a nested accordion: bslib
      # would tie a nested .accordion-collapse to the outer accordion's parent
      # and close the "Export" step when this opens.
      shiny::div(
        class = "d-flex align-items-center",
        shiny::tags$button(
          type = "button", class = "btn btn-link subsection-toggle collapsed",
          `data-bs-toggle` = "collapse", `data-bs-target` = "#edit_json_package",
          `aria-expanded` = "false", `aria-controls` = "edit_json_package",
          t_("Arc JSON package")
        ),
        help_icon("For adding new data to empty periods. Add: new enabled item alongside existing data. Disabled item: imports fine but stays hidden; Arc has no UI to enable it. Reuse original item ID: skipped by the importer (INSERT OR IGNORE). To replace a bad route, use GPX instead.")
      ),
      shiny::div(
        id = "edit_json_package", class = "collapse subsection-body",
        shiny::radioButtons(
          "edit_export_mode", NULL,
          choiceNames = list(
            t_("Add: new item, enabled"),
            t_("Disabled item (imports hidden; Arc has no UI to enable it)"),
            t_("Reuse original item ID (not working yet, needs importer upsert)")
          ),
          choiceValues = c("additive", "replace_disabled", "reuse_id"),
          selected = "additive"
        ),
        shiny::downloadButton("download_edit_arc", t_("Export (zip)"), class = "btn-sm btn-success w-100 mb-1"),
        if (icloud_available()) {
          bslib::input_task_button("send_edit_icloud", t_("Send to iCloud (Import)"), label_busy = t_("Sending..."), type = "outline-primary", class = "btn-sm w-100")
        }
      )
    )
  )
)

edit_route_pane <- shiny::tagList(
  mode_hint(
    "Edit the waypoints of a trip.",
    "Select a trip from the timeline to edit its waypoints. Drag nodes to move, right-click to delete, or insert new waypoints. Average speed is preserved when you apply changes."
  ),
  bslib::accordion(
    id = "edit_route_steps",
    multiple = FALSE,
    open = "route",
    bslib::accordion_panel(
      shiny::tagList("1 · ", t_("Route")), value = "route",
      shiny::selectInput("selected_route", t_("Route:"), choices = NULL, width = "100%"),
      shiny::actionButton("load_route", t_("Load route"), class = "btn-sm btn-primary w-100")
    ),
    bslib::accordion_panel(
      shiny::tagList("2 · ", t_("Edit waypoints")), value = "edit",
      shiny::div(class = "mode-hint", t_("Drag: move node. Right-click: delete node.")),
      shiny::checkboxInput("insert_waypoint_mode", t_("Insert waypoint mode"), FALSE),
      shiny::div(class = "mode-hint", t_("Click the map to insert")),
      shiny::checkboxInput("node_selection_mode", t_("Selection mode (lasso)"), FALSE),
      shiny::div(
        class = "btn-row",
        shiny::actionButton("delete_selected_nodes", t_("Delete sel."), class = "btn-sm btn-outline-danger"),
        shiny::actionButton("clear_node_selection", t_("Clear sel."), class = "btn-sm btn-outline-secondary")
      )
    )
  ),
  mode_actions(
    shiny::actionButton("apply_route_edits", t_("Apply"), class = "btn-sm btn-success"),
    shiny::actionButton("cancel_route_edits", t_("Cancel"), class = "btn-sm btn-outline-danger")
  )
)

batch_review_pane <- shiny::tagList(
  mode_hint(
    "Fix trips that stray off the rails or roads.",
    "Trips are measured against the rails (metro, train, tram) or the roads matched by the local OSRM (car, taxi, bus). Accepted fixes are exported as one GPX with a track per trip: in Arc, Import GPX File replaces the overlapping data. Keys: A accept, S skip, R re-route."
  ),
  shiny::uiOutput("review_resume"),
  bslib::accordion(
    id = "review_steps",
    multiple = FALSE,
    open = "find",
    bslib::accordion_panel(
      shiny::tagList("1 · ", t_("Find")), value = "find",
      shiny::selectizeInput("review_activities", NULL, choices = tr_choices(REVIEW_ACTIVITY_CHOICES),
                            selected = "metro", multiple = TRUE, width = "100%",
                            options = list(plugins = list("remove_button"))),
      shiny::div(
        class = "range-row",
        shiny::span(class = "dt-label", t_("From")),
        date_input("review_from", NULL, as.Date(format(Sys.Date(), "%Y-01-01")), width = "100%"),
        shiny::span(class = "dt-label", t_("To")),
        date_input("review_to", NULL, Sys.Date(), width = "100%")
      ),
      if (icloud_available()) {
        shiny::checkboxInput("review_fetch_icloud", t_("Fetch everything from iCloud first"), FALSE)
      },
      bslib::input_task_button("review_find", t_("Find trips"), label_busy = t_("Searching..."), class = "btn-sm w-100"),
      shiny::uiOutput("review_summary")
    ),
    bslib::accordion_panel(
      shiny::tagList("2 · ", t_("Review")), value = "review",
      shiny::div(
        class = "review-toolbar",
        shiny::checkboxInput("review_only_flagged", t_("Only flagged"), TRUE),
        shiny::selectInput("review_sort", NULL, choices = tr_choices(REVIEW_SORT_CHOICES), selected = "date",
                           width = "106px")
      ),
      shiny::div(id = "review_list_box", class = "review-list", shiny::uiOutput("review_list")),
      shiny::uiOutput("review_detail"),
      shiny::div(
        class = "btn-row review-actions",
        shiny::actionButton("review_accept", t_("Accept (A)"), class = "btn-sm btn-success"),
        shiny::actionButton("review_skip", t_("Skip (S)"), class = "btn-sm btn-outline-secondary"),
        shiny::actionButton("review_reroute", t_("Re-route (R)"), class = "btn-sm btn-outline-primary")
      ),
      shiny::actionLink("review_keep", t_("Use the recorded path as is"), class = "review-keep"),
      shiny::uiOutput("review_bulk")
    ),
    bslib::accordion_panel(
      shiny::tagList("3 · ", t_("Export")), value = "export",
      shiny::uiOutput("review_export_count"),
      shiny::checkboxInput("review_only_checked", t_("Send only checked"), FALSE),
      if (icloud_available()) {
        shiny::tagList(
          shiny::div(
            class = "review-notes-opts",
            title = tr_lang("With Send: the split trips' Jaé notes (bus: SMTR data) go to iCloud Import as a notes package."),
            `data-i18n-title` = "With Send: the split trips' Jaé notes (bus: SMTR data) go to iCloud Import as a notes package.",
            shiny::checkboxInput("review_jae_notes", t_("Add Jaé notes"), TRUE),
            shiny::checkboxInput("review_wait_note", t_("Wait note"), TRUE)
          ),
          shiny::div(
            class = "mb-1",
            bslib::input_task_button("review_send_icloud", t_("Send batch GPX to iCloud"), label_busy = t_("Sending..."),
                                     class = "btn-sm w-100")
          )
        )
      },
      shiny::downloadButton("review_download", t_("Download batch GPX"), class = "btn-sm btn-outline-secondary w-100")
    )
  )
)

export_pane <- shiny::tagList(
  shiny::div(
    class = "pane-section",
    shiny::div(
      class = "pane-title",
      t_("Day timeline GPX"),
      help_icon("One track per trip (visits are skipped). In Arc, Import GPX File adds the trips and disables and hides the overlapping data, so it can replace badly recorded trips. The Arc package below only adds data.")
    ),
    if (icloud_available()) {
      shiny::div(
        class = "mb-1",
        bslib::input_task_button("send_timeline_gpx_icloud", t_("Send timeline GPX to iCloud"), label_busy = t_("Sending..."), class = "btn-sm w-100")
      )
    },
    shiny::downloadButton("download_timeline_gpx", t_("Download timeline GPX"), class = "btn-sm btn-outline-secondary w-100")
  ),
  shiny::div(
    class = "pane-section",
    shiny::div(
      class = "pane-title",
      t_("Day timeline package"),
      help_icon("Export as a LocoKit2 JSON zip (items + samples + metadata), ready for Arc Editor import.")
    ),
    shiny::downloadButton("download_arc", t_("Download Arc package (zip)"), class = "btn-sm btn-success w-100"),
    if (icloud_available()) {
      shiny::div(
        class = "mt-1",
        bslib::input_task_button("send_arc_icloud", t_("Send Arc package to iCloud"), label_busy = t_("Sending..."), type = "outline-primary", class = "btn-sm w-100")
      )
    }
  ),
  shiny::div(
    class = "mode-hint",
    t_("Corrected routes (GPX / JSON): Edit → Edit samples → 3 · Export. All samples as GPX: ⚙ menu.")
  )
)

# ---- Top bar pieces ----

gear_menu <- shiny::div(
  class = "dropdown",
  shiny::tags$button(
    type = "button", class = "btn btn-sm btn-outline-secondary",
    `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
    title = tr_lang("More"), `data-i18n-title` = "More",
    "⚙"
  ),
  shiny::tags$ul(
    class = "dropdown-menu dropdown-menu-end",
    shiny::tags$li(
      shiny::downloadLink("download_gpx", t_("Download GPX (all samples)"), class = "dropdown-item")
    ),
    shiny::tags$li(shiny::actionLink("rebuild_rail_reference", t_("Rebuild rail reference"), class = "dropdown-item")),
    shiny::tags$li(shiny::tags$hr(class = "dropdown-divider")),
    shiny::tags$li(shiny::tags$h6(class = "dropdown-header", t_("Help (README)"))),
    lapply(
      list(
        c(i18n_key("Replacing an existing route"), "#replacing-an-existing-route"),
        c(i18n_key("iCloud sync (rclone)"), "#icloud-sync-optional-via-rclone"),
        c(i18n_key("Importing into Arc Editor"), "#importing-into-arc-editor"),
        c(i18n_key("Setup"), "#setup")
      ),
      function(x) {
        shiny::tags$li(shiny::tags$a(class = "dropdown-item", href = paste0(README_URL, x[2]), target = "_blank", t_(x[1])))
      }
    ),
    shiny::tags$li(shiny::tags$hr(class = "dropdown-divider")),
    shiny::tags$li(
      shiny::actionLink("clear_all", t_("Clear all"), class = "dropdown-item text-danger")
    )
  )
)

ui <- bslib::page_fillable(
  title = "Arc Timeline Builder",
  theme = bslib::bs_theme(version = 5, `font-size-base` = "0.875rem"),
  padding = 0,
  gap = 0,
  shiny::tags$head(shiny::tags$style(shiny::HTML(APP_CSS))),

  # ---- Top bar ----
  shiny::div(
    class = "app-topbar",
    shiny::span(class = "app-name", "Arc Timeline Builder"),
    shiny::div(
      class = "topbar-date",
      shiny::span(class = "topbar-label", t_("Working date")),
      date_input("work_date", NULL, Sys.Date(), width = "120px")
    ),
    shiny::div(
      class = "topbar-right",
      shiny::div(id = "status_dots_wrap", class = "status-dots", shiny::uiOutput("status_dots", inline = TRUE)),
      # Language toggle (client-side; remembered in localStorage)
      shiny::div(
        class = "btn-group btn-group-sm lang-toggle", role = "group", `aria-label` = "Language",
        shiny::tags$button(type = "button", class = "btn btn-outline-secondary", `data-lang` = "pt", "PT"),
        shiny::tags$button(type = "button", class = "btn btn-outline-secondary", `data-lang` = "en", "EN")
      ),
      gear_menu
    )
  ),

  # ---- Main area: left sidebar | map | right sidebar ----
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      id = "left_sidebar",
      width = 320,
      open = "desktop",
      padding = c(6, 10),
      gap = 0,
      # Hidden real input: same values as the old "Edit mode" radio buttons
      shiny::div(
        class = "d-none",
        shiny::radioButtons(
          "mode", NULL,
          choices = c("OSRM", "Visit", "Manual", "Import", "EditSamples", "EditRoute", "BatchReview"),
          selected = "OSRM"
        )
      ),
      bslib::navset_underline(
        id = "nav_section",
        bslib::nav_panel(
          t_("Create"), value = "create",
          mode_segments(
            "create_mode_seg",
            c("Route" = "OSRM", "Visit" = "Visit", "Manual" = "Manual", "Import file" = "Import"),
            "OSRM"
          ),
          shiny::conditionalPanel("input.mode == 'OSRM'", osrm_pane),
          shiny::conditionalPanel("input.mode == 'Visit'", visit_pane),
          shiny::conditionalPanel("input.mode == 'Manual'", manual_pane),
          shiny::conditionalPanel("input.mode == 'Import'", import_pane)
        ),
        bslib::nav_panel(
          t_("Edit"), value = "edit",
          mode_segments(
            "edit_mode_seg",
            c("Edit samples" = "EditSamples", "Edit route" = "EditRoute", "Batch review" = "BatchReview"),
            "EditSamples"
          ),
          shiny::conditionalPanel("input.mode == 'EditSamples'", edit_samples_pane),
          shiny::conditionalPanel("input.mode == 'EditRoute'", edit_route_pane),
          shiny::conditionalPanel("input.mode == 'BatchReview'", batch_review_pane)
        ),
        bslib::nav_panel(t_("Export"), value = "export", export_pane)
      )
    ),
    padding = 0,
    gap = 0,
    border = FALSE,
    border_radius = FALSE,
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        id = "right_sidebar",
        position = "right",
        width = 280,
        open = "desktop",
        title = shiny::div(
          class = "timeline-title",
          t_("Day timeline"),
          help_icon("Timeline items for the current working date. Click Edit to change times, Delete to remove. Items are sorted by start time.")
        ),
        shiny::div(class = "timeline-scroll", shiny::uiOutput("timeline_list"))
      ),
      padding = 0,
      gap = 0,
      border = FALSE,
      border_radius = FALSE,
      leaflet::leafletOutput("map", height = "100%"),
      if (!MAP_RASTER_BASE) htmltools::attachDependencies(shiny::tags$span(class = "d-none"), MAPLIBRE_DEPS)
    )
  ),

  # Context menu for samples
  shiny::div(
    id = "sample-context-menu",
    class = "sample-context-menu",
    shiny::div(id = "ctx-properties", class = "sample-context-menu-item", t_("Properties")),
    shiny::div(class = "sample-context-menu-separator"),
    shiny::div(id = "ctx-ignore", class = "sample-context-menu-item", t_("Ignore")),
    shiny::div(id = "ctx-insert", class = "sample-context-menu-item", t_("Insert sample after")),
    shiny::div(class = "sample-context-menu-separator"),
    shiny::div(id = "ctx-discard", class = "sample-context-menu-item danger", t_("Discard"))
  ),

  # Context menu for route nodes
  shiny::div(
    id = "route-node-context-menu",
    class = "sample-context-menu",
    shiny::div(id = "ctx-node-props", class = "sample-context-menu-item", t_("Node properties")),
    shiny::div(class = "sample-context-menu-separator"),
    shiny::div(id = "ctx-delete-node", class = "sample-context-menu-item danger", t_("Delete node"))
  ),

  # Context menu for recorded samples in Batch review
  shiny::div(
    id = "review-context-menu",
    class = "sample-context-menu",
    shiny::div(class = "sample-context-menu-item", `data-endpoint` = "start", t_("Set as start")),
    shiny::div(class = "sample-context-menu-item", `data-endpoint` = "end", t_("Set as end"))
  ),

  # JavaScript for Batch review: trip list, keys, endpoint menu, list height
  shiny::tags$script(shiny::HTML("
    $(document).ready(function() {
      // Rows: click selects, checkbox toggles 'checked' (kept server-side)
      $(document).on('click', '.review-row', function(e) {
        if ($(e.target).is('input')) return;
        Shiny.setInputValue('review_select', $(this).attr('data-id'), {priority: 'event'});
      });
      $(document).on('change', '.review-row input.rv-check', function() {
        Shiny.setInputValue('review_check', {id: $(this).closest('.review-row').attr('data-id'),
                                             checked: this.checked, ts: Date.now()}, {priority: 'event'});
      });

      // Fine time adjustments: data-review-adj='start:-60'
      $(document).on('click', '[data-review-adj]', function() {
        var parts = $(this).attr('data-review-adj').split(':');
        Shiny.setInputValue('review_time_adj', {which: parts[0], delta: parseInt(parts[1], 10), ts: Date.now()},
                            {priority: 'event'});
      });

      // Road detours: pick the matched path or the one through the recording
      $(document).on('click', '[data-review-pick]', function() {
        Shiny.setInputValue('review_pick', {key: $(this).attr('data-review-pick'), ts: Date.now()}, {priority: 'event'});
      });

      // Split at the Jaé tap: on / off
      $(document).on('click', '[data-review-split]', function() {
        Shiny.setInputValue('review_split', {action: $(this).attr('data-review-split'), ts: Date.now()}, {priority: 'event'});
      });

      // Control pins: pin mode on / off, clear; an ambiguous pin placed by hand
      $(document).on('click', '[data-review-pin]', function(e) {
        e.preventDefault();
        Shiny.setInputValue('review_pin', {action: $(this).attr('data-review-pin'), ts: Date.now()}, {priority: 'event'});
      });
      $(document).on('click', '[data-edit-pin]', function(e) {
        e.preventDefault();
        Shiny.setInputValue('edit_pin', {action: $(this).attr('data-edit-pin'), ts: Date.now()}, {priority: 'event'});
      });
      $(document).on('change', 'select[data-review-pin-after]', function() {
        Shiny.setInputValue('review_pin_after', {pin: $(this).attr('data-review-pin-after'), after: $(this).val(), ts: Date.now()},
                            {priority: 'event'});
      });
      // Pin markers (Batch review: review_pins; Edit Samples: edit_pins): drag moves, right-click removes
      window.bindPinMarker = function(layer, prefix) {
        var pinId = String(layer.options.layerId).replace(/^pin_/, '');
        layer.off('dragend');
        layer.off('contextmenu');
        layer.on('dragend', function() {
          var ll = layer.getLatLng();
          Shiny.setInputValue(prefix + '_moved', {id: pinId, lat: ll.lat, lng: ll.lng, ts: Date.now()}, {priority: 'event'});
        });
        layer.on('contextmenu', function(e) {
          L.DomEvent.stopPropagation(e);
          L.DomEvent.preventDefault(e);
          Shiny.setInputValue(prefix + '_remove', {id: pinId, ts: Date.now()}, {priority: 'event'});
        });
      };
      Shiny.addCustomMessageHandler('pin_mode', function(msg) {
        $('#map').toggleClass('pin-mode', !!msg.on);
        if (msg.scope === 'edit') $('#edit_pin_toggle').toggleClass('btn-primary', !!msg.on).toggleClass('btn-outline-secondary', !msg.on);
      });

      // Keys: A accept, S skip, R re-route (Batch review only, not while typing)
      $(document).on('keydown', function(e) {
        var mode = $('#mode input:checked').val();
        if (mode !== 'BatchReview' && mode !== 'EditSamples') return;
        if (e.ctrlKey || e.metaKey || e.altKey || $('.modal.show').length) return;
        if ($(e.target).is('input, textarea, select, [contenteditable]')) return;
        if (mode === 'EditSamples') {
          if (String(e.key).toLowerCase() === 'p') { e.preventDefault(); $('#edit_pin_toggle').click(); }
          return;
        }
        var id = {a: 'review_accept', s: 'review_skip', r: 'review_reroute', p: 'review_pin_toggle'}[String(e.key).toLowerCase()];
        if (id) {
          e.preventDefault();
          $('#' + id).click();
        }
      });

      // Right-click on a recorded sample: set as start / end
      var $rvMenu = $('#review-context-menu');
      var rvSampleIdx = null;
      $(document).on('click', function() { $rvMenu.removeClass('show'); });
      $rvMenu.on('click', function(e) { e.stopPropagation(); });
      $rvMenu.find('[data-endpoint]').on('click', function() {
        if (rvSampleIdx !== null) {
          Shiny.setInputValue('review_set_endpoint', {which: $(this).attr('data-endpoint'), idx: rvSampleIdx,
                                                      ts: Date.now()}, {priority: 'event'});
        }
        $rvMenu.removeClass('show');
      });
      function bindReviewSample(layer) {
        layer.off('contextmenu');
        layer.on('contextmenu', function(e) {
          L.DomEvent.stopPropagation(e);
          L.DomEvent.preventDefault(e);
          rvSampleIdx = parseInt(String(layer.options.layerId).replace('rvs_', ''), 10);
          $rvMenu.css({left: e.originalEvent.pageX + 'px', top: e.originalEvent.pageY + 'px'}).addClass('show');
        });
      }
      function hookReviewMap() {
        var widget = HTMLWidgets.find('#map');
        var map = widget && widget.getMap();
        if (!map) { setTimeout(hookReviewMap, 500); return; }
        if (map._reviewHook) return;
        map._reviewHook = true;
        map.on('layeradd', function(e) {
          var layer = e.layer;
          if (layer.options && layer.options.group === 'review_samples' && layer.options.layerId) bindReviewSample(layer);
          if (layer.options && layer.options.layerId && (layer.options.group === 'review_pins' || layer.options.group === 'edit_pins')) {
            bindPinMarker(layer, layer.options.group === 'review_pins' ? 'review_pin' : 'edit_pin');
          }
        });
      }
      $(document).on('shiny:connected', function() { setTimeout(hookReviewMap, 500); });

      // The trip list takes whatever height the sidebar has left (no page or
      // sidebar scroll); the list itself scrolls
      window.fitReviewList = function() {
        var box = document.getElementById('review_list_box');
        var aside = document.getElementById('left_sidebar');
        var sb = aside && aside.querySelector(':scope > .sidebar-content');
        if (!box || !sb || !box.offsetParent) return;
        // A selected row in view stays in view when the list shrinks (the detail grew)
        var row = box.querySelector('.review-row.selected');
        var below = function() { return row.getBoundingClientRect().bottom - box.getBoundingClientRect().bottom; };
        var keep = row && below() <= 1 && row.getBoundingClientRect().top >= box.getBoundingClientRect().top - 1;
        // .sidebar-content is as tall as its content; the aside is the full column
        box.style.height = '60px';
        var limit = aside.getBoundingClientRect().bottom - (parseFloat(getComputedStyle(sb).paddingBottom) || 0);
        var bottom = sb.getBoundingClientRect().top;
        Array.prototype.forEach.call(sb.children, function(c) {
          if (c.offsetParent) bottom = Math.max(bottom, c.getBoundingClientRect().bottom);
        });
        box.style.height = Math.max(60, 60 + Math.floor(limit - bottom) - 2) + 'px';
        if (keep && below() > 0) box.scrollTop += Math.ceil(below()) + 1;
      };
      var fitSoon = function() { setTimeout(window.fitReviewList, 60); };
      $(document).on('shiny:value', function(e) {
        if (!/^review_/.test(e.name)) return;
        fitSoon();
        if (e.name === 'review_list') {
          setTimeout(function() {
            var row = document.querySelector('.review-row.selected');
            if (row) row.scrollIntoView({block: 'nearest'});
          }, 80);
        }
      });
      $(document).on('shown.bs.collapse hidden.bs.collapse', '#review_steps', fitSoon);
      $(document).on('change', '#mode input', fitSoon);
      $(window).on('resize', fitSoon);
    });
  ")),

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
      $('#ctx-properties').on('click', function() {
        if (currentSampleId) {
          Shiny.setInputValue('ctx_properties', {id: currentSampleId, ts: Date.now()});
        }
        $menu.removeClass('show');
      });

      $('#ctx-ignore').on('click', function() {
        if (currentSampleId) {
          Shiny.setInputValue('ctx_ignore', {id: currentSampleId, ts: Date.now()});
        }
        $menu.removeClass('show');
      });

      $('#ctx-insert').on('click', function() {
        if (currentSampleId) {
          Shiny.setInputValue('ctx_insert', {id: currentSampleId, ts: Date.now()});
        }
        $menu.removeClass('show');
      });

      $('#ctx-discard').on('click', function() {
        if (currentSampleId) {
          Shiny.setInputValue('ctx_discard', {id: currentSampleId, ts: Date.now()});
        }
        $menu.removeClass('show');
      });

      // Expose global function to show menu (called from Shiny)
      window.showSampleContextMenu = function(sampleId, x, y) {
        currentSampleId = sampleId;
        $menu.css({left: x + 'px', top: y + 'px'}).addClass('show');
      };

      // Add handlers on a sample Marker (draggable, with layerId)
      function bindSampleMarker(layer) {
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

      // Handler for Shiny message to set up context menu
      Shiny.addCustomMessageHandler('setup_context_menu', function(msg) {
        setTimeout(function() {
          var widget = HTMLWidgets.find('#map');
          if (!widget) return;
          var map = widget.getMap();
          if (!map) return;

          map.eachLayer(function(layer) {
            if (layer.options && layer.options.layerId && layer.options.draggable &&
                layer.options.group !== 'review_pins' && layer.options.group !== 'edit_pins') {
              bindSampleMarker(layer);
            }
          });

          // Markers can arrive after this message (leafletProxy calls are sent
          // at the end of the flush): also bind sample markers as they are added
          if (!map._sampleMarkerHook) {
            map._sampleMarkerHook = true;
            map.on('layeradd', function(e) {
              var layer = e.layer;
              if (layer.options && layer.options.layerId && layer.options.draggable &&
                  layer.options.group === 'edit_samples') {
                bindSampleMarker(layer);
              }
            });
          }
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
        if (e.name === 'selection_mode') {
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

      // Add handlers on a route node Marker (group edit_route_nodes)
      function bindRouteNode(layer) {
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

      function isRouteNode(layer) {
        return layer.options && layer.options.layerId && layer.options.draggable &&
          layer.options.group === 'edit_route_nodes';
      }

      // Handler to set up route nodes (called from Shiny)
      Shiny.addCustomMessageHandler('setup_route_nodes', function(msg) {
        setTimeout(function() {
          var widget = HTMLWidgets.find('#map');
          if (!widget) return;
          var map = widget.getMap();
          if (!map) return;

          map.eachLayer(function(layer) {
            if (isRouteNode(layer)) bindRouteNode(layer);
          });

          // Nodes can arrive after this message: also bind them as they are added
          if (!map._routeNodeHook) {
            map._routeNodeHook = true;
            map.on('layeradd', function(e) {
              if (isRouteNode(e.layer)) bindRouteNode(e.layer);
            });
          }
        }, 500);
      });

      // Listen to insert waypoint mode checkbox changes
      $(document).on('shiny:inputchanged', function(e) {
        if (e.name === 'insert_waypoint_mode') {
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
        if (e.name === 'node_selection_mode') {
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

      // Handler to clear node selection
      Shiny.addCustomMessageHandler('clear_node_selection', function(msg) {
        selectedNodeIds = [];
        Shiny.setInputValue('selected_route_nodes', []);
        updateNodeSelectionVisual();
      });
    });
  "))
,

  # JavaScript for the layout: navigation -> input$mode, status dots, map resize
  shiny::tags$script(shiny::HTML("
    $(document).ready(function() {
      // ---- Navigation -> input$mode (hidden radio, same values as before) ----
      function setMode(value) {
        if (!value) return;
        var $radio = $('#mode input[type=radio][value=\"' + value + '\"]');
        if ($radio.length && !$radio.prop('checked')) {
          $radio.prop('checked', true).trigger('change');
        }
      }
      function segmentValue(name) {
        return $('input[name=\"' + name + '\"]:checked').val();
      }
      $(document).on('change', 'input[name=\"create_mode_seg\"], input[name=\"edit_mode_seg\"]', function() {
        setMode(this.value);
      });

      // ---- Jaé boardings: one input for every row's button ----
      $(document).on('click', '[data-jae-start]', function() {
        Shiny.setInputValue('jae_start', $(this).attr('data-jae-start'), {priority: 'event'});
      });
      // Starting a route from a boarding outside Create > Route: select the
      // Route segment, show the Create tab, then set the mode
      Shiny.addCustomMessageHandler('jae_route_mode', function(msg) {
        $('#seg_OSRM').prop('checked', true);
        var tab = document.querySelector('#nav_section [data-value=\"create\"]');
        if (tab && !tab.classList.contains('active')) bootstrap.Tab.getOrCreateInstance(tab).show();
        setMode('OSRM');
      });
      $(document).on('shown.bs.tab', '#nav_section [data-bs-toggle=\"tab\"]', function(e) {
        var section = $(e.target).attr('data-value');
        if (section === 'create') setMode(segmentValue('create_mode_seg'));
        if (section === 'edit') setMode(segmentValue('edit_mode_seg'));
      });

      // ---- Status dots: first check after load, re-check on click ----
      $(document).on('shiny:connected', function() {
        setTimeout(function() {
          Shiny.setInputValue('refresh_status', {force: false, t: Date.now()});
        }, 800);
      });
      $(document).on('click', '#status_dots_wrap', function() {
        Shiny.setInputValue('refresh_status', {force: true, t: Date.now()});
      });

      // ---- Map resize when a sidebar opens/closes ----
      document.addEventListener('bslib.sidebar', function() {
        setTimeout(function() {
          var widget = HTMLWidgets.find('#map');
          var map = widget && widget.getMap();
          if (map) map.invalidateSize();
        }, 350);
      });
    });
  ")),

  # Route editing by dragging, sample lasso, Ctrl+Z, map overlays (ui_route_edit_js.R)
  shiny::tags$script(shiny::HTML(ROUTE_EDIT_JS)),

  # Translations for the client-side language switch
  shiny::tags$script(shiny::HTML(paste0(
    "window.I18N = ", jsonlite::toJSON(I18N, auto_unbox = TRUE), ";\n",
    "window.I18N_DEFAULT = '", APP_LANGUAGE, "';"
  ))),

  # JavaScript for the language toggle (PT | EN): swaps texts in place, so no
  # reload and no lost state; the choice is remembered in localStorage
  shiny::tags$script(shiny::HTML("
    (function() {
      function i18nText(key, lang) {
        var entry = window.I18N[key];
        return (entry && entry[lang]) || key;
      }

      function storedLang() {
        try {
          var lang = localStorage.getItem('arc_app_lang');
          if (lang === 'pt' || lang === 'en') return lang;
        } catch (e) {}
        return window.I18N_DEFAULT;
      }

      // Date pickers (bootstrap-datepicker as $.fn.bsDatepicker). Set before Shiny
      // creates them (this script runs first), so pickers in modals get it too:
      // 3-letter weekdays in pt-BR (its 2-letter daysMin repeats Qu Qu / Se Se),
      // and a z-index above the Leaflet controls (1000) and the sidebar toggles.
      if ($.fn.bsDatepicker) {
        $.fn.bsDatepicker.dates['pt-BR'].daysMin = ['Dom', 'Seg', 'Ter', 'Qua', 'Qui', 'Sex', 'Sáb'];
        $.fn.bsDatepicker.dates['en'].daysMin = ['Su', 'Mo', 'Tu', 'We', 'Th', 'Fr', 'Sa'];
        $.fn.bsDatepicker.defaults.zIndexOffset = 1060;
      }
      // Keep the popup inside the window: right-aligned next to a narrow date
      // field (sidebar rows) it can start a few px past the left edge
      $(document).on('show', '.shiny-date-input input', function() {
        var dp = $(this).data('datepicker');
        if (dp && parseFloat(dp.picker.css('left')) < 4) dp.picker.css('left', 4);
      });

      // Date pickers: dd/mm/yyyy (pt-BR) or yyyy-mm-dd (en); value unchanged.
      // Idempotent: applyI18n() runs several times (load, map ready, language switch)
      function relocalizeDatepickers(lang) {
        var opts = lang === 'pt' ? {language: 'pt-BR', format: 'dd/mm/yyyy'} : {language: 'en', format: 'yyyy-mm-dd'};
        $('.shiny-date-input input').each(function() {
          var $input = $(this);
          var dp = $input.data('datepicker');
          $input.attr('data-date-language', opts.language).attr('data-date-format', opts.format);
          $input.attr('title', i18nText('Date format:', lang) + ' ' + opts.format);
          if (!dp) return;
          var $dowRows = dp.picker.find('.datepicker-days thead tr').has('th.dow');
          if (dp.o.language === opts.language && $dowRows.length === 1) return;
          var date = $input.bsDatepicker('getUTCDate');
          dp._process_options(opts);
          $dowRows.remove();  // fillDow() appends a row: drop the old one first
          dp.fillDow();
          dp.fillMonths();
          if (date) $input.bsDatepicker('setUTCDate', date);
          dp.fill();
        });
      }

      window.applyI18n = function(lang) {
        lang = lang || storedLang();
        window.APP_LANG = lang;
        document.documentElement.lang = lang === 'pt' ? 'pt-BR' : 'en';
        document.querySelectorAll('[data-i18n]').forEach(function(el) {
          el.textContent = i18nText(el.getAttribute('data-i18n'), lang);
        });
        document.querySelectorAll('[data-i18n-title]').forEach(function(el) {
          el.setAttribute('title', i18nText(el.getAttribute('data-i18n-title'), lang));
        });
        // Leaflet controls (group names stay English identifiers for the server)
        $('.leaflet-control-layers-selector + span').each(function() {
          var $span = $(this);
          var key = $span.attr('data-layer-key') || $.trim($span.text());
          $span.attr('data-layer-key', key).text(' ' + i18nText(key, lang));
        });
        $('.leaflet-control-zoom-in').attr('title', i18nText('Zoom in', lang));
        $('.leaflet-control-zoom-out').attr('title', i18nText('Zoom out', lang));
        relocalizeDatepickers(lang);
        $('.lang-toggle [data-lang]').each(function() {
          $(this).toggleClass('active', $(this).attr('data-lang') === lang);
        });
      };

      $(document).on('click', '.lang-toggle [data-lang]', function() {
        var lang = $(this).attr('data-lang');
        try { localStorage.setItem('arc_app_lang', lang); } catch (e) {}
        window.applyI18n(lang);
        Shiny.setInputValue('app_lang', lang);
      });

      // Inputs (and their date pickers) exist once Shiny has connected
      $(document).on('shiny:connected', function() {
        var lang = storedLang();
        window.applyI18n(lang);
        Shiny.setInputValue('app_lang', lang);
      });

      $(function() { window.applyI18n(storedLang()); });
    })();
  "))
)
