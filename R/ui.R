# ---- UI ----
# Layout: compact top bar + collapsible left sidebar (Create / Edit / Export
# tabs) + full-size map + collapsible right sidebar (day timeline). The page
# itself never scrolls; only sidebar contents (e.g. the timeline list) do.
#
# input$mode keeps its original values (OSRM, Visit, Manual, Import,
# EditSamples, EditRoute): it is a hidden radio input driven by the tabs and
# segmented controls (see the navigation script at the end).

README_URL <- "https://github.com/gbragaalves/arc_timeline_editor"
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
  "Car" = "car", "Foot" = "foot", "Bike" = "bike", "Bus" = "bus",
  "Metro" = "metro", "Train" = "train", "Tram/VLT" = "tram"
)

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
    date_input(date_id, NULL, Sys.Date(), width = "100%"),
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
    "Click waypoints on the map, then calculate a snap-to-road route. Car/Foot/Bike/Bus use local OSRM (Docker ports 5000-5003). Metro/Train/Tram use Google Maps Directions API (requires GOOGLE_MAPS_API_KEY)."
  ),
  shiny::selectInput("osrm_profile", t_("Profile:"), choices = tr_choices(PROFILE_CHOICES), selected = "car", width = "100%"),
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
        shiny::selectInput("edit_osrm_profile", NULL, choices = tr_choices(PROFILE_CHOICES), selected = "car", width = "100%"),
        bslib::input_task_button(
          "snap_to_road", t_("Snap"), label_busy = t_("Processing..."), type = "warning", class = "btn-sm",
          title = tr_lang("Snap all active samples to the nearest road via OSRM and generate virtual intermediate points along the route"),
          `data-i18n-title` = "Snap all active samples to the nearest road via OSRM and generate virtual intermediate points along the route"
        )
      ),
      shiny::div(
        class = "d-flex align-items-center",
        shiny::checkboxInput("selection_mode", t_("Selection mode"), FALSE),
        help_icon("Draw a lasso on the map to select multiple samples at once for bulk ignore or discard.")
      ),
      shiny::div(
        class = "btn-row",
        shiny::actionButton("ignore_selected", t_("Ignore"), class = "btn-sm btn-outline-secondary"),
        shiny::actionButton("discard_selected", t_("Discard"), class = "btn-sm btn-outline-danger")
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

export_pane <- shiny::tagList(
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
          choices = c("OSRM", "Visit", "Manual", "Import", "EditSamples", "EditRoute"),
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
            c("Edit samples" = "EditSamples", "Edit route" = "EditRoute"),
            "EditSamples"
          ),
          shiny::conditionalPanel("input.mode == 'EditSamples'", edit_samples_pane),
          shiny::conditionalPanel("input.mode == 'EditRoute'", edit_route_pane)
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
      htmltools::attachDependencies(shiny::tags$span(class = "d-none"), MAPLIBRE_DEPS)
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
            if (layer.options && layer.options.layerId && layer.options.draggable) {
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

      // Date pickers: dd/mm/yyyy (pt-BR) or yyyy-mm-dd (en); value unchanged
      function relocalizeDatepickers(lang) {
        var opts = lang === 'pt' ? {language: 'pt-BR', format: 'dd/mm/yyyy'} : {language: 'en', format: 'yyyy-mm-dd'};
        $('.shiny-date-input input').each(function() {
          var $input = $(this);
          var dp = $input.data('datepicker');
          $input.attr('data-date-language', opts.language).attr('data-date-format', opts.format);
          $input.attr('title', i18nText('Date format:', lang) + ' ' + opts.format);
          if (!dp) return;
          var date = $input.bsDatepicker('getUTCDate');
          dp._process_options(opts);
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
