# Arc Timeline Builder

A [Shiny](https://shiny.posit.co/) web app for manually constructing and editing [Arc App](https://www.bigpaua.com/arcapp) timeline data — visits, trips, and locomotion samples.

## Why?

Arc is great at automatic recording, but sometimes you need to fix or build timeline data manually:

- **Snap-to-road** messy GPS paths using a local [OSRM](http://project-osrm.org/) server
- **Fix metro/subway routes** where GPS was poor underground (see `railway/metro/` for reference geometries)
- **Fill data gaps** where Arc lost recording
- **Import flight tracks** from FlightRadar24 KML files
- **Build timeline entries from scratch** when the phone was off
- **Edit existing samples** — drag markers, ignore bad points, re-snap entire paths

## Arc Editor migration

This tool originally exported in the LocoKit format used by Arc Timeline. It now targets **Arc Editor** (the ground-up rebuild of Arc Timeline) and generates data in the **LocoKit2 JSON format** (schema 2.2.0).

> **Status:** The export format matches Arc Editor's own JSON exports, but we're still working out the correct import mechanism. See the [JSON Import Format](https://support.bigpaua.com/t/json-import-format/219) discussion on the Big Paua forum for context.

## Features

### Editing modes

| Mode | Description |
|------|-------------|
| **OSRM** | Click waypoints, select a transport profile (car/foot/bike/bus), calculate a route via local OSRM. |
| **Visit** | Mark a point, name it, set entry/exit times. Frequent places auto-fill coordinates. |
| **Manual Route** | Click points for a polyline, set times, pick an activity type. |
| **Import File** | Import GeoJSON, GPX, KML, or GPKG. Special handling for FlightRadar24 KML. |
| **Edit Samples** | Load existing samples from weekly `.json.gz` backups. Drag, ignore, lasso-select, snap-to-road. |
| **Edit Route** | Select a trip, edit its waypoints (drag, delete, insert), apply changes preserving average speed. |

### Activity types

All LocoKit2 activity types are supported:

| Code | Type | Code | Type |
|------|------|------|------|
| 1 | stationary | 21 | bus |
| 2 | walking | 24 | tram |
| 4 | cycling | 26 | tuk tuk |
| 5 | car | 29 | metro |
| 6 | airplane | 30 | cable car |
| 20 | train | 31 | funicular |
| 34 | taxi | 50 | skateboarding |

### Export format

Generates a zip in the LocoKit2 format:

```
items/2026-03.json          # Timeline items grouped by month
samples/2026-W12.json.gz    # Samples grouped by ISO week (gzipped)
metadata.json               # Export metadata (schema 2.2.0)
```

### Other features

- **Location History overlay** — Google Location History for multiple people, with per-person colors
- **Overlap detection** — prevents overlapping timeline items
- **Auto-suggest times** — next item starts 1 minute after the previous one
- **Multi-timezone** — detects timezone from coordinates
- **GPX export** — for use with other tools

## Requirements

### R packages

```r
install.packages(c(
  "shiny", "leaflet", "httr", "jsonlite", "sf", "dplyr",
  "lubridate", "uuid", "lutz", "xml2", "stringr", "purrr",
  "geosphere", "zip"
))
```

### OSRM (optional)

Local OSRM servers via Docker for snap-to-road:

| Profile | Port | OSRM profile |
|---------|------|--------------|
| Car | 5000 | driving |
| Foot | 5001 | foot |
| Bike | 5002 | bicycle |
| Bus | 5003 | driving |

See the [OSRM backend guide](https://github.com/Project-OSRM/osrm-backend#using-docker) for setup.

## Setup

1. Clone this repo
2. Create `config_local.R` (not tracked) with your local settings:

```r
# Path to weekly sample backup files (.json.gz)
SEMANA_DIR <- "path/to/weekly/samples"

# Frequently-used places (optional)
LOCAIS_FREQUENTES <- list(
  "Home" = list(lat = -22.88, lon = -43.26),
  "Work" = list(lat = -22.90, lon = -43.17)
)
```

3. Optionally create `R/config_pessoas.R` for Location History overlay (see `config_pessoas.R` for the template)

4. Run the app:

```r
shiny::runApp()
```

## Project structure

```
app.R                          # Entry point
arc_timeline.R                 # Standalone timeline processing script
preprocessar_location_history.R # Location History preprocessor (JSON → RDS cache)
R/
  utils_time.R                 # Time/timezone helpers
  utils_geo.R                  # Geographic parsers, Location History loader
  utils_osrm.R                # OSRM routing, snap-to-road
  utils_arc.R                 # LocoKit2 sample/item creation, activity types
  utils_semanas.R             # Weekly file I/O, overlap detection
  export_arc.R                # LocoKit2 JSON export
  ui.R                        # Shiny UI
  server.R                    # Shiny server
railway/
  metro/                       # Reference geometries for metro lines
```

## License

[MIT](LICENSE)
