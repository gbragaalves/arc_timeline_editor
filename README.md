# Arc Timeline Builder

A [Shiny](https://shiny.posit.co/) web app for manually constructing and editing [Arc App](https://www.bigpaua.com/arcapp) timeline data — visits, trips, and locomotion samples.

## Why?

Arc is great at automatic recording, but sometimes you need to fix or build timeline data manually:

- **Snap-to-road** messy GPS paths using a local [OSRM](http://project-osrm.org/) server
- **Fix metro/subway routes** where GPS was poor underground
- **Fill data gaps** where Arc lost recording
- **Import flight tracks** from FlightRadar24 KML files
- **Build timeline entries from scratch** when the phone was off
- **Edit existing samples** — drag markers, ignore bad points, re-snap entire paths

## Arc Editor migration

This tool originally exported in the LocoKit format used by Arc Timeline. It now targets **Arc Editor** (the ground-up rebuild of Arc Timeline) and generates data in the **LocoKit2 JSON format** (schema 2.2.0).

> The export format matches Arc Editor's LocoKit2 JSON format (schema 2.2.0) and imports successfully via Restore from Backup.

## Features

### Editing modes

| Mode | Description |
|------|-------------|
| **Route** | Click waypoints, select a transport profile (car/foot/bike/bus via OSRM, or metro/train/tram via Google Maps), calculate a snap-to-road route. |
| **Visit** | Mark a point, name it, set entry/exit times. Frequent places auto-fill coordinates. |
| **Manual Route** | Click points for a polyline, set times, pick an activity type. |
| **Import File** | Import GeoJSON, GPX, KML, or GPKG. Special handling for FlightRadar24 KML. |
| **Edit Samples** | Load existing samples from weekly `.json.gz` backups. Drag, ignore, lasso-select, snap-to-road. |
| **Edit Route** | Select a trip, edit its waypoints (drag, delete, insert), apply changes preserving average speed. |

### Activity types

The tool supports all [LocoKit2 activity types](https://github.com/sobri909/LocoKit2/blob/main/Sources/LocoKit2/ActivityTypes/ActivityType.swift):

| Code | Type | Code | Type | Code | Type |
|------|------|------|------|------|------|
| 1 | stationary | 24 | tram | 50 | skateboarding |
| 2 | walking | 25 | tractor | 51 | inline skating |
| 3 | running | 26 | tuk tuk | 52 | snowboarding |
| 4 | cycling | 27 | songthaew | 53 | skiing |
| 5 | car | 28 | scooter | 54 | horseback |
| 6 | airplane | 29 | metro | 55 | swimming |
| 20 | train | 30 | cable car | 56 | golf |
| 21 | bus | 31 | funicular | 57 | wheelchair |
| 22 | motorcycle | 32 | chairlift | 58 | rowing |
| 23 | boat | 33 | ski lift | 59 | kayaking |
| | | 34 | taxi | 60 | surfing |
| | | 35 | hot air balloon | 61 | hiking |

### Export format

Generates a zip in the LocoKit2 format:

```
metadata.json               # Export metadata (schema 2.2.0)
items/2026-03.json          # Timeline items grouped by month
samples/2026-W12.json.gz    # Samples grouped by ISO week (gzipped)
places/0.json               # Places referenced by visits
```

### Importing into Arc Editor

1. In the Shiny app, click the **Download Arc** button (or **Download Edit** if using Edit Samples mode)
2. The zip is named `Import.zip` — extract it to get the `Import` folder
3. Upload the `Import` folder to **iCloud Drive**, then move it inside the **Arc Editor** folder
4. In Arc Editor on your device: **Menu > Backup & Restore > Restore from Backup**
5. Select the `Import` folder and tap **Open**
6. After import, Arc will run an automatic incremental backup that includes the new data

> **Important — existing data is never modified by import.** See [How the LocoKit2 Import Works](#how-the-locokit2-import-works) below for details.

### Other features

- **Location History overlay** — Google Location History for multiple people, with per-person colors
- **Overlap detection** — prevents overlapping timeline items
- **Auto-suggest times** — next item starts 1 minute after the previous one
- **Multi-timezone** — detects timezone from coordinates
- **GPX export** — for use with other tools

## How the LocoKit2 Import Works

These findings come from investigating the [LocoKit2 source code](https://github.com/sobri909/LocoKit2) (`ImportManager.swift`, `SampleImportProcessor.swift`, `ExportMetadata.swift`):

- The **ImportManager** uses `INSERT OR IGNORE` (GRDB's `.ignore` conflict resolution) for **all data types**: places, timeline items (base, visit, trip), and samples
- This means import is **strictly additive** — records with IDs that already exist in the database are silently skipped, never updated
- The `lastSaved` field is used by the **ExportManager** to determine which weekly/monthly buckets to re-export during incremental backups. This comparison happens on the **export** side, not during import
- **Samples** in LocoKit2 use `disabled` instead of `deleted` (which existed in the original LocoKit). The `deleted` field is not part of the `LocomotionSample` model
- **Timeline items** (`TimelineItemBase`) still have a `deleted` field for soft-delete, but since existing records are skipped during import, setting `deleted: true` on an item that already exists has no effect
- The `ExportType` enum in `ExportMetadata.swift` accepts only `"full"` and `"incremental"` — any other value causes a decode error

### Implication for route correction

It is **not possible** to replace or delete existing data via import. When importing a corrected route:

- The corrected route is added as a **new item** alongside the original
- The original item and its samples remain unchanged in the database
- There is no supported mechanism to remove or update existing items/samples through the import pipeline

This is a fundamental constraint of the LocoKit2 ImportManager design (`INSERT OR IGNORE`), not a bug in this tool.

## Known Limitations

- **Cannot replace existing items/samples via import** — the LocoKit2 ImportManager uses `INSERT OR IGNORE`, so records with existing IDs are always skipped
- **Old route remains after importing a correction** — the corrected route is added as a new separate item; the original stays in the database
- **`deleted: true` on items has no effect during import** — the item already exists with the same ID, so the import record is ignored entirely
- **`deleted` field on samples is not recognized by LocoKit2** — the `LocomotionSample` model uses `disabled` instead (inherited from the LocoKit1 -> LocoKit2 migration)
- **Marking items as "bogus" in Arc Editor** changes the activity type but does not remove the item or its samples from the timeline

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

Local OSRM servers via Docker for snap-to-road (car, foot, bike, bus profiles):

| Profile | Port | OSRM profile |
|---------|------|--------------|
| Car | 5000 | driving |
| Foot | 5001 | foot |
| Bike | 5002 | bicycle |
| Bus | 5003 | driving |

See the [OSRM backend guide](https://github.com/Project-OSRM/osrm-backend#using-docker) for setup.

### Google Maps API (optional)

For metro, train, and tram routes, set the `GOOGLE_MAPS_API_KEY` environment variable in your `.Renviron`:

```
GOOGLE_MAPS_API_KEY=your_key_here
```

The key needs the **Directions API** enabled in the [Google Cloud Console](https://console.cloud.google.com/apis/library/directions-backend.googleapis.com).

## Setup

1. Clone this repo
2. Create `config_local.R` (not tracked) with your local settings:

```r
# Path to weekly sample backup files (.json.gz)
WEEKLY_BACKUP_DIR <- "path/to/weekly/samples"

# Frequently-used places (optional)
FREQUENT_PLACES <- list(
  "Home" = list(lat = -23.5505, lon = -46.6333),
  "Work" = list(lat = -23.5600, lon = -46.6500)
)
```

3. Optionally create `R/config_people.R` for Location History overlay:

```r
PEOPLE_CONFIG <- list(
  list(id = "person_1", label = "Person 1", file = "location_history/me.json", color = "#E91E63"),
  list(id = "person_2", label = "Person 2", file = "location_history/other.json", color = "#2196F3")
)
```

4. Run the app:

```r
shiny::runApp()
```

## Project structure

```
app.R                          # Entry point
R/
  utils_time.R                 # Time parsing, timezone detection, interval validation
  utils_geo.R                  # Geographic calculations, FR24 KML parsing, Location History
  utils_osrm.R                # OSRM routing, snap-to-road, route smoothing
  utils_google.R              # Google Maps Directions API (transit routes)
  utils_arc.R                 # LocoKit2 data model: samples, items, places, activity types
  utils_weeks.R               # Weekly backup file I/O, overlap detection, sample loading
  export_arc.R                # LocoKit2 JSON export (zip package generation)
  ui.R                        # Shiny UI layout and controls
  server.R                    # Shiny server logic, editing modes, import/export handlers
```

## License

[MIT](LICENSE)
