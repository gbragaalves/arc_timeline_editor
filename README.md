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
| **Edit Samples** | Load existing samples from weekly `.json.gz` backups (optionally fetched from iCloud). Drag, ignore, lasso-select, snap-to-road, export as GPX or JSON. |
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

In **Edit Samples** mode, the corrected route is exported first as GPX (**Send GPX to iCloud** / **Export GPX**), which is the way to replace a bad route (see [Replacing an existing route](#replacing-an-existing-route)). Below it, under **Arc JSON package**, a mode selector controls how **Export (zip)** / **Send to iCloud (Import)** write the route:

| Mode | Item | Use |
|------|------|-----|
| **Add** (default) | New ID, enabled | Corrected route added alongside the original; mainly for filling empty periods |
| **Disabled item** | New ID, item and samples `disabled` | Imports fine but stays hidden: Arc has no UI to enable it |
| **Reuse original item ID** | Original ID | Not working yet: needs upsert support in the importer |

Exported samples always get new IDs and the original trip's activity type, and the item gets real distance and speed. The GPX contains the same samples as the zip, with the activity type in `<trk><type>`.

### Importing into Arc Editor

1. In the Shiny app, open the **Export** tab and click **Download Arc package (zip)** (for Edit Samples: **Edit → Edit samples → 3 · Export → Arc JSON package → Export (zip)**)
2. The zip is named `Import.zip` — extract it to get the `Import` folder
3. Upload the `Import` folder to **iCloud Drive**, then move it inside the **Arc Editor** folder
4. In Arc Editor on your device: **Menu > Backup & Restore > Restore from Backup**
5. Select the `Import` folder and tap **Open**
6. After import, Arc will run an automatic incremental backup that includes the new data

With [iCloud sync](#icloud-sync-optional-via-rclone) set up, **Send Arc package to iCloud** / **Send to iCloud (Import)** replace steps 2–3: the package is written straight to `Arc Timeline/Import`.

> **Important — existing data is never modified by import.** See [How the LocoKit2 Import Works](#how-the-locokit2-import-works) below for details.

### Other features

- **Overlap detection** — prevents overlapping timeline items
- **Auto-suggest times** — next item starts 1 minute after the previous one
- **Multi-timezone** — detects timezone from coordinates
- **GPX export** — the day's trips (Export tab: **Send timeline GPX to iCloud** / **Download timeline GPX**), one track per trip with its activity type and altitude; all samples including visits: ⚙ menu
- **OpenFreeMap base map** — vector base layer by default (falls back to OSM if it can't load); OSM, CartoDB, Satellite and Topo in the layers control
- **No page scrolling** — the layout fits the browser window (1366×768 and up): Create / Edit / Export tabs on the left, the map in the middle, the day timeline on the right; both sidebars can be collapsed
- **Portuguese / English** — see [Language](#language)

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
- **Old route remains after importing a correction** — the corrected route is added as a new separate item; the original stays in the database. Use the GPX workflow below instead
- **`deleted: true` on items has no effect during import** — the item already exists with the same ID, so the import record is ignored entirely
- **`deleted` field on samples is not recognized by LocoKit2** — the `LocomotionSample` model uses `disabled` instead (inherited from the LocoKit1 -> LocoKit2 migration)
- **Marking items as "bogus" in Arc Editor** changes the activity type but does not remove the item or its samples from the timeline

### Replacing an existing route

Verified on device (Arc Timeline 4): import a **GPX** of the corrected route with Arc's own **Import GPX File**. Arc reads the activity type from `<trk><type>` (e.g. Metro), shows the local time correctly, warns that the GPX overlaps existing timeline data, and then disables and hides the overlapping data, replacing the bad trip. Internally this is LocoKit2's `enableItem(itemId:)` (`TimelineProcessor+EnabledState.swift`): items that overlap the new one are disabled along with their samples, and partial overlaps are split.

1. In Edit Samples mode, load the samples, keep only the boarding and alighting points (Ignore/Discard the rest), and **Snap**. Metro/Train/Tram use Google transit directions (origin to destination only); road modes use OSRM.
2. Click **Send GPX to iCloud** (or **Export GPX** and copy the file to iCloud yourself).
3. In Arc, use **Import GPX File** and pick the file from `Arc Timeline/GPX`.

The existing data is disabled and hidden, not deleted, and Arc can undo the import.

The same works for any trip built in this app, e.g. a flight imported from a FlightRadar24 KML over a badly recorded one: in the **Export** tab, **Send timeline GPX to iCloud** (or **Download timeline GPX**) writes `arc_timeline_<date>_<HHMM>.gpx` with one `<trk>` per trip of the day timeline (visits are skipped), each with its activity type in `<trk><type>` and altitude in `<ele>` when known. Arc imports each track as its own item.

The JSON **Disabled item** mode is not an alternative: the package imports fine (Arc logs the item and samples), but the item stays hidden and Arc has no UI to enable it.

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

Google transit directions don't support waypoints, so transit routes always go from the first point to the last; intermediate points are ignored.

### iCloud sync (optional, via rclone)

Fetches the weekly sample backups and sends import packages / GPX files without moving files by hand. The iCloud controls only appear when rclone and the remote are available.

1. Install [rclone](https://rclone.org/install/) (a version with the `iclouddrive` backend).
2. Run `rclone config` and create a new remote named `icloud`, type `iclouddrive`, with your normal Apple ID password and the 2FA code.
3. Check it with `rclone lsd icloud:`.

The login creates a trust token that lasts about 30 days. When it expires, the app shows "iCloud authorization expired"; renew it in a terminal with `rclone reconnect icloud:`. With **Advanced Data Protection** on, **Access iCloud Data on the Web** must be enabled on the iPhone.

The app reads weekly backups from `Arc Timeline/Backup/<backup-id>/samples/`, picking the backup folder with the most recently modified file. Optional overrides in `config_local.R`:

```r
ICLOUD_REMOTE    <- "icloud"        # rclone remote name
ICLOUD_ARC_DIR   <- "Arc Timeline"  # Arc folder in iCloud Drive
ICLOUD_BACKUP_ID <- "<backup-id>"   # skip auto-detection
```

- **Fetch weeks from iCloud first** (Edit Samples) copies the needed weeks into `WEEKLY_BACKUP_DIR` before loading. Only changed files are downloaded. If it fails, the app loads what is already on disk.
- **Send GPX to iCloud** copies the GPX to `Arc Timeline/GPX/`.
- **Send to iCloud (Import)** / **Send Arc package to iCloud** **replace** the contents of `Arc Timeline/Import` (`rclone sync`), so leftovers from earlier imports are removed.

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

# Default UI language (optional): "pt" (default) or "en"
APP_LANGUAGE <- "pt"
```

3. Run the app:

```r
shiny::runApp()
```

## Language

The interface is available in Brazilian Portuguese and English. Switch with the **PT | EN** toggle in the top bar: texts change in place, without reloading or losing what you were doing, and the browser remembers the choice. The default for a new browser is `APP_LANGUAGE` in `config_local.R` (`"pt"` if unset). Dates are shown as `dd/mm/yyyy` in Portuguese and `yyyy-mm-dd` in English. Arc's own feature names (Import GPX File, Restore from Backup) are kept as Arc shows them.

Translations live in `i18n/translations.json` (English key → `pt` / `en`). After adding or changing user-facing text, run `Rscript i18n/check_translations.R` from the project root: it fails if a key used in the code is missing, a key is unused, or placeholders like `%d` differ between languages.

## Project structure

```
app.R                          # Entry point
i18n/translations.json         # UI translations (PT/EN) + check_translations.R
R/
  utils_time.R                 # Time parsing, timezone detection, interval validation
  utils_geo.R                  # Geographic calculations, FR24 KML parsing
  utils_osrm.R                # OSRM routing, snap-to-road, route smoothing
  utils_google.R              # Google Maps Directions API (transit routes)
  utils_arc.R                 # LocoKit2 data model: samples, items, places, activity types
  utils_weeks.R               # Weekly backup file I/O, overlap detection, sample loading
  utils_icloud.R              # iCloud Drive round-trip via rclone (fetch backups, send imports/GPX)
  utils_i18n.R                # UI language (PT/EN): translation lookup, date formats
  export_arc.R                # LocoKit2 JSON export (zip package generation)
  ui.R                        # Shiny UI layout and controls
  server.R                    # Shiny server logic, editing modes, import/export handlers
```

## License

[MIT](LICENSE)
