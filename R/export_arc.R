# ---- Arc Editor JSON Exporter (LocoKit2 format) ----
# Structure: items/{YYYY-MM}.json, samples/{YYYY-Www}.json.gz, metadata.json

export_arc_json <- function(timeline_items, samples, output_dir, work_date) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  items_dir  <- file.path(output_dir, "items")
  sample_dir <- file.path(output_dir, "samples")
  places_dir <- file.path(output_dir, "places")

  dir.create(items_dir,  recursive = TRUE, showWarnings = FALSE)
  dir.create(sample_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(places_dir, recursive = TRUE, showWarnings = FALSE)

  if (length(timeline_items) == 0 || length(samples) == 0) {
    return(invisible(NULL))
  }

  now_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")

  # ---- Link samples to timeline items ----
  sample_idx <- setNames(
    seq_along(samples),
    vapply(samples, sample_id, "")
  )

  for (i in seq_along(timeline_items)) {
    it <- timeline_items[[i]]
    internal_id <- it$.internalId %||% it$base$id %||% toupper(uuid::UUIDgenerate(use.time = TRUE))

    if (!is.null(it$samples)) {
      for (sid in it$samples) {
        idx <- sample_idx[[sid]]
        if (!is.na(idx)) {
          samples[[idx]]$timelineItemId <- internal_id
        }
      }
    }
  }

  # ---- Items: group by month and generate one JSON per month ----
  items_export <- lapply(timeline_items, function(it) {
    internal_id <- it$.internalId %||% it$base$id
    is_visit <- isTRUE(it$.isVisit) || isTRUE(it$base$isVisit)

    # Build base
    base <- it$base
    if (is.null(base)) {
      # Fallback: build base from old-format fields
      base <- list(
        id              = internal_id,
        source          = "LocoKit2",
        sourceVersion   = "9.0.0",
        isVisit         = is_visit,
        startDate       = it$startDate$date %||% it$startDate,
        endDate         = it$endDate$date %||% it$endDate,
        lastSaved       = now_utc,
        deleted         = FALSE,
        disabled        = FALSE,
        locked          = FALSE,
        samplesChanged  = FALSE,
        stepCount       = 0L,
        activeEnergyBurned = 0
      )
    }
    base$lastSaved <- now_utc

    result <- list(base = base)

    if (is_visit) {
      visit <- it$visit
      if (is.null(visit)) {
        # Fallback: build visit from old-format fields
        lat <- it$.place$latitude %||% it$place$center$latitude %||% 0
        lon <- it$.place$longitude %||% it$place$center$longitude %||% 0
        visit <- list(
          itemId         = internal_id,
          latitude       = lat,
          longitude      = lon,
          radiusMean     = it$place$radius %||% 25,
          radiusSD       = 0,
          confirmedPlace = FALSE,
          uncertainPlace = TRUE,
          lastSaved      = now_utc
        )
        if (!is.null(it$place$name) && nzchar(it$place$name)) {
          visit$customTitle <- it$place$name
        }
      }
      result$visit <- visit
    } else {
      trip <- it$trip
      if (is.null(trip)) {
        at_code <- activity_type_code(it$activityType %||% "car")
        trip <- list(
          itemId                 = internal_id,
          classifiedActivityType = at_code,
          confirmedActivityType  = at_code,
          uncertainActivityType  = FALSE,
          distance               = 0,
          speed                  = 0,
          lastSaved              = now_utc
        )
      }
      result$trip <- trip
    }

    result
  })

  # Group items by month (based on startDate)
  items_by_month <- list()
  for (i in seq_along(items_export)) {
    start_str <- items_export[[i]]$base$startDate
    month_key <- substr(start_str, 1, 7)  # "YYYY-MM"
    if (!month_key %in% names(items_by_month)) {
      items_by_month[[month_key]] <- list()
    }
    items_by_month[[month_key]][[length(items_by_month[[month_key]]) + 1L]] <- items_export[[i]]
  }

  for (month_key in names(items_by_month)) {
    items_path <- file.path(items_dir, paste0(month_key, ".json"))
    jsonlite::write_json(
      items_by_month[[month_key]],
      items_path,
      auto_unbox = TRUE,
      pretty     = TRUE,
      digits     = NA
    )
  }

  # ---- Samples: group by ISO week and generate .json.gz per week ----
  samples_by_week <- list()
  for (s in samples) {
    ts <- tryCatch(
      as.POSIXct(s$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      error = function(e) NULL
    )
    if (is.null(ts) || is.na(ts)) next

    week_key <- strftime(ts, "%G-W%V")
    if (!week_key %in% names(samples_by_week)) {
      samples_by_week[[week_key]] <- list()
    }

    # Remove internal fields before exporting
    s_export <- s
    s_export$.virtual <- NULL
    s_export$lastSaved <- now_utc

    samples_by_week[[week_key]][[length(samples_by_week[[week_key]]) + 1L]] <- s_export
  }

  # Write all weekly files
  for (week_key in names(samples_by_week)) {
    gz_path <- file.path(sample_dir, paste0(week_key, ".json.gz"))
    json_str <- jsonlite::toJSON(
      samples_by_week[[week_key]],
      auto_unbox = TRUE,
      pretty     = TRUE,
      digits     = NA
    )
    con <- gzfile(gz_path, "w")
    writeLines(json_str, con)
    close(con)
  }

  # ---- Places: collect from visit items and write to places/{N}.json ----
  all_places <- list()
  for (it in timeline_items) {
    if (!is.null(it$.place) && !is.null(it$.place$id)) {
      all_places[[length(all_places) + 1L]] <- it$.place
    }
  }

  if (length(all_places) > 0) {
    places_path <- file.path(places_dir, "0.json")
    jsonlite::write_json(
      all_places,
      places_path,
      auto_unbox = TRUE,
      pretty     = TRUE,
      digits     = NA
    )
  } else {
    # ImportManager needs at least one file (iCloud drops empty dirs)
    jsonlite::write_json(list(), file.path(places_dir, "0.json"), auto_unbox = TRUE)
  }

  # ---- Metadata ----
  n_items <- length(timeline_items)
  n_samples <- length(samples)
  n_places <- length(all_places)

  metadata <- list(
    exportId         = toupper(uuid::UUIDgenerate(use.time = TRUE)),
    lastBackupDate   = now_utc,
    sessionStartDate = now_utc,
    sessionFinishDate = now_utc,
    exportMode       = "bucketed",
    exportType       = "incremental",
    schemaVersion    = "2.2.0",
    stats = list(
      itemCount   = n_items,
      sampleCount = n_samples,
      placeCount  = n_places
    ),
    itemsCompleted   = TRUE,
    samplesCompleted = TRUE,
    placesCompleted  = TRUE
  )

  jsonlite::write_json(
    metadata,
    file.path(output_dir, "metadata.json"),
    auto_unbox = TRUE,
    pretty     = TRUE
  )

  invisible(NULL)
}

# ---- Edit Samples export (corrected route) ----
# Modes (see download_edit_arc in server.R):
#   replace_disabled: new item + samples, all disabled. Imports fine but stays
#                     hidden: Arc has no UI to enable it (tested 2026-10-10).
#   additive:         new item + samples, enabled, alongside the original (default).
#   reuse_id:         original item ID. Skipped by INSERT OR IGNORE until the
#                     importer supports upsert.
EDIT_EXPORT_MODES <- c("replace_disabled", "additive", "reuse_id")

# Samples to export: virtual (snapped) samples if any, otherwise non-ignored edit samples.
# Returns an empty list when every edit sample is ignored.
select_edit_export_samples <- function(edit_samples, ignored = character(0), virtual = list()) {
  if (length(edit_samples) == 0) return(list())

  ids <- vapply(edit_samples, sample_id, character(1))
  active <- edit_samples[!ids %in% ignored]
  if (length(active) == 0) return(list())

  if (length(virtual) > 0) virtual else active
}

# Build the item + samples for an edit export. Pure: no Shiny state, no I/O.
# Returns list(item = <item list>, samples = <list of samples sorted by date>).
build_edit_package <- function(samples, originals, original_item_id, mode, now_utc) {
  mode <- match.arg(mode, EDIT_EXPORT_MODES)
  new_uuid <- function() toupper(uuid::UUIDgenerate(use.time = TRUE))

  # Sort by date, dropping samples without a parseable date (they can't be bucketed)
  ts <- as.POSIXct(
    vapply(samples, function(s) as.character(s$date %||% NA_character_)[1], character(1)),
    format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
  )
  ord <- order(ts, na.last = NA)
  if (length(ord) == 0) stop("No samples with a valid date to export.")
  samples <- samples[ord]
  ts <- ts[ord]

  has_original_id <- is.character(original_item_id) && length(original_item_id) == 1 &&
    !is.na(original_item_id) && nzchar(original_item_id)
  item_id <- if (mode == "reuse_id" && has_original_id) original_item_id else new_uuid()

  disabled <- mode == "replace_disabled"

  # Activity type from the original samples (virtual samples are created as car)
  at_code <- if (length(originals) > 0) {
    originals[[1]]$confirmedActivityType %||% originals[[1]]$classifiedActivityType %||% 5L
  } else {
    5L
  }
  at_code <- as.integer(at_code)

  samples <- lapply(samples, function(s) {
    s[startsWith(names(s), ".")] <- NULL  # internal fields (.virtual)
    s$id                     <- new_uuid()
    s$timelineItemId         <- item_id
    s$lastSaved              <- now_utc
    s$classifiedActivityType <- at_code
    s$confirmedActivityType  <- at_code
    s$disabled               <- disabled
    s
  })

  # Trip stats: distance in metres, speed in m/s
  coords <- cbind(
    lon = vapply(samples, sample_lon, numeric(1)),
    lat = vapply(samples, sample_lat, numeric(1))
  )
  coords <- coords[stats::complete.cases(coords), , drop = FALSE]
  distance_m <- track_distance_km(coords) * 1000
  duration_s <- as.numeric(difftime(ts[length(ts)], ts[1], units = "secs"))
  speed_ms <- if (duration_s > 0) distance_m / duration_s else 0

  item <- list(
    base = list(
      id              = item_id,
      source          = "LocoKit2",
      sourceVersion   = "9.0.0",
      isVisit         = FALSE,
      startDate       = format(ts[1], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      endDate         = format(ts[length(ts)], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      lastSaved       = now_utc,
      deleted         = FALSE,
      disabled        = disabled,
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
      distance               = distance_m,
      speed                  = speed_ms,
      lastSaved              = now_utc
    )
  )

  list(item = item, samples = samples)
}

# Write an edit package (see build_edit_package) as a LocoKit2 import folder:
# items/{YYYY-MM}.json, samples/{YYYY-Www}.json.gz, places/0.json, metadata.json
write_edit_package_dir <- function(pkg, dir) {
  new_item <- pkg$item
  new_samples <- pkg$samples
  current_timestamp <- new_item$base$lastSaved

  # Create LocoKit2 structure (places/, items/, samples/ required by ImportManager)
  sample_dir <- file.path(dir, "samples")
  items_dir  <- file.path(dir, "items")
  places_dir <- file.path(dir, "places")
  dir.create(sample_dir, recursive = TRUE)
  dir.create(items_dir,  recursive = TRUE)
  dir.create(places_dir, recursive = TRUE)

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

  # Write all weekly files
  for (week_key in names(samples_by_week)) {
    gz_path <- file.path(sample_dir, paste0(week_key, ".json.gz"))
    json_str <- jsonlite::toJSON(samples_by_week[[week_key]], auto_unbox = TRUE, pretty = TRUE, digits = NA)
    con <- gzfile(gz_path, "w")
    writeLines(json_str, con)
    close(con)
  }

  # ---- TIMELINE ITEMS ----
  items_export <- list(new_item)

  # Write items grouped by month
  month_key <- substr(new_item$base$startDate, 1, 7)
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
  jsonlite::write_json(metadata, file.path(dir, "metadata.json"), auto_unbox = TRUE, pretty = TRUE)

  invisible(dir)
}

# Notes-only LocoKit2 import package in dir. notes: list of list(body,
# start_utc, end_utc) (POSIXct or epoch s; end_utc NULL = an instant). Each
# becomes a new Arc note (new UUID, source "LocoKit2", no timelineItemId: the
# items it belongs to come from a GPX import, with ids unknown here) in
# notes/{YYYY-MM}.json by its UTC start month, as in Arc's backups.
# ImportManager copies only the package's files and then requires places/,
# items/ and samples/, so each holds an empty array file (as places/0.json in
# the edit package). metadata.json: incremental, extensions.notes.recordCount.
write_notes_package_dir <- function(notes, dir, now_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) {
  iso <- function(t) format(as.POSIXct(round(as.numeric(t)), origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  for (sub in c("items", "places", "samples", "notes")) dir.create(file.path(dir, sub), recursive = TRUE, showWarnings = FALSE)

  records <- lapply(notes, function(n) list(
    id = toupper(uuid::UUIDgenerate()), timelineItemId = NA, body = n$body, source = "LocoKit2",
    startDate = iso(n$start_utc), endDate = iso(n$end_utc %||% n$start_utc),
    creationDate = now_utc, lastSaved = now_utc, deleted = FALSE
  ))
  months <- vapply(records, function(r) substr(r$startDate, 1, 7), "")
  for (m in unique(months)) {
    jsonlite::write_json(records[months == m], file.path(dir, "notes", paste0(m, ".json")),
                         auto_unbox = TRUE, pretty = TRUE, na = "null")
  }

  # Empty buckets so the required folders survive the copy
  first <- as.POSIXct(if (length(notes)) as.numeric(notes[[1]]$start_utc) else as.numeric(Sys.time()),
                      origin = "1970-01-01", tz = "UTC")
  jsonlite::write_json(list(), file.path(dir, "places", "0.json"))
  jsonlite::write_json(list(), file.path(dir, "items", paste0(format(first, "%Y-%m", tz = "UTC"), ".json")))
  con <- gzfile(file.path(dir, "samples", paste0(strftime(first, "%G-W%V", tz = "UTC"), ".json.gz")), "w")
  writeLines("[]", con)
  close(con)

  metadata <- list(
    exportId = toupper(uuid::UUIDgenerate()),
    lastBackupDate = now_utc,
    sessionStartDate = now_utc,
    sessionFinishDate = now_utc,
    exportMode = "bucketed",
    exportType = "incremental",
    schemaVersion = "2.4.0",
    stats = list(itemCount = 0L, sampleCount = 0L, placeCount = 0L),
    itemsCompleted = TRUE,
    samplesCompleted = TRUE,
    placesCompleted = TRUE,
    extensions = list(notes = list(recordCount = length(records)))
  )
  jsonlite::write_json(metadata, file.path(dir, "metadata.json"), auto_unbox = TRUE, pretty = TRUE)
  invisible(dir)
}

# Local wall-clock time of a sample (UTC date shifted by secondsFromGMT)
sample_local_time <- function(s) {
  as.POSIXct(s$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") + as.numeric(s$secondsFromGMT %||% 0)[1]
}

# GPX filename for an edit package: arc_edit_<YYYYmmdd_HHMM of first sample, local>.gpx
edit_gpx_filename <- function(pkg) {
  paste0("arc_edit_", format(sample_local_time(pkg$samples[[1]]), "%Y%m%d_%H%M"), ".gpx")
}

# ---- GPX export (Arc → Import GPX File) ----
# Arc imports each <trk> as its own item, reads the activity type from
# <trk><type> and disables/hides the overlapping data (verified on device), so
# a GPX can replace any recorded trip.

# <trk><type> for an activity code, as LocoKit2's ActivityType(stringValue:)
# expects: lowerCamelCase ("cable_car" -> "cableCar"); unknown codes -> "unknown"
gpx_activity_string <- function(code) {
  name <- unname(ACTIVITY_TYPE_NAMES[as.character(code)])
  if (is.na(name)) return("unknown")
  gsub("_([a-z])", "\\U\\1", name, perl = TRUE)
}

# Timeline items paired with their samples, one entry per GPX track:
# list(name, type, desc, samples), sorted by start. An item's .gpxName is used
# as the track name as is (otherwise its description plus the start time);
# .gpxDesc becomes the track's <desc>. Samples are linked by item$samples
# or sample$timelineItemId (as in export_arc_json), sorted by date and limited to
# those with coordinates and a parseable date. Visits and items without points
# are skipped. include_all keeps visits (no type) and adds samples not linked to
# any item as a last track, so every sample is exported.
gpx_tracks <- function(items, samples, include_all = FALSE) {
  if (length(samples) == 0) return(list())

  ids   <- vapply(samples, sample_id, character(1))
  owner <- vapply(samples, function(s) as.character(s$timelineItemId %||% NA_character_)[1], character(1))
  dates <- vapply(samples, function(s) as.character(s$date %||% NA_character_)[1], character(1))
  ts <- as.POSIXct(dates, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  usable <- !is.na(ts) & vapply(samples, sample_has_coords, logical(1))

  make_track <- function(name, type, in_track, exact_name = NULL, desc = NULL) {
    idx <- which(in_track & usable)
    if (length(idx) == 0) return(NULL)
    idx <- idx[order(ts[idx])]
    trk_samples <- samples[idx]
    list(
      name    = exact_name %||% paste(name, format(sample_local_time(trk_samples[[1]]), "%Y-%m-%d %H:%M")),
      type    = type,
      desc    = desc,
      samples = trk_samples,
      start   = ts[idx[1]]
    )
  }

  linked <- rep(FALSE, length(samples))
  tracks <- lapply(items, function(it) {
    item_id <- it$.internalId %||% it$base$id %||% NA_character_
    in_item <- ids %in% (it$samples %||% character(0)) | (!is.na(owner) & owner %in% item_id)
    linked <<- linked | in_item

    is_visit <- isTRUE(it$.isVisit) || isTRUE(it$base$isVisit)
    if (is_visit && !include_all) return(NULL)

    type <- NA_character_
    if (!is_visit) {
      at_code <- it$trip$confirmedActivityType %||% it$trip$classifiedActivityType %||%
        activity_type_code(it$activityType %||% "car")
      type <- gpx_activity_string(at_code)
    }
    name <- it$description %||% it$visit$customTitle %||% it$.place$name %||%
      (if (is_visit) "Visit" else "Trip")
    make_track(name, type, in_item, it$.gpxName, it$.gpxDesc)
  })
  if (include_all) tracks <- c(tracks, list(make_track("Arc samples", NA_character_, !linked)))

  tracks <- Filter(Negate(is.null), tracks)
  if (length(tracks) == 0) return(list())
  tracks[order(vapply(tracks, function(t) as.numeric(t$start), numeric(1)))]
}

# GPX 1.1 with one <trk> per timeline item (see gpx_tracks). Returns lines.
build_gpx <- function(items, samples, now_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                      include_all = FALSE) {
  xml_escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    gsub("'", "&apos;", x, fixed = TRUE)
  }

  trk_lines <- lapply(gpx_tracks(items, samples, include_all), function(trk) {
    trkpts <- vapply(trk$samples, function(s) {
      time_str <- format(as.POSIXct(s$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                         "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      alt <- as.numeric(s$altitude %||% 0)[1]
      ele <- if (!is.na(alt) && alt != 0) sprintf("<ele>%.2f</ele>", alt) else ""
      sprintf('      <trkpt lat="%.8f" lon="%.8f">%s<time>%s</time></trkpt>',
              sample_lat(s), sample_lon(s), ele, time_str)
    }, character(1))

    c(
      "  <trk>",
      sprintf("    <name>%s</name>", xml_escape(trk$name)),
      if (!is.null(trk$desc) && nzchar(trk$desc)) sprintf("    <desc>%s</desc>", xml_escape(trk$desc)),
      if (!is.na(trk$type)) sprintf("    <type>%s</type>", xml_escape(trk$type)),
      "    <trkseg>",
      trkpts,
      "    </trkseg>",
      "  </trk>"
    )
  })

  c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<gpx version="1.1" creator="ArcTimelineBuilder" xmlns="http://www.topografix.com/GPX/1/1">',
    sprintf("  <metadata><time>%s</time></metadata>", now_utc),
    unlist(trk_lines),
    "</gpx>"
  )
}

# GPX of an edit package (corrected route): one track named "Arc edit <start>"
build_edit_gpx <- function(pkg, now_utc) {
  item <- pkg$item
  item$description <- "Arc edit"
  build_gpx(list(item), pkg$samples, now_utc)
}

# GPX filename for the day timeline: arc_timeline_<work_date>_<HHMM of first sample, local>.gpx
timeline_gpx_filename <- function(work_date, tracks) {
  paste0("arc_timeline_", format(as.Date(work_date), "%Y-%m-%d"), "_",
         format(sample_local_time(tracks[[1]]$samples[[1]]), "%H%M"), ".gpx")
}
