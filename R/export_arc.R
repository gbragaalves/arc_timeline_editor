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
