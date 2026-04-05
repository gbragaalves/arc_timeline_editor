# ---- Helpers: weeks + samples to delete + overlap ----

# Load existing samples from a time interval from weekly files
load_samples_interval <- function(start_utc, end_utc, weekly_dir = WEEKLY_BACKUP_DIR) {
  if (is.null(start_utc) || is.null(end_utc) || is.na(start_utc) || is.na(end_utc)) {
    return(list())
  }
  if (start_utc > end_utc) return(list())

  weeks <- weeks_for_interval(start_utc, end_utc)
  if (length(weeks) == 0) return(list())

  found_samples <- list()

  for (sem in weeks) {
    gz_path <- file.path(weekly_dir, paste0(sem, ".json.gz"))
    if (!file.exists(gz_path)) next

    week_samples <- tryCatch(
      jsonlite::fromJSON(gzfile(gz_path), simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(week_samples) || length(week_samples) == 0) next

    for (s in week_samples) {
      date_str <- s$date
      if (is.null(date_str)) next

      ts <- tryCatch(
        as.POSIXct(date_str, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        error = function(e) NULL
      )
      if (is.null(ts) || is.na(ts)) next

      if (ts >= start_utc && ts <= end_utc) {
        found_samples[[length(found_samples) + 1L]] <- s
      }
    }
  }

  # Sort by timestamp
  if (length(found_samples) > 0) {
    ts_vec <- vapply(found_samples, function(s) {
      as.numeric(as.POSIXct(s$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    }, numeric(1))
    found_samples <- found_samples[order(ts_vec)]
  }

  found_samples
}

# Given an interval [start_utc, end_utc], generate the names of involved weekly files
weeks_for_interval <- function(start_utc, end_utc) {
  if (start_utc > end_utc) return(character(0))
  dias <- seq(lubridate::floor_date(start_utc, "day"), lubridate::ceiling_date(end_utc, "day"), by = "1 day")
  unique(strftime(dias, "%G-W%V"))
}

# Generate list of samples marked as deleted based on the new items
generate_samples_to_delete <- function(timeline_items, weekly_dir = WEEKLY_BACKUP_DIR) {
  if (length(timeline_items) == 0) return(list())

  # Intervals of each item
  intervals <- lapply(timeline_items, function(it) {
    # Supports LocoKit2 format (base$startDate as string) and legacy
    start_str <- it$base$startDate %||% it$startDate$date %||% it$startDate
    end_str   <- it$base$endDate   %||% it$endDate$date   %||% it$endDate

    item_start <- parse_timestamp_utc(start_str)
    item_end <- parse_timestamp_utc(end_str)
    if (is.null(item_start) || is.null(item_end) || is.na(item_start) || is.na(item_end)) return(NULL)
    list(start = item_start[1], end = item_end[1])
  })
  intervals <- Filter(Negate(is.null), intervals)
  if (length(intervals) == 0) return(list())

  weeks <- unique(unlist(lapply(intervals, function(iv) {
    weeks_for_interval(iv$start, iv$end)
  })))

  if (length(weeks) == 0) return(list())

  delete_samples <- list()
  current_timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  for (sem in weeks) {
    gz_path <- file.path(weekly_dir, paste0(sem, ".json.gz"))
    if (!file.exists(gz_path)) next

    week_samples <- tryCatch(
      jsonlite::fromJSON(gzfile(gz_path), simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(week_samples) || length(week_samples) == 0) next

    for (s in week_samples) {
      date_str <- s$date
      if (is.null(date_str)) next

      ts <- tryCatch(
        as.POSIXct(date_str, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        error = function(e) NULL
      )
      if (is.null(ts) || is.na(ts)) next

      overlap <- any(vapply(intervals, function(iv) {
        ts >= iv$start && ts <= iv$end
      }, logical(1)))

      if (overlap) {
        s$deleted <- TRUE
        s$lastSaved <- current_timestamp
        delete_samples[[length(delete_samples) + 1L]] <- s
      }
    }
  }

  delete_samples
}


# Check if [start_utc, end_utc] overlaps any item in the timeline
has_overlap <- function(timeline_items, start_utc, end_utc, ignore_id = NULL) {
  if (length(timeline_items) == 0) return(FALSE)
  if (is.null(start_utc) || is.null(end_utc) || is.na(start_utc) || is.na(end_utc)) return(FALSE)

  any(vapply(timeline_items, function(it) {
    if (!is.null(ignore_id) && identical(it$.internalId, ignore_id)) return(FALSE)

    # Supports LocoKit2 format (base$startDate) and legacy (startDate$date)
    start_str <- it$base$startDate %||% it$startDate$date %||% it$startDate
    end_str   <- it$base$endDate   %||% it$endDate$date   %||% it$endDate

    existing_start <- parse_timestamp_utc(start_str)
    existing_end <- parse_timestamp_utc(end_str)
    if (is.null(existing_start) || is.null(existing_end) || is.na(existing_start[1]) || is.na(existing_end[1])) return(FALSE)

    (start_utc < existing_end[1]) && (end_utc > existing_start[1])
  }, logical(1)))
}

# Load ALL samples for a specific timelineItemId
load_samples_by_timeline_item <- function(timeline_item_id, weekly_dir = WEEKLY_BACKUP_DIR) {
  if (is.null(timeline_item_id) || !nzchar(timeline_item_id)) return(list())

  files <- list.files(weekly_dir, pattern = "\\.json\\.gz$", full.names = TRUE)

  found_samples <- list()

  for (gz_path in files) {
    week_samples <- tryCatch(
      jsonlite::fromJSON(gzfile(gz_path), simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(week_samples) || length(week_samples) == 0) next

    for (s in week_samples) {
      if (!is.null(s$timelineItemId) && s$timelineItemId == timeline_item_id) {
        found_samples[[length(found_samples) + 1L]] <- s
      }
    }
  }

  if (length(found_samples) > 0) {
    ts_vec <- vapply(found_samples, function(s) {
      as.numeric(as.POSIXct(s$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    }, numeric(1))
    found_samples <- found_samples[order(ts_vec)]
  }

  found_samples
}
