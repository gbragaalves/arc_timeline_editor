# ---- Helpers: weeks + samples_to_delete + overlap ----

# Load existing samples from a time interval from weekly files
carregar_samples_intervalo <- function(inicio_utc, fim_utc, semana_dir = SEMANA_DIR) {
  if (is.null(inicio_utc) || is.null(fim_utc) || is.na(inicio_utc) || is.na(fim_utc)) {
    return(list())
  }
  if (inicio_utc > fim_utc) return(list())

  semanas <- semanas_para_intervalo(inicio_utc, fim_utc)
  if (length(semanas) == 0) return(list())

  samples_encontrados <- list()

  for (sem in semanas) {
    gz_path <- file.path(semana_dir, paste0(sem, ".json.gz"))
    if (!file.exists(gz_path)) next

    semana_samples <- tryCatch(
      jsonlite::fromJSON(gzfile(gz_path), simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(semana_samples) || length(semana_samples) == 0) next

    for (s in semana_samples) {
      date_str <- s$date
      if (is.null(date_str)) next

      ts <- tryCatch(
        as.POSIXct(date_str, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        error = function(e) NULL
      )
      if (is.null(ts) || is.na(ts)) next

      if (ts >= inicio_utc && ts <= fim_utc) {
        samples_encontrados[[length(samples_encontrados) + 1L]] <- s
      }
    }
  }

  # Sort by timestamp
  if (length(samples_encontrados) > 0) {
    ts_vec <- vapply(samples_encontrados, function(s) {
      as.numeric(as.POSIXct(s$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    }, numeric(1))
    samples_encontrados <- samples_encontrados[order(ts_vec)]
  }

  samples_encontrados
}

# Given an interval [ini_utc, fim_utc], generate the names of involved weekly files
semanas_para_intervalo <- function(ini_utc, fim_utc) {
  if (ini_utc > fim_utc) return(character(0))
  dias <- seq(lubridate::floor_date(ini_utc, "day"), lubridate::ceiling_date(fim_utc, "day"), by = "1 day")
  unique(strftime(dias, "%G-W%V"))
}

# Generate list of samples marked as deleted based on the new items
gerar_samples_apagar <- function(timeline_items, semana_dir = SEMANA_DIR) {
  if (length(timeline_items) == 0) return(list())

  # Intervals of each item
  intervalos <- lapply(timeline_items, function(it) {
    # Supports LocoKit2 format (base$startDate as string) and legacy
    start_str <- it$base$startDate %||% it$startDate$date %||% it$startDate
    end_str   <- it$base$endDate   %||% it$endDate$date   %||% it$endDate

    ini <- parse_timestamp_utc(start_str)
    fim <- parse_timestamp_utc(end_str)
    if (is.null(ini) || is.null(fim) || is.na(ini) || is.na(fim)) return(NULL)
    list(inicio = ini[1], fim = fim[1])
  })
  intervalos <- Filter(Negate(is.null), intervalos)
  if (length(intervalos) == 0) return(list())

  semanas <- unique(unlist(lapply(intervalos, function(iv) {
    semanas_para_intervalo(iv$inicio, iv$fim)
  })))

  if (length(semanas) == 0) return(list())

  samples_apagar <- list()
  current_timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  for (sem in semanas) {
    gz_path <- file.path(semana_dir, paste0(sem, ".json.gz"))
    if (!file.exists(gz_path)) next

    semana_samples <- tryCatch(
      jsonlite::fromJSON(gzfile(gz_path), simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(semana_samples) || length(semana_samples) == 0) next

    for (s in semana_samples) {
      date_str <- s$date
      if (is.null(date_str)) next

      ts <- tryCatch(
        as.POSIXct(date_str, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        error = function(e) NULL
      )
      if (is.null(ts) || is.na(ts)) next

      overlap <- any(vapply(intervalos, function(iv) {
        ts >= iv$inicio && ts <= iv$fim
      }, logical(1)))

      if (overlap) {
        s$deleted <- TRUE
        s$lastSaved <- current_timestamp
        samples_apagar[[length(samples_apagar) + 1L]] <- s
      }
    }
  }

  samples_apagar
}


# Check if [inicio_utc, fim_utc] overlaps any item in the timeline
has_overlap <- function(timeline_items, inicio_utc, fim_utc, ignore_id = NULL) {
  if (length(timeline_items) == 0) return(FALSE)
  if (is.null(inicio_utc) || is.null(fim_utc) || is.na(inicio_utc) || is.na(fim_utc)) return(FALSE)

  any(vapply(timeline_items, function(it) {
    if (!is.null(ignore_id) && identical(it$.internalId, ignore_id)) return(FALSE)

    # Supports LocoKit2 format (base$startDate) and legacy (startDate$date)
    start_str <- it$base$startDate %||% it$startDate$date %||% it$startDate
    end_str   <- it$base$endDate   %||% it$endDate$date   %||% it$endDate

    ex_ini <- parse_timestamp_utc(start_str)
    ex_fim <- parse_timestamp_utc(end_str)
    if (is.null(ex_ini) || is.null(ex_fim) || is.na(ex_ini[1]) || is.na(ex_fim[1])) return(FALSE)

    (inicio_utc < ex_fim[1]) && (fim_utc > ex_ini[1])
  }, logical(1)))
}

# Load ALL samples for a specific timelineItemId
carregar_samples_por_timeline_item <- function(timeline_item_id, semana_dir = SEMANA_DIR) {
  if (is.null(timeline_item_id) || !nzchar(timeline_item_id)) return(list())

  arquivos <- list.files(semana_dir, pattern = "\\.json\\.gz$", full.names = TRUE)

  samples_encontrados <- list()

  for (gz_path in arquivos) {
    semana_samples <- tryCatch(
      jsonlite::fromJSON(gzfile(gz_path), simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(semana_samples) || length(semana_samples) == 0) next

    for (s in semana_samples) {
      if (!is.null(s$timelineItemId) && s$timelineItemId == timeline_item_id) {
        samples_encontrados[[length(samples_encontrados) + 1L]] <- s
      }
    }
  }

  if (length(samples_encontrados) > 0) {
    ts_vec <- vapply(samples_encontrados, function(s) {
      as.numeric(as.POSIXct(s$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    }, numeric(1))
    samples_encontrados <- samples_encontrados[order(ts_vec)]
  }

  samples_encontrados
}
