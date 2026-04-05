# ---- Helpers: semanas + samples_apagar + overlap ----

# Carrega samples existentes de um intervalo de tempo dos arquivos semanais
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

  # Ordena por timestamp
  if (length(samples_encontrados) > 0) {
    ts_vec <- vapply(samples_encontrados, function(s) {
      as.numeric(as.POSIXct(s$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    }, numeric(1))
    samples_encontrados <- samples_encontrados[order(ts_vec)]
  }

  samples_encontrados
}

# Dado um intervalo [ini_utc, fim_utc], gera nomes de arquivos semanais envolvidos
semanas_para_intervalo <- function(ini_utc, fim_utc) {
  if (ini_utc > fim_utc) return(character(0))
  dias <- seq(lubridate::floor_date(ini_utc, "day"), lubridate::ceiling_date(fim_utc, "day"), by = "1 day")
  unique(strftime(dias, "%G-W%V"))
}

# Gera lista de samples marcados como deleted com base nos itens novos
gerar_samples_apagar <- function(timeline_items, semana_dir = SEMANA_DIR) {
  if (length(timeline_items) == 0) return(list())

  # Intervalos de cada item
  intervalos <- lapply(timeline_items, function(it) {
    # Suporta formato LocoKit2 (base$startDate como string) e antigo
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


# Verifica se [inicio_utc, fim_utc] se sobrepõe a algum item da timeline
has_overlap <- function(timeline_items, inicio_utc, fim_utc, ignore_id = NULL) {
  if (length(timeline_items) == 0) return(FALSE)
  if (is.null(inicio_utc) || is.null(fim_utc) || is.na(inicio_utc) || is.na(fim_utc)) return(FALSE)

  any(vapply(timeline_items, function(it) {
    if (!is.null(ignore_id) && identical(it$.internalId, ignore_id)) return(FALSE)

    # Suporta formato LocoKit2 (base$startDate) e antigo (startDate$date)
    start_str <- it$base$startDate %||% it$startDate$date %||% it$startDate
    end_str   <- it$base$endDate   %||% it$endDate$date   %||% it$endDate

    ex_ini <- parse_timestamp_utc(start_str)
    ex_fim <- parse_timestamp_utc(end_str)
    if (is.null(ex_ini) || is.null(ex_fim) || is.na(ex_ini[1]) || is.na(ex_fim[1])) return(FALSE)

    (inicio_utc < ex_fim[1]) && (fim_utc > ex_ini[1])
  }, logical(1)))
}

# Carrega TODOS os samples de um timelineItemId específico
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
