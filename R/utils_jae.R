# ---- Jaé fare card boardings (Rio de Janeiro) ----
# CSV exported from the Jaé fare history: one row per boarding (tap-in only,
# no alighting). Path in config_local.R: JAE_CSV_PATH <- "/path/to/jae.csv".
# Only rows with usar_no_arc == "sim" are used.

JAE_COLUMNS <- c(
  "datetime_local", "datetime_utc", "modo", "consorcio", "linha_ou_estacao", "descricao", "sentido",
  "id_veiculo", "tipo_transacao", "produto", "meio_pagamento", "valor", "latitude", "longitude",
  "fonte_coordenada", "usar_no_arc", "obs"
)

JAE_TZ <- "America/Sao_Paulo"

# Jaé mode -> routing profile (PROFILE_CHOICES value)
JAE_PROFILES <- c("Ônibus" = "bus", "BRT" = "bus", "Metrô" = "metro", "VLT" = "tram")

# Marker / badge colour by Jaé mode; used boardings are grey
JAE_MODE_COLORS <- c("Ônibus" = "#1565C0", "BRT" = "#E65100", "Metrô" = "#2E7D32", "VLT" = "#6A1B9A")
JAE_USED_COLOR <- "#9E9E9E"

# Boardings to use in Arc, sorted by time. Empty (same columns) when the path
# is not configured, the file is missing or can't be read.
read_jae_boardings <- function(path = get0("JAE_CSV_PATH", ifnotfound = NULL)) {
  empty <- jae_prepare(as.data.frame(
    stats::setNames(rep(list(character(0)), length(JAE_COLUMNS)), JAE_COLUMNS),
    stringsAsFactors = FALSE
  ))
  if (!is.character(path) || length(path) != 1 || !file.exists(path)) return(empty)

  d <- tryCatch(
    utils::read.csv(path, colClasses = "character", fileEncoding = "UTF-8-BOM",
                    na.strings = character(0), check.names = FALSE),
    error = function(e) {
      message("[jae] Could not read ", basename(path), ": ", conditionMessage(e))
      NULL
    }
  )
  missing_cols <- setdiff(JAE_COLUMNS, names(d))
  if (is.null(d) || length(missing_cols)) {
    if (!is.null(d)) message("[jae] Missing columns in ", basename(path), ": ", paste(missing_cols, collapse = ", "))
    return(empty)
  }

  d[JAE_COLUMNS] %>%
    dplyr::filter(trimws(usar_no_arc) == "sim") %>%
    jae_prepare() %>%
    dplyr::filter(!is.na(datetime_utc)) %>%
    dplyr::arrange(datetime_utc)
}

# Typed columns plus profile, label and jae_id
jae_prepare <- function(d) {
  d %>%
    dplyr::mutate(
      jae_id = paste(datetime_utc, linha_ou_estacao),
      datetime_utc = lubridate::ymd_hms(datetime_utc, tz = "UTC", quiet = TRUE),
      valor = suppressWarnings(as.numeric(valor)),
      latitude = suppressWarnings(as.numeric(latitude)),
      longitude = suppressWarnings(as.numeric(longitude)),
      profile = unname(JAE_PROFILES[modo]),
      label = jae_label(modo, linha_ou_estacao, descricao, id_veiculo)
    )
}

# Bus/BRT: "<line> · <description> · veículo <id>"; Metrô/VLT: "<mode> · <description>".
# Empty parts are left out.
jae_label <- function(modo, line, desc, vehicle) {
  bus <- modo %in% c("Ônibus", "BRT")
  first <- ifelse(bus, line, modo)
  veh <- ifelse(bus & nzchar(trimws(vehicle)), paste0("veículo ", trimws(vehicle)), "")
  vapply(seq_along(modo), function(i) {
    p <- trimws(c(first[i], desc[i], veh[i]))
    p <- p[nzchar(p)]
    if (length(p)) paste(p, collapse = " · ") else modo[i]
  }, character(1))
}

# Boardings on a local (America/Sao_Paulo) date, by time. as.Date() needs the
# tz: on a POSIXct it otherwise returns the UTC date (wrong after 21:00 local).
jae_on_date <- function(boardings, date) {
  if (is.null(date) || length(date) != 1 || is.na(as.Date(date))) return(boardings[0, ])
  boardings %>%
    dplyr::filter(as.Date(lubridate::with_tz(datetime_utc, JAE_TZ), tz = JAE_TZ) == as.Date(date)) %>%
    dplyr::arrange(datetime_utc)
}

jae_local_time <- function(t, fmt = "%H:%M") format(lubridate::with_tz(t, JAE_TZ), fmt)

# "R$ 4,70" (PT) / "R$ 4.70" (EN)
jae_fare <- function(valor, lang = "pt") {
  ifelse(is.na(valor), "",
         paste0("R$ ", formatC(valor, format = "f", digits = 2, decimal.mark = if (identical(lang, "pt")) "," else ".")))
}

# Map popup: local time, label, fare type, fare
jae_popup <- function(b, lang = "pt") {
  paste0(
    "<b>", jae_local_time(b$datetime_utc, "%H:%M:%S"), "</b> · ", htmltools::htmlEscape(b$label),
    "<br>", htmltools::htmlEscape(b$tipo_transacao), " · ", jae_fare(b$valor, lang)
  )
}
