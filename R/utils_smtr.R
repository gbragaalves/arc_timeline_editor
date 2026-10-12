# ---- SMTR bus note (Rio de Janeiro) ----
# Arc note for a Jaé bus boarding, from SMTR data in BigQuery (bigrquery,
# billed to GTFS_BQ_PROJECT with the Google account GTFS_BQ_EMAIL, see
# utils_stops.R):
#   Linha <servico>: <destino>
#   Ônibus <id_veiculo> - Placa <placa>
#   Consórcio <consorcio>, operadora <operadora>
#   <carroceria> · <chassi> · <ano de fabricação>
#   Início da viagem: <HH:MM> | Fim da viagem: <HH:MM>
#   Embarques totais na viagem: <n> Jaé + <n> RioCard
# A line whose data is missing is left out; when the subsidy tables don't
# cover the date yet, the note ends with SMTR_NOTE_PENDING.
#
# Sources (one batch of queries per date, every query dry-run first and
# refused above SMTR_BQ_MAX_GB):
#   vehicle trip: projeto_subsidio_sppo.viagem_completa (data up to
#     2026-07-31) or monitoramento.viagem_valida (from 2026-08-01); the trip of
#     the vehicle (same digits: Jaé has "33103", SMTR "D33103") whose
#     [partida, chegada] contains the tap, the Jaé line preferred (a bus can
#     switch lines later in the day);
#   boardings and validators: subsidio.viagem_transacao (data, id_viagem);
#   plate and vehicle: cadastro.veiculo_licenciamento_dia (data, id_veiculo);
#   operator: cadastro.validador_operadora (validator valid at the tap) ->
#     cadastro.operadoras;
#   destination: gtfs.trips trip_headsign in the feed valid on the date (max
#     feed_start_date <= date), by the trip's trip_id when the feed has it,
#     otherwise by direction (sentido I -> direction_id 0, V -> 1).
# bilhetagem.transacao (huge) is never queried.
# Fetched data is cached per (date, vehicle) in SMTR_NOTE_CACHE_FILE; an
# entry whose date wasn't covered yet is fetched again after an hour.

SMTR_NOTE_CACHE_FILE <- if (exists("SMTR_NOTE_CACHE_FILE")) SMTR_NOTE_CACHE_FILE else file.path("reference", "smtr_note_cache.rds")
SMTR_BQ_MAX_GB       <- 2
SMTR_VALIDA_FROM     <- as.Date("2026-08-01")  # viagem_valida from this date, viagem_completa before
SMTR_NOTE_PENDING    <- "(dados SMTR ainda não disponíveis)"
SMTR_TZ              <- "America/Sao_Paulo"    # SMTR DATETIMEs are local wall-clock times
SMTR_REFETCH_S       <- 3600                   # incomplete cache entries are fetched again after this
SMTR_BUS_MODES       <- c("Ônibus", "BRT")

# Words the title-casing can't accent by itself (operators, bodies)
SMTR_ACCENTS <- c(
  ANDRE = "André", ANTONIO = "Antônio", CECILIA = "Cecília", COMERCIO = "Comércio", CONCEICAO = "Conceição",
  ESTACAO = "Estação", FAMILIA = "Família", GLORIA = "Glória", IRMAOS = "Irmãos", JACAREPAGUA = "Jacarepaguá",
  JOAO = "João", JOSE = "José", LUCIA = "Lúcia", MEIER = "Méier", ONIBUS = "Ônibus", RAPIDO = "Rápido",
  RODOVIARIA = "Rodoviária", SAO = "São", SERVICOS = "Serviços", TRES = "Três", UNIAO = "União",
  VIACAO = "Viação", VITORIA = "Vitória"
)
SMTR_SMALL_WORDS <- c("DA", "DAS", "DE", "DO", "DOS", "E")

# ---- Text helpers ----

# "SANCETUR - SANTA CECILIA TURISMO LTDA" -> "Sancetur – Santa Cecília Turismo":
# legal suffixes dropped, title case (small words lower case), accents from
# SMTR_ACCENTS, " - " as an en dash
smtr_title <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(trimws(x[1]))) return(NA_character_)
  x <- toupper(trimws(x[1]))
  repeat {
    y <- trimws(sub("[,.]?\\s+(LTDA|S/A|S\\.A|SA|ME|EIRELI|EPP)\\.?$", "", x))
    if (identical(y, x)) break
    x <- y
  }
  words <- strsplit(x, "\\s+")[[1]]
  out <- vapply(seq_along(words), function(i) {
    w <- words[i]
    if (w == "-") return("–")
    if (i > 1 && w %in% SMTR_SMALL_WORDS) return(tolower(w))
    if (w %in% names(SMTR_ACCENTS)) return(SMTR_ACCENTS[[w]])
    paste0(substr(w, 1, 1), tolower(substr(w, 2, nchar(w))))
  }, character(1))
  paste(out, collapse = " ")
}

# Chassis as registered, with only its plain words (4+ letters) in title
# case: "MBB OF 1721L EURO VI" -> "MBB OF 1721L Euro VI"
smtr_chassis <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(trimws(x[1]))) return(NA_character_)
  gsub("\\b([[:upper:]])([[:upper:]]{3,})\\b", "\\1\\L\\2", trimws(x[1]), perl = TRUE)
}

smtr_digits <- function(x) gsub("[^0-9]", "", as.character(x))

# SMTR DATETIME (local wall clock, "YYYY-mm-dd HH:MM:SS") -> POSIXct instant
smtr_time <- function(x) as.POSIXct(as.character(x), format = "%Y-%m-%d %H:%M:%S", tz = SMTR_TZ)

# ---- BigQuery ----

# Query with a cost guard: dry-run first, refused above max_gb. Returns a
# data.frame (DATETIME columns as "YYYY-mm-dd HH:MM:SS" text)
smtr_bq_query <- function(sql, billing = GTFS_BQ_PROJECT, max_gb = SMTR_BQ_MAX_GB) {
  bytes <- as.numeric(bigrquery::bq_perform_query_dry_run(sql, billing = billing))
  if (is.na(bytes) || bytes > max_gb * 1e9) {
    stop(sprintf("SMTR query refused: it would read %.2f GB (limit %g GB)", bytes / 1e9, max_gb), call. = FALSE)
  }
  d <- as.data.frame(bigrquery::bq_table_download(bigrquery::bq_project_query(billing, sql), quiet = TRUE))
  for (k in names(d)) if (inherits(d[[k]], "POSIXct")) d[[k]] <- format(d[[k]], "%Y-%m-%d %H:%M:%S", tz = "UTC")
  d
}

smtr_bq_auth <- function(email = GTFS_BQ_EMAIL) {
  bigrquery::bq_auth(email = email %||% TRUE, cache = gargle::gargle_oauth_cache())
}

sql_str <- function(x) paste0("'", gsub("'", "", x, fixed = TRUE), "'")
sql_list <- function(x) paste(sql_str(unique(x)), collapse = ", ")

# Everything the notes of `vehicles` (digit strings) on `date` need, in one
# batch of queries. Returns a list by vehicle (see smtr_note_body for the
# fields), each with the date's coverage.
smtr_fetch_day <- function(date, vehicles, query_fn = smtr_bq_query) {
  date <- as.Date(date)
  vehicles <- unique(vehicles[grepl("^[0-9]+$", vehicles)])
  if (length(vehicles) == 0) return(list())
  d <- sql_str(format(date))
  trips_sql <- if (date >= SMTR_VALIDA_FROM) {
    sprintf("SELECT id_viagem, id_veiculo, servico, sentido, trip_id, datetime_partida, datetime_chegada, consorcio
             FROM `rj-smtr.monitoramento.viagem_valida` WHERE data = %s", d)
  } else {
    sprintf("SELECT id_viagem, id_veiculo, servico_informado AS servico, sentido, trip_id, datetime_partida,
                    datetime_chegada, consorcio
             FROM `rj-smtr.projeto_subsidio_sppo.viagem_completa` WHERE data = %s", d)
  }
  trips_table <- if (date >= SMTR_VALIDA_FROM) "`rj-smtr.monitoramento.viagem_valida`" else
    "`rj-smtr.projeto_subsidio_sppo.viagem_completa`"

  # Coverage of the date, the vehicles' trips and their boardings / validators
  trips <- query_fn(sprintf(
    "WITH cov AS (
       SELECT (SELECT COUNT(*) FROM %s WHERE data = %s) AS n_trips_day,
              (SELECT COUNT(*) FROM `rj-smtr.subsidio.viagem_transacao` WHERE data = %s) AS n_transacao_day
     ),
     v AS (%s AND REGEXP_REPLACE(id_veiculo, r'[^0-9]', '') IN (%s)),
     t AS (
       SELECT id_viagem, quantidade_transacao, quantidade_transacao_riocard, id_validador
       FROM `rj-smtr.subsidio.viagem_transacao`
       WHERE data = %s AND id_viagem IN (SELECT id_viagem FROM v)
     )
     SELECT cov.n_trips_day, cov.n_transacao_day, v.*, t.quantidade_transacao, t.quantidade_transacao_riocard,
            t.id_validador
     FROM cov LEFT JOIN (v LEFT JOIN t USING (id_viagem)) ON TRUE",
    trips_table, d, d, trips_sql, sql_list(vehicles), d))
  coverage <- list(trips = isTRUE(trips$n_trips_day[1] > 0), transacao = isTRUE(trips$n_transacao_day[1] > 0))
  trips <- trips[!is.na(trips$id_viagem), , drop = FALSE]
  if (is.null(trips$id_validador)) trips$id_validador <- vector("list", nrow(trips))

  veh <- query_fn(sprintf(
    "SELECT id_veiculo, placa, carroceria, nome_chassi, ano_fabricacao
     FROM `rj-smtr.cadastro.veiculo_licenciamento_dia`
     WHERE data = %s AND REGEXP_REPLACE(id_veiculo, r'[^0-9]', '') IN (%s)", d, sql_list(vehicles)))

  validators <- unique(unlist(trips$id_validador))
  validators <- validators[!is.na(validators) & nzchar(validators)]
  ops <- if (length(validators)) {
    query_fn(sprintf(
      "SELECT v.id_validador, v.datetime_inicio_validade, v.datetime_fim_validade, o.operadora
       FROM `rj-smtr.cadastro.validador_operadora` v
       JOIN `rj-smtr.cadastro.operadoras` o USING (id_operadora_jae)
       WHERE v.id_validador IN (%s)", sql_list(validators)))
  }

  # GTFS feed valid on the date, then the headsigns of the trips' lines
  servicos <- unique(trips$servico[!is.na(trips$servico)])
  heads <- NULL
  if (length(servicos)) {
    feed <- query_fn(sprintf("SELECT MAX(feed_start_date) AS feed FROM `rj-smtr.gtfs.trips` WHERE feed_start_date <= %s", d))
    if (nrow(feed) && !is.na(feed$feed[1])) {
      f <- sql_str(format(as.Date(feed$feed[1])))
      ids <- unique(trips$trip_id[!is.na(trips$trip_id)])
      heads <- query_fn(sprintf(
        "SELECT r.route_short_name AS servico, t.direction_id, t.trip_headsign, COUNT(*) AS n,
                ARRAY_AGG(IF(t.trip_id IN (%s), t.trip_id, NULL) IGNORE NULLS) AS trip_ids
         FROM `rj-smtr.gtfs.trips` t JOIN `rj-smtr.gtfs.routes` r USING (feed_start_date, route_id)
         WHERE t.feed_start_date = %s AND r.route_short_name IN (%s)
         GROUP BY 1, 2, 3", if (length(ids)) sql_list(ids) else "''", f, sql_list(servicos)))
    }
  }

  fetched_at <- as.numeric(Sys.time())
  stats::setNames(lapply(vehicles, function(v) {
    tv <- trips[smtr_digits(trips$id_veiculo) == v, , drop = FALSE]
    vv <- veh[smtr_digits(veh$id_veiculo) == v, , drop = FALSE]
    vals <- unique(unlist(tv$id_validador))
    list(
      date = format(date), vehicle = v, fetched_at = fetched_at, coverage = coverage,
      trips = tv,
      vehicle_info = if (nrow(vv)) as.list(vv[1, , drop = FALSE]),
      operators = if (!is.null(ops)) ops[ops$id_validador %in% vals, , drop = FALSE],
      headsigns = if (!is.null(heads)) heads[heads$servico %in% tv$servico, , drop = FALSE]
    )
  }), vehicles)
}

# ---- Cache ----

smtr_cache <- function(file = SMTR_NOTE_CACHE_FILE) {
  if (!file.exists(file)) return(list())
  tryCatch(readRDS(file), error = function(e) list())
}

smtr_cache_save <- function(cache, file = SMTR_NOTE_CACHE_FILE) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cache, file)
}

smtr_key <- function(date, vehicle) paste(format(as.Date(date)), vehicle, sep = "|")

smtr_entry_complete <- function(e) isTRUE(e$coverage$trips) && isTRUE(e$coverage$transacao)

# Jaé bus taps (rows of read_jae_boardings()) -> date (local), vehicle digits, line, time
smtr_taps <- function(taps) {
  taps <- taps[taps$modo %in% SMTR_BUS_MODES, , drop = FALSE]
  data.frame(
    date = format(lubridate::with_tz(taps$datetime_utc, SMTR_TZ), "%Y-%m-%d"),
    vehicle = smtr_digits(taps$id_veiculo), line = trimws(taps$linha_ou_estacao),
    consorcio = trimws(taps$consorcio), t = as.numeric(taps$datetime_utc), stringsAsFactors = FALSE
  )[nzchar(smtr_digits(taps$id_veiculo)), , drop = FALSE]
}

# Fetches what the cache lacks for these taps (or has incomplete and older
# than SMTR_REFETCH_S), one batch per date. Returns list(cache, fetched =
# number of dates queried, error = message or NULL); the cache file is
# updated after each date.
smtr_prefetch <- function(taps, cache_file = SMTR_NOTE_CACHE_FILE, fetch_fn = smtr_fetch_day, auth_fn = smtr_bq_auth,
                          progress = NULL) {
  cache <- smtr_cache(cache_file)
  tp <- smtr_taps(taps)
  if (nrow(tp) == 0) return(list(cache = cache, fetched = 0L, error = NULL))
  keys <- unique(smtr_key(tp$date, tp$vehicle))
  stale <- vapply(keys, function(k) {
    e <- cache[[k]]
    is.null(e) || (!smtr_entry_complete(e) && as.numeric(Sys.time()) - e$fetched_at > SMTR_REFETCH_S)
  }, logical(1))
  todo <- tp[smtr_key(tp$date, tp$vehicle) %in% keys[stale], , drop = FALSE]
  dates <- sort(unique(todo$date))
  if (length(dates) == 0) return(list(cache = cache, fetched = 0L, error = NULL))
  err <- tryCatch({ auth_fn(); NULL }, error = function(e) conditionMessage(e))
  if (!is.null(err)) return(list(cache = cache, fetched = 0L, error = err))
  done <- 0L
  for (d in dates) {
    if (is.function(progress)) progress(done / length(dates), d)
    got <- tryCatch(fetch_fn(d, unique(todo$vehicle[todo$date == d])), error = function(e) e)
    if (inherits(got, "error")) return(list(cache = cache, fetched = done, error = conditionMessage(got)))
    for (v in names(got)) cache[[smtr_key(d, v)]] <- got[[v]]
    smtr_cache_save(cache, cache_file)
    done <- done + 1L
  }
  list(cache = cache, fetched = done, error = NULL)
}

# ---- Note ----

# The trip of entry e containing tap time t (epoch s), the Jaé line
# preferred. NULL when none contains it.
smtr_pick_trip <- function(trips, t, line) {
  if (is.null(trips) || nrow(trips) == 0) return(NULL)
  dep <- as.numeric(smtr_time(trips$datetime_partida))
  arr <- as.numeric(smtr_time(trips$datetime_chegada))
  k <- which(!is.na(dep) & !is.na(arr) & dep <= t & t <= arr)
  if (length(k) == 0) return(NULL)
  same <- k[toupper(trimws(trips$servico[k])) == toupper(line)]
  if (length(same)) k <- same
  trips[k[which.max(dep[k])], , drop = FALSE]
}

# Destination of a trip: the headsign of its GTFS trip_id, else of its
# line's direction (I -> 0, V -> 1; the most common headsign)
smtr_destination <- function(heads, trip) {
  if (is.null(heads) || nrow(heads) == 0 || is.null(trip)) return(NA_character_)
  if (!is.na(trip$trip_id) && nzchar(trip$trip_id)) {
    hit <- which(vapply(heads$trip_ids, function(x) trip$trip_id %in% x, logical(1)))
    if (length(hit)) return(heads$trip_headsign[hit[1]])
  }
  dir <- c(I = "0", V = "1")[toupper(trimws(trip$sentido))]
  if (is.na(dir)) return(NA_character_)
  h <- heads[heads$servico == trip$servico & heads$direction_id == dir, , drop = FALSE]
  if (nrow(h) == 0) return(NA_character_)
  h$trip_headsign[which.max(h$n)]
}

# Operator of the trip's validators valid at time t (epoch s), formatted
smtr_operator <- function(ops, trip, t) {
  if (is.null(ops) || nrow(ops) == 0 || is.null(trip)) return(NA_character_)
  vals <- unlist(trip$id_validador)
  from <- as.numeric(smtr_time(ops$datetime_inicio_validade))
  to <- as.numeric(smtr_time(ops$datetime_fim_validade))
  k <- which(ops$id_validador %in% vals & (is.na(from) | from <= t) & (is.na(to) | t <= to))
  if (length(k) == 0) return(NA_character_)
  smtr_title(names(sort(table(ops$operadora[k]), decreasing = TRUE))[1])
}

# Note body for one Jaé bus tap (a read_jae_boardings() row) from the cache
# (see smtr_prefetch). NULL when the tap is not a bus tap with a vehicle.
smtr_note_body <- function(tap, cache = smtr_cache()) {
  tp <- smtr_taps(tap)
  if (nrow(tp) == 0) return(NULL)
  tp <- tp[1, ]
  e <- cache[[smtr_key(tp$date, tp$vehicle)]]
  trip <- smtr_pick_trip(e$trips, tp$t, tp$line)
  vi <- e$vehicle_info
  have <- function(x) !is.null(x) && length(x) && !is.na(x[1]) && nzchar(trimws(as.character(x[1])))
  hhmm <- function(x) format(smtr_time(x), "%H:%M")

  servico <- if (!is.null(trip) && have(trip$servico)) trimws(trip$servico) else tp$line
  dest <- smtr_destination(e$headsigns, trip)
  veh_id <- if (!is.null(trip) && have(trip$id_veiculo)) trip$id_veiculo else if (have(vi$id_veiculo)) vi$id_veiculo else tp$vehicle
  consorcio <- if (!is.null(trip) && have(trip$consorcio)) trip$consorcio else if (have(tp$consorcio)) tp$consorcio else NA
  operator <- smtr_operator(e$operators, trip, tp$t)
  body_parts <- c(if (have(vi$carroceria)) smtr_title(vi$carroceria), if (have(vi$nome_chassi)) smtr_chassis(vi$nome_chassi),
                  if (have(vi$ano_fabricacao)) as.character(vi$ano_fabricacao))

  lines <- c(
    if (have(servico)) paste0("Linha ", servico, if (have(dest)) paste0(": ", dest)),
    paste0("Ônibus ", veh_id, if (have(vi$placa)) paste0(" - Placa ", vi$placa)),
    if (have(consorcio) || have(operator)) {
      if (have(consorcio) && have(operator)) sprintf("Consórcio %s, operadora %s", consorcio, operator)
      else if (have(consorcio)) paste("Consórcio", consorcio) else paste("Operadora", operator)
    },
    if (length(body_parts)) paste(body_parts, collapse = " · "),
    if (!is.null(trip) && have(trip$datetime_partida) && have(trip$datetime_chegada)) {
      sprintf("Início da viagem: %s | Fim da viagem: %s", hhmm(trip$datetime_partida), hhmm(trip$datetime_chegada))
    },
    if (!is.null(trip) && have(trip$quantidade_transacao)) {
      sprintf("Embarques totais na viagem: %d Jaé + %d RioCard", as.integer(trip$quantidade_transacao),
              as.integer(trip$quantidade_transacao_riocard %||% 0L))
    },
    if (is.null(e) || !smtr_entry_complete(e)) SMTR_NOTE_PENDING
  )
  paste(lines, collapse = "\n")
}
