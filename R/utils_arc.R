# ---- Helpers Arc: samples & timeline items (LocoKit2 format) ----

# Null coalescing operator
`%||%` <- function(x, y) if (!is.null(x)) x else y

# ---- Activity Type mapping (LocoKit2 integer codes) ----
ACTIVITY_TYPES <- list(
  stationary    = 1L,
  walking       = 2L,
  cycling       = 4L,
  car           = 5L,
  airplane      = 6L,
  train         = 20L,
  bus           = 21L,
  tram          = 24L,
  tuk_tuk       = 26L,
  metro         = 29L,
  cable_car     = 30L,
  funicular     = 31L,
  taxi          = 34L,
  skateboarding = 50L
)

# Reverse mapping: code -> name
ACTIVITY_TYPE_NAMES <- stats::setNames(
  names(ACTIVITY_TYPES),
  vapply(ACTIVITY_TYPES, as.character, "")
)

# Convert activity name (string) to LocoKit2 integer code
activity_type_code <- function(name) {
  code <- ACTIVITY_TYPES[[name]]
  if (is.null(code)) {
    warning("Unknown activity type: ", name, ". Defaulting to 'car' (5).")
    return(5L)
  }
  code
}

# ---- Moving/Recording state (inteiros no LocoKit2) ----
# movingState:  -1 = unknown, 0 = stationary, 1 = moving
# recordingState: 1 = off/sleeping, 2 = recording

moving_state_code <- function(state_str) {
  switch(state_str,
    "moving"     = 1L,
    "stationary" = 0L,
    -1L
  )
}

# ---- Helpers for accessing sample fields (LocoKit2 compatible) ----
sample_id <- function(s)  s$id %||% s$sampleId %||% ""
sample_lat <- function(s) as.numeric(s$latitude %||% s$location$latitude)[1]
sample_lon <- function(s) as.numeric(s$longitude %||% s$location$longitude)[1]

sample_has_coords <- function(s) {
  lat <- s$latitude %||% s$location$latitude
  lon <- s$longitude %||% s$location$longitude
  !is.null(lat) && !is.null(lon) && length(lat) > 0 && length(lon) > 0
}

# ---- Helpers for accessing TimelineItem fields (LocoKit2 compatible) ----
item_start_date <- function(it) it$base$startDate %||% it$startDate$date %||% it$startDate
item_end_date   <- function(it) it$base$endDate   %||% it$endDate$date   %||% it$endDate

# secondsFromGMT: not present in LocoKit2 items, computed from coordinates
item_start_offset <- function(it) {
  # Try old format first
  off <- it$startDate$secondsFromGMT
  if (!is.null(off)) return(off)
  # LocoKit2: use system timezone
  ts_utc <- parse_timestamp_utc(item_start_date(it))
  if (is.null(ts_utc) || is.na(ts_utc[1])) return(0L)
  seconds_from_gmt(ts_utc[1], Sys.timezone())
}

item_end_offset <- function(it) {
  off <- it$endDate$secondsFromGMT
  if (!is.null(off)) return(off)
  item_start_offset(it)
}

# Set start/end date on item (LocoKit2 format compatible)
set_item_start_date <- function(it, date_str) {
  if (!is.null(it$base)) {
    it$base$startDate <- date_str
  } else {
    it$startDate$date <- date_str
  }
  it
}

set_item_end_date <- function(it, date_str) {
  if (!is.null(it$base)) {
    it$base$endDate <- date_str
  } else {
    it$endDate$date <- date_str
  }
  it
}

# ---- Create LocomotionSamples (LocoKit2 flat format) ----
criar_locomotion_samples <- function(coords,
                                     timestamps_utc,
                                     altitude = NULL,
                                     speed = NULL,
                                     heading = NULL,
                                     accuracy = 10,
                                     force_single_tz = FALSE,
                                     moving_state = "moving",
                                     activity_type = "car") {

  n <- nrow(coords)
  if (length(timestamps_utc) != n) stop("coords and timestamps_utc have different lengths.")

  if (is.null(altitude)) altitude <- rep(0, n)
  if (is.null(speed))    speed    <- rep(0, n)
  if (is.null(heading))  heading  <- calcular_bearings(coords)

  if (force_single_tz) {
    tz_all <- rep(tz_from_coords(coords[1, 2], coords[1, 1]), n)
  } else {
    tz_all <- vapply(
      seq_len(n),
      function(i) tz_from_coords(coords[i, 2], coords[i, 1]),
      character(1)
    )
  }

  ms_code <- moving_state_code(moving_state)
  at_code <- activity_type_code(activity_type)

  samples <- vector("list", n)

  for (i in seq_len(n)) {
    ts_utc <- timestamps_utc[i]
    tz_i   <- tz_all[i]
    if (is.na(tz_i)) tz_i <- "UTC"

    sec_gmt <- seconds_from_gmt(ts_utc, tz_i)
    date_str <- format(ts_utc, "%Y-%m-%dT%H:%M:%SZ")

    samples[[i]] <- list(
      id                     = toupper(uuid::UUIDgenerate(use.time = TRUE)),
      source                 = "LocoKit2",
      sourceVersion          = "9.0.0",
      date                   = date_str,
      secondsFromGMT         = sec_gmt,
      latitude               = coords[i, 2],
      longitude              = coords[i, 1],
      altitude               = ifelse(is.na(altitude[i]), 0, unname(altitude[i])),
      horizontalAccuracy     = accuracy,
      verticalAccuracy       = 10,
      speed                  = ifelse(is.na(speed[i]), 0, unname(speed[i])),
      course                 = ifelse(is.na(heading[i]), 0, unname(heading[i])),
      movingState            = ms_code,
      recordingState         = 2L,
      classifiedActivityType = at_code,
      confirmedActivityType  = at_code,
      stepHz                 = 0,
      disabled               = FALSE,
      lastSaved              = date_str
    )
  }

  samples
}

# ---- Create visit TimelineItem (LocoKit2 format: base + visit) ----
criar_timeline_item_visit <- function(lat, lon, inicio_local, fim_local, nome = NULL) {
  tz <- tz_from_coords(lat, lon)
  if (is.na(tz)) tz <- "UTC"

  inicio_utc <- lubridate::with_tz(inicio_local, tzone = "UTC")
  fim_utc    <- lubridate::with_tz(fim_local,    tzone = "UTC")

  sec_ini <- seconds_from_gmt(inicio_utc, tz)

  start_date_str <- format(inicio_utc, "%Y-%m-%dT%H:%M:%SZ")
  end_date_str   <- format(fim_utc,    "%Y-%m-%dT%H:%M:%SZ")

  n_samp <- max(3L, min(20L, as.integer(as.numeric(difftime(fim_utc, inicio_utc, units = "mins")) / 5)))
  if (is.na(n_samp) || n_samp < 1) n_samp <- 3L

  ts_seq <- seq(inicio_utc, fim_utc, length.out = n_samp)
  coords <- cbind(rep(lon, n_samp), rep(lat, n_samp))

  samples <- criar_locomotion_samples(
    coords         = coords,
    timestamps_utc = ts_seq,
    altitude       = rep(0, n_samp),
    moving_state   = "stationary",
    accuracy       = 10,
    force_single_tz = TRUE,
    activity_type  = "stationary"
  )

  sample_ids <- vapply(samples, `[[`, "", "id")

  item_id  <- toupper(uuid::UUIDgenerate(use.time = TRUE))
  place_id <- toupper(uuid::UUIDgenerate(use.time = TRUE))
  now_utc  <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")

  place_name <- nome %||% "Visit"

  # Place object (LocoKit2 format)
  place <- list(
    id             = place_id,
    source         = "LocoKit2",
    name           = place_name,
    latitude       = lat,
    longitude      = lon,
    radiusMean     = 25,
    radiusSD       = 0,
    secondsFromGMT = sec_ini,
    isStale        = FALSE,
    visitCount     = 1L,
    visitDays      = 1L,
    lastSaved      = now_utc,
    lastVisitDate  = end_date_str
  )

  item <- list(
    .internalId = item_id,
    base = list(
      id              = item_id,
      source          = "LocoKit2",
      sourceVersion   = "9.0.0",
      isVisit         = TRUE,
      startDate       = start_date_str,
      endDate         = end_date_str,
      lastSaved       = now_utc,
      deleted         = FALSE,
      disabled        = FALSE,
      locked          = FALSE,
      samplesChanged  = FALSE,
      stepCount       = 0L,
      activeEnergyBurned = 0
    ),
    visit = list(
      itemId          = item_id,
      placeId         = place_id,
      latitude        = lat,
      longitude       = lon,
      radiusMean      = 25,
      radiusSD        = 0,
      confirmedPlace  = FALSE,
      uncertainPlace  = TRUE,
      lastSaved       = now_utc
    ),
    # Internal fields used by the Shiny app
    .isVisit = TRUE,
    samples = sample_ids,
    .place = place
  )

  if (!is.null(nome) && nzchar(nome)) {
    item$visit$customTitle <- nome
  }

  list(item = item, samples = samples, place = place)
}

# ---- Create trip TimelineItem (LocoKit2 format: base + trip) ----
criar_timeline_item_path <- function(timestamps_utc,
                                     coords,
                                     sample_ids,
                                     tipo = "rota",
                                     descricao = NULL,
                                     activity_type = "car") {

  n <- length(timestamps_utc)
  if (n == 0) stop("No timestamps to create path.")
  if (nrow(coords) != n) stop("timestamps and coords have different lengths.")

  ini_utc <- timestamps_utc[1]
  fim_utc <- timestamps_utc[n]

  item_id <- toupper(uuid::UUIDgenerate(use.time = TRUE))
  now_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")

  at_code <- activity_type_code(activity_type)

  # Calculate total distance (m)
  total_dist <- 0
  if (n >= 2) {
    for (i in 2:n) {
      total_dist <- total_dist + geosphere::distHaversine(
        c(coords[i - 1, 1], coords[i - 1, 2]),
        c(coords[i, 1], coords[i, 2])
      )
    }
  }

  duration_s <- as.numeric(difftime(fim_utc, ini_utc, units = "secs"))
  avg_speed <- if (duration_s > 0) total_dist / duration_s else 0

  item <- list(
    .internalId = item_id,
    base = list(
      id              = item_id,
      source          = "LocoKit2",
      sourceVersion   = "9.0.0",
      isVisit         = FALSE,
      startDate       = format(ini_utc, "%Y-%m-%dT%H:%M:%SZ"),
      endDate         = format(fim_utc, "%Y-%m-%dT%H:%M:%SZ"),
      lastSaved       = now_utc,
      deleted         = FALSE,
      disabled        = FALSE,
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
      distance               = total_dist,
      speed                  = avg_speed,
      lastSaved              = now_utc
    ),
    # Internal fields used by the Shiny app
    .isVisit = FALSE,
    samples = sample_ids,
    tipo = tipo,
    descricao = descricao %||% "Trip",
    activityType = activity_type
  )

  item
}
