# ---- Stop names for split waits ----
# Name of the stop where a Jaé boarding waited, in this order:
#   1. SMTR GTFS (Rio): the nearest stop within STOP_MATCH_RADIUS_M of the
#      boarding point; a platform / child stop gives its parent_station's name
#      (BRT stations, terminals). reference/gtfs_stops_rio.csv, refreshed from
#      BigQuery by refresh_gtfs_stops() (tools/refresh_gtfs_stops.R).
#   2. Google Places Nearby Search (transit stop types), only when GTFS has
#      nothing (outside Rio, or no stop that close); results cached by rounded
#      coordinates in reference/places_cache.rds.
#   3. A generic name ("Ponto de Ônibus" / "Bus stop").
# OpenStreetMap is not used: its stop names are poor in Rio.

GTFS_STOPS_FILE      <- if (exists("GTFS_STOPS_FILE")) GTFS_STOPS_FILE else file.path("reference", "gtfs_stops_rio.csv")
PLACES_CACHE_FILE    <- if (exists("PLACES_CACHE_FILE")) PLACES_CACHE_FILE else file.path("reference", "places_cache.rds")
GTFS_BQ_PROJECT      <- if (exists("GTFS_BQ_PROJECT")) GTFS_BQ_PROJECT else "rj-smtr-dev"  # BigQuery billing project
GTFS_BQ_EMAIL        <- if (exists("GTFS_BQ_EMAIL")) GTFS_BQ_EMAIL else NULL            # Google account for bigrquery
STOP_MATCH_RADIUS_M  <- 60
PLACES_STOP_TYPES    <- c("bus_stop", "bus_station", "transit_station")

# Session state: GTFS table (by file mtime), Places disabled flag
.stops_state <- new.env(parent = emptyenv())

# ---- GTFS (SMTR) ----

# Latest feed of rj-smtr.gtfs.stops (feed_start_date = MAX) to `out` (CSV,
# UTF-8). bigrquery with the billing project; email picks the cached Google
# login (a browser sign-in is needed when there is none). Returns
# list(rows, feed_start_date, file).
refresh_gtfs_stops <- function(out = GTFS_STOPS_FILE, billing = GTFS_BQ_PROJECT, email = GTFS_BQ_EMAIL) {
  bigrquery::bq_auth(email = email %||% TRUE, cache = gargle::gargle_oauth_cache())
  sql <- paste(
    "SELECT stop_id, stop_name, stop_lat, stop_lon, location_type, parent_station, feed_start_date",
    "FROM `rj-smtr.gtfs.stops`",
    "WHERE feed_start_date = (SELECT MAX(feed_start_date) FROM `rj-smtr.gtfs.stops`)"
  )
  d <- bigrquery::bq_table_download(bigrquery::bq_project_query(billing, sql), n_max = 50000)
  d <- as.data.frame(d)
  d$feed_start_date <- as.character(d$feed_start_date)
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(d, out, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  .stops_state$gtfs_key <- NULL
  list(rows = nrow(d), feed_start_date = unique(d$feed_start_date), file = out)
}

# GTFS stops (cached per file mtime); NULL when the file is missing
gtfs_stops <- function(file = GTFS_STOPS_FILE) {
  if (!file.exists(file)) return(NULL)
  key <- paste(normalizePath(file), as.numeric(file.mtime(file)))
  if (identical(.stops_state$gtfs_key, key)) return(.stops_state$gtfs)
  d <- utils::read.csv(file, colClasses = "character", fileEncoding = "UTF-8", na.strings = character(0))
  d$stop_lat <- as.numeric(d$stop_lat)
  d$stop_lon <- as.numeric(d$stop_lon)
  d <- d[!is.na(d$stop_lat) & !is.na(d$stop_lon) & nzchar(d$stop_name), ]
  .stops_state$gtfs <- d
  .stops_state$gtfs_key <- key
  d
}

# Nearest GTFS stop within radius_m of (lon, lat): list(name, source = "gtfs",
# distance_m, stop_id), the name being the parent station's when the stop has
# one. NULL when none.
gtfs_stop_name <- function(lon, lat, radius_m = STOP_MATCH_RADIUS_M, stops = gtfs_stops()) {
  if (is.null(stops) || nrow(stops) == 0 || is.na(lon) || is.na(lat)) return(NULL)
  dlat <- radius_m / 111320
  dlon <- dlat / cos(lat * pi / 180)
  k <- which(abs(stops$stop_lat - lat) <= dlat & abs(stops$stop_lon - lon) <= dlon)
  if (length(k) == 0) return(NULL)
  d <- geosphere::distHaversine(cbind(stops$stop_lon[k], stops$stop_lat[k]), c(lon, lat))
  if (min(d) > radius_m) return(NULL)
  j <- k[which.min(d)]
  name <- stops$stop_name[j]
  parent <- stops$parent_station[j]
  if (!is.na(parent) && nzchar(parent)) {
    p <- match(parent, stops$stop_id)
    if (!is.na(p) && nzchar(stops$stop_name[p])) name <- stops$stop_name[p]
  }
  list(name = name, source = "gtfs", distance_m = min(d), stop_id = stops$stop_id[j])
}

# ---- Google Places ----

# Nearest transit stop within radius_m from Google Places Nearby Search
# (Places API (New); the legacy Nearby Search when only that one is enabled).
# Results, including "none", are cached by coordinates rounded to 4 decimals
# (~11 m). When the key has neither API enabled, says so once per session and
# returns NULL from then on. list(name, source = "google", distance_m) or NULL.
google_stop_name <- function(lon, lat, radius_m = STOP_MATCH_RADIUS_M, key = Sys.getenv("GOOGLE_MAPS_API_KEY"),
                             cache_file = PLACES_CACHE_FILE, request_fn = places_nearby) {
  if (!nzchar(key) || isTRUE(.stops_state$places_disabled) || is.na(lon) || is.na(lat)) return(NULL)
  ckey <- sprintf("%.4f,%.4f", lat, lon)
  cache <- places_cache(cache_file)
  hit <- cache[[ckey]]
  if (is.null(hit)) {
    res <- request_fn(lon, lat, radius_m, key)
    if (identical(res$status, "disabled")) {
      .stops_state$places_disabled <- TRUE
      message("[Stops] Google Places is not enabled for GOOGLE_MAPS_API_KEY: stop names from GTFS or generic only.")
      return(NULL)
    }
    if (!res$status %in% c("ok", "none")) return(NULL)  # transient error: not cached
    hit <- list(name = res$name %||% NA_character_, distance_m = res$distance_m %||% NA_real_, api = res$api,
                at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    cache[[ckey]] <- hit
    places_cache_save(cache, cache_file)
  }
  if (is.na(hit$name) || !nzchar(hit$name)) return(NULL)
  list(name = hit$name, source = "google", distance_m = hit$distance_m)
}

places_cache <- function(file = PLACES_CACHE_FILE) {
  if (!file.exists(file)) return(list())
  tryCatch(readRDS(file), error = function(e) list())
}

places_cache_save <- function(cache, file = PLACES_CACHE_FILE) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cache, file)
}

# One Nearby Search: list(status = "ok" | "none" | "disabled" | "error", name,
# distance_m, api). Places API (New) first; when it is not enabled, the legacy
# endpoint (one type: transit_station).
places_nearby <- function(lon, lat, radius_m, key) {
  new <- places_nearby_new(lon, lat, radius_m, key)
  if (!identical(new$status, "disabled")) return(new)
  places_nearby_legacy(lon, lat, radius_m, key)
}

places_nearby_new <- function(lon, lat, radius_m, key) {
  body <- list(
    includedTypes = as.list(PLACES_STOP_TYPES), maxResultCount = 5, rankPreference = "DISTANCE",
    locationRestriction = list(circle = list(center = list(latitude = lat, longitude = lon), radius = radius_m))
  )
  res <- tryCatch(httr::POST(
    "https://places.googleapis.com/v1/places:searchNearby",
    httr::add_headers(`X-Goog-Api-Key` = key, `X-Goog-FieldMask` = "places.displayName,places.location"),
    body = jsonlite::toJSON(body, auto_unbox = TRUE, digits = NA), httr::content_type_json(), httr::timeout(15)
  ), error = function(e) NULL)
  if (is.null(res)) return(list(status = "error", api = "new"))
  js <- tryCatch(httr::content(res, as = "parsed", encoding = "UTF-8"), error = function(e) NULL)
  if (httr::status_code(res) %in% c(401, 403)) {
    return(list(status = if (places_disabled_reply(js)) "disabled" else "error", api = "new"))
  }
  if (httr::status_code(res) != 200) return(list(status = "error", api = "new"))
  pl <- js$places %||% list()
  if (length(pl) == 0) return(list(status = "none", api = "new"))
  d <- vapply(pl, function(p) geosphere::distHaversine(c(p$location$longitude, p$location$latitude), c(lon, lat)), 0)
  best <- pl[[which.min(d)]]
  list(status = "ok", name = best$displayName$text %||% NA_character_, distance_m = min(d), api = "new")
}

places_nearby_legacy <- function(lon, lat, radius_m, key) {
  res <- tryCatch(httr::GET(
    "https://maps.googleapis.com/maps/api/place/nearbysearch/json",
    query = list(location = sprintf("%.6f,%.6f", lat, lon), radius = radius_m, type = "transit_station", key = key),
    httr::timeout(15)
  ), error = function(e) NULL)
  if (is.null(res)) return(list(status = "error", api = "legacy"))
  js <- tryCatch(httr::content(res, as = "parsed", encoding = "UTF-8"), error = function(e) NULL)
  st <- js$status %||% ""
  if (identical(st, "REQUEST_DENIED")) return(list(status = "disabled", api = "legacy"))
  if (identical(st, "ZERO_RESULTS")) return(list(status = "none", api = "legacy"))
  if (!identical(st, "OK") || length(js$results) == 0) return(list(status = "error", api = "legacy"))
  d <- vapply(js$results, function(p) geosphere::distHaversine(c(p$geometry$location$lng, p$geometry$location$lat), c(lon, lat)), 0)
  list(status = "ok", name = js$results[[which.min(d)]]$name %||% NA_character_, distance_m = min(d), api = "legacy")
}

# Places replies that mean "this API is not enabled / not allowed for the key"
places_disabled_reply <- function(js) {
  txt <- paste(unlist(js), collapse = " ")
  grepl("SERVICE_DISABLED|API_KEY_SERVICE_BLOCKED|has not been used|is disabled|not authorized to use this API|PERMISSION_DENIED", txt)
}

# ---- Chain ----

# Name of the stop at (lon, lat): GTFS, then Google, then generic.
# list(name, source = "gtfs" | "google" | "generic", distance_m)
stop_name_for <- function(lon, lat, lang = APP_LANGUAGE, gtfs_fn = gtfs_stop_name, google_fn = google_stop_name) {
  hit <- gtfs_fn(lon, lat)
  if (is.null(hit)) hit <- google_fn(lon, lat)
  if (is.null(hit)) hit <- list(name = tr_lang("Bus stop", lang), source = "generic", distance_m = NA_real_)
  hit
}

# Label of a name source for the UI: GTFS, Google, genérico / generic, estação / station
stop_source_label <- function(source, lang = APP_LANGUAGE) {
  switch(source, gtfs = "GTFS", google = "Google", generic = tr_lang("generic", lang),
         station = tr_lang("station", lang), source)
}
