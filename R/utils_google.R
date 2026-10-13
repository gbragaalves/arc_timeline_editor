# ---- Helpers: Google Maps Directions API (transit) ----

# API key from environment variable (never hardcoded)
GOOGLE_MAPS_API_KEY <- Sys.getenv("GOOGLE_MAPS_API_KEY")

# Transit profiles that use Google instead of OSRM
GOOGLE_TRANSIT_MODES <- c("metro", "train", "tram")

# Mapping app profile to Google API transit_mode
GOOGLE_TRANSIT_MODE_MAP <- list(
  metro = "subway",
  train = "train",
  tram   = "tram"
)

# Check if the API key is configured and functional
check_google_api <- function() {
  key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
  if (is.null(key) || !nzchar(key)) return(FALSE)

  # Minimal request to validate the key
  url <- paste0(
    "https://maps.googleapis.com/maps/api/directions/json",
    "?origin=0,0&destination=0,0&mode=transit&key=", key
  )
  res <- tryCatch(httr::GET(url, httr::timeout(5)), error = function(e) NULL)
  if (is.null(res) || res$status_code >= 500) return(FALSE)

  # If it returns 200 (even without a route), the key is valid
  # REQUEST_DENIED indicates an invalid key
  js <- tryCatch(httr::content(res, as = "parsed", encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(js)) return(FALSE)
  if (identical(js$status, "REQUEST_DENIED")) return(FALSE)

  TRUE
}

# Decode Google Maps encoded polyline
# Returns n x 2 matrix (lon, lat)
decode_google_polyline <- function(encoded) {
  if (!nzchar(encoded)) return(matrix(numeric(0), ncol = 2))

  chars <- utf8ToInt(encoded)
  n <- length(chars)
  lat <- 0
  lng <- 0
  coords <- list()
  i <- 1


  while (i <= n) {
    # Decode latitude
    shift <- 0
    result <- 0
    repeat {
      b <- chars[i] - 63
      i <- i + 1
      result <- bitwOr(result, bitwAnd(b, 31) * (2^shift))
      shift <- shift + 5
      if (b < 32) break
    }
    dlat <- if (bitwAnd(result, 1) != 0) -bitwShiftR(result + 1, 1) else bitwShiftR(result, 1)
    lat <- lat + dlat

    # Decode longitude
    shift <- 0
    result <- 0
    repeat {
      b <- chars[i] - 63
      i <- i + 1
      result <- bitwOr(result, bitwAnd(b, 31) * (2^shift))
      shift <- shift + 5
      if (b < 32) break
    }
    dlng <- if (bitwAnd(result, 1) != 0) -bitwShiftR(result + 1, 1) else bitwShiftR(result, 1)
    lng <- lng + dlng

    coords[[length(coords) + 1]] <- c(lng / 1e5, lat / 1e5)
  }

  mat <- do.call(rbind, coords)
  colnames(mat) <- c("lon", "lat")
  mat
}

# Mapping: API transit_mode -> expected vehicle.type in the response
# The API returns vehicle.type in UPPERCASE (RAIL, SUBWAY, TRAM, BUS, etc.)
GOOGLE_ACCEPTED_VEHICLE_TYPES <- list(
  subway = c("SUBWAY", "METRO_RAIL"),
  train  = c("RAIL", "HEAVY_RAIL", "COMMUTER_TRAIN", "HIGH_SPEED_TRAIN",
             "LONG_DISTANCE_TRAIN"),
  tram   = c("TRAM", "LIGHT_RAIL")
)

# Extract vehicle.type from all TRANSIT steps of a route
extract_vehicle_types <- function(route) {
  types <- character(0)
  for (leg in route$legs) {
    for (step in leg$steps) {
      if (identical(step$travel_mode, "TRANSIT")) {
        vt <- step$transit_details$line$vehicle$type
        if (!is.null(vt)) types <- c(types, vt)
      }
    }
  }
  types
}

# Every step of a route, in order, for timing the transit path piecewise.
# kind: "transit" (a TRANSIT step of an accepted vehicle type), "other" (a
# TRANSIT step of another type) or "walk" (anything else). Each step:
# list(kind, coords (n x 2 lon/lat or NULL), duration_s, distance_m, line,
# vehicle, from_stop, to_stop, departure_t, arrival_t); times are epoch
# seconds from transit_details, NA when Google gives none.
transit_route_steps <- function(route, accepted_types) {
  num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
  out <- list()
  for (leg in route$legs) {
    for (step in leg$steps) {
      td <- step$transit_details
      vt <- td$line$vehicle$type
      kind <- if (!identical(step$travel_mode, "TRANSIT")) "walk"
              else if (!is.null(vt) && vt %in% accepted_types) "transit" else "other"
      enc <- step$polyline$points
      out[[length(out) + 1L]] <- list(
        kind        = kind,
        coords      = if (!is.null(enc) && nzchar(enc)) decode_google_polyline(enc) else NULL,
        duration_s  = num(step$duration$value),
        distance_m  = num(step$distance$value),
        line        = td$line$name %||% td$line$short_name,
        vehicle     = vt,
        from_stop   = td$departure_stop$name,
        to_stop     = td$arrival_stop$name,
        departure_t = num(td$departure_time$value),
        arrival_t   = num(td$arrival_time$value)
      )
    }
  }
  out
}

# ---- departure_time for transit requests ----
# Without departure_time Google plans for "now": at night or on weekends that
# can mean the metro is closed and only buses come back. Requests use a future
# instant instead (Google rejects past dates for transit).

# Timezone for the local time of day: the origin's, else America/Sao_Paulo
transit_local_tz <- function(lat, lng) {
  tz <- tryCatch(lutz::tz_lookup_coords(lat, lng, method = "fast", warn = FALSE),
                 error = function(e) NA_character_)
  if (length(tz) != 1 || is.na(tz) || !nzchar(tz)) "America/Sao_Paulo" else tz
}

# Next instant after now_utc with the same weekday and local time of day as
# ref_time_utc: same service pattern as the original trip (weekday vs weekend)
next_same_weekday_time <- function(ref_time_utc, tz, now_utc = Sys.time()) {
  ref_local <- lubridate::with_tz(ref_time_utc, tz)
  today <- as.Date(lubridate::with_tz(now_utc, tz), tz = tz)
  days_ahead <- (lubridate::wday(ref_local) - lubridate::wday(today)) %% 7
  at_date <- function(d) as.POSIXct(paste(d, format(ref_local, "%H:%M:%S")), tz = tz)
  cand <- at_date(today + days_ahead)
  if (cand <= now_utc) cand <- at_date(today + days_ahead + 7)
  cand
}

# Next weekday (Mon-Fri) at 10:00 local, after now_utc
next_weekday_10am <- function(tz, now_utc = Sys.time()) {
  today <- as.Date(lubridate::with_tz(now_utc, tz), tz = tz)
  for (i in 0:7) {
    d <- today + i
    if (lubridate::wday(d, week_start = 1) > 5) next
    cand <- as.POSIXct(paste(d, "10:00:00"), tz = tz)
    if (cand > now_utc) return(cand)
  }
}

transit_departure_time <- function(ref_time_utc, tz, now_utc = Sys.time()) {
  if (is.null(ref_time_utc) || length(ref_time_utc) == 0 || is.na(ref_time_utc[1])) {
    next_weekday_10am(tz, now_utc)
  } else {
    next_same_weekday_time(ref_time_utc[1], tz, now_utc)
  }
}

# One Directions request. Returns list(status, routes, error_message) or NULL on
# a request/HTTP/parse failure.
google_directions_get <- function(params) {
  url <- "https://maps.googleapis.com/maps/api/directions/json"
  res <- tryCatch(
    httr::GET(url, query = params, httr::timeout(30)),
    error = function(e) {
      message("[Google API] Request error: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(res) || res$status_code >= 400) {
    message("[Google API] Request failed (status: ", res$status_code %||% "NULL", ")")
    return(NULL)
  }

  js <- tryCatch(
    httr::content(res, as = "parsed", encoding = "UTF-8"),
    error = function(e) {
      message("[Google API] Error parsing response: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(js)) return(NULL)

  list(status = js$status %||% "unknown", routes = js$routes %||% list(),
       error_message = js$error_message %||% "")
}

# Calculate route via Google Maps Directions API with mode=transit
# points: data.frame with lat and lng columns (minimum 2 rows)
# transit_mode: "subway", "train" or "tram"
# ref_time_utc: start of the original trip (POSIXct) or NULL. The request uses
#   the next future instant with its weekday and local time of day (NULL: next
#   weekday at 10:00); if that returns no route of the requested mode (e.g. metro
#   closed at night), it retries once at the next weekday at 10:00.
# Returns: list(coords = matrix(lon, lat), distance_m, duration_s, steps) or
# NULL. coords, distance_m and duration_s cover the TRANSIT steps of the
# requested mode only (no walking); steps: transit_route_steps(), all steps.
# on_fail(code, detail) is told why when NULL: "no_key", "request" (request,
# HTTP or parse failure, timeout), "status" (detail: Google's status),
# "zero_results", "no_transit" (routes without a leg of the mode; detail: the
# vehicle types they have, "" when walking only), "no_polyline", "points".
calculate_google_transit_route <- function(points, transit_mode = c("subway", "train", "tram"),
                                           ref_time_utc = NULL, on_fail = NULL) {
  transit_mode <- match.arg(transit_mode)
  fail <- function(code, detail = "") {
    if (is.function(on_fail)) on_fail(code, detail)
    NULL
  }

  key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
  if (is.null(key) || !nzchar(key)) {
    message("[Google API] GOOGLE_MAPS_API_KEY not configured.")
    return(fail("no_key"))
  }

  if (nrow(points) < 2) {
    message("[Google API] At least 2 points required.")
    return(fail("points"))
  }

  origin <- paste0(points$lat[1], ",", points$lng[1])
  destination <- paste0(points$lat[nrow(points)], ",", points$lng[nrow(points)])

  params <- list(
    origin       = origin,
    destination  = destination,
    mode         = "transit",
    transit_mode = transit_mode,
    alternatives = "true",
    key          = key
  )

  # Transit directions don't support waypoints (MAX_WAYPOINTS_EXCEEDED):
  # always route origin -> destination only
  if (nrow(points) > 2) {
    message(sprintf("[Google API] Transit: using origin/destination only (%d intermediate points ignored)",
                    nrow(points) - 2L))
  }

  tz <- transit_local_tz(points$lat[1], points$lng[1])
  fmt_departure <- function(t) {
    sprintf("%d (%s)", as.integer(as.numeric(t)), format(t, "%a %Y-%m-%d %H:%M %Z", tz = tz))
  }
  accepted_types <- GOOGLE_ACCEPTED_VEHICLE_TYPES[[transit_mode]]

  # Request at `departure`; returns list(route, found_modes, n_routes), or
  # list(fail = c(code, detail)) when the request itself failed (no retry then)
  request_at <- function(departure) {
    params$departure_time <- as.character(as.integer(as.numeric(departure)))
    message("[Google API] Transit departure_time: ", fmt_departure(departure))
    js <- google_directions_get(params)
    if (is.null(js)) return(list(fail = c("request", "")))
    if (!js$status %in% c("OK", "ZERO_RESULTS")) {
      message("[Google API] Status: ", js$status, " ", js$error_message)
      return(list(fail = c("status", js$status)))
    }

    # First route that actually uses the requested transport mode
    for (r in js$routes) {
      if (any(extract_vehicle_types(r) %in% accepted_types)) return(list(route = r))
    }
    list(route = NULL, found_modes = unique(unlist(lapply(js$routes, extract_vehicle_types))), n_routes = length(js$routes))
  }

  departure <- transit_departure_time(ref_time_utc, tz)
  attempt <- request_at(departure)
  if (!is.null(attempt$fail)) return(fail(attempt$fail[1], attempt$fail[2]))

  # Requested mode not running at that time (e.g. metro closed at night):
  # retry once at the next weekday at 10:00
  if (is.null(attempt$route)) {
    retry_departure <- next_weekday_10am(tz)
    if (!identical(as.numeric(retry_departure), as.numeric(departure))) {
      message(
        "[Google API] No ", transit_mode, " route at ", fmt_departure(departure),
        " (modes found: ", paste(attempt$found_modes, collapse = ", "),
        "). Retrying at the next weekday 10:00."
      )
      attempt <- request_at(retry_departure)
      if (!is.null(attempt$fail)) return(fail(attempt$fail[1], attempt$fail[2]))
    }
  }

  route <- attempt$route
  if (is.null(route)) {
    message(
      "[Google API] No route found using ", transit_mode,
      ". Modes found: ", paste(attempt$found_modes, collapse = ", ")
    )
    if (!isTRUE(attempt$n_routes > 0)) return(fail("zero_results"))
    return(fail("no_transit", paste(attempt$found_modes, collapse = ",")))
  }

  # Extract only TRANSIT steps of the requested mode (no walking segments)
  steps <- transit_route_steps(route, accepted_types)
  transit <- Filter(function(st) identical(st$kind, "transit"), steps)
  coords_list <- Filter(function(m) !is.null(m) && nrow(m) >= 2, lapply(transit, `[[`, "coords"))
  total_distance <- sum(vapply(transit, `[[`, numeric(1), "distance_m"), na.rm = TRUE)
  total_duration <- sum(vapply(transit, `[[`, numeric(1), "duration_s"), na.rm = TRUE)

  if (length(coords_list) == 0) {
    message("[Google API] No TRANSIT step of the requested mode has a polyline.")
    return(fail("no_polyline"))
  }

  # Concatenate coordinates from all accepted transit steps
  coords_mat <- do.call(rbind, coords_list)
  colnames(coords_mat) <- c("lon", "lat")

  if (nrow(coords_mat) < 2) {
    message("[Google API] Transit polyline with less than 2 points.")
    return(fail("no_polyline"))
  }

  list(
    coords     = coords_mat,
    distance_m = as.numeric(total_distance),
    duration_s = as.numeric(total_duration),
    steps      = steps
  )
}
