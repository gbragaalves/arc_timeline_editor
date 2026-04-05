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

# Calculate route via Google Maps Directions API with mode=transit
# points: data.frame with lat and lng columns (minimum 2 rows)
# transit_mode: "subway", "train" or "tram"
# Returns: list(coords = matrix(lon, lat), distance_m, duration_s) or NULL
calculate_google_transit_route <- function(points, transit_mode = c("subway", "train", "tram")) {
  transit_mode <- match.arg(transit_mode)

  key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
  if (is.null(key) || !nzchar(key)) {
    message("[Google API] GOOGLE_MAPS_API_KEY not configured.")
    return(NULL)
  }

  if (nrow(points) < 2) {
    message("[Google API] At least 2 points required.")
    return(NULL)
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

  # Intermediate waypoints (if more than 2 points)
  if (nrow(points) > 2) {
    wps <- vapply(2:(nrow(points) - 1), function(i) {
      paste0(points$lat[i], ",", points$lng[i])
    }, character(1))
    params$waypoints <- paste(wps, collapse = "|")
  }

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

  if (is.null(js) || !identical(js$status, "OK")) {
    msg <- js$status %||% "unknown"
    detail <- js$error_message %||% ""
    message("[Google API] Status: ", msg, " ", detail)
    return(NULL)
  }

  if (length(js$routes) == 0) {
    message("[Google API] No route found.")
    return(NULL)
  }

  # Filter routes that actually use the requested transport mode
  accepted_types <- GOOGLE_ACCEPTED_VEHICLE_TYPES[[transit_mode]]
  route <- NULL

  for (r in js$routes) {
    vts <- extract_vehicle_types(r)
    if (any(vts %in% accepted_types)) {
      route <- r
      break
    }
  }

  if (is.null(route)) {
    found_modes <- unique(unlist(lapply(js$routes, extract_vehicle_types)))
    message(
      "[Google API] No route found using ", transit_mode,
      ". Modes found: ", paste(found_modes, collapse = ", ")
    )
    return(NULL)
  }

  # Extract only TRANSIT steps of the requested mode (no walking segments)
  coords_list <- list()
  total_distance <- 0
  total_duration <- 0

  for (leg in route$legs) {
    for (step in leg$steps) {
      if (!identical(step$travel_mode, "TRANSIT")) next
      vt <- step$transit_details$line$vehicle$type
      if (is.null(vt) || !(vt %in% accepted_types)) next

      # Decode individual step polyline
      enc <- step$polyline$points
      if (!is.null(enc) && nzchar(enc)) {
        step_coords <- decode_google_polyline(enc)
        if (nrow(step_coords) >= 2) {
          coords_list[[length(coords_list) + 1]] <- step_coords
        }
      }
      total_distance <- total_distance + (step$distance$value %||% 0)
      total_duration <- total_duration + (step$duration$value %||% 0)
    }
  }

  if (length(coords_list) == 0) {
    message("[Google API] No TRANSIT step of the requested mode has a polyline.")
    return(NULL)
  }

  # Concatenate coordinates from all accepted transit steps
  coords_mat <- do.call(rbind, coords_list)
  colnames(coords_mat) <- c("lon", "lat")

  if (nrow(coords_mat) < 2) {
    message("[Google API] Transit polyline with less than 2 points.")
    return(NULL)
  }

  list(
    coords     = coords_mat,
    distance_m = as.numeric(total_distance),
    duration_s = as.numeric(total_duration)
  )
}
