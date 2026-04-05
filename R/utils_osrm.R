# ---- Helpers: OSRM ----

# Smooth OSRM route by removing redundant points and applying simplification
# coords: n x 2 matrix (lon, lat)
# tolerancia_m: minimum distance between consecutive points (meters)
# angulo_min: minimum direction change angle to keep a point (degrees)
suavizar_rota_osrm <- function(coords, tolerancia_m = 5, angulo_min = 1) {
  if (!is.matrix(coords)) coords <- as.matrix(coords)
  n <- nrow(coords)
  if (n < 3) return(coords)

  # Step 1: Remove duplicate or very close points
  manter <- rep(TRUE, n)
  for (i in 2:n) {
    dist <- geosphere::distHaversine(
      c(coords[i - 1, 1], coords[i - 1, 2]),
      c(coords[i, 1], coords[i, 2])
    )
    if (dist < tolerancia_m) {
      manter[i] <- FALSE
    }
  }
  # Always keep first and last
  manter[1] <- TRUE
  manter[n] <- TRUE
  coords <- coords[manter, , drop = FALSE]
  n <- nrow(coords)
  if (n < 3) return(coords)

  # Step 2: Remove collinear points (no significant direction change)
  manter <- rep(FALSE, n)
  manter[1] <- TRUE  # first
  manter[n] <- TRUE  # last

  for (i in 2:(n - 1)) {
    # Bearing of the previous segment
    b1 <- geosphere::bearing(
      c(coords[i - 1, 1], coords[i - 1, 2]),
      c(coords[i, 1], coords[i, 2])
    )
    # Bearing of the next segment
    b2 <- geosphere::bearing(
      c(coords[i, 1], coords[i, 2]),
      c(coords[i + 1, 1], coords[i + 1, 2])
    )

    # Angular difference (accounting for wrap-around)
    diff_ang <- abs((b2 - b1 + 180) %% 360 - 180)

    # Keep if direction change > angulo_min
    if (diff_ang > angulo_min) {
      manter[i] <- TRUE
    }
  }

  coords <- coords[manter, , drop = FALSE]
  n <- nrow(coords)
  if (n < 2) return(coords)

  # Step 3: Ensure minimum spacing between kept points
  # (avoids clusters of very close points after angle filtering)
  final <- matrix(coords[1, ], nrow = 1)
  colnames(final) <- colnames(coords)

  for (i in 2:(n - 1)) {
    dist_ultimo <- geosphere::distHaversine(
      c(final[nrow(final), 1], final[nrow(final), 2]),
      c(coords[i, 1], coords[i, 2])
    )
    if (dist_ultimo >= tolerancia_m) {
      final <- rbind(final, coords[i, ])
    }
  }

  # Always include the last point
  final <- rbind(final, coords[n, ])

  final
}

# Local OSRM servers (adjust ports if needed)
OSRM_SERVERS <- list(
  car  = "http://127.0.0.1:5000",
  foot = "http://127.0.0.1:5001",
  bike = "http://127.0.0.1:5002",
  bus  = "http://127.0.0.1:5003"
)

# Mapping of app profiles to OSRM profiles
OSRM_PROFILES <- list(
  car  = "driving",
  foot = "foot",
  bike = "bicycle",
  bus  = "driving"  # custom profile based on driving
)

check_osrm_server <- function(base_url, perfil = "car") {
  # try a minimal request just to check if the server responds
  osrm_profile <- OSRM_PROFILES[[perfil]] %||% "driving"
  test_url <- paste0(base_url, "/route/v1/", osrm_profile, "/0,0;0,0")
  res <- tryCatch(
    httr::GET(test_url, httr::timeout(2)),
    error = function(e) NULL
  )
  !is.null(res)
}

calcular_rota_osrm <- function(pontos, perfil = c("car", "foot", "bike", "bus")) {
  perfil <- match.arg(perfil)
  base_url <- OSRM_SERVERS[[perfil]]
  osrm_profile <- OSRM_PROFILES[[perfil]] %||% "driving"
  if (is.null(base_url)) return(NULL)

  if (!check_osrm_server(base_url, perfil)) {
    return(NULL)
  }

  coords_str <- apply(pontos[, c("lng", "lat")], 1, paste, collapse = ",")
  url <- paste0(
    base_url, "/route/v1/", osrm_profile, "/",
    paste(coords_str, collapse = ";"),
    "?overview=full&geometries=geojson"
  )

  res <- tryCatch(httr::GET(url, httr::timeout(30)), error = function(e) NULL)
  if (is.null(res) || res$status_code >= 400) return(NULL)

  js <- httr::content(res, as = "parsed", encoding = "UTF-8")
  if (is.null(js$routes) || length(js$routes) == 0) return(NULL)

  route <- js$routes[[1]]
  coords <- route$geometry$coordinates

  # Convert coordinate list to numeric matrix
  n_coords <- length(coords)
  coords_mat <- matrix(0, nrow = n_coords, ncol = 2)
  for (i in seq_len(n_coords)) {
    pt <- coords[[i]]
    if (is.list(pt)) pt <- unlist(pt)
    coords_mat[i, 1] <- as.numeric(pt[1])  # lon
    coords_mat[i, 2] <- as.numeric(pt[2])  # lat
  }
  colnames(coords_mat) <- c("lon", "lat")

  list(
    coords      = coords_mat,
    distance_m  = as.numeric(route$distance),
    duration_s  = as.numeric(route$duration)
  )
}

# Recalculate a route segment between two points via OSRM
# Returns: list(coords, distance_m)
recalcular_segmento <- function(from_lat, from_lng, to_lat, to_lng,
                                perfil = c("car", "foot", "bike", "bus",
                                           "metro", "train", "tram")) {
  perfil <- match.arg(perfil)

  pts <- data.frame(
    lat = c(from_lat, to_lat),
    lng = c(from_lng, to_lng)
  )

  # Perfis de transit usam Google Maps API
  if (perfil %in% GOOGLE_TRANSIT_MODES) {
    transit_mode <- GOOGLE_TRANSIT_MODE_MAP[[perfil]]
    rota <- calcular_rota_google_transit(pts, transit_mode = transit_mode)
  } else {
    rota <- calcular_rota_osrm(pts, perfil = perfil)
  }

  if (is.null(rota) || is.null(rota$coords)) {
    # Fallback: straight line between points
    coords <- matrix(c(from_lng, to_lng, from_lat, to_lat), ncol = 2)
    colnames(coords) <- c("lon", "lat")
    distance_m <- geosphere::distHaversine(c(from_lng, from_lat), c(to_lng, to_lat))

    return(list(
      coords = coords,
      distance_m = distance_m
    ))
  }

  # Apply smoothing to reduce redundant points
  coords_smooth <- suavizar_rota_osrm(rota$coords, tolerancia_m = 5, angulo_min = 2)

  list(
    coords = coords_smooth,
    distance_m = rota$distance_m
  )
}

# Recalculate timestamps for an edited route keeping original average speed
# coords: n x 2 matrix (lon, lat)
# start_time_utc: POSIXct of the route start
# avg_speed_mps: average speed in meters per second
# Returns: list(timestamps_utc, end_time_utc, total_distance_m, total_duration_s)
recalcular_tempos_rota <- function(coords, start_time_utc, avg_speed_mps) {
  if (!is.matrix(coords)) coords <- as.matrix(coords)
  n <- nrow(coords)
  if (n < 2) stop("Route must have at least 2 points")

  # Validate average speed (minimum 1 m/s = 3.6 km/h, maximum 55.5 m/s = 200 km/h)
  if (is.null(avg_speed_mps) || is.na(avg_speed_mps) || avg_speed_mps <= 0) {
    avg_speed_mps <- 8.33  # 30 km/h default
  }
  if (avg_speed_mps > 55.5) avg_speed_mps <- 55.5

  # Calculate accumulated distance along the route
  dist_acum <- numeric(n)
  dist_acum[1] <- 0
  for (i in 2:n) {
    p1 <- c(coords[i - 1, 1], coords[i - 1, 2])
    p2 <- c(coords[i, 1], coords[i, 2])
    dist_acum[i] <- dist_acum[i - 1] + geosphere::distHaversine(p1, p2)
  }
  total_distance <- dist_acum[n]

  # If total distance is 0, assume minimum distance
  if (total_distance <= 0) total_distance <- 1

  # Calculate duration based on average speed
  total_duration_s <- total_distance / avg_speed_mps

  # Minimum duration of 60 seconds
  if (total_duration_s < 60) total_duration_s <- 60

  # Distribute timestamps proportionally to distance
  timestamps <- start_time_utc + (dist_acum / total_distance) * total_duration_s
  end_time <- start_time_utc + total_duration_s

  list(
    timestamps_utc = timestamps,
    end_time_utc = end_time,
    total_distance_m = total_distance,
    total_duration_s = total_duration_s
  )
}

# Find the closest segment to a clicked point
# Returns: segment index or NULL
find_closest_segment <- function(click_lat, click_lng, nodes, segments) {
  if (length(segments) == 0) return(NULL)

  # Create node lookup by ID
  node_lookup <- stats::setNames(
    lapply(nodes, function(n) c(lng = n$lng, lat = n$lat)),
    vapply(nodes, `[[`, "", "id")
  )

  min_dist <- Inf
  closest_idx <- NULL

  for (i in seq_along(segments)) {
    seg <- segments[[i]]

    if (!is.null(seg$coords) && nrow(seg$coords) >= 2) {
      # Check distance to each sub-segment of the polyline
      for (j in 1:(nrow(seg$coords) - 1)) {
        dist <- point_to_segment_distance(
          click_lng, click_lat,
          seg$coords[j, 1], seg$coords[j, 2],
          seg$coords[j + 1, 1], seg$coords[j + 1, 2]
        )
        if (dist < min_dist) {
          min_dist <- dist
          closest_idx <- i
        }
      }
    } else {
      # Fallback: straight line between nodes
      from <- node_lookup[[seg$from_node]]
      to <- node_lookup[[seg$to_node]]
      if (!is.null(from) && !is.null(to)) {
        dist <- point_to_segment_distance(
          click_lng, click_lat,
          from["lng"], from["lat"],
          to["lng"], to["lat"]
        )
        if (dist < min_dist) {
          min_dist <- dist
          closest_idx <- i
        }
      }
    }
  }

  closest_idx
}

# Distance from a point to a line segment (in meters)
point_to_segment_distance <- function(px, py, x1, y1, x2, y2) {
  dx <- x2 - x1
  dy <- y2 - y1

  if (dx == 0 && dy == 0) {
    # Segment is a single point
    return(geosphere::distHaversine(c(px, py), c(x1, y1)))
  }

  # Project point onto segment
  t <- max(0, min(1, ((px - x1) * dx + (py - y1) * dy) / (dx * dx + dy * dy)))

  proj_x <- x1 + t * dx
  proj_y <- y1 + t * dy

  geosphere::distHaversine(c(px, py), c(proj_x, proj_y))
}

# Extract coordinates from a list of samples (LocoKit2 compatible)
extract_coords_from_samples <- function(samples) {
  n <- length(samples)
  coords <- matrix(0, nrow = n, ncol = 2)
  colnames(coords) <- c("lon", "lat")

  for (i in seq_len(n)) {
    coords[i, 1] <- sample_lon(samples[[i]])
    coords[i, 2] <- sample_lat(samples[[i]])
  }

  coords
}
