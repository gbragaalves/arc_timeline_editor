# ---- Helpers: OSRM ----

# Smooth OSRM route by removing redundant points and applying simplification
# coords: n x 2 matrix (lon, lat)
# tolerance_m: minimum distance between consecutive points (meters)
# angle_min: minimum direction change angle to keep a point (degrees)
smooth_osrm_route <- function(coords, tolerance_m = 5, angle_min = 1) {
  if (!is.matrix(coords)) coords <- as.matrix(coords)
  n <- nrow(coords)
  if (n < 3) return(coords)

  # Step 1: Remove duplicate or very close points
  keep <- rep(TRUE, n)
  for (i in 2:n) {
    dist <- geosphere::distHaversine(
      c(coords[i - 1, 1], coords[i - 1, 2]),
      c(coords[i, 1], coords[i, 2])
    )
    if (dist < tolerance_m) {
      keep[i] <- FALSE
    }
  }
  # Always keep first and last
  keep[1] <- TRUE
  keep[n] <- TRUE
  coords <- coords[keep, , drop = FALSE]
  n <- nrow(coords)
  if (n < 3) return(coords)

  # Step 2: Remove collinear points (no significant direction change)
  keep <- rep(FALSE, n)
  keep[1] <- TRUE  # first
  keep[n] <- TRUE  # last

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

    # Keep if direction change > angle_min
    if (diff_ang > angle_min) {
      keep[i] <- TRUE
    }
  }

  coords <- coords[keep, , drop = FALSE]
  n <- nrow(coords)
  if (n < 2) return(coords)

  # Step 3: Ensure minimum spacing between kept points
  # (avoids clusters of very close points after angle filtering)
  final <- matrix(coords[1, ], nrow = 1)
  colnames(final) <- colnames(coords)

  for (i in 2:(n - 1)) {
    dist_last <- geosphere::distHaversine(
      c(final[nrow(final), 1], final[nrow(final), 2]),
      c(coords[i, 1], coords[i, 2])
    )
    if (dist_last >= tolerance_m) {
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
  bus  = "http://127.0.0.1:5003",
  official = "http://127.0.0.1:5005"  # car + bus/psv roads (official.lua)
)

# Mapping of app profiles to OSRM profiles
OSRM_PROFILES <- list(
  car  = "driving",
  foot = "foot",
  bike = "bicycle",
  bus  = "driving",  # custom profile based on driving
  official = "driving"
)

# OSRM profiles in use (config_local.R can override). The others are hidden
# from the profile selects and the status dot; add "bike" back to use the
# bike server again. Cycling stays an activity type either way.
OSRM_ENABLED_PROFILES <- if (exists("OSRM_ENABLED_PROFILES")) OSRM_ENABLED_PROFILES else c("car", "foot", "bus", "official")

# Configured OSRM profiles that are enabled, in OSRM_SERVERS order
osrm_enabled_profiles <- function() intersect(names(OSRM_SERVERS), OSRM_ENABLED_PROFILES)

# Status of the last nightly data refresh (~/osrm-data/nightly_refresh.sh):
# last_refresh.json, NULL when missing or unreadable
OSRM_REFRESH_STATUS_FILE <- if (exists("OSRM_REFRESH_STATUS_FILE")) OSRM_REFRESH_STATUS_FILE else
  path.expand("~/osrm-data/last_refresh.json")

osrm_refresh_status <- function(file = OSRM_REFRESH_STATUS_FILE) {
  if (!file.exists(file)) return(NULL)
  tryCatch(jsonlite::fromJSON(file, simplifyVector = FALSE), error = function(e) NULL)
}

# Tooltip text for the OSRM dot: "Base OSM: dd/mm/yyyy · minhas edições até
# dd/mm hh:mm" (PT) / "OSM base: yyyy-mm-dd · my edits up to yyyy-mm-dd hh:mm"
# (EN), local times; plus the reason when the last run FAILED. "" without a status.
osrm_refresh_text <- function(st, lang = APP_LANGUAGE, tz = "America/Sao_Paulo") {
  if (is.null(st)) return("")
  when <- function(x) {
    t <- as.POSIXct(x %||% NA_character_, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    if (is.na(t)) NULL else t
  }
  pt <- identical(lang, "pt")
  base <- when(st$base_osm_timestamp)
  edits <- when(st$last_changeset_time)
  txt <- if (is.null(base)) "" else if (is.null(edits)) {
    sprintf(tr_lang("OSM base: %s", lang), format(base, if (pt) "%d/%m/%Y" else "%Y-%m-%d", tz = tz))
  } else {
    sprintf(tr_lang("OSM base: %s · my edits up to %s", lang), format(base, if (pt) "%d/%m/%Y" else "%Y-%m-%d", tz = tz),
            format(edits, if (pt) "%d/%m %H:%M" else "%Y-%m-%d %H:%M", tz = tz))
  }
  if (identical(st$status, "FAILED")) {
    ran <- when(st$date)
    failed <- sprintf(tr_lang("Last data refresh FAILED (%s): %s", lang),
                      if (is.null(ran)) "?" else format(ran, if (pt) "%d/%m %H:%M" else "%Y-%m-%d %H:%M", tz = tz),
                      st$reason %||% "")
    txt <- paste(c(if (nzchar(txt)) txt, failed), collapse = ". ")
  }
  txt
}

osrm_refresh_failed <- function(st) identical(st$status, "FAILED")

check_osrm_server <- function(base_url, profile = "car") {
  # try a minimal request just to check if the server responds
  osrm_profile <- OSRM_PROFILES[[profile]] %||% "driving"
  test_url <- paste0(base_url, "/route/v1/", osrm_profile, "/0,0;0,0")
  res <- tryCatch(
    httr::GET(test_url, httr::timeout(2)),
    error = function(e) NULL
  )
  !is.null(res)
}

calculate_osrm_route <- function(points, profile = c("car", "official", "foot", "bike", "bus")) {
  profile <- match.arg(profile)
  base_url <- OSRM_SERVERS[[profile]]
  osrm_profile <- OSRM_PROFILES[[profile]] %||% "driving"
  if (is.null(base_url)) return(NULL)

  if (!check_osrm_server(base_url, profile)) {
    return(NULL)
  }

  coords_str <- apply(points[, c("lng", "lat")], 1, paste, collapse = ",")
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
# ref_time_utc: trip start, for transit profiles (see calculate_google_transit_route)
# Returns: list(coords, distance_m)
recalculate_segment <- function(from_lat, from_lng, to_lat, to_lng,
                                profile = c("car", "official", "foot", "bike", "bus",
                                           "metro", "train", "tram"),
                                ref_time_utc = NULL) {
  profile <- match.arg(profile)

  pts <- data.frame(
    lat = c(from_lat, to_lat),
    lng = c(from_lng, to_lng)
  )

  # Transit profiles use Google Maps API
  if (profile %in% GOOGLE_TRANSIT_MODES) {
    transit_mode <- GOOGLE_TRANSIT_MODE_MAP[[profile]]
    route <- calculate_google_transit_route(pts, transit_mode = transit_mode, ref_time_utc = ref_time_utc)
  } else {
    route <- calculate_osrm_route(pts, profile = profile)
  }

  if (is.null(route) || is.null(route$coords)) {
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
  coords_smooth <- smooth_osrm_route(route$coords, tolerance_m = 5, angle_min = 2)

  list(
    coords = coords_smooth,
    distance_m = route$distance_m
  )
}

# Recalculate timestamps for an edited route keeping original average speed
# coords: n x 2 matrix (lon, lat)
# start_time_utc: POSIXct of the route start
# avg_speed_mps: average speed in meters per second
# Returns: list(timestamps_utc, end_time_utc, total_distance_m, total_duration_s)
recalculate_route_times <- function(coords, start_time_utc, avg_speed_mps) {
  if (!is.matrix(coords)) coords <- as.matrix(coords)
  n <- nrow(coords)
  if (n < 2) stop("Route must have at least 2 points")

  # Validate average speed (minimum 1 m/s = 3.6 km/h, maximum 55.5 m/s = 200 km/h)
  if (is.null(avg_speed_mps) || is.na(avg_speed_mps) || avg_speed_mps <= 0) {
    avg_speed_mps <- 8.33  # 30 km/h default
  }
  if (avg_speed_mps > 55.5) avg_speed_mps <- 55.5

  # Calculate accumulated distance along the route
  cumulative_dist <- numeric(n)
  cumulative_dist[1] <- 0
  for (i in 2:n) {
    p1 <- c(coords[i - 1, 1], coords[i - 1, 2])
    p2 <- c(coords[i, 1], coords[i, 2])
    cumulative_dist[i] <- cumulative_dist[i - 1] + geosphere::distHaversine(p1, p2)
  }
  total_distance <- cumulative_dist[n]

  # If total distance is 0, assume minimum distance
  if (total_distance <= 0) total_distance <- 1

  # Calculate duration based on average speed
  total_duration_s <- total_distance / avg_speed_mps

  # Minimum duration of 60 seconds
  if (total_duration_s < 60) total_duration_s <- 60

  # Distribute timestamps proportionally to distance
  timestamps <- start_time_utc + (cumulative_dist / total_distance) * total_duration_s
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

# ---- Map matching (OSRM /match) ----
# coords: n x 2 (lon, lat) in time order; timestamps: epoch seconds;
# radiuses: metres (one per point). Sent in chunks of chunk_size points that
# overlap by `overlap` points, with gaps=split and tidy=true (tidy = FALSE:
# tidy=false). Each chunk keeps only its middle: the overlap is split in
# half, so no chunk's result is used near its own ends (where a match lacks
# the context of the points after it and can snap to a parallel road); its
# matched geometry is cut at the tracepoints of the first and last points it
# keeps. Points whose timestamp doesn't increase are not sent.
# Returns list(
#   ok          = TRUE when at least one matching came back,
#   pieces      = list of list(coords = matched geometry, idx = matched point
#                 indices in order); a new piece starts at every gap split,
#   tracepoints = n x 2 snapped locations (NA when dropped or not sent),
#   sent        = logical, point was sent to OSRM,
#   confidence  = minimum over all matchings (NA when none),
#   n_dropped   = points sent but not matched)
osrm_match <- function(coords, timestamps, radiuses, profile = c("car", "official", "bus", "foot", "bike"),
                       chunk_size = 100, tidy = TRUE, overlap = 20) {
  profile <- match.arg(profile)
  base_url <- OSRM_SERVERS[[profile]]
  osrm_profile <- OSRM_PROFILES[[profile]] %||% "driving"
  n <- nrow(coords)

  tracepoints <- matrix(NA_real_, nrow = n, ncol = 2, dimnames = list(NULL, c("lon", "lat")))
  sent <- c(TRUE, diff(timestamps) > 0)
  idx_all <- which(sent)
  result <- list(ok = FALSE, pieces = list(), tracepoints = tracepoints, sent = sent,
                 confidence = NA_real_, n_dropped = 0L)
  if (is.null(base_url) || length(idx_all) < 2) return(result)

  overlap <- max(1L, min(as.integer(overlap), chunk_size %/% 2L))
  n_all <- length(idx_all)
  starts <- seq(1L, max(n_all - overlap, 1L), by = chunk_size - overlap)
  starts <- starts[starts == 1L | starts + overlap <= n_all]

  # 1. Every chunk's reply: per position (in idx_all) its matching and location
  replies <- lapply(starts, function(s) {
    pos <- s:min(s + chunk_size - 1L, n_all)
    chunk <- idx_all[pos]
    if (length(chunk) < 2) return(NULL)
    url <- paste0(
      base_url, "/match/v1/", osrm_profile, "/",
      paste(sprintf("%.6f,%.6f", coords[chunk, 1], coords[chunk, 2]), collapse = ";"),
      "?timestamps=", paste(as.integer(round(timestamps[chunk])), collapse = ";"),
      "&radiuses=", paste(sprintf("%.1f", radiuses[chunk]), collapse = ";"),
      "&gaps=split&tidy=", if (isTRUE(tidy)) "true" else "false", "&geometries=geojson&overview=full"
    )
    res <- tryCatch(httr::GET(url, httr::timeout(30)), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    js <- tryCatch(httr::content(res, as = "parsed", encoding = "UTF-8"), error = function(e) NULL)
    if (is.null(js) || !identical(js$code, "Ok") || length(js$matchings) == 0) return(NULL)
    mi <- rep(NA_integer_, length(chunk))
    loc <- matrix(NA_real_, length(chunk), 2)
    for (k in seq_along(js$tracepoints)) {
      tp <- js$tracepoints[[k]]
      if (is.null(tp)) next
      mi[k] <- as.integer(tp$matchings_index)
      loc[k, ] <- as.numeric(unlist(tp$location))[1:2]
    }
    list(pos = pos, chunk = chunk, mi = mi, loc = loc, matchings = js$matchings)
  })

  # 2. Boundaries: in each overlap, a point nearest to the overlap's middle
  # that the earlier chunk matched in one matching since the overlap's start
  # and the later chunk in one matching up to its end (so the two halves join
  # there); else one matched in both; else the middle
  same_run <- function(mi, from, to) {   # positions from..to whose matching continues the one at `from`
    v <- mi[from:to]
    first <- v[!is.na(v)][1]
    if (is.na(first)) return(integer(0))
    run <- cumsum(!is.na(v) & v != first) == 0
    (from:to)[run & !is.na(v)]
  }
  own_lo <- rep(1L, length(starts))
  for (ci in seq_along(starts)[-1]) {
    mid <- starts[ci] + overlap %/% 2L
    a <- replies[[ci - 1L]]
    b <- replies[[ci]]
    cand <- integer(0)
    if (!is.null(a) && !is.null(b)) {
      lo <- starts[ci]
      hi <- max(a$pos)
      ia <- match(lo:hi, a$pos)
      ib <- match(lo:hi, b$pos)
      fwd <- (lo:hi)[match(same_run(a$mi, ia[1], ia[length(ia)]), ia)]
      bwd <- (lo:hi)[rev(length(ib) + 1L - same_run(rev(b$mi[ib]), 1L, length(ib)))]
      cand <- intersect(fwd, bwd)
      if (!length(cand)) {
        o <- intersect(a$pos[!is.na(a$mi)], b$pos[!is.na(b$mi)])
        cand <- o[o >= lo & o <= hi]
      }
    }
    own_lo[ci] <- if (length(cand)) cand[which.min(abs(cand - mid))] else mid
  }
  own_hi <- c(utils::tail(own_lo, -1L), n_all)

  # 3. Each chunk's part: its kept tracepoints and the matched geometry between them
  pieces <- list()
  confidences <- numeric(0)
  for (ci in seq_along(starts)) {
    r <- replies[[ci]]
    if (is.null(r)) next
    keep <- r$pos >= own_lo[ci] & r$pos <= own_hi[ci]
    for (k in which(keep & !is.na(r$mi))) {
      if (is.na(tracepoints[r$chunk[k], 1])) tracepoints[r$chunk[k], ] <- r$loc[k, ]
    }
    for (m in seq_along(r$matchings)) {
      mt <- r$matchings[[m]]
      geom <- do.call(rbind, lapply(mt$geometry$coordinates, function(p) as.numeric(unlist(p))[1:2]))
      if (is.null(geom) || nrow(geom) < 2) next
      colnames(geom) <- c("lon", "lat")
      all_k <- which(r$mi == m - 1L)
      ks <- all_k[keep[all_k]]
      if (length(ks) == 0) next
      confidences <- c(confidences, as.numeric(mt$confidence %||% NA_real_))
      if (length(ks) < length(all_k)) {
        # Only the part between the first and last kept points
        cum <- path_cumulative_m(geom)
        d <- project_along(geom, cum, r$loc[all_k, , drop = FALSE])
        d1 <- d[match(ks[1], all_k)]
        d2 <- d[match(ks[length(ks)], all_k)]
        inner <- which(cum > d1 & cum < d2)
        geom <- rbind(path_points_at(geom, cum, d1), geom[inner, , drop = FALSE], path_points_at(geom, cum, d2))
        colnames(geom) <- c("lon", "lat")
      }
      pieces[[length(pieces) + 1L]] <- list(coords = geom, idx = r$chunk[ks])
    }
  }

  if (length(pieces) == 0) {
    result$n_dropped <- sum(sent)
    return(result)
  }

  # Join consecutive pieces: on the boundary point matched in both chunks, or
  # by a short straight link when they are at most 20 s apart and close for
  # that time (tidy thins each chunk's points differently, and OSRM itself
  # splits where one point snapped to the other carriageway; a routed gap
  # there can loop to reach it)
  pieces <- pieces[order(vapply(pieces, function(p) p$idx[1] %||% Inf, numeric(1)))]
  merged <- list(pieces[[1]])
  for (p in pieces[-1]) {
    last <- merged[[length(merged)]]
    if (length(last$idx) && length(p$idx) && utils::tail(last$idx, 1) == p$idx[1]) {
      merged[[length(merged)]] <- list(coords = rbind(last$coords, p$coords[-1, , drop = FALSE]),
                                       idx = c(last$idx, p$idx[-1]))
    } else if (length(last$idx) && length(p$idx) &&
               (dt <- timestamps[p$idx[1]] - timestamps[utils::tail(last$idx, 1)]) <= 20 &&
               path_cumulative_m(rbind(last$coords[nrow(last$coords), ], p$coords[1, ]))[2] <= 30 + 30 * dt) {
      merged[[length(merged)]] <- list(coords = rbind(last$coords, p$coords), idx = c(last$idx, p$idx))
    } else {
      merged[[length(merged) + 1L]] <- p
    }
  }

  result$ok <- TRUE
  result$pieces <- lapply(merged, function(p) list(coords = p$coords, idx = p$idx))
  result$tracepoints <- tracepoints
  result$confidence <- if (all(is.na(confidences))) NA_real_ else min(confidences, na.rm = TRUE)
  result$n_dropped <- sum(sent & is.na(tracepoints[, 1]))
  result
}

# OSRM /route through many waypoints (coords: n x 2 lon/lat, in order).
# bearings: degrees per waypoint (NA = none), sent as value,range so each
# waypoint snaps to the carriageway going the right way; radiuses: metres per
# waypoint (NA = unlimited). Long lists go in chunks of chunk_size sharing one
# waypoint. A chunk that fails with bearings is retried without them.
# Returns list(coords = n x 2, distance_m, legs_m = one per consecutive pair)
# or NULL.
osrm_route_via <- function(coords, profile = c("car", "official", "bus", "foot", "bike"),
                           bearings = NULL, radiuses = NULL, bearing_range = 60, chunk_size = 80) {
  profile <- match.arg(profile)
  base_url <- OSRM_SERVERS[[profile]]
  osrm_profile <- OSRM_PROFILES[[profile]] %||% "driving"
  n <- nrow(coords)
  if (is.null(base_url) || n < 2) return(NULL)

  request <- function(idx, with_options) {
    url <- paste0(base_url, "/route/v1/", osrm_profile, "/",
                  paste(sprintf("%.6f,%.6f", coords[idx, 1], coords[idx, 2]), collapse = ";"),
                  "?overview=full&geometries=geojson")
    if (with_options && !is.null(bearings)) {
      b <- bearings[idx]
      url <- paste0(url, "&bearings=", paste(ifelse(is.na(b), "", sprintf("%.0f,%d", b %% 360, as.integer(bearing_range))),
                                             collapse = ";"))
    }
    if (with_options && !is.null(radiuses)) {
      r <- radiuses[idx]
      url <- paste0(url, "&radiuses=", paste(ifelse(is.na(r), "unlimited", sprintf("%.0f", r)), collapse = ";"))
    }
    res <- tryCatch(httr::GET(url, httr::timeout(30)), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    js <- tryCatch(httr::content(res, as = "parsed", encoding = "UTF-8"), error = function(e) NULL)
    if (is.null(js) || !identical(js$code, "Ok") || length(js$routes) == 0) return(NULL)
    route <- js$routes[[1]]
    geom <- do.call(rbind, lapply(route$geometry$coordinates, function(p) as.numeric(unlist(p))[1:2]))
    if (is.null(geom) || nrow(geom) < 2) return(NULL)
    list(coords = geom, legs_m = vapply(route$legs, function(l) as.numeric(l$distance), numeric(1)))
  }

  starts <- seq(1L, max(n - 1L, 1L), by = chunk_size - 1L)
  geom <- NULL
  legs <- numeric(0)
  for (s in starts) {
    idx <- s:min(s + chunk_size - 1L, n)
    r <- request(idx, TRUE)
    if (is.null(r) && (!is.null(bearings) || !is.null(radiuses))) r <- request(idx, FALSE)
    if (is.null(r)) return(NULL)
    geom <- if (is.null(geom)) r$coords else rbind(geom, r$coords[-1, , drop = FALSE])
    legs <- c(legs, r$legs_m)
  }
  colnames(geom) <- c("lon", "lat")
  list(coords = geom, distance_m = sum(legs), legs_m = legs)
}
