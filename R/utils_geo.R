# ---- Helpers: Geographic parsers and calculations ----

# Calculate geodesic bearing between consecutive points
# coords: n x 2 matrix (lon, lat)
# Returns vector of n bearings (0-360 degrees)
calculate_bearings <- function(coords) {
  # Convert to numeric matrix if needed
  if (is.data.frame(coords)) coords <- as.matrix(coords)
  if (!is.matrix(coords)) {
    coords <- matrix(as.numeric(unlist(coords)), ncol = 2, byrow = TRUE)
  }
  storage.mode(coords) <- "double"

  n <- nrow(coords)
  if (is.null(n) || n < 2) return(rep(0, max(1, n)))

  bearings <- numeric(n)

  for (i in seq_len(n - 1)) {
    p1 <- c(as.numeric(coords[i, 1]), as.numeric(coords[i, 2]))
    p2 <- c(as.numeric(coords[i + 1, 1]), as.numeric(coords[i + 1, 2]))
    bearings[i] <- geosphere::bearing(p1, p2)
  }

  # Last point: keep the same bearing as the second-to-last
  bearings[n] <- bearings[n - 1]

  # Normalize to 0-360 (bearing can return negative values)
  bearings <- bearings %% 360

  bearings
}

# Clean and sort FR24 flight track
clean_fr24_track <- function(df,
                                ts_col  = "timestamp",
                                lon_col = "longitude",
                                lat_col = "latitude") {
  # Ensure required columns exist
  if (!all(c(ts_col, lon_col, lat_col) %in% names(df))) {
    stop("FR24 data frame is missing expected columns: ",
         ts_col, ", ", lon_col, ", ", lat_col)
  }

  ts_chr <- as.character(df[[ts_col]])
  ts_utc <- parse_timestamp_utc(ts_chr)

  lon <- suppressWarnings(as.numeric(df[[lon_col]]))
  lat <- suppressWarnings(as.numeric(df[[lat_col]]))

  # remove broken rows
  ok <- !is.na(ts_utc) & !is.na(lon) & !is.na(lat)
  ts_utc <- ts_utc[ok]
  lon    <- lon[ok]
  lat    <- lat[ok]

  if (length(ts_utc) < 2) {
    stop("FR24 track has fewer than 2 valid points.")
  }

  # sort by time
  ord    <- order(ts_utc)
  ts_utc <- ts_utc[ord]
  lon    <- lon[ord]
  lat    <- lat[ord]

  # remove exact consecutive duplicates
  dup <- c(FALSE,
           ts_utc[-1] == ts_utc[-length(ts_utc)] &
             lon[-1]  == lon[-length(lon)] &
             lat[-1]  == lat[-length(lat)])
  ts_utc <- ts_utc[!dup]
  lon    <- lon[!dup]
  lat    <- lat[!dup]

  coords <- cbind(lon, lat)
  colnames(coords) <- c("lon", "lat")

  list(
    ts_utc = ts_utc,
    coords = coords
  )
}

# Distance of a trajectory in km (consecutive pairs)
track_distance_km <- function(coords) {
  n <- nrow(coords)
  if (is.null(n) || n < 2) return(0)

  dists_m <- geosphere::distHaversine(
    coords[-n, , drop = FALSE],
    coords[-1, , drop = FALSE]
  )
  sum(dists_m) / 1000
}


# Rich KML parser (FlightRadar24)
parse_kml_rich <- function(kml_file) {
  kml <- tryCatch(xml2::read_xml(kml_file), error = function(e) NULL)
  if (is.null(kml)) return(NULL)

  ns <- xml2::xml_ns(kml)
  placemarks <- xml2::xml_find_all(kml, ".//d1:Placemark", ns)
  if (length(placemarks) == 0) return(NULL)

  out <- lapply(placemarks, function(pm) {
    point <- xml2::xml_find_first(pm, ".//d1:Point", ns)
    if (inherits(point, "xml_missing")) return(NULL)

    coord_node <- xml2::xml_find_first(point, ".//d1:coordinates", ns)
    if (inherits(coord_node, "xml_missing")) return(NULL)

    coord_str <- xml2::xml_text(coord_node)
    coords <- strsplit(trimws(coord_str), ",")[[1]]
    if (length(coords) < 2) return(NULL)

    lon <- as.numeric(coords[1])
    lat <- as.numeric(coords[2])

    altitude_m <- NA_real_
    if (length(coords) >= 3) {
      alt_raw <- suppressWarnings(as.numeric(coords[3]))
      if (!is.na(alt_raw)) altitude_m <- alt_raw
    }

    desc_node <- xml2::xml_find_first(pm, ".//d1:description", ns)
    desc <- if (!inherits(desc_node, "xml_missing")) xml2::xml_text(desc_node) else ""

    # altitude in feet -> meters
    alt_ft <- stringr::str_match(desc, "Altitude[: ]+([0-9.,]+) ?ft")[, 2]
    if (!is.na(alt_ft)) {
      alt_ft_num <- as.numeric(gsub(",", "", alt_ft))
      if (!is.na(alt_ft_num)) altitude_m <- alt_ft_num * 0.3048
    }

    # speed in knots -> m/s
    speed_mps <- NA_real_
    spd_knots <- stringr::str_match(desc, "Speed[: ]+([0-9.,]+) ?kt")[, 2]
    if (!is.na(spd_knots)) {
      spd_knots_num <- as.numeric(gsub(",", "", spd_knots))
      if (!is.na(spd_knots_num)) speed_mps <- spd_knots_num * 0.514444
    }

    # heading
    heading <- NA_real_
    hdg <- stringr::str_match(desc, "Heading[: ]+([0-9.,]+) ?deg")[, 2]
    if (!is.na(hdg)) {
      heading_num <- as.numeric(gsub(",", "", hdg))
      if (!is.na(heading_num)) heading <- heading_num
    }

    when_node <- xml2::xml_find_first(pm, ".//d1:when", ns)
    timestamp <- if (!inherits(when_node, "xml_missing")) xml2::xml_text(when_node) else NA_character_

    data.frame(
      lon        = lon,
      lat        = lat,
      altitude_m = altitude_m,
      speed_mps  = speed_mps,
      heading    = heading,
      timestamp  = timestamp,
      stringsAsFactors = FALSE
    )
  })

  out <- Filter(Negate(is.null), out)
  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}


# ---- Path helpers (n x 2 lon/lat matrices) ----
# Shared by the Edit Samples snap and the batch review fixes.

# Cumulative great-circle distance (m) at each vertex
path_cumulative_m <- function(coords) {
  n <- nrow(coords)
  if (n < 2) return(rep(0, n))
  c(0, cumsum(geosphere::distHaversine(coords[-n, , drop = FALSE], coords[-1, , drop = FALSE])))
}

# Indices of the vertices worth keeping: direction change > angle_deg, or more
# than max_gap_m from the last kept vertex. First and last are always kept.
path_key_points <- function(coords, angle_deg = 2, max_gap_m = 50) {
  n <- nrow(coords)
  if (n <= 2) return(seq_len(n))
  bearings <- geosphere::bearing(coords[-n, , drop = FALSE], coords[-1, , drop = FALSE])

  keep <- 1L
  last <- 1L
  for (i in 2:(n - 1)) {
    turn <- abs((bearings[i] - bearings[i - 1] + 180) %% 360 - 180)
    if (isTRUE(turn > angle_deg) ||
        geosphere::distHaversine(coords[last, ], coords[i, ]) > max_gap_m) {
      keep <- c(keep, i)
      last <- i
    }
  }
  c(keep, n)
}

# Points at distances d along a path (cum = path_cumulative_m(coords)),
# linear within each segment; d is clamped to the path
path_points_at <- function(coords, cum, d) {
  n <- nrow(coords)
  if (n < 2) return(coords[rep(1L, length(d)), , drop = FALSE])
  seg <- pmin(pmax(findInterval(d, cum), 1L), n - 1L)
  seg_len <- cum[seg + 1L] - cum[seg]
  seg_len[seg_len <= 0] <- 1
  frac <- pmax(0, pmin(1, (d - cum[seg]) / seg_len))
  out <- coords[seg, , drop = FALSE] + frac * (coords[seg + 1L, , drop = FALSE] - coords[seg, , drop = FALSE])
  colnames(out) <- c("lon", "lat")
  out
}

# Timed path: the vertices of `coords` plus anchor points at along-path
# distances anchor_d with times anchor_t (both non-decreasing, numeric seconds).
# Vertex times are linear in distance between neighbouring anchors, so the
# speed varies with the anchors; anchors at the same distance make a dwell.
# Returns data.frame(lon, lat, t, d, anchor).
timed_path_from_anchors <- function(coords, anchor_d, anchor_t) {
  cum <- path_cumulative_m(coords)
  anchor_d <- pmin(pmax(anchor_d, 0), cum[length(cum)])
  anchor_xy <- path_points_at(coords, cum, anchor_d)

  rows <- data.frame(
    lon = c(coords[, 1], anchor_xy[, 1]),
    lat = c(coords[, 2], anchor_xy[, 2]),
    d = c(cum, anchor_d),
    t = c(rep(NA_real_, length(cum)), anchor_t),
    anchor = c(rep(FALSE, length(cum)), rep(TRUE, length(anchor_d)))
  )
  # By distance; at equal distance anchors keep their own order, vertices go first
  rows <- rows[order(rows$d, rows$anchor, c(seq_along(cum), seq_along(anchor_d))), ]

  a <- which(rows$anchor)
  prev_a <- a[pmax(findInterval(seq_len(nrow(rows)), a), 1L)]
  next_a <- a[pmin(findInterval(seq_len(nrow(rows)) - 1L, a) + 1L, length(a))]
  span <- rows$d[next_a] - rows$d[prev_a]
  frac <- ifelse(span > 0, (rows$d - rows$d[prev_a]) / span, 0)
  frac <- pmax(0, pmin(1, frac))
  rows$t <- ifelse(rows$anchor, rows$t, rows$t[prev_a] + frac * (rows$t[next_a] - rows$t[prev_a]))

  # A vertex at the same distance as an anchor adds nothing
  rows <- rows[rows$anchor | !(rows$d %in% anchor_d), ]
  rownames(rows) <- NULL
  rows
}

# Resample a timed path (timed_path_from_anchors) so consecutive points are at
# most max_m and max_s apart, whichever gives more points. Shape vertices
# (path_key_points), anchors and dwells are kept; inserted points follow the
# original geometry. A dwell (no movement) gets a point every max_dwell_s.
resample_timed_path <- function(path, max_m = 100, max_s = 10, angle_deg = 2, max_dwell_s = max_s) {
  n <- nrow(path)
  if (n < 2) return(path)
  coords <- cbind(path$lon, path$lat)
  dwell <- c(FALSE, diff(path$d) == 0) | c(diff(path$d) == 0, FALSE)
  keep <- sort(unique(c(path_key_points(coords, angle_deg, max_gap_m = max_m),
                        which(path$anchor | dwell))))

  out <- vector("list", length(keep))
  for (k in seq_along(keep)) {
    i <- keep[k]
    out[[k]] <- path[i, c("lon", "lat", "t", "d", "anchor")]
    if (k == length(keep)) break
    j <- keep[k + 1]
    len <- path$d[j] - path$d[i]
    dur <- path$t[j] - path$t[i]
    steps <- max(ceiling(len / max_m), ceiling(dur / if (len > 0) max_s else max_dwell_s), 1)
    if (steps > 1) {
      f <- seq_len(steps - 1) / steps
      d_new <- path$d[i] + f * len
      xy <- if (len > 0) path_points_at(coords, path$d, d_new) else coords[rep(i, length(f)), , drop = FALSE]
      out[[k]] <- rbind(out[[k]], data.frame(lon = xy[, 1], lat = xy[, 2], t = path$t[i] + f * dur,
                                             d = d_new, anchor = FALSE))
    }
  }
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}
