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


# ---- Google Location History (your own export, optional overlay) ----

# Load and parse Google Location History file
# Returns data.frame with: start_time, end_time, lat, lon, lat_end, lon_end, type (visit/activity)
load_location_history <- function(file_path) {
  if (!file.exists(file_path)) return(NULL)

  raw_data <- tryCatch(
    jsonlite::fromJSON(file_path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(raw_data) || length(raw_data) == 0) return(NULL)

  # Extract fields per record, then parse everything in vectorized calls
  # (per-record ymd_hms takes ~10 ms each)
  str_or_na <- function(x) if (is.character(x) && length(x) == 1) x else NA_character_
  field <- function(f) vapply(raw_data, function(item) str_or_na(f(item)), character(1))

  type <- vapply(raw_data, function(item) {
    if (!is.null(item$visit)) "visit" else if (!is.null(item$activity)) "activity" else NA_character_
  }, character(1))
  start_str <- field(function(item) item$startTime)
  end_str   <- field(function(item) item$endTime)
  # Visits use the place location for both ends; activities have start/end
  from_geo <- field(function(item) {
    if (!is.null(item$visit)) item$visit$topCandidate$placeLocation else item$activity$start
  })
  to_geo <- field(function(item) {
    if (!is.null(item$visit)) item$visit$topCandidate$placeLocation else item$activity$end
  })

  # ISO8601 with offset (e.g.: 2024-09-10T08:33:50.000-03:00) -> UTC
  start_time <- lubridate::with_tz(lubridate::ymd_hms(start_str, quiet = TRUE), "UTC")
  end_time   <- lubridate::with_tz(lubridate::ymd_hms(end_str, quiet = TRUE), "UTC")
  from <- parse_geo_strings(from_geo)
  to   <- parse_geo_strings(to_geo)

  keep <- !is.na(type) & !is.na(start_time) & !is.na(end_time) &
    !is.na(from$lat) & !is.na(to$lat)
  if (!any(keep)) return(NULL)

  data.frame(
    start_time = start_time[keep],
    end_time   = end_time[keep],
    lat        = from$lat[keep],
    lon        = from$lon[keep],
    lat_end    = to$lat[keep],
    lon_end    = to$lon[keep],
    type       = type[keep],
    stringsAsFactors = FALSE
  )
}

# Parse "geo:lat,lon" strings to list(lat, lon) numeric vectors (NA when invalid)
parse_geo_strings <- function(geo_str) {
  valid <- !is.na(geo_str) & grepl("^geo:", geo_str)
  parts <- strsplit(sub("^geo:", "", geo_str), ",", fixed = TRUE)
  part_num <- function(i) suppressWarnings(as.numeric(vapply(parts, function(p) {
    if (length(p) >= 2) p[i] else NA_character_
  }, character(1))))

  lat <- part_num(1)
  lon <- part_num(2)
  bad <- !valid | is.na(lat) | is.na(lon)
  lat[bad] <- NA_real_
  lon[bad] <- NA_real_
  list(lat = lat, lon = lon)
}

# Filter Location History by date (considering local timezone)
filter_location_history_by_date <- function(df, target_date, local_tz = Sys.timezone()) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  # Convert target_date to UTC timestamp range
  start_local <- lubridate::ymd_hms(paste(target_date, "00:00:00"), tz = local_tz)
  end_local    <- lubridate::ymd_hms(paste(target_date, "23:59:59"), tz = local_tz)

  start_utc <- lubridate::with_tz(start_local, "UTC")
  end_utc    <- lubridate::with_tz(end_local, "UTC")

  # Filter records that overlap with the day
  filtered_df <- df[
    (df$start_time <= end_utc & df$end_time >= start_utc),
  ]

  if (nrow(filtered_df) == 0) return(NULL)

  filtered_df
}

# Format date/time for map labels: dd/mm/yyyy HH:MM (pt) or yyyy-mm-dd HH:MM (en)
format_history_time <- function(timestamp, lang = "en", local_tz = Sys.timezone()) {
  fmt_datetime(lubridate::with_tz(timestamp, local_tz), lang)
}
