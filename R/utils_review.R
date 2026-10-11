# ---- Batch review: geometric conformity of recorded trips ----
# Finds trips whose recorded path doesn't follow the network it should be on
# (rails for metro/train/tram, roads for car/taxi/bus), proposes a clean path
# for each and exports the accepted ones as one GPX track per trip
# (Arc → Import GPX File disables and hides the overlapping data).

# Config (config_local.R can override any of these)
OSM_PBF_FILE        <- if (exists("OSM_PBF_FILE")) OSM_PBF_FILE else path.expand("~/osrm-data/clipped.osm.pbf")
RAIL_REFERENCE_FILE <- if (exists("RAIL_REFERENCE_FILE")) RAIL_REFERENCE_FILE else file.path("reference", "rails.gpkg")
REVIEW_SCRATCH_DIR  <- if (exists("REVIEW_SCRATCH_DIR")) REVIEW_SCRATCH_DIR else "scratch"
# OSRM profile for car trips: "official" also uses bus/psv roads (busways, BRT
# terminals); "car" when the official server is not enabled. Taxi stays on car.
ROAD_PROFILE_FOR_CAR <- if (exists("ROAD_PROFILE_FOR_CAR")) ROAD_PROFILE_FOR_CAR else
  if ("official" %in% osrm_enabled_profiles()) "official" else "car"

# All thresholds in one place. Distances in metres unless named otherwise.
REVIEW_THRESHOLDS <- list(
  rail = list(tol_m = 120, off_share = 0.15, longest_off_m = 400, max_offset_m = 600),
  # Road flags are geometric only (OSRM match confidence is informational).
  # tol_m 35: urban-canyon GPS noise. Proposal length vs the recorded path:
  # over detour_ratio x is a possible detour, under short_ratio x possibly
  # skips part of the trip. Bulk accept takes proposals from short_ratio to
  # bulk_max_ratio x that stay on the roads when re-matched (off share <=
  # bulk_max_off_share). via_every_m: waypoint spacing for "Re-route" on detours.
  road = list(tol_m = 35, off_share = 0.10, longest_off_m = 120, detour_ratio = 1.25, short_ratio = 0.9,
              bulk_max_ratio = 1.2, bulk_max_off_share = 0.01, via_every_m = 300),
  min_path_km       = 0.5,  # recorded path shorter than this: a fragment (junk), whatever the type
  densify_m         = 20,   # the recorded path is tested every 20 m
  rail_speed_kmh    = 35,   # RAIL_SPEED_KMH: station <-> first/last sample
  max_time_adjust_s = 180,  # cap for each station time adjustment
  transfer_dwell_point_s = 120,  # waiting at a transfer: one point every 2 min
  station_radius_m  = 600,  # stations considered for the endpoints
  good_accuracy_m   = 50,   # origin/destination samples
  min_pair_s        = 5,    # max speed ignores sample pairs closer in time
  match_radius_m    = c(5, 50),  # OSRM /match radiuses: horizontalAccuracy clamped
  point_every_m     = 100,  # clean path: one point per 100 m or 10 s,
  point_every_s     = 10    #   whichever gives more points
)

# Activities reviewed. code: LocoKit2 activity code; family: reference network;
# rail_types: OSM railway=* values; station_modes: station mode flags;
# google: Directions transit_mode; osrm: local OSRM profile.
REVIEW_ACTIVITIES <- list(
  metro = list(code = 29L, family = "rail", label = "Metro", icon = "\U0001F687", color = "#E65100",
               rail_types = c("subway", "light_rail"), station_modes = c("subway", "light_rail"), google = "subway"),
  train = list(code = 20L, family = "rail", label = "Train", icon = "\U0001F686", color = "#6A1B9A",
               rail_types = "rail", station_modes = "train", google = "train"),
  tram  = list(code = 24L, family = "rail", label = "Tram", icon = "\U0001F68A", color = "#00838F",
               rail_types = c("tram", "light_rail"), station_modes = c("tram", "light_rail"), google = "tram"),
  car   = list(code = 5L,  family = "road", label = "Car", icon = "\U0001F697", color = "#1565C0", osrm = ROAD_PROFILE_FOR_CAR),
  taxi  = list(code = 34L, family = "road", label = "Taxi", icon = "\U0001F695", color = "#F9A825", osrm = "car"),
  bus   = list(code = 21L, family = "road", label = "Bus", icon = "\U0001F68C", color = "#2E7D32", osrm = "bus")
)

review_activity_of <- function(code) {
  codes <- vapply(REVIEW_ACTIVITIES, `[[`, integer(1), "code")
  names(codes)[match(as.integer(code), codes)]
}

# Bump a family's version when its cached metrics change meaning
REVIEW_CACHE_VERSION <- c(rail = 1L, road = 2L)


# ---- Items and samples ----

# Timeline items overlapping [from, to] (local dates) from items/YYYY-MM.json.
# One row per item: id, is_visit, start_utc, end_utc, activity_code
# (confirmed %||% classified), distance_m, source, deleted, disabled, last_saved.
load_items <- function(items_dir = ITEMS_BACKUP_DIR, from, to, tz = Sys.timezone()) {
  from_utc <- lubridate::force_tz(as.POSIXct(paste(as.Date(from), "00:00:00"), tz = "UTC"), tz)
  to_utc   <- lubridate::force_tz(as.POSIXct(paste(as.Date(to), "23:59:59"), tz = "UTC"), tz)
  first_month <- lubridate::floor_date(as.Date(from), "month")
  months <- format(seq(lubridate::floor_date(first_month - 1, "month"),  # items can start the month before
                       lubridate::floor_date(as.Date(to), "month"), by = "month"), "%Y-%m")
  files <- file.path(items_dir, paste0(months, ".json"))
  files <- files[file.exists(files)]

  empty <- data.frame(id = character(0), is_visit = logical(0), start_utc = as.POSIXct(character(0), tz = "UTC"),
                      end_utc = as.POSIXct(character(0), tz = "UTC"), activity_code = integer(0),
                      distance_m = numeric(0), source = character(0), deleted = logical(0),
                      disabled = logical(0), last_saved = character(0), stringsAsFactors = FALSE)
  if (length(files) == 0) return(empty)

  rows <- lapply(files, function(f) {
    df <- tryCatch(jsonlite::fromJSON(f, flatten = TRUE), error = function(e) NULL)
    if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
    col <- function(name, default) if (name %in% names(df)) df[[name]] else rep(default, nrow(df))
    confirmed <- col("trip.confirmedActivityType", NA_integer_)
    classified <- col("trip.classifiedActivityType", NA_integer_)
    data.frame(
      id            = col("base.id", NA_character_),
      is_visit      = as.logical(col("base.isVisit", FALSE)),
      start_utc     = parse_timestamp_utc(col("base.startDate", NA_character_)),
      end_utc       = parse_timestamp_utc(col("base.endDate", NA_character_)),
      activity_code = as.integer(ifelse(is.na(confirmed), classified, confirmed)),
      distance_m    = as.numeric(col("trip.distance", NA_real_)),
      source        = as.character(col("base.source", NA_character_)),
      deleted       = as.logical(col("base.deleted", FALSE)) %in% TRUE,
      disabled      = as.logical(col("base.disabled", FALSE)) %in% TRUE,
      last_saved    = as.character(col("base.lastSaved", NA_character_)),
      stringsAsFactors = FALSE
    )
  })
  items <- do.call(rbind, c(list(empty), rows))
  items <- items[!is.na(items$id) & !is.na(items$start_utc) & !is.na(items$end_utc), ]
  items <- items[items$start_utc <= to_utc & items$end_utc >= from_utc, ]
  # An item saved in two monthly files: keep the latest save
  items <- items[order(items$id, items$last_saved, decreasing = TRUE), ]
  items <- items[!duplicated(items$id), ]
  items <- items[order(items$start_utc), ]
  rownames(items) <- NULL
  items
}

# Parsed weekly sample files (a few weeks kept in memory)
.review_weeks <- new.env(parent = emptyenv())

# Samples of one weekly file as a data.frame: id, item_id, t (epoch s), lat,
# lon, acc, sec_gmt, alt. Disabled samples and rows without coordinates dropped.
read_week_samples <- function(week, weekly_dir = WEEKLY_BACKUP_DIR) {
  path <- file.path(weekly_dir, paste0(week, ".json.gz"))
  if (!file.exists(path)) return(NULL)
  key <- paste(normalizePath(path), as.numeric(file.mtime(path)))
  if (!is.null(.review_weeks[[key]])) return(.review_weeks[[key]])

  df <- tryCatch(jsonlite::fromJSON(gzfile(path)), error = function(e) NULL)
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
  col <- function(name, default) if (name %in% names(df)) df[[name]] else rep(default, nrow(df))

  dates <- as.character(col("date", NA_character_))
  t <- as.numeric(as.POSIXct(dates, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  if (anyNA(t)) t[is.na(t)] <- as.numeric(parse_timestamp_utc(dates[is.na(t)]))
  out <- data.frame(
    id      = as.character(col("id", NA_character_)),
    item_id = as.character(col("timelineItemId", NA_character_)),
    t       = t,
    lat     = as.numeric(col("latitude", NA_real_)),
    lon     = as.numeric(col("longitude", NA_real_)),
    acc     = as.numeric(col("horizontalAccuracy", NA_real_)),
    sec_gmt = as.numeric(col("secondsFromGMT", 0)),
    alt     = as.numeric(col("altitude", 0)),
    stringsAsFactors = FALSE
  )
  out <- out[!(col("disabled", FALSE) %in% TRUE) & !is.na(out$t) & !is.na(out$lat) & !is.na(out$lon), ]

  keys <- ls(.review_weeks)
  if (length(keys) >= 12) rm(list = keys[order(vapply(keys, function(k) .review_weeks[[k]]$.loaded[1], numeric(1)))][1],
                             envir = .review_weeks)
  out$.loaded <- as.numeric(Sys.time())
  assign(key, out, envir = .review_weeks)
  out
}

# Weekly files that may hold a trip's samples (padded: the bucketing week may
# follow UTC or local time)
trip_weeks <- function(start_utc, end_utc) {
  weeks_for_interval(start_utc - 14 * 3600, end_utc + 14 * 3600)
}

# Samples of each trip (rows of `trips`), only from the weeks covering it,
# filtered by timelineItemId. Returns a list of data.frames sorted by time.
# progress(fraction, detail) is called once per weekly file.
trip_samples_for <- function(trips, weekly_dir = WEEKLY_BACKUP_DIR, progress = NULL) {
  out <- rep(list(NULL), nrow(trips))
  if (nrow(trips) == 0) return(out)
  weeks_by_trip <- lapply(seq_len(nrow(trips)), function(i) trip_weeks(trips$start_utc[i], trips$end_utc[i]))
  weeks <- sort(unique(unlist(weeks_by_trip)))
  parts <- rep(list(list()), nrow(trips))

  for (w in seq_along(weeks)) {
    if (is.function(progress)) progress(w / length(weeks), weeks[w])
    wk <- read_week_samples(weeks[w], weekly_dir)
    if (is.null(wk)) next
    need <- which(vapply(weeks_by_trip, function(x) weeks[w] %in% x, logical(1)))
    by_item <- split(wk[wk$item_id %in% trips$id[need], ], wk$item_id[wk$item_id %in% trips$id[need]])
    for (i in need) {
      rows <- by_item[[trips$id[i]]]
      if (!is.null(rows) && nrow(rows)) parts[[i]] <- c(parts[[i]], list(rows))
    }
  }

  for (i in seq_len(nrow(trips))) {
    s <- do.call(rbind, parts[[i]])
    if (is.null(s)) s <- data.frame(id = character(0), item_id = character(0), t = numeric(0), lat = numeric(0),
                                    lon = numeric(0), acc = numeric(0), sec_gmt = numeric(0), alt = numeric(0))
    s$.loaded <- NULL
    s <- s[!duplicated(s$id), ]
    s <- s[order(s$t), ]
    rownames(s) <- NULL
    out[[i]] <- s
  }
  out
}

# Trips of the given activity codes that are neither deleted nor disabled.
# source other than "LocoKit2" (e.g. a GPX import) means already_fixed.
# with_samples: TRUE (all trips), FALSE, or a logical vector per trip.
trip_inventory <- function(items, activity_codes, weekly_dir = WEEKLY_BACKUP_DIR, with_samples = TRUE,
                           progress = NULL) {
  trips <- items[!items$is_visit & items$activity_code %in% activity_codes & !items$deleted & !items$disabled, ]
  trips$already_fixed <- !is.na(trips$source) & trips$source != "LocoKit2"
  trips$activity <- review_activity_of(trips$activity_code)
  rownames(trips) <- NULL

  load <- rep_len(as.logical(with_samples), nrow(trips))
  trips$samples <- rep(list(NULL), nrow(trips))
  if (any(load)) trips$samples[load] <- trip_samples_for(trips[load, ], weekly_dir, progress)
  trips
}


# ---- Rail reference (OSM) ----

RAIL_LINE_TYPES <- c("subway", "light_rail", "rail", "tram")
RAIL_EXCLUDED_SERVICE <- c("yard", "siding", "spur")
STATION_MODES <- c("subway", "train", "light_rail", "tram")

# Rails and stations from the OSM extract, written to `out` (GeoPackage,
# layers "rails" and "stations"). Uses osmium when installed, otherwise GDAL's
# OSM driver. Returns list(ok, message, method, counts, stations, bbox).
build_rail_reference <- function(pbf = OSM_PBF_FILE, out = RAIL_REFERENCE_FILE, on_step = NULL) {
  step <- function(fraction, label) if (is.function(on_step)) on_step(fraction, label)
  if (!file.exists(pbf)) return(list(ok = FALSE, message = paste("OSM file not found:", pbf)))

  features <- NULL
  method <- NULL
  if (nzchar(Sys.which("osmium"))) {
    step(0.1, "osmium tags-filter")
    features <- tryCatch(rail_features_osmium(pbf), error = function(e) {
      message("[Rails] osmium failed: ", conditionMessage(e))
      NULL
    })
    method <- "osmium"
  }
  if (is.null(features)) {
    step(0.1, "GDAL OSM driver")
    features <- tryCatch(rail_features_gdal(pbf), error = function(e) {
      message("[Rails] GDAL OSM driver failed: ", conditionMessage(e))
      NULL
    })
    method <- "gdal"
  }
  if (is.null(features)) return(list(ok = FALSE, message = "Could not read rails from the OSM file."))

  step(0.8, "rails + stations")
  rails <- rail_lines_from_features(features)
  stations <- stations_from_features(features)

  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(out, ".tmp.gpkg")
  unlink(tmp)
  sf::st_write(rails, tmp, layer = "rails", quiet = TRUE)
  sf::st_write(stations, tmp, layer = "stations", append = TRUE, quiet = TRUE)
  file.rename(tmp, out)
  .rail_ref$key <- NULL

  counts <- table(rails$railway)
  bbox <- sf::st_bbox(rails)
  list(
    ok = TRUE, method = method, counts = counts, stations = nrow(stations), bbox = bbox,
    message = sprintf("%s: %s; %d stations", basename(out),
                      paste(names(counts), counts, sep = " ", collapse = ", "), nrow(stations))
  )
}

# osmium tags-filter + export → sf features (all tags as columns)
rail_features_osmium <- function(pbf) {
  tmp_dir <- tempfile("rails_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  filtered <- file.path(tmp_dir, "rails.osm.pbf")
  seq_file <- file.path(tmp_dir, "rails.geojsonseq")

  run <- function(args) {
    out <- suppressWarnings(system2("osmium", shQuote(args), stdout = TRUE, stderr = TRUE))
    if (!is.null(attr(out, "status")) && attr(out, "status") != 0) stop(paste(out, collapse = "\n"))
  }
  run(c("tags-filter", normalizePath(pbf),
        paste0("w/railway=", paste(RAIL_LINE_TYPES, collapse = ",")),
        "nwr/railway=station,halt", "nwr/public_transport=station",
        "-o", filtered, "--overwrite"))
  run(c("export", filtered, "-f", "geojsonseq", "-o", seq_file, "--overwrite"))
  sf::st_read(seq_file, quiet = TRUE)
}

# GDAL OSM driver with SQL filters (other_tags holds the tags without a column)
rail_features_gdal <- function(pbf) {
  tag <- function(other, key) {
    m <- regmatches(other, regexec(sprintf('"%s"=>"([^"]*)"', key), other))
    vapply(m, function(x) if (length(x) == 2) x[2] else NA_character_, character(1))
  }
  with_tags <- function(x) {
    other <- if ("other_tags" %in% names(x)) x$other_tags else rep(NA_character_, nrow(x))
    for (key in c("railway", "service", "public_transport", "station", STATION_MODES)) {
      if (!key %in% names(x)) x[[key]] <- tag(other, key)
    }
    x
  }
  station_where <- paste(sprintf("other_tags LIKE '%%%s%%'",
                                 c('"railway"=>"station"', '"railway"=>"halt"', '"public_transport"=>"station"')),
                         collapse = " OR ")

  lines <- sf::st_read(pbf, query = sprintf(
    "SELECT osm_id, name, railway, other_tags FROM lines WHERE railway IN (%s)",
    paste0("'", RAIL_LINE_TYPES, "'", collapse = ",")), quiet = TRUE)
  points <- sf::st_read(pbf, query = paste("SELECT osm_id, name, other_tags FROM points WHERE", station_where), quiet = TRUE)
  areas <- sf::st_read(pbf, query = paste("SELECT osm_id, osm_way_id, name, other_tags FROM multipolygons WHERE",
                                          station_where), quiet = TRUE)
  cols <- c("name", "railway", "service", "public_transport", "station", STATION_MODES)
  parts <- lapply(list(lines, points, areas), function(x) {
    x <- with_tags(x)
    sf::st_sf(x[, cols, drop = TRUE], geometry = sf::st_geometry(x))
  })
  do.call(rbind, parts)
}

rail_lines_from_features <- function(x) {
  col <- function(name) if (name %in% names(x)) x[[name]] else rep(NA_character_, nrow(x))
  gt <- as.character(sf::st_geometry_type(x))
  keep <- gt %in% c("LINESTRING", "MULTILINESTRING") & col("railway") %in% RAIL_LINE_TYPES &
    !(col("service") %in% RAIL_EXCLUDED_SERVICE)
  rails <- sf::st_sf(railway = col("railway")[keep], name = col("name")[keep], geometry = sf::st_geometry(x)[keep])
  suppressWarnings(sf::st_cast(rails, "LINESTRING"))
}

stations_from_features <- function(x) {
  col <- function(name) if (name %in% names(x)) x[[name]] else rep(NA_character_, nrow(x))
  gt <- as.character(sf::st_geometry_type(x))
  is_station <- col("railway") %in% c("station", "halt") | col("public_transport") %in% "station"
  # Closed ways come as both outline and area: points and areas only
  keep <- is_station & gt %in% c("POINT", "POLYGON", "MULTIPOLYGON")
  geom <- sf::st_geometry(x)[keep]
  poly <- !as.character(sf::st_geometry_type(geom)) %in% "POINT"
  if (any(poly)) {
    # Planar centroid: fine at station size, and tolerant of invalid rings
    # (GDAL-built areas can have them; s2 refuses those)
    old_s2 <- sf::sf_use_s2()
    suppressMessages(sf::sf_use_s2(FALSE))
    on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
    geom[poly] <- suppressWarnings(sf::st_centroid(geom[poly]))
  }

  modes <- vapply(which(keep), function(i) {
    m <- STATION_MODES[vapply(STATION_MODES, function(k) identical(col(k)[i], "yes"), logical(1))]
    st <- col("station")[i]
    if (!is.na(st) && st %in% STATION_MODES) m <- c(m, st)
    if (st %in% "train" || (length(m) == 0 && col("railway")[i] %in% c("station", "halt"))) m <- c(m, "train")
    paste(unique(m), collapse = ",")
  }, character(1))

  xy <- sf::st_coordinates(geom)
  sf::st_sf(name = col("name")[keep], modes = modes, lon = xy[, 1], lat = xy[, 2], geometry = geom)
}

.rail_ref <- new.env(parent = emptyenv())

# Rails + stations, loaded once per file version. Rails get a bbox index
# (xmin, ymin, xmax, ymax per feature) for fast cropping. NULL when missing.
rail_reference <- function(file = RAIL_REFERENCE_FILE) {
  if (!file.exists(file)) return(NULL)
  key <- paste(normalizePath(file), as.numeric(file.mtime(file)))
  if (identical(.rail_ref$key, key)) return(.rail_ref$data)

  rails <- sf::st_read(file, layer = "rails", quiet = TRUE)
  stations <- sf::st_read(file, layer = "stations", quiet = TRUE)
  bb <- t(vapply(sf::st_geometry(rails), function(g) as.numeric(sf::st_bbox(g)), numeric(4)))
  colnames(bb) <- c("xmin", "ymin", "xmax", "ymax")
  .rail_ref$data <- list(rails = rails, bbox = bb, stations = sf::st_drop_geometry(stations),
                         mtime = as.numeric(file.mtime(file)))
  .rail_ref$key <- key
  .rail_ref$data
}

# Rails of the activity's types near a lon/lat bbox (c(xmin, ymin, xmax, ymax)),
# as an sfc in crs (metric). Empty sfc when none or no reference.
rail_lines_near <- function(activity, bbox, crs, buffer_m = 1500, ref = rail_reference()) {
  if (is.null(ref)) return(sf::st_sfc(crs = crs))
  pad_lat <- buffer_m / 110540
  pad_lon <- buffer_m / (111320 * cos(mean(bbox[c(2, 4)]) * pi / 180))
  b <- ref$bbox
  hit <- ref$rails$railway %in% REVIEW_ACTIVITIES[[activity]]$rail_types &
    b[, "xmax"] >= bbox[1] - pad_lon & b[, "xmin"] <= bbox[3] + pad_lon &
    b[, "ymax"] >= bbox[2] - pad_lat & b[, "ymin"] <= bbox[4] + pad_lat
  if (!any(hit)) return(sf::st_sfc(crs = crs))
  sf::st_transform(sf::st_geometry(ref$rails)[hit], crs)
}

# Stations of the activity's modes, nearest first: data.frame(name, lon, lat, dist_m)
station_candidates <- function(activity, lon, lat, k = 3, radius_m = Inf, ref = rail_reference()) {
  empty <- data.frame(name = character(0), lon = numeric(0), lat = numeric(0), dist_m = numeric(0))
  if (is.null(ref)) return(empty)
  st <- ref$stations
  modes <- REVIEW_ACTIVITIES[[activity]]$station_modes
  ok <- vapply(strsplit(st$modes, ",", fixed = TRUE), function(m) any(m %in% modes), logical(1))
  st <- st[ok & !is.na(st$name) & nzchar(st$name), ]
  if (nrow(st) == 0) return(empty)
  st$dist_m <- geosphere::distHaversine(cbind(st$lon, st$lat), c(lon, lat))
  st <- st[order(st$dist_m), ]
  st <- st[!duplicated(st$name) & st$dist_m <= radius_m, c("name", "lon", "lat", "dist_m")]
  rownames(st) <- NULL
  utils::head(st, k)
}


# ---- Geometry helpers (metric) ----

utm_epsg <- function(lon, lat) {
  zone <- floor((mean(lon, na.rm = TRUE) + 180) / 6) + 1
  as.integer((if (mean(lat, na.rm = TRUE) < 0) 32700 else 32600) + zone)
}

to_metric <- function(lon, lat, crs) {
  xy <- sf::sf_project("EPSG:4326", paste0("EPSG:", crs), cbind(lon, lat), warn = FALSE)
  colnames(xy) <- c("x", "y")
  xy
}

from_metric <- function(x, y, crs) {
  ll <- sf::sf_project(paste0("EPSG:", crs), "EPSG:4326", cbind(x, y), warn = FALSE)
  colnames(ll) <- c("lon", "lat")
  ll
}

# Points every step_m along a polyline (n x 2, metric), vertices included.
# Returns data.frame(x, y, s) with s = distance along the path.
densify_xy <- function(xy, step_m = 20) {
  n <- nrow(xy)
  if (n < 2) return(data.frame(x = xy[, 1], y = xy[, 2], s = rep(0, n)))
  seg <- sqrt(rowSums((xy[-1, , drop = FALSE] - xy[-n, , drop = FALSE])^2))
  k <- pmax(1L, as.integer(ceiling(seg / step_m)))
  idx <- rep(seq_len(n - 1), k)
  f <- sequence(k, from = 0L) / rep(k, k)
  cum <- c(0, cumsum(seg))
  data.frame(
    x = c(xy[idx, 1] + f * (xy[idx + 1, 1] - xy[idx, 1]), xy[n, 1]),
    y = c(xy[idx, 2] + f * (xy[idx + 1, 2] - xy[idx, 2]), xy[n, 2]),
    s = c(cum[idx] + f * seg[idx], cum[n])
  )
}

# How far a path strays from a reference network. xy: path (metric, n x 2);
# ref: sfc of lines in the same crs. The path is densified every step_m, so a
# long straight jump between sparse samples is tested along its whole length.
# Returns list(off_share, max_offset_m, longest_off_m, dens = data.frame(x, y, s, d, off)).
offset_metrics <- function(xy, ref, tol_m, step_m = 20, crs = sf::st_crs(ref)) {
  dens <- densify_xy(xy, step_m)
  n <- nrow(dens)
  if (length(ref) == 0) {
    dens$d <- rep(Inf, n)
  } else {
    pts <- sf::st_as_sf(dens[, c("x", "y")], coords = c("x", "y"), crs = crs)
    dens$d <- as.numeric(sf::st_distance(pts, sf::st_union(ref)))
  }
  dens$off <- dens$d > tol_m

  s <- dens$s
  total <- s[n]
  if (n < 2 || total <= 0) {
    return(list(off_share = as.numeric(any(dens$off)), max_offset_m = max(dens$d), longest_off_m = 0, dens = dens))
  }
  # Each point stands for half of each adjacent step
  mid <- c(0, (s[-1] + s[-n]) / 2, total)
  w <- diff(mid)
  runs <- rle(dens$off)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1
  run_len <- mid[ends + 1] - mid[starts]

  list(
    off_share     = sum(w[dens$off]) / total,
    max_offset_m  = if (all(is.finite(dens$d))) max(dens$d) else Inf,
    longest_off_m = if (any(runs$values)) max(run_len[runs$values]) else 0,
    dens          = dens
  )
}

# Distance along a polyline (lon/lat, cum = path_cumulative_m) of each point,
# searching forward from the previous point's position (monotonic)
project_along <- function(coords, cum, pts) {
  n <- nrow(coords)
  if (n < 2 || nrow(pts) == 0) return(rep(0, nrow(pts)))
  lat0 <- mean(coords[, 2])
  kx <- 111320 * cos(lat0 * pi / 180)
  ky <- 110540
  ax <- coords[-n, 1] * kx; ay <- coords[-n, 2] * ky
  dx <- coords[-1, 1] * kx - ax; dy <- coords[-1, 2] * ky - ay
  len2 <- dx^2 + dy^2
  out <- numeric(nrow(pts))
  start <- 1L
  for (k in seq_len(nrow(pts))) {
    segs <- start:(n - 1L)
    px <- pts[k, 1] * kx; py <- pts[k, 2] * ky
    f <- ifelse(len2[segs] > 0, ((px - ax[segs]) * dx[segs] + (py - ay[segs]) * dy[segs]) / len2[segs], 0)
    f <- pmax(0, pmin(1, f))
    d2 <- (px - ax[segs] - f * dx[segs])^2 + (py - ay[segs] - f * dy[segs])^2
    j <- which.min(d2)
    seg <- segs[j]
    out[k] <- cum[seg] + f[j] * (cum[seg + 1L] - cum[seg])
    start <- seg
  }
  cummax(out)
}


# ---- Conformity ----

# Secondary metrics (context, not triggers)
trip_secondary_metrics <- function(s, th = REVIEW_THRESHOLDS) {
  n <- nrow(s)
  if (n < 2) {
    return(list(n_samples = n, path_km = 0, straight_km = 0, max_speed_kmh = NA_real_,
                duration_min = 0))
  }
  coords <- cbind(s$lon, s$lat)
  step_m <- geosphere::distHaversine(coords[-n, , drop = FALSE], coords[-1, , drop = FALSE])
  dt <- diff(s$t)
  ok <- dt >= th$min_pair_s
  list(
    n_samples     = n,
    path_km       = sum(step_m) / 1000,
    straight_km   = geosphere::distHaversine(coords[1, ], coords[n, ]) / 1000,
    max_speed_kmh = if (any(ok)) max(step_m[ok] / dt[ok]) * 3.6 else NA_real_,
    duration_min  = (s$t[n] - s$t[1]) / 60
  )
}

# A trip's samples in time order, with coordinates and time (what the metrics
# and the fixes index into)
review_clean_samples <- function(s) {
  s <- s[!is.na(s$lon) & !is.na(s$lat) & !is.na(s$t), ]
  s <- s[order(s$t), ]
  rownames(s) <- NULL
  s
}

# OSRM /match radiuses from horizontalAccuracy
match_radiuses <- function(acc, th = REVIEW_THRESHOLDS) {
  acc[is.na(acc)] <- 25
  pmin(pmax(acc, th$match_radius_m[1]), th$match_radius_m[2])
}

# Geometric conformity of a trip's recorded path. trip_samples: data.frame
# (lon, lat, t, acc). Rail activities are measured against the rails of their
# types (reference: sfc of lines, any crs; default from rails.gpkg), road
# activities against the OSRM-matched geometry (match: osrm_match() result;
# default: matched now). Returns list(metrics, match, dens, crs).
conformity <- function(trip_samples, activity, reference = NULL, match = NULL, th = REVIEW_THRESHOLDS) {
  act <- REVIEW_ACTIVITIES[[activity]]
  tol <- th[[act$family]]$tol_m
  s <- review_clean_samples(trip_samples)

  m <- trip_secondary_metrics(s, th)
  m[c("off_share", "max_offset_m", "longest_off_m", "match_confidence")] <- NA_real_
  m$n_dropped <- NA_integer_
  m$no_match <- FALSE
  if (nrow(s) < 2) return(list(metrics = m, match = NULL, dens = NULL, crs = NA_integer_))

  crs <- utm_epsg(s$lon, s$lat)
  xy <- to_metric(s$lon, s$lat, crs)
  ref <- NULL

  if (act$family == "rail") {
    ref <- if (is.null(reference)) {
      rail_lines_near(activity, c(min(s$lon), min(s$lat), max(s$lon), max(s$lat)), crs)
    } else {
      sf::st_transform(sf::st_geometry(reference), crs)
    }
  } else {
    if (is.null(match)) {
      match <- osrm_match(cbind(s$lon, s$lat), s$t, match_radiuses(s$acc, th), profile = act$osrm)
    }
    m$match_confidence <- match$confidence
    m$n_dropped <- as.integer(match$n_dropped)
    if (!isTRUE(match$ok)) {
      m$no_match <- TRUE
      return(list(metrics = m, match = match, dens = NULL, crs = crs))
    }
    ref <- sf::st_sfc(lapply(match$pieces, function(p) {
      sf::st_linestring(to_metric(p$coords[, 1], p$coords[, 2], crs))
    }), crs = crs)
  }

  om <- offset_metrics(xy, ref, tol, th$densify_m, crs)
  m$off_share <- om$off_share
  m$max_offset_m <- om$max_offset_m
  m$longest_off_m <- om$longest_off_m
  list(metrics = m, match = match, dens = om$dens, crs = crs)
}

# Reasons a trip is flagged: list of list(code, value). Empty: conforming.
review_flags <- function(m, activity, th = REVIEW_THRESHOLDS) {
  fam <- REVIEW_ACTIVITIES[[activity]]$family
  lim <- th[[fam]]
  out <- list()
  add <- function(code, value) out[[length(out) + 1L]] <<- list(code = code, value = value)

  if (isTRUE(m$n_samples < 2)) add("no_samples", m$n_samples)
  if (isTRUE(m$path_km < th$min_path_km)) add("fragment", m$path_km)
  if (fam == "road" && isTRUE(m$no_match)) add("no_match", NA_real_)
  if (isTRUE(m$off_share > lim$off_share)) add("off_share", m$off_share)
  if (isTRUE(m$longest_off_m > lim$longest_off_m)) add("longest_off", m$longest_off_m)
  if (fam == "rail" && isTRUE(m$max_offset_m > lim$max_offset_m)) add("max_offset", m$max_offset_m)
  out
}

# Human-readable reasons in lang
format_reasons <- function(reasons, lang = "en") {
  if (length(reasons) == 0) return("")
  txt <- vapply(reasons, function(r) {
    switch(r$code,
      no_samples  = tr_lang("no samples found", lang),
      fragment    = sprintf(tr_lang("fragment: %.2f km recorded", lang), r$value),
      no_match    = tr_lang("no road match", lang),
      off_share   = sprintf(tr_lang("%.0f%% of the path off the network", lang), 100 * r$value),
      longest_off = sprintf(tr_lang("%.0f m in a row off the network", lang), r$value),
      max_offset  = sprintf(tr_lang("up to %.0f m from the rails", lang), r$value),
      r$code
    )
  }, character(1))
  paste(txt, collapse = "; ")
}

# Sort key: larger = worse
review_severity <- function(m) {
  v <- function(x) if (is.null(x) || is.na(x)) 0 else x
  200 * isTRUE(m$no_match) + 100 * v(m$off_share) + v(m$longest_off_m) / 10 + v(m$max_offset_m) / 100
}


# ---- Inventory with per-trip cache ----

review_cache_file <- function() file.path(REVIEW_SCRATCH_DIR, "review_cache.rds")

review_cache_key <- function(trip, th = REVIEW_THRESHOLDS, rails_mtime = NA) {
  fam <- REVIEW_ACTIVITIES[[trip$activity]]$family
  paste(trip$id, trip$last_saved, fam, th[[fam]]$tol_m, th$densify_m,
        if (fam == "rail") rails_mtime else REVIEW_ACTIVITIES[[trip$activity]]$osrm, REVIEW_CACHE_VERSION[[fam]], sep = "|")
}

# Metrics for every trip of a trip_inventory(..., with_samples = FALSE) table.
# Only trips missing from the cache load samples and get measured (rails
# locally, roads with the local OSRM; no Google calls). Flagged road trips also
# get their default proposal built once (road_proposal_summary: lengths,
# re-match check). progress(fraction, detail). Returns the trips with metric
# columns, flagged, reasons, severity, fragment, and for road trips the
# proposal columns (proposal_method, proposed_km, len_ratio,
# rematch_off_share, detour, short_proposal, bulk_ok); attributes "matches" (road matches) and "proposals" (summaries
# with paths), one per row.
review_trips <- function(trips, weekly_dir = WEEKLY_BACKUP_DIR, th = REVIEW_THRESHOLDS,
                         cache_file = review_cache_file(), progress = NULL) {
  cache <- if (file.exists(cache_file)) tryCatch(readRDS(cache_file), error = function(e) list()) else list()
  rails_mtime <- if (file.exists(RAIL_REFERENCE_FILE)) as.numeric(file.mtime(RAIL_REFERENCE_FILE)) else NA
  keys <- vapply(seq_len(nrow(trips)), function(i) review_cache_key(trips[i, ], th, rails_mtime), character(1))
  todo <- which(!keys %in% names(cache))
  say <- function(f, d) if (is.function(progress)) progress(f, d)
  day <- function(i) format(trips$start_utc[i], "%Y-%m-%d")
  needs_proposal <- function(i) {
    e <- cache[[keys[i]]]
    !is.null(e) && is.null(e$proposal) && REVIEW_ACTIVITIES[[trips$activity[i]]]$family == "road" &&
      !isTRUE(trips$already_fixed[i]) && length(review_flags(e$metrics, trips$activity[i], th)) > 0
  }
  changed <- FALSE

  if (length(todo)) {
    samples <- trip_samples_for(trips[todo, ], weekly_dir, function(f, d) say(0.3 * f, d))
    for (k in seq_along(todo)) {
      i <- todo[k]
      say(0.3 + 0.6 * k / length(todo), day(i))
      res <- tryCatch(conformity(samples[[k]], trips$activity[i], th = th), error = function(e) {
        message("[Review] ", trips$id[i], ": ", conditionMessage(e))
        NULL
      })
      if (is.null(res)) next
      cache[[keys[i]]] <- list(metrics = res$metrics, match = res$match, saved = Sys.time())
      if (needs_proposal(i)) {
        cache[[keys[i]]]$proposal <- road_proposal_summary(samples[[k]], res$match, trips$activity[i], th)
      }
    }
    changed <- TRUE
  }
  # Flagged road trips measured earlier without a proposal (e.g. other thresholds)
  later <- Filter(needs_proposal, seq_len(nrow(trips)))
  if (length(later)) {
    samples <- trip_samples_for(trips[later, ], weekly_dir)
    for (k in seq_along(later)) {
      i <- later[k]
      say(0.9 + 0.1 * k / length(later), day(i))
      cache[[keys[i]]]$proposal <- road_proposal_summary(samples[[k]], cache[[keys[i]]]$match, trips$activity[i], th)
    }
    changed <- TRUE
  }
  if (changed) {
    dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
    saveRDS(cache, cache_file)
  }

  metric_names <- c("n_samples", "path_km", "straight_km", "max_speed_kmh", "duration_min", "off_share",
                    "max_offset_m", "longest_off_m", "match_confidence", "n_dropped", "no_match")
  for (nm in metric_names) {
    trips[[nm]] <- vapply(keys, function(k) {
      v <- cache[[k]]$metrics[[nm]]
      if (is.null(v) || length(v) == 0) NA_real_ else as.numeric(v)
    }, numeric(1), USE.NAMES = FALSE)
  }
  trips$no_match <- trips$no_match %in% 1
  trips$reasons <- lapply(seq_len(nrow(trips)), function(i) {
    if (is.null(cache[[keys[i]]])) return(list(list(code = "no_samples", value = 0)))
    review_flags(cache[[keys[i]]]$metrics, trips$activity[i], th)
  })
  trips$flagged <- lengths(trips$reasons) > 0 & !trips$already_fixed
  trips$severity <- vapply(keys, function(k) review_severity(cache[[k]]$metrics %||% list()), numeric(1),
                           USE.NAMES = FALSE)
  proposal <- lapply(keys, function(k) cache[[k]]$proposal)
  num <- function(nm) vapply(proposal, function(p) as.numeric(p[[nm]] %||% NA_real_), numeric(1))
  trips$proposal_method <- vapply(proposal, function(p) p$method %||% NA_character_, character(1))
  trips$proposed_km <- num("proposed_km")
  trips$len_ratio <- num("len_ratio")
  trips$rematch_off_share <- num("rematch_off_share")
  verdicts <- lapply(proposal, function(p) if (is.null(p) || isTRUE(p$failed)) NULL else road_proposal_status(p, th))
  trips$detour <- vapply(verdicts, function(v) isTRUE(v$detour), logical(1))
  trips$short_proposal <- vapply(verdicts, function(v) isTRUE(v$short), logical(1))
  trips$fragment <- vapply(trips$reasons, function(r) "fragment" %in% vapply(r, `[[`, "", "code"), logical(1))
  trips$bulk_ok <- vapply(verdicts, function(v) isTRUE(v$bulk_ok), logical(1))
  trips$cache_key <- keys
  attr(trips, "matches") <- lapply(keys, function(k) cache[[k]]$match)
  attr(trips, "proposals") <- proposal
  trips
}


# ---- Proposed fixes ----

# Origin/destination samples: first/last with horizontalAccuracy <= good_accuracy_m,
# else first/last. Returns c(origin, destination) row indices.
review_endpoints <- function(s, th = REVIEW_THRESHOLDS) {
  good <- which(!is.na(s$acc) & s$acc <= th$good_accuracy_m)
  if (length(good) >= 2) c(good[1], good[length(good)]) else c(1L, nrow(s))
}

# Direction of travel at sample i: towards the first sample at least min_m away
# (forward) or from the last one at least min_m before (backward), in degrees
travel_bearing <- function(s, i, direction = c("forward", "backward"), min_m = 300) {
  direction <- match.arg(direction)
  others <- if (direction == "forward") seq.int(i, nrow(s)) else rev(seq_len(i))
  d <- geosphere::distHaversine(cbind(s$lon[others], s$lat[others]), c(s$lon[i], s$lat[i]))
  j <- others[which(d >= min_m)[1]]
  if (is.na(j)) j <- others[length(others)]
  if (j == i) return(NA_real_)
  if (direction == "forward") {
    geosphere::bearing(c(s$lon[i], s$lat[i]), c(s$lon[j], s$lat[j]))
  } else {
    geosphere::bearing(c(s$lon[j], s$lat[j]), c(s$lon[i], s$lat[i]))
  }
}

angle_diff <- function(a, b) abs((a - b + 180) %% 360 - 180)

# Station for an endpoint among candidates (station_candidates()) within
# radius_m: the origin prefers a station behind the direction of travel (the
# train left it before the first sample), the destination one ahead of it.
# Returns the chosen row (data.frame) or NULL to keep the sample point.
pick_station <- function(candidates, lon, lat, bearing, side = c("origin", "destination"),
                         radius_m = REVIEW_THRESHOLDS$station_radius_m) {
  side <- match.arg(side)
  cand <- candidates[candidates$dist_m <= radius_m, ]
  if (nrow(cand) == 0) return(NULL)
  if (!is.na(bearing)) {
    b <- if (side == "origin") {
      geosphere::bearing(cbind(cand$lon, cand$lat), c(lon, lat))       # station -> sample
    } else {
      geosphere::bearing(c(lon, lat), cbind(cand$lon, cand$lat))       # sample -> station
    }
    good <- !is.na(b) & angle_diff(b, bearing) < 90 | cand$dist_m < 30
    if (any(good)) cand <- cand[good, ]
  }
  cand[which.min(cand$dist_m), ]
}

# Clean rail path with times. route: n x 2 lon/lat, station to station;
# origin/dest: the chosen samples (lon, lat, t). The time at the departure
# station is the origin sample's minus the along-rail distance at
# rail_speed_kmh (capped), likewise after the destination sample;
# start_offset_s/end_offset_s are the user's fine adjustments.
# segments (rail_segments()): t_start..t_end is shared among Google's legs and
# transfers in proportion to Google's durations (a transfer's wait is a dwell
# at the next leg's departure stop), linear by distance within each; their
# geometry replaces route. Without segments, times are linear by distance
# between the station and sample times (timing = "distance").
rail_timed_path <- function(route, origin, dest, th = REVIEW_THRESHOLDS, start_offset_s = 0, end_offset_s = 0,
                            segments = NULL) {
  if (!is.null(segments)) {
    geo <- rail_segments_geometry(segments)
    route <- geo$coords
  }
  cum <- path_cumulative_m(route)
  total <- cum[length(cum)]
  d <- project_along(route, cum, rbind(c(origin$lon, origin$lat), c(dest$lon, dest$lat)))
  speed <- th$rail_speed_kmh / 3.6
  adj_start <- min(d[1] / speed, th$max_time_adjust_s)
  adj_end <- min((total - d[2]) / speed, th$max_time_adjust_s)
  t_start <- origin$t - adj_start + start_offset_s
  t_end <- dest$t + adj_end + end_offset_s

  if (t_end <= t_start) t_end <- t_start + 1

  if (!is.null(segments)) {
    st <- rail_step_anchors(segments, cum[geo$ends], t_start, t_end)
    path <- timed_path_from_anchors(route, st$anchor_d, st$anchor_t)
    return(list(
      path = resample_timed_path(path, th$point_every_m, th$point_every_s, max_dwell_s = th$transfer_dwell_point_s),
      t_start = t_start, t_end = t_end,
      adj_start_s = adj_start, adj_end_s = adj_end,
      along_m = d, total_m = total,
      timing = "steps", legs = st$legs, scale = st$scale
    ))
  }

  # Station times at the ends; the samples in between when they fit (a fine
  # adjustment past a sample's time drops that sample's anchor)
  inside <- function(dd, tt) dd > 0 && dd < total && tt > t_start && tt < t_end
  anchor_d <- 0
  anchor_t <- t_start
  if (inside(d[1], origin$t)) {
    anchor_d <- c(anchor_d, d[1])
    anchor_t <- c(anchor_t, origin$t)
  }
  if (inside(d[2], dest$t) && d[2] > anchor_d[length(anchor_d)] && dest$t > anchor_t[length(anchor_t)]) {
    anchor_d <- c(anchor_d, d[2])
    anchor_t <- c(anchor_t, dest$t)
  }
  anchor_d <- c(anchor_d, total)
  anchor_t <- c(anchor_t, t_end)

  path <- timed_path_from_anchors(route, anchor_d, anchor_t)
  list(
    path = resample_timed_path(path, th$point_every_m, th$point_every_s),
    t_start = t_start, t_end = t_end,
    adj_start_s = adj_start, adj_end_s = adj_end,
    along_m = d, total_m = total,
    timing = "distance"
  )
}

# Timing segments of a Google transit route (steps: transit_route_steps()):
# its legs of the requested mode (kind "transit", with a polyline) and the
# transfers between them. A transfer moves along whatever lies between two
# legs (walking, other steps; straight from platform to platform otherwise)
# for those steps' durations, then waits at the next leg's departure stop for
# the rest of the gap between the previous leg's arrival and the next leg's
# departure (no wait when Google gives no times). Each segment:
# list(kind = "leg" | "transfer", coords, move_s, wait_s, line | stop,
# timetable). NULL when a leg has no duration: timing falls back to distance.
rail_segments <- function(steps) {
  ok <- function(x) length(x) == 1 && is.finite(x)
  legs <- which(vapply(steps, function(st) {
    identical(st$kind, "transit") && !is.null(st$coords) && nrow(st$coords) >= 2
  }, logical(1)))
  if (length(legs) == 0) return(NULL)
  out <- list()
  for (k in seq_along(legs)) {
    st <- steps[[legs[k]]]
    ride <- if (ok(st$departure_t) && ok(st$arrival_t) && st$arrival_t > st$departure_t) {
      st$arrival_t - st$departure_t
    } else {
      st$duration_s
    }
    if (!ok(ride) || ride <= 0) return(NULL)
    if (k > 1) {
      prev <- steps[[legs[k - 1]]]
      between <- if (legs[k] - legs[k - 1] > 1) steps[(legs[k - 1] + 1):(legs[k] - 1)] else list()
      move <- sum(vapply(between, function(b) if (ok(b$duration_s)) b$duration_s else 0, numeric(1)))
      gap <- st$departure_t - prev$arrival_t
      geom <- rbind(prev$coords[nrow(prev$coords), , drop = FALSE],
                    do.call(rbind, lapply(between, `[[`, "coords")),
                    st$coords[1, , drop = FALSE])
      out[[length(out) + 1L]] <- list(kind = "transfer", coords = geom, move_s = move,
                                      wait_s = if (ok(gap)) max(0, gap - move) else 0,
                                      stop = st$from_stop %||% prev$to_stop %||% "", timetable = ok(gap))
    }
    out[[length(out) + 1L]] <- list(kind = "leg", coords = st$coords, move_s = ride, wait_s = 0,
                                    line = st$line %||% "")
  }
  out
}

# One polyline for the segments (consecutive segments share their end points;
# repeated points dropped). ends: row of each segment's last point.
rail_segments_geometry <- function(segments) {
  dedup <- function(m) m[c(TRUE, rowSums(abs(diff(m))) > 0), , drop = FALSE]
  g <- dedup(segments[[1]]$coords)
  ends <- nrow(g)
  for (sg in segments[-1]) {
    g <- rbind(g, dedup(sg$coords)[-1, , drop = FALSE])
    ends <- c(ends, nrow(g))
  }
  colnames(g) <- c("lon", "lat")
  list(coords = g, ends = ends)
}

# Anchors along rail_segments_geometry() (end_d: distance at each segment's
# end): t_start..t_end shared in proportion to Google's durations; a wait is
# two anchors at the same distance (a dwell). legs: per segment, Google's
# seconds and the seconds given here.
rail_step_anchors <- function(segments, end_d, t_start, t_end) {
  google <- vapply(segments, function(sg) sg$move_s + sg$wait_s, numeric(1))
  scale <- (t_end - t_start) / sum(google)
  t <- t_start
  anchor_d <- 0
  anchor_t <- t_start
  for (i in seq_along(segments)) {
    t <- t + scale * segments[[i]]$move_s
    anchor_d <- c(anchor_d, end_d[i])
    anchor_t <- c(anchor_t, t)
    if (segments[[i]]$wait_s > 0) {
      t <- t + scale * segments[[i]]$wait_s
      anchor_d <- c(anchor_d, end_d[i])
      anchor_t <- c(anchor_t, t)
    }
  }
  anchor_t[length(anchor_t)] <- t_end
  keep <- !duplicated(data.frame(anchor_d, anchor_t))
  legs <- lapply(seq_along(segments), function(i) {
    sg <- segments[[i]]
    list(kind = sg$kind, label = if (sg$kind == "leg") sg$line else sg$stop, google_s = google[i],
         s = scale * google[i], move_s = sg$move_s, wait_s = sg$wait_s, timetable = sg$timetable %||% TRUE)
  })
  list(anchor_d = anchor_d[keep], anchor_t = anchor_t[keep], legs = legs, scale = scale)
}

# "Linha 2 · 18 min → transfer Estácio 4 min → Linha 1 · 9 min" (the times
# given to each segment; google = TRUE: Google's own, with each transfer's
# walk and wait)
format_rail_legs <- function(legs, lang = "en", google = FALSE) {
  mins <- function(x) if (x < 30) "<1 min" else sprintf("%.0f min", x / 60)
  txt <- vapply(legs, function(l) {
    x <- if (google) l$google_s else l$s
    if (l$kind == "leg") return(sprintf("%s · %s", if (nzchar(l$label)) l$label else "?", mins(x)))
    if (!google) return(sprintf(tr_lang("transfer %s %s", lang), l$label, mins(x)))
    sprintf(tr_lang("transfer %s %s (walk %s, wait %s)", lang), l$label, mins(x), mins(l$move_s),
            if (isTRUE(l$timetable)) mins(l$wait_s) else "?")
  }, character(1))
  paste(txt, collapse = " → ")
}

# Endpoints and stations for a rail fix. Overrides: origin_idx/dest_idx (sample
# rows, from "Set as start/end"), dep_name/arr_name (station names from the
# selects; "" = keep the sample point). Returns the plan, including the points
# and reference time for calculate_google_transit_route().
rail_plan <- function(s, activity, origin_idx = NULL, dest_idx = NULL, dep_name = NULL, arr_name = NULL,
                      th = REVIEW_THRESHOLDS, ref = rail_reference()) {
  ends <- review_endpoints(s, th)
  oi <- origin_idx %||% ends[1]
  di <- dest_idx %||% ends[2]
  if (di <= oi) di <- nrow(s)

  choose <- function(i, name, bearing, side) {
    if (!is.null(name)) {
      if (!nzchar(name)) return(NULL)
      hit <- station_candidates(activity, s$lon[i], s$lat[i], k = 50, ref = ref)
      hit <- hit[hit$name == name, ]
      if (nrow(hit)) return(hit[1, ])
    }
    near <- station_candidates(activity, s$lon[i], s$lat[i], k = 20, radius_m = th$station_radius_m, ref = ref)
    pick_station(near, s$lon[i], s$lat[i], bearing, side, th$station_radius_m)
  }
  dep <- choose(oi, dep_name, travel_bearing(s, oi, "forward"), "origin")
  arr <- choose(di, arr_name, travel_bearing(s, di, "backward"), "destination")

  # Select options: the 3 nearest stations, plus the pick when it isn't one of them
  options_for <- function(i, picked) {
    cands <- station_candidates(activity, s$lon[i], s$lat[i], k = 3, ref = ref)
    if (!is.null(picked) && !picked$name %in% cands$name) cands <- rbind(picked, cands)
    cands
  }
  from <- if (is.null(dep)) c(s$lon[oi], s$lat[oi]) else c(dep$lon, dep$lat)
  to <- if (is.null(arr)) c(s$lon[di], s$lat[di]) else c(arr$lon, arr$lat)
  list(
    origin_idx = oi, dest_idx = di, dep = dep, arr = arr,
    dep_options = options_for(oi, dep), arr_options = options_for(di, arr),
    points = data.frame(lat = c(from[2], to[2]), lng = c(from[1], to[1])),
    ref_time_utc = as.POSIXct(s$t[oi], origin = "1970-01-01", tz = "UTC")
  )
}

# Rail fix from a plan and the transit route: calculate_google_transit_route()'s
# result (coords: transit steps only; steps: all of them, for the timing), or
# a bare n x 2 lon/lat matrix (distance timing). Timed path plus the route's
# own conformity against the rails (Google can return legs that don't follow
# them).
rail_fix <- function(plan, route, s, activity, th = REVIEW_THRESHOLDS,
                     start_offset_s = 0, end_offset_s = 0, ref = rail_reference()) {
  if (is.matrix(route)) route <- list(coords = route)
  route_coords <- route$coords
  oi <- plan$origin_idx
  di <- plan$dest_idx
  tp <- rail_timed_path(route_coords,
                        list(lon = s$lon[oi], lat = s$lat[oi], t = s$t[oi]),
                        list(lon = s$lon[di], lat = s$lat[di], t = s$t[di]),
                        th, start_offset_s, end_offset_s,
                        segments = if (length(route$steps)) rail_segments(route$steps))
  crs <- utm_epsg(route_coords[, 1], route_coords[, 2])
  rails <- rail_lines_near(activity, c(range(route_coords[, 1]), range(route_coords[, 2]))[c(1, 3, 2, 4)], crs, ref = ref)
  om <- offset_metrics(to_metric(route_coords[, 1], route_coords[, 2], crs), rails, th$rail$tol_m, th$densify_m, crs)
  route_reasons <- Filter(function(r) r$code != "fragment",
                          review_flags(list(off_share = om$off_share, longest_off_m = om$longest_off_m,
                                            max_offset_m = om$max_offset_m), activity, th))
  c(tp, list(
    method = "google", on_network = length(route_reasons) == 0, route_reasons = route_reasons,
    route_off_share = om$off_share, route_longest_off_m = om$longest_off_m, route_max_offset_m = om$max_offset_m,
    route = route
  ))
}

# Clean road path from an osrm_match() result: the matched geometry with the
# real sample times at the matched tracepoints (speed varies like the trip);
# gaps between matched pieces are routed with OSRM /route (straight line when
# that fails) with times linear across the gap. route_fn(points, profile) is
# calculate_osrm_route by default. Returns list(path, method, on_network) or
# NULL when nothing matched.
road_timed_path <- function(s, match, profile = "car", th = REVIEW_THRESHOLDS,
                            route_fn = calculate_osrm_route) {
  if (!isTRUE(match$ok) || length(match$pieces) == 0) return(NULL)
  t <- s$t
  segments <- list()
  on_network <- TRUE

  for (k in seq_along(match$pieces)) {
    p <- match$pieces[[k]]
    geom <- p$coords
    cum <- path_cumulative_m(geom)
    anchor_d <- project_along(geom, cum, match$tracepoints[p$idx, , drop = FALSE])
    segments[[length(segments) + 1L]] <- timed_path_from_anchors(geom, anchor_d, t[p$idx])

    if (k < length(match$pieces)) {
      a <- p$idx[length(p$idx)]
      b <- match$pieces[[k + 1L]]$idx[1]
      from <- match$tracepoints[a, ]
      to <- match$tracepoints[b, ]
      gap <- tryCatch(route_fn(data.frame(lat = c(from[2], to[2]), lng = c(from[1], to[1])), profile),
                      error = function(e) NULL)
      gap_coords <- if (!is.null(gap$coords) && nrow(gap$coords) >= 2) gap$coords else NULL
      if (is.null(gap_coords)) {
        on_network <- FALSE
        gap_coords <- rbind(from, to)
      }
      gap_coords[1, ] <- from
      gap_coords[nrow(gap_coords), ] <- to
      colnames(gap_coords) <- c("lon", "lat")
      gap_cum <- path_cumulative_m(gap_coords)
      segments[[length(segments) + 1L]] <- timed_path_from_anchors(gap_coords, c(0, gap_cum[length(gap_cum)]),
                                                                   c(t[a], t[b]))
    }
  }

  path <- do.call(rbind, lapply(seq_along(segments), function(i) {
    seg <- segments[[i]]
    if (i > 1) seg <- seg[-1, ]  # first row repeats the previous segment's last
    seg
  }))
  # Samples dropped at the ends: dwell at the first/last matched location
  first_idx <- match$pieces[[1]]$idx[1]
  last_piece <- match$pieces[[length(match$pieces)]]
  last_idx <- last_piece$idx[length(last_piece$idx)]
  if (t[1] < t[first_idx]) {
    lead <- path[1, ]
    lead$t <- t[1]
    lead$anchor <- TRUE
    path <- rbind(lead, path)
  }
  if (t[length(t)] > t[last_idx]) {
    tail_row <- path[nrow(path), ]
    tail_row$t <- t[length(t)]
    tail_row$anchor <- TRUE
    path <- rbind(path, tail_row)
  }
  path$d <- path_cumulative_m(cbind(path$lon, path$lat))
  list(path = resample_timed_path(path, th$point_every_m, th$point_every_s),
       method = if (length(match$pieces) > 1) "match+gaps" else "match", on_network = on_network)
}

# Fallback / re-route: OSRM /route between two samples, times linear between
# them, dwelling at the ends until the first/last sample times
route_timed_path <- function(s, from_i, to_i, profile = "car", th = REVIEW_THRESHOLDS,
                             route_fn = calculate_osrm_route) {
  route <- tryCatch(route_fn(data.frame(lat = s$lat[c(from_i, to_i)], lng = s$lon[c(from_i, to_i)]), profile),
                    error = function(e) NULL)
  if (is.null(route$coords) || nrow(route$coords) < 2) return(NULL)
  coords <- route$coords
  cum <- path_cumulative_m(coords)
  total <- cum[length(cum)]
  anchor_d <- c(0, 0, total, total)
  anchor_t <- c(s$t[1], s$t[from_i], s$t[to_i], s$t[nrow(s)])
  keep <- !duplicated(data.frame(anchor_d, anchor_t))
  path <- timed_path_from_anchors(coords, anchor_d[keep], anchor_t[keep])
  list(path = resample_timed_path(path, th$point_every_m, th$point_every_s), method = "route", on_network = TRUE)
}

# Re-route through the recorded path: OSRM /route via waypoints from
# via_waypoints(), each snapped with the travel direction so it lands on the
# right carriageway. A leg much longer than what was driven between its two
# waypoints is a loop around a badly snapped waypoint: its interior waypoints
# are dropped and the route is asked again (up to 3 tries). Times: each
# waypoint keeps its sample's time, linear by distance in between; dwelling at
# the ends until the first/last sample times.
via_timed_path <- function(s, from_i, to_i, profile = "car", th = REVIEW_THRESHOLDS,
                           route_via_fn = osrm_route_via) {
  wp <- via_waypoints(s, from_i, to_i, th$road$via_every_m, th)
  bearing <- vapply(wp, function(i) travel_bearing(s, i, "forward", min_m = 80), numeric(1))
  radius <- pmin(pmax(2 * s$acc[wp], 100, na.rm = TRUE), 200)
  bearing[c(1, length(wp))] <- NA  # the ends snap freely (parking lots, garages)
  radius[c(1, length(wp))] <- NA
  driven_cum <- path_cumulative_m(cbind(s$lon, s$lat))
  pruned <- 0L

  for (pass in 1:3) {
    r <- tryCatch(route_via_fn(cbind(s$lon[wp], s$lat[wp]), profile, bearings = bearing, radiuses = radius),
                  error = function(e) NULL)
    if (is.null(r$coords) || nrow(r$coords) < 2) return(NULL)
    loopy <- which(r$legs_m > 1.5 * diff(driven_cum[wp]) + 200)
    drop <- setdiff(unique(c(loopy, loopy + 1L)), c(1L, length(wp)))
    if (pass == 3 || length(drop) == 0) break
    wp <- wp[-drop]
    bearing <- bearing[-drop]
    radius <- radius[-drop]
    pruned <- pruned + length(drop)
  }

  coords <- r$coords
  colnames(coords) <- c("lon", "lat")
  cum <- path_cumulative_m(coords)
  total <- cum[length(cum)]
  legs_cum <- c(0, cumsum(r$legs_m))
  # OSRM's leg lengths, scaled to the geometry's own length
  anchor_d <- if (legs_cum[length(legs_cum)] > 0) legs_cum * total / legs_cum[length(legs_cum)] else legs_cum
  anchor_t <- s$t[wp]
  if (s$t[1] < anchor_t[1]) {
    anchor_d <- c(0, anchor_d)
    anchor_t <- c(s$t[1], anchor_t)
  }
  if (s$t[nrow(s)] > anchor_t[length(anchor_t)]) {
    anchor_d <- c(anchor_d, total)
    anchor_t <- c(anchor_t, s$t[nrow(s)])
  }
  keep <- !duplicated(data.frame(anchor_d, anchor_t))
  path <- timed_path_from_anchors(coords, anchor_d[keep], anchor_t[keep])
  list(path = resample_timed_path(path, th$point_every_m, th$point_every_s), method = "via", on_network = TRUE,
       n_waypoints = length(wp), n_pruned = pruned)
}

# Waypoints through the recorded samples from_i..to_i (row indices): the ends,
# plus the next sample at least every_m in a straight line from the previous
# waypoint. Samples worse than good_accuracy_m wait until 2 x every_m.
via_waypoints <- function(s, from_i, to_i, every_m = 300, th = REVIEW_THRESHOLDS) {
  out <- from_i
  if (to_i - from_i >= 2) {
    good <- is.na(s$acc) | s$acc <= th$good_accuracy_m
    for (i in (from_i + 1L):(to_i - 1L)) {
      last <- out[length(out)]
      d <- geosphere::distHaversine(c(s$lon[last], s$lat[last]), c(s$lon[i], s$lat[i]))
      if (d >= every_m && (good[i] || d >= 2 * every_m)) out <- c(out, i)
    }
    last <- out[length(out)]
    if (length(out) > 1 &&
        geosphere::distHaversine(c(s$lon[last], s$lat[last]), c(s$lon[to_i], s$lat[to_i])) < every_m / 2) {
      out <- out[-length(out)]
    }
  }
  c(out, to_i)
}

# Default proposal for a road trip (s: review_clean_samples()): the OSRM match
# with its gaps routed, or a route between the ends when nothing matched; with
# the checks of road_proposal_check(). profile: OSRM profile (default: the activity's)
road_proposal <- function(s, match, activity, th = REVIEW_THRESHOLDS,
                          route_fn = calculate_osrm_route, match_fn = osrm_match,
                          profile = REVIEW_ACTIVITIES[[activity]]$osrm) {
  fx <- if (isTRUE(match$ok)) road_timed_path(s, match, profile, th, route_fn) else NULL
  if (is.null(fx)) {
    fx <- route_timed_path(s, 1L, nrow(s), profile, th, route_fn)
    if (!is.null(fx)) fx$method <- "route_fallback"
  }
  if (is.null(fx)) return(NULL)
  road_proposal_check(fx, s, activity, 1L, nrow(s), th, match_fn, profile)
}

# Checks of a road proposal (fx with path) against the recorded samples
# from_i..to_i. len_ratio: proposed / recorded length. rematch_*: the proposed
# path matched again (tight radius: its points lie on the roads) and measured
# against that match; ~0 when it stays on the roads, without straight lines.
# Adds proposed_km, recorded_km, len_ratio, rematch_off_share,
# rematch_longest_off_m, detour, bulk_ok.
road_proposal_check <- function(fx, s, activity, from_i = 1L, to_i = nrow(s), th = REVIEW_THRESHOLDS,
                                match_fn = osrm_match, profile = REVIEW_ACTIVITIES[[activity]]$osrm) {
  p <- fx$path
  fx$proposed_km <- path_cumulative_m(cbind(p$lon, p$lat))[nrow(p)] / 1000
  fx$recorded_km <- trip_secondary_metrics(s[from_i:to_i, , drop = FALSE], th)$path_km
  fx$len_ratio <- if (fx$recorded_km > 0) fx$proposed_km / fx$recorded_km else NA_real_
  pts <- data.frame(lon = p$lon, lat = p$lat, t = p$t, acc = 10)
  m <- tryCatch(match_fn(cbind(pts$lon, pts$lat), pts$t, match_radiuses(pts$acc, th), profile = profile),
                error = function(e) NULL)
  cf <- if (is.null(m)) NULL else conformity(pts, activity, match = m, th = th)$metrics
  fx$rematch_off_share <- if (is.null(cf) || isTRUE(cf$no_match)) NA_real_ else cf$off_share
  fx$rematch_longest_off_m <- if (is.null(cf) || isTRUE(cf$no_match)) NA_real_ else cf$longest_off_m
  c(fx, road_proposal_status(fx, th))
}

# Verdicts for a road proposal (fx, or its inventory summary): detour = longer
# than detour_ratio x the recorded path; short = shorter than short_ratio x
# (possibly skips part of the trip); bulk_ok = not a fragment, built from the
# recording (match), from short_ratio to bulk_max_ratio x and on the roads
# when re-matched
road_proposal_status <- function(p, th = REVIEW_THRESHOLDS) {
  lim <- th$road
  r <- p$len_ratio %||% NA_real_
  detour <- isTRUE(r > lim$detour_ratio)
  short <- isTRUE(r < lim$short_ratio)
  fragment <- isTRUE(p$recorded_km < th$min_path_km)
  bulk_ok <- !detour && !short && !fragment && isTRUE(p$on_network) && startsWith(p$method %||% "", "match") &&
    isTRUE(r <= lim$bulk_max_ratio) && isTRUE(p$rematch_off_share <= lim$bulk_max_off_share)
  list(detour = detour, short = short, bulk_ok = bulk_ok)
}

# What the inventory keeps of a trip's default road proposal, with its path
# (lon, lat, t), so opening the trip or accepting in bulk needs no OSRM call
ROAD_PROPOSAL_FIELDS <- c("method", "on_network", "proposed_km", "recorded_km", "len_ratio",
                          "rematch_off_share", "rematch_longest_off_m")

road_proposal_summary <- function(trip_samples, match, activity, th = REVIEW_THRESHOLDS) {
  fx <- tryCatch(road_proposal(review_clean_samples(trip_samples), match, activity, th), error = function(e) {
    message("[Review] road proposal: ", conditionMessage(e))
    NULL
  })
  if (is.null(fx)) return(list(failed = TRUE))
  c(fx[ROAD_PROPOSAL_FIELDS], list(path = fx$path[, c("lon", "lat", "t")]))
}


# ---- Multimodal split (experiment): walk -> wait -> bus -> walk / visit ----

# Splits a bus trip that Arc started at the front door, using the Jaé boarding
# (time and place of the card tap) and the alighting point clicked on the map.
# trip_samples: data.frame(lon, lat, t, acc); boarding: lon, lat, t (epoch s or
# POSIXct; a jae_boardings row also works) and an optional label;
# alighting_point: c(lon, lat); after_mode: "walk" (OSRM foot to the trip's
# last sample) or "visit" (stationary at the last sample, as part of the
# destination visit).
# Times: the walk leaves at the first sample and arrives after the OSRM foot
# duration, capped at the boarding; the wait at the stop ends exactly at the
# boarding time; the bus runs until the alighting time, which is the time of
# the recorded sample nearest the click along the recording after the boarding;
# the remainder runs until the last sample. Returns a list of segments, each
# list(role, type, t_start, t_end, path = data.frame(lon, lat, t), info),
# continuous in time; zero-length walks and waits are left out.
build_multimodal_segments <- function(trip_samples, boarding, alighting_point, after_mode = c("walk", "visit"),
                                      th = REVIEW_THRESHOLDS, route_fn = calculate_osrm_route, match_fn = osrm_match,
                                      wait_point_s = 30, walk_mps = 1.3) {
  after_mode <- match.arg(after_mode)
  s <- review_clean_samples(trip_samples)
  if (is.null(s$acc)) s$acc <- NA_real_
  if (nrow(s) < 2) stop("the trip needs at least 2 samples")
  b <- multimodal_point(boarding)
  t0 <- s$t[1]
  t_end <- s$t[nrow(s)]
  if (!isTRUE(b$t >= t0 && b$t < t_end)) stop("the boarding time is outside the trip")

  ib <- which(s$t >= b$t)[1]
  ia <- multimodal_alighting_index(s, ib, alighting_point)
  t_alight <- s$t[ia]
  if (t_alight <= b$t) stop("the alighting point must come after the boarding along the recording")

  segs <- list()
  stop_xy <- c(b$lon, b$lat)

  # 1. Walk to the stop, 2. wait there until the tap
  walk <- multimodal_walk(c(s$lon[1], s$lat[1]), stop_xy, route_fn, walk_mps)
  t_arrive <- min(t0 + walk$duration_s, b$t)
  if (t_arrive > t0) {
    segs[[length(segs) + 1L]] <- multimodal_moving("walk_to_stop", "walking", walk, t0, t_arrive, th)
  }
  if (b$t > t_arrive) {
    segs[[length(segs) + 1L]] <- multimodal_stationary("wait", stop_xy, t_arrive, b$t, wait_point_s, list(name = b$label))
  }

  # 3. Bus: the recording between boarding and alighting, matched on the bus profile
  segs[[length(segs) + 1L]] <- multimodal_bus(s, b, alighting_point, t_alight, th, route_fn, match_fn)

  # 4. After alighting, until the trip's original end
  if (t_end > t_alight) {
    dest <- c(s$lon[nrow(s)], s$lat[nrow(s)])
    segs[[length(segs) + 1L]] <- if (after_mode == "walk") {
      multimodal_moving("walk_from_stop", "walking", multimodal_walk(alighting_point, dest, route_fn, walk_mps),
                        t_alight, t_end, th)
    } else {
      multimodal_stationary("at_destination", dest, t_alight, t_end, wait_point_s)
    }
  }
  segs
}

# lon, lat, t (epoch s), label from a list / one-row data.frame, or a jae_boardings row.
# Exact names: `$` on a data.frame would partially match t to tipo_transacao.
multimodal_point <- function(b) {
  field <- function(...) {
    for (k in c(...)) if (!is.null(b[[k]])) return(b[[k]])
    NULL
  }
  list(lon = as.numeric(field("lon", "longitude"))[1], lat = as.numeric(field("lat", "latitude"))[1],
       t = as.numeric(field("t", "datetime_utc"))[1], label = as.character(field("label") %||% NA_character_)[1])
}

# Alighting sample: the click is projected onto the recorded path from sample
# ib on, and the sample closest to that position along the path is taken. A
# sample that is merely close on the map (an earlier pass, or the way back) is
# not picked.
multimodal_alighting_index <- function(s, ib, point) {
  sub <- ib:nrow(s)
  if (length(sub) < 2) return(sub[1])
  coords <- cbind(s$lon[sub], s$lat[sub])
  cum <- path_cumulative_m(coords)
  d_click <- project_along(coords, cum, matrix(point[1:2], nrow = 1))
  sub[which.min(abs(cum - d_click))]
}

# Walking geometry from -> to: OSRM foot, joined to the exact end points;
# straight line at walk_mps when OSRM has no route
multimodal_walk <- function(from, to, route_fn, walk_mps) {
  r <- tryCatch(route_fn(data.frame(lat = c(from[2], to[2]), lng = c(from[1], to[1])), "foot"), error = function(e) NULL)
  if (!is.null(r$coords) && nrow(r$coords) >= 2) {
    coords <- rbind(from[1:2], r$coords[, 1:2], to[1:2])
    coords <- coords[c(TRUE, rowSums(abs(diff(coords))) > 0), , drop = FALSE]
    return(list(coords = coords, duration_s = as.numeric(r$duration_s), method = "osrm_foot"))
  }
  coords <- rbind(from[1:2], to[1:2])
  list(coords = coords, duration_s = path_cumulative_m(coords)[2] / walk_mps, method = "straight")
}

# Moving segment over walk$coords from t_start to t_end, timed by distance
multimodal_moving <- function(role, type, walk, t_start, t_end, th) {
  coords <- walk$coords
  colnames(coords) <- c("lon", "lat")
  total <- path_cumulative_m(coords)[nrow(coords)]
  path <- if (total > 0) {
    resample_timed_path(timed_path_from_anchors(coords, c(0, total), c(t_start, t_end)), th$point_every_m, th$point_every_s)
  } else {
    data.frame(lon = coords[1, 1], lat = coords[1, 2], t = c(t_start, t_end))
  }
  list(role = role, type = type, t_start = t_start, t_end = t_end, path = multimodal_path(path, t_start, t_end),
       info = list(method = walk$method, distance_m = total, osrm_duration_s = walk$duration_s))
}

# Stationary segment at point: one point every every_s, the last at t_end
multimodal_stationary <- function(role, point, t_start, t_end, every_s, info = list()) {
  t <- unique(c(seq(t_start, t_end, by = every_s), t_end))
  list(role = role, type = "stationary", t_start = t_start, t_end = t_end,
       path = data.frame(lon = point[1], lat = point[2], t = t), info = info)
}

# Bus segment: boarding point (tap time), the samples in between, the clicked
# alighting point (alighting time); OSRM /match on the bus profile, /route
# when matching fails, a straight line as a last resort
multimodal_bus <- function(s, b, alighting_point, t_alight, th, route_fn, match_fn) {
  inner <- s[s$t > b$t & s$t < t_alight, c("lon", "lat", "t", "acc")]
  pts <- rbind(data.frame(lon = b$lon, lat = b$lat, t = b$t, acc = 10), inner,
               data.frame(lon = alighting_point[1], lat = alighting_point[2], t = t_alight, acc = 10))
  m <- tryCatch(match_fn(cbind(pts$lon, pts$lat), pts$t, match_radiuses(pts$acc, th), profile = "bus"),
                error = function(e) NULL)
  fx <- if (isTRUE(m$ok)) tryCatch(road_timed_path(pts, m, "bus", th, route_fn), error = function(e) NULL)
  if (is.null(fx)) fx <- route_timed_path(pts, 1L, nrow(pts), "bus", th, route_fn)
  if (is.null(fx)) {
    coords <- cbind(lon = pts$lon[c(1, nrow(pts))], lat = pts$lat[c(1, nrow(pts))])
    fx <- list(path = timed_path_from_anchors(coords, c(0, path_cumulative_m(coords)[2]), c(b$t, t_alight)),
               method = "straight")
  }
  p <- multimodal_path(fx$path, b$t, t_alight)
  list(role = "bus", type = "bus", t_start = b$t, t_end = t_alight, path = p,
       info = list(method = fx$method, distance_m = path_cumulative_m(cbind(p$lon, p$lat))[nrow(p)],
                   samples = nrow(inner)))
}

# lon, lat, t with strictly increasing times, starting at t_start and ending at t_end exactly
multimodal_path <- function(path, t_start, t_end) {
  p <- path[path$t >= t_start & path$t <= t_end, c("lon", "lat", "t")]
  p$t[1] <- t_start
  p$t[nrow(p)] <- t_end
  keep <- c(TRUE, diff(p$t) > 0)
  keep[nrow(p)] <- TRUE
  p <- p[keep, ]
  if (nrow(p) > 2 && p$t[nrow(p)] <= p$t[nrow(p) - 1L]) p <- p[-(nrow(p) - 1L), ]
  rownames(p) <- NULL
  p
}


# ---- Export and state ----

# build_gpx() inputs for accepted fixes (named list by trip id; each with
# activity and path = data.frame(lon, lat, t))
review_gpx_input <- function(accepted) {
  items <- list()
  samples <- list()
  for (id in names(accepted)) {
    a <- accepted[[id]]
    p <- a$path
    item_id <- toupper(uuid::UUIDgenerate(use.time = TRUE))
    tz <- tz_from_coords(p$lat[1], p$lon[1])
    t0 <- as.POSIXct(p$t[1], origin = "1970-01-01", tz = "UTC")
    sec_gmt <- seconds_from_gmt(t0, if (is.na(tz)) "UTC" else tz)
    items[[length(items) + 1L]] <- list(
      .internalId = item_id,
      base = list(id = item_id, isVisit = FALSE),
      trip = list(confirmedActivityType = REVIEW_ACTIVITIES[[a$activity]]$code),
      description = REVIEW_ACTIVITIES[[a$activity]]$label
    )
    dates <- format(as.POSIXct(round(p$t), origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    samples <- c(samples, lapply(seq_len(nrow(p)), function(i) list(
      id = paste0(item_id, "-", i), date = dates[i], latitude = p$lat[i], longitude = p$lon[i],
      altitude = 0, secondsFromGMT = sec_gmt, timelineItemId = item_id
    )))
  }
  list(items = items, samples = samples)
}

# arc_batch_<types>_<first>_<last>.gpx (local dates of the first/last trip)
review_batch_filename <- function(accepted) {
  types <- sort(unique(vapply(accepted, `[[`, character(1), "activity")))
  starts <- vapply(accepted, function(a) a$path$t[1], numeric(1))
  day <- function(t) format(as.POSIXct(t, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d", tz = Sys.timezone())
  paste0("arc_batch_", paste(types, collapse = "-"), "_", day(min(starts)), "_", day(max(starts)), ".gpx")
}

review_state_file <- function() file.path(REVIEW_SCRATCH_DIR, "review_state.rds")

save_review_state <- function(state, file = review_state_file()) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  state$saved_at <- Sys.time()
  saveRDS(state, file)
  invisible(file)
}

load_review_state <- function(file = review_state_file()) {
  if (!file.exists(file)) return(NULL)
  tryCatch(readRDS(file), error = function(e) NULL)
}
