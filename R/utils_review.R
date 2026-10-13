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
  rail_gate_to_train_s = 60,   # RAIL_GATE_TO_TRAIN_S: Jaé gate tap -> train, when the tap is after the computed start
  rail_gate_max_late_s = 180,  # a gate tap later than this after the computed start is ignored (another trip)
  rail_tap_lead_s = 900,       # a Jaé rail tap up to 15 min before the trip's start is its gate tap
  rail_tap_station_m = 300,    # the tap's station (the trip's start, departing at tap + rail_gate_to_train_s) when this close
  # Route along the OSM rails when Google has no transit leg (rail_osm_route,
  # rail_osm_fix): each station joins the nearest track within snap_m; tracks
  # are densified every step_m, taken within max(pad_m, half the stations'
  # distance) of them; a line change costs transfer_cost_m and needs a station
  # of the system within transfer_m of both lines; a track of no line costs
  # untagged_factor x its length; the arrival is the last sample within
  # arrival_m of the end station; at most max_speed_kmh on average
  rail_osm = list(snap_m = 300, step_m = 25, pad_m = 3000, transfer_m = 250, transfer_cost_m = 500, untagged_factor = 2,
                  arrival_m = 300, max_speed_kmh = 80),
  transfer_dwell_point_s = 120,  # waiting at a transfer: one point every 2 min
  # Coverage of the original item on export (review_cover): a gap under
  # stretch_s stretches the main track; over notice_s it shows in the panel;
  # "auto" fills with a wait when the recorded samples in the gap stay within
  # wait_m of the main track's end, else with them as a walk; a wait gets a
  # point every wait_every_s
  cover = list(stretch_s = 5, notice_s = 60, wait_m = 300, wait_every_s = 60),
  station_radius_m  = 600,  # stations considered for the endpoints
  good_accuracy_m   = 50,   # origin/destination samples
  min_pair_s        = 5,    # max speed ignores sample pairs closer in time
  match_radius_m    = c(5, 50),  # OSRM /match radiuses: horizontalAccuracy clamped
  point_every_m     = 100,  # clean path: one point per 100 m or 10 s,
  point_every_s     = 10,   #   whichever gives more points
  # Control pins (road trips): a pin within near_m of two passes of the
  # recording pass_gap_m apart is ambiguous; pins snap to roads within
  # radius_m (with the recording's bearing); samples become time anchors on
  # the pinned route within max_off_m, searching ahead at most
  # max(min_reach_m, max_speed_mps x the time since the last anchor)
  # Pins as local splices (splice_route_fix): a pin's splice runs from where
  # the route into it (from a far point behind: the previous pin, the route
  # start or far_m back) last leaves the base path to where the route out of
  # it (to the next pin, the route end or far_m ahead) first joins the base
  # for good: within merge_tol_m of it, heading the same way, for
  # merge_len_m, looked for from merge_from_m past the pin (a pin on the
  # base's own road gets a short local splice). merge_tol_m is tight: both
  # paths come from OSRM on the same OSM ways, and parallel carriageways can
  # run 7 m apart (Avenida Brasil, pista central / lateral). No join before
  # the far point: the splice runs to it (to the next pin: one splice through
  # both). A splice over warn_ratio x the base stretch it replaces that turns
  # back on itself (a U-turn or an out-and-back: two points over back_gap_m
  # apart along it, within back_near_m, heading over back_deg apart) gets a
  # warning. window_m: the drag preview's reach, and the splice's when OSRM
  # fails. Anchors snap within anchor_radius_m; a recorded leg is simplified
  # to recorded_tol_m; a leg ending farther than pin_snap_m from its pin (the
  # recorded bearing chose another road) is asked again without the bearing
  # there, kept when it ends under a third as far
  pins = list(near_m = 100, pass_gap_m = 300, radius_m = 50, max_off_m = 100, max_speed_mps = 50, min_reach_m = 300,
              splice = list(far_m = 5000, merge_tol_m = 5, merge_len_m = 150, merge_from_m = 30, warn_ratio = 1.5,
                            back_gap_m = 20, back_near_m = 25, back_deg = 150, window_m = 300, anchor_radius_m = 30,
                            recorded_tol_m = 10, pin_snap_m = 5)),
  # Road matching (review_match): collapse_stops sends a run of at least
  # stop_min_samples slow samples (< stop_speed_mps) staying within
  # stop_radius_m of their centroid as one point there (GPS jitter while
  # stopped, e.g. at a traffic light); remove_spurs cuts out-and-back spurs of
  # the matched route up to spur_max_m long; tidy: OSRM /match tidy. Measured
  # on the September 2026 car inventory: remove_spurs lowered the manual work
  # with no regression (on); collapse_stops lowered it more but flagged more
  # trips and worsened the conformity (off); tidy=false gave longer detours
  match_fixes = list(collapse_stops = FALSE, stop_speed_mps = 2, stop_radius_m = 30, stop_min_samples = 3,
                     remove_spurs = TRUE, spur_max_m = 150, tidy = TRUE)
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

# Bump a family's version when its cached metrics change meaning (road 3:
# official.lua allows wrong-way driving; road 4: busways open to buses in
# official.lua, overlapping /match chunks, short splits joined; road 5: data
# rebuilt 12/10 with changesets 190412331 (TransBrasil), 190417182 and
# 190417765, official queried with exclude=wwmajor)
REVIEW_CACHE_VERSION <- c(rail = 1L, road = 5L)


# Day stepping of a review period (from, to: dates; dir: -1 back, 1 forward):
# a one-day range moves one day, a longer one by its own length. NULL when
# the range or the direction is invalid.
review_step_range <- function(from, to, dir) {
  dir <- sign(suppressWarnings(as.numeric(dir %||% 0)))
  from <- as.Date(from)
  to <- as.Date(to)
  if (length(dir) != 1 || is.na(dir) || dir == 0 || length(from) != 1 || length(to) != 1 || is.na(from) || is.na(to) ||
      from > to) return(NULL)
  len <- as.integer(to - from) + 1L
  list(from = from + dir * len, to = to + dir * len)
}


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
  routes <- if (identical(method, "osmium")) {
    step(0.85, "lines of the stations and rails")
    tryCatch(rail_routes(pbf), error = function(e) {
      message("[Rails] route relations failed: ", conditionMessage(e))
      NULL
    })
  }
  stations$lines <- rail_station_lines(stations, routes$stops)
  rails$lines <- rail_way_lines(rails$osm, routes$track)

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
        "nwr/railway=station,halt,tram_stop", "nwr/public_transport=station",
        "-o", filtered, "--overwrite"))
  run(c("export", filtered, "-f", "geojsonseq", "-a", "type,id", "-o", seq_file, "--overwrite"))
  sf::st_read(seq_file, quiet = TRUE)
}

# Route relations (subway, train, light rail, tram, monorail) (osmium): stops,
# their stop and platform members as points (a way at the mean of its nodes)
# with the route's type, network, operator and ref; track, the ways the route
# runs on (its other way members) with its line id (rail_line_id()).
# list(stops, track = data.frame(way = "w<id>", line))
rail_routes <- function(pbf) {
  tmp_dir <- tempfile("routes_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  routes <- file.path(tmp_dir, "routes.osm.pbf")
  opl <- file.path(tmp_dir, "routes.opl")
  run <- function(args) {
    out <- suppressWarnings(system2("osmium", shQuote(args), stdout = TRUE, stderr = TRUE))
    if (!is.null(attr(out, "status")) && attr(out, "status") != 0) stop(paste(out, collapse = "\n"))
  }
  run(c("tags-filter", normalizePath(pbf), "r/route=subway,train,light_rail,tram,monorail", "-o", routes, "--overwrite"))
  run(c("add-locations-to-ways", routes, "-f", "opl", "-o", opl, "--overwrite"))
  lines <- readLines(opl, encoding = "UTF-8", warn = FALSE)
  unescape <- function(x) {
    m <- gregexpr("%[0-9a-fA-F]+%", x)
    regmatches(x, m) <- lapply(regmatches(x, m), function(h) {
      vapply(h, function(code) intToUtf8(strtoi(gsub("%", "", code, fixed = TRUE), 16L)), character(1))
    })
    x
  }
  field <- function(l, f) {
    v <- regmatches(l, regexpr(paste0(" ", f, "[^ ]*"), l))
    out <- rep("", length(l))
    out[regexpr(paste0(" ", f, "[^ ]*"), l) > 0] <- substring(v, 3)
    out
  }
  kind <- substr(lines, 1, 1)
  ids <- sub("^[nwr]([0-9]+) .*$", "\\1", lines)
  nl <- lines[kind == "n"]
  nodes <- data.frame(lon = suppressWarnings(as.numeric(field(nl, "x"))), lat = suppressWarnings(as.numeric(field(nl, "y"))))
  rownames(nodes) <- paste0("n", ids[kind == "n"])
  wl <- lines[kind == "w"]
  way_xy <- t(vapply(field(wl, "N"), function(nn) {
    x <- suppressWarnings(as.numeric(regmatches(nn, gregexpr("(?<=x)[-0-9.]+", nn, perl = TRUE))[[1]]))
    y <- suppressWarnings(as.numeric(regmatches(nn, gregexpr("(?<=y)[-0-9.]+", nn, perl = TRUE))[[1]]))
    c(mean(x, na.rm = TRUE), mean(y, na.rm = TRUE))
  }, numeric(2), USE.NAMES = FALSE))
  ways <- data.frame(lon = way_xy[, 1], lat = way_xy[, 2])
  rownames(ways) <- paste0("w", ids[kind == "w"])
  rl <- lines[kind == "r"]
  out <- lapply(seq_along(rl), function(i) {
    tags <- strsplit(field(rl[i], "T"), ",", fixed = TRUE)[[1]]
    kv <- stats::setNames(unescape(sub("^[^=]*=", "", tags)), unescape(sub("=.*$", "", tags)))
    mem <- strsplit(field(rl[i], "M"), ",", fixed = TRUE)[[1]]
    role <- unescape(sub("^[^@]*@?", "", mem))
    ref <- sub("@.*$", "", mem)
    keep <- grepl("^(stop|platform)", role)
    get <- function(k) if (k %in% names(kv)) kv[[k]] else ""
    track <- unique(ref[!keep & startsWith(ref, "w")])
    track <- if (length(track)) {
      data.frame(way = track, line = rail_line_id(get("route"), get("network"), get("operator"), get("ref")))
    }
    xy <- rbind(nodes[intersect(ref[keep], rownames(nodes)), , drop = FALSE], ways[intersect(ref[keep], rownames(ways)), , drop = FALSE])
    xy <- xy[!is.na(xy$lon) & !is.na(xy$lat), , drop = FALSE]
    stops <- if (nrow(xy)) {
      data.frame(route = get("route"), network = get("network"), operator = get("operator"), ref = get("ref"),
                 lon = xy$lon, lat = xy$lat)
    }
    list(stops = stops, track = track)
  })
  list(stops = do.call(rbind, lapply(out, `[[`, "stops")), track = do.call(rbind, lapply(out, `[[`, "track")))
}

# Line id of a route: "route|network|ref" (the operator when no network)
rail_line_id <- function(route, network, operator, ref) {
  net <- ifelse(!is.na(network) & nzchar(network), network, operator)
  paste(route, net, ref, sep = "|")
}

# The lines running on each rail way (osm: "w<id>"; track: rail_routes()$track),
# as "line;line"; "" for none
rail_way_lines <- function(osm, track) {
  out <- rep("", length(osm))
  if (is.null(track) || nrow(track) == 0) return(out)
  by_way <- vapply(split(track$line, track$way), function(l) paste(unique(l), collapse = ";"), character(1))
  hit <- !is.na(osm) & osm %in% names(by_way)
  out[hit] <- by_way[osm[hit]]
  out
}

# The lines of each station: the routes with a stop or platform within
# radius_m of it, of a compatible type, as "route|network|ref;..."
rail_station_lines <- function(stations, stops, radius_m = 300) {
  out <- rep("", nrow(stations))
  if (is.null(stops) || nrow(stops) == 0) return(out)
  fits <- list(subway = "subway", train = "train", light_rail = c("light_rail", "tram"), tram = c("tram", "light_rail"),
               monorail = STATION_MODES)
  sxy <- cbind(stops$lon, stops$lat)
  for (i in seq_len(nrow(stations))) {
    m <- strsplit(stations$modes[i] %||% "", ",", fixed = TRUE)[[1]]
    if (!length(m) || is.na(stations$lon[i])) next
    d <- geosphere::distHaversine(sxy, c(stations$lon[i], stations$lat[i]))
    near <- which(d <= radius_m)
    near <- near[vapply(near, function(k) any(m %in% fits[[stops$route[k]]]), logical(1))]
    if (!length(near)) next
    out[i] <- paste(unique(rail_line_id(stops$route[near], stops$network[near], stops$operator[near], stops$ref[near])),
                    collapse = ";")
  }
  out
}

# GDAL OSM driver with SQL filters (other_tags holds the tags without a column)
rail_features_gdal <- function(pbf) {
  tag <- function(other, key) {
    m <- regmatches(other, regexec(sprintf('"%s"=>"([^"]*)"', key), other))
    vapply(m, function(x) if (length(x) == 2) x[2] else NA_character_, character(1))
  }
  with_tags <- function(x) {
    other <- if ("other_tags" %in% names(x)) x$other_tags else rep(NA_character_, nrow(x))
    for (key in c("railway", "service", "public_transport", "station", "operator", "network", STATION_MODES)) {
      if (!key %in% names(x)) x[[key]] <- tag(other, key)
    }
    x
  }
  station_where <- paste(sprintf("other_tags LIKE '%%%s%%'",
                                 c('"railway"=>"station"', '"railway"=>"halt"', '"railway"=>"tram_stop"', '"public_transport"=>"station"')),
                         collapse = " OR ")

  lines <- sf::st_read(pbf, query = sprintf(
    "SELECT osm_id, name, railway, other_tags FROM lines WHERE railway IN (%s)",
    paste0("'", RAIL_LINE_TYPES, "'", collapse = ",")), quiet = TRUE)
  points <- sf::st_read(pbf, query = paste("SELECT osm_id, name, other_tags FROM points WHERE", station_where), quiet = TRUE)
  areas <- sf::st_read(pbf, query = paste("SELECT osm_id, osm_way_id, name, other_tags FROM multipolygons WHERE",
                                          station_where), quiet = TRUE)
  cols <- c("name", "railway", "service", "public_transport", "station", "operator", "network", STATION_MODES)
  parts <- lapply(list(lines, points, areas), function(x) {
    x <- with_tags(x)
    sf::st_sf(x[, cols, drop = TRUE], geometry = sf::st_geometry(x))
  })
  do.call(rbind, parts)
}

# "n123", "w456", "r789" from osmium's "@type" / "@id" (sf reads them as
# X.type / X.id); NA without them
osm_ids <- function(x) {
  pick <- function(names) {
    hit <- intersect(names, names(x))
    if (length(hit)) as.character(x[[hit[1]]]) else rep(NA_character_, nrow(x))
  }
  type <- pick(c("@type", "X.type", "X_type"))
  id <- pick(c("@id", "X.id", "X_id"))
  ifelse(is.na(type) | is.na(id), NA_character_, paste0(substr(type, 1, 1), id))
}

rail_lines_from_features <- function(x) {
  col <- function(name) if (name %in% names(x)) x[[name]] else rep(NA_character_, nrow(x))
  gt <- as.character(sf::st_geometry_type(x))
  keep <- gt %in% c("LINESTRING", "MULTILINESTRING") & col("railway") %in% RAIL_LINE_TYPES &
    !(col("service") %in% RAIL_EXCLUDED_SERVICE)
  osm <- osm_ids(x)
  rails <- sf::st_sf(railway = col("railway")[keep], name = col("name")[keep], osm = osm[keep],
                     geometry = sf::st_geometry(x)[keep])
  suppressWarnings(sf::st_cast(rails, "LINESTRING"))
}

stations_from_features <- function(x) {
  col <- function(name) if (name %in% names(x)) x[[name]] else rep(NA_character_, nrow(x))
  gt <- as.character(sf::st_geometry_type(x))
  # Tram stops too (the VLT's and the Santa Teresa tram's stops are mostly only that)
  is_station <- col("railway") %in% c("station", "halt", "tram_stop") | col("public_transport") %in% "station"
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
    if (identical(col("railway")[i], "tram_stop")) m <- c(m, "tram")
    if (st %in% "train" || (length(m) == 0 && col("railway")[i] %in% c("station", "halt"))) m <- c(m, "train")
    paste(unique(m), collapse = ",")
  }, character(1))

  xy <- sf::st_coordinates(geom)
  osm <- osm_ids(x)[keep]
  sf::st_sf(name = col("name")[keep], modes = modes, lon = xy[, 1], lat = xy[, 2], osm = osm,
            operator = col("operator")[keep], network = col("network")[keep], geometry = geom)
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
station_candidates <- function(activity, lon, lat, k = 3, radius_m = Inf, ref = rail_reference(), all_systems = FALSE) {
  st <- rail_stations(ref)
  if (!all_systems && nrow(st)) {
    modes <- REVIEW_ACTIVITIES[[activity]]$station_modes
    st <- st[vapply(strsplit(st$modes, ",", fixed = TRUE), function(m) any(m %in% modes), logical(1)), ]
  }
  st$dist_m <- if (nrow(st)) geosphere::distHaversine(cbind(st$lon, st$lat), c(lon, lat)) else numeric(0)
  st <- st[order(st$dist_m), ]
  if (!all_systems) st <- st[!duplicated(st$name), ]
  st <- st[st$dist_m <= radius_m, ]
  rownames(st) <- NULL
  utils::head(st, k)
}

# Short names of rail systems (a station's network or operator, or its lines')
RAIL_SYSTEM_SHORT <- c("Metrô Rio" = "Metrô", "MetrôRio" = "Metrô", "Metro Rio" = "Metrô", "VLT Carioca" = "VLT")

# Every rail station (metro, train, light rail / tram) with a name, for the
# station selects and the map pick: key (its OSM id; s<row> in a reference
# without ids), name, modes, system (short: its lines' network, else its own
# network or operator; "" when none), metro lines ("L1/L2"), lon, lat. A
# station mapped twice (node and area) is kept once. Cached per reference.
rail_stations <- function(ref = rail_reference()) {
  empty <- data.frame(key = character(0), name = character(0), modes = character(0), system = character(0),
                      metro_lines = character(0), lon = numeric(0), lat = numeric(0))
  if (is.null(ref) || is.null(ref$stations)) return(empty)
  cache_key <- paste(ref$mtime %||% "", nrow(ref$stations))
  if (identical(.rail_ref$stations_key, cache_key)) return(.rail_ref$stations_table)
  st <- ref$stations
  col <- function(name) if (name %in% names(st)) as.character(st[[name]]) else rep(NA_character_, nrow(st))
  ok <- !is.na(st$name) & nzchar(st$name) & !is.na(st$modes) & nzchar(st$modes)
  osm <- col("osm")
  key <- ifelse(is.na(osm), paste0("s", seq_len(nrow(st))), osm)
  lines <- col("lines")
  lines[is.na(lines)] <- ""
  parsed <- lapply(strsplit(lines, ";", fixed = TRUE), function(l) {
    p <- strsplit(l, "|", fixed = TRUE)
    data.frame(route = vapply(p, function(x) x[1] %||% "", ""), net = vapply(p, function(x) if (length(x) >= 2) x[2] else "", ""),
               ref = vapply(p, function(x) if (length(x) >= 3) x[3] else "", ""))
  })
  first_nz <- function(...) {
    v <- c(...)
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v)) v[1] else ""
  }
  network <- col("network")
  operator <- col("operator")
  system <- vapply(seq_len(nrow(st)), function(i) {
    nets <- parsed[[i]]$net[nzchar(parsed[[i]]$net)]
    s <- first_nz(if (length(nets)) names(sort(table(nets), decreasing = TRUE))[1], network[i], operator[i])
    if (s %in% names(RAIL_SYSTEM_SHORT)) RAIL_SYSTEM_SHORT[[s]] else s
  }, character(1))
  metro <- vapply(parsed, function(p) {
    refs <- unique(p$ref[p$route == "subway" & nzchar(p$ref)])
    if (!length(refs)) return("")
    num <- suppressWarnings(as.numeric(refs))
    paste0("L", refs[order(is.na(num), num, refs)], collapse = "/")
  }, character(1))
  out <- data.frame(key = key, name = st$name, modes = st$modes, system = system, metro_lines = metro,
                    lon = st$lon, lat = st$lat, stringsAsFactors = FALSE)[ok, ]
  # the same station twice (node and area, same name and system, within 300 m): the node
  out <- out[order(!startsWith(out$key, "n")), ]
  dup <- rep(FALSE, nrow(out))
  grp <- paste(out$name, out$system, out$metro_lines)
  for (g in unique(grp[duplicated(grp)])) {
    k <- which(grp == g)
    for (j in k[-1]) {
      prev <- k[k < j & !dup[k]]
      if (any(geosphere::distHaversine(cbind(out$lon[prev], out$lat[prev]), c(out$lon[j], out$lat[j])) < 300)) dup[j] <- TRUE
    }
  }
  out <- out[!dup, ]
  rownames(out) <- NULL
  .rail_ref$stations_key <- cache_key
  .rail_ref$stations_table <- out
  out
}

# "Maracanã — Metrô L2", "Maracanã — SuperVia": a station (row of
# rail_stations()) with its system and metro lines; without a system, its mode
rail_station_label <- function(st, lang = APP_LANGUAGE) {
  if (is.null(st) || nrow(st) == 0) return(character(0))
  mode_word <- c(subway = "Metro", train = "Train", light_rail = "Tram", tram = "Tram")
  sys <- vapply(seq_len(nrow(st)), function(i) {
    s <- st$system[i] %||% ""
    if (is.na(s) || !nzchar(s)) {
      m <- intersect(names(mode_word), strsplit(st$modes[i] %||% "", ",", fixed = TRUE)[[1]])
      s <- if (length(m)) tr_lang(mode_word[[m[1]]], lang) else ""
    }
    trimws(paste(s, st$metro_lines[i] %||% ""))
  }, character(1))
  ifelse(nzchar(sys), paste0(st$name, " \u2014 ", sys), st$name)
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
  ax <- coords[-n, 1] * kx
  ay <- coords[-n, 2] * ky
  dx <- coords[-1, 1] * kx - ax
  dy <- coords[-1, 2] * ky - ay
  len2 <- dx^2 + dy^2
  out <- numeric(nrow(pts))
  start <- 1L
  for (k in seq_len(nrow(pts))) {
    segs <- start:(n - 1L)
    px <- pts[k, 1] * kx
    py <- pts[k, 2] * ky
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

# ---- Road matching with the jitter fixes and manual exclusions ----

# The match settings that change results (part of the review cache key)
review_match_signature <- function(th = REVIEW_THRESHOLDS) {
  f <- th$match_fixes %||% list()
  paste0("stops", if (isTRUE(f$collapse_stops)) paste(f$stop_speed_mps, f$stop_radius_m, f$stop_min_samples, sep = ",") else "0",
         ";spurs", if (isTRUE(f$remove_spurs)) f$spur_max_m else "0", ";tidy", if (isFALSE(f$tidy)) "0" else "1")
}

# Runs of slow samples staying put (s: lon, lat, t by time; use: the samples
# considered): consecutive samples whose speed between them is < speed_mps,
# all within radius_m of the run's centroid, at least min_n of them. List of
# sample index vectors.
stationary_clusters <- function(s, use = rep(TRUE, nrow(s)), speed_mps = 2, radius_m = 30, min_n = 3) {
  idx <- which(use)
  if (length(idx) < min_n) return(list())
  lat0 <- mean(s$lat[idx])
  kx <- 111320 * cos(lat0 * pi / 180)
  ky <- 110540
  x <- s$lon * kx
  y <- s$lat * ky
  dt <- diff(s$t[idx])
  v <- sqrt(diff(x[idx])^2 + diff(y[idx])^2) / pmax(dt, 1e-6)
  fits <- function(m) max(sqrt((x[m] - mean(x[m]))^2 + (y[m] - mean(y[m]))^2)) <= radius_m
  out <- list()
  k <- 1L
  while (k < length(idx)) {
    if (v[k] >= speed_mps || !fits(idx[c(k, k + 1L)])) {
      k <- k + 1L
      next
    }
    j <- k + 1L
    while (j < length(idx) && v[j] < speed_mps && fits(idx[k:(j + 1L)])) j <- j + 1L
    if (j - k + 1L >= min_n) out[[length(out) + 1L]] <- idx[k:j]
    k <- j + 1L
  }
  out
}

# Out-and-back spurs of a route (n x 2 lon/lat): the route leaves a node and
# comes back to it along the same points (a palindrome around a tip of 1 or
# 2 points). Each spur is measured whole (from its tip outwards) and cut out,
# the node kept, when it goes at most max_m out.
remove_route_spurs <- function(coords, max_m = 150) {
  repeat {
    n <- nrow(coords)
    if (n < 3) return(coords)
    key <- paste(round(coords[, 1], 6), round(coords[, 2], 6))
    cum <- path_cumulative_m(coords)
    cut <- NULL
    for (t in 2:(n - 1L)) {
      for (w in 1:2) {                       # tip of 1 point (t) or 2 points (t, t + 1)
        i <- t - 1L
        j <- t + w
        if (j > n || key[i] != key[j]) next
        while (i > 1L && j < n && key[i - 1L] == key[j + 1L]) {
          i <- i - 1L
          j <- j + 1L
        }
        if ((cum[j] - cum[i]) / 2 <= max_m && n - (j - i) >= 2L) {
          cut <- (i + 1L):j
          break
        }
      }
      if (!is.null(cut)) break
    }
    if (is.null(cut)) return(coords)
    coords <- coords[-cut, , drop = FALSE]
  }
}

# OSRM /match for a road trip (s: review_clean_samples()) with
# th$match_fixes and manual exclusions (exclude: sample times, epoch s, kept
# out of the match input). The result is osrm_match()'s, on all the samples:
# a collapsed stop is sent as one point at its centroid and every sample of
# it takes that point's tracepoint (a dwell); an excluded sample inside a
# matched piece gets a tracepoint on that piece between its neighbours' (the
# time spent there stays real); spurs are cut from the matched pieces.
# Extra fields: held_out (not sent: excluded or collapsed), excluded, stops
# (list of collapsed index vectors), spurs_removed (points cut).
review_match <- function(s, profile, th = REVIEW_THRESHOLDS, exclude = NULL, match_fn = osrm_match) {
  f <- th$match_fixes %||% list()
  n <- nrow(s)
  excluded <- if (length(exclude)) round(s$t) %in% round(as.numeric(exclude)) else rep(FALSE, n)
  send <- !excluded
  lon <- s$lon
  lat <- s$lat
  stops <- if (isTRUE(f$collapse_stops)) {
    stationary_clusters(s, send, f$stop_speed_mps %||% 2, f$stop_radius_m %||% 30, f$stop_min_samples %||% 3)
  } else {
    list()
  }
  reps <- integer(0)
  for (m in stops) {
    r <- m[ceiling(length(m) / 2)]
    lon[r] <- mean(s$lon[m])
    lat[r] <- mean(s$lat[m])
    send[setdiff(m, r)] <- FALSE
    reps <- c(reps, r)
  }
  idx <- which(send)
  m <- if (identical(match_fn, osrm_match)) {
    match_fn(cbind(lon[idx], lat[idx]), s$t[idx], match_radiuses(s$acc[idx], th), profile = profile, tidy = !isFALSE(f$tidy))
  } else {
    match_fn(cbind(lon[idx], lat[idx]), s$t[idx], match_radiuses(s$acc[idx], th), profile = profile)
  }
  out <- list(ok = isTRUE(m$ok), pieces = list(), confidence = m$confidence %||% NA_real_,
              tracepoints = matrix(NA_real_, n, 2, dimnames = list(NULL, c("lon", "lat"))),
              sent = rep(FALSE, n), n_dropped = as.integer(m$n_dropped %||% 0L), held_out = !send, excluded = excluded,
              stops = stops, spurs_removed = 0L)
  if (length(idx)) {
    out$tracepoints[idx, ] <- m$tracepoints
    out$sent[idx] <- m$sent
  }
  pieces <- lapply(m$pieces, function(p) list(coords = p$coords, idx = idx[p$idx]))
  if (isTRUE(f$remove_spurs)) {
    pieces <- lapply(pieces, function(p) {
      trimmed <- remove_route_spurs(p$coords, f$spur_max_m %||% 150)
      out$spurs_removed <<- out$spurs_removed + nrow(p$coords) - nrow(trimmed)
      p$coords <- trimmed
      p
    })
  }
  # Collapsed stops: every sample at the representative's tracepoint
  for (k in seq_along(stops)) {
    r <- reps[k]
    pk <- which(vapply(pieces, function(p) r %in% p$idx, logical(1)))
    if (length(pk) == 0) next
    out$tracepoints[stops[[k]], ] <- matrix(out$tracepoints[r, ], length(stops[[k]]), 2, byrow = TRUE)
    pieces[[pk]]$idx <- sort(unique(c(pieces[[pk]]$idx, stops[[k]])))
  }
  # Excluded samples inside a piece: on the piece, between their neighbours
  for (e in which(excluded)) {
    pk <- which(vapply(pieces, function(p) length(p$idx) && min(p$idx) < e && e < max(p$idx), logical(1)))
    if (length(pk) == 0) next
    p <- pieces[[pk[1]]]
    cum <- path_cumulative_m(p$coords)
    a <- max(p$idx[p$idx < e])
    b <- min(p$idx[p$idx > e])
    dab <- project_along(p$coords, cum, out$tracepoints[c(a, b), , drop = FALSE])
    sp <- polyline_segment_projection(p$coords, cum, c(s$lon[e], s$lat[e]))
    win <- which(cum[-1] >= dab[1] & cum[-length(cum)] <= dab[2])
    d <- if (length(win)) sp$d[win][which.min(sp$off[win])] else dab[1]
    d <- min(max(d, dab[1]), dab[2])
    out$tracepoints[e, ] <- path_points_at(p$coords, cum, d)
    pieces[[pk[1]]]$idx <- sort(c(p$idx, e))
  }
  out$pieces <- pieces
  out
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
    if (is.null(match)) match <- review_match(s, act$osrm, th)
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
        if (fam == "rail") rails_mtime else REVIEW_ACTIVITIES[[trip$activity]]$osrm, REVIEW_CACHE_VERSION[[fam]],
        if (fam == "road") review_match_signature(th), sep = "|")
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
# strict: only a station in the right direction (or under 30 m), else NULL
# (also without a direction);
# exclude: station keys left out.
pick_station <- function(candidates, lon, lat, bearing, side = c("origin", "destination"),
                         radius_m = REVIEW_THRESHOLDS$station_radius_m, strict = FALSE, exclude = NULL) {
  side <- match.arg(side)
  if (strict && is.na(bearing)) return(NULL)
  keep <- candidates$dist_m <= radius_m
  if (length(exclude)) keep <- keep & !candidates$key %in% exclude
  cand <- candidates[keep, ]
  if (nrow(cand) == 0) return(NULL)
  if (!is.na(bearing)) {
    b <- if (side == "origin") {
      geosphere::bearing(cbind(cand$lon, cand$lat), c(lon, lat))       # station -> sample
    } else {
      geosphere::bearing(c(lon, lat), cbind(cand$lon, cand$lat))       # sample -> station
    }
    good <- !is.na(b) & angle_diff(b, bearing) < 90 | cand$dist_m < 30
    if (any(good)) cand <- cand[good, ] else if (strict) return(NULL)
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
                            segments = NULL, start_at = NULL) {
  if (!is.null(segments)) {
    geo <- rail_segments_geometry(segments)
    route <- geo$coords
  }
  cum <- path_cumulative_m(route)
  total <- cum[length(cum)]
  d <- project_along(route, cum, rbind(c(origin$lon, origin$lat), c(dest$lon, dest$lat)))
  speed <- th$rail_speed_kmh / 3.6
  adj_start <- min(d[1] / speed, th$max_time_adjust_s)
  # start_at (a Jaé gate tap + rail_gate_to_train_s): the backward extrapolation
  # from the first sample is shortened (or reversed) to start there
  if (!is.null(start_at)) adj_start <- origin$t - start_at
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
# rows, from "Set as start/end"), dep_name/arr_name (station keys from the
# selects; "" = keep the sample point). tap_station (rail_tap_station()): the
# start unless one was chosen. The automatic picks never give the same station
# at both ends: the farther one from its sample looks again, up to twice
# station_radius_m and only in the direction of travel, else keeps the
# sample. Each station gets source "jae", "chosen" or "auto". Returns the
# plan, including the points and reference time for
# calculate_google_transit_route().
rail_plan <- function(s, activity, origin_idx = NULL, dest_idx = NULL, dep_name = NULL, arr_name = NULL,
                      th = REVIEW_THRESHOLDS, ref = rail_reference(), tap_station = NULL) {
  ends <- review_endpoints(s, th)
  oi <- origin_idx %||% ends[1]
  di <- dest_idx %||% ends[2]
  if (di <= oi) di <- nrow(s)

  # A chosen station (its key, any system; an older choice by name: the
  # trip's system) or the automatic pick; "" keeps the recorded point
  choose <- function(i, name, bearing, side) {
    if (!is.null(name)) {
      if (!nzchar(name)) return(NULL)
      all <- rail_stations(ref)
      hit <- all[all$key == name, ]
      if (nrow(hit) == 0) {
        hit <- station_candidates(activity, s$lon[i], s$lat[i], k = 50, ref = ref)
        hit <- hit[hit$name == name, ]
      }
      if (nrow(hit)) {
        hit$dist_m <- geosphere::distHaversine(cbind(hit$lon, hit$lat), c(s$lon[i], s$lat[i]))
        rownames(hit) <- NULL
        return(hit[1, ])
      }
    }
    near <- station_candidates(activity, s$lon[i], s$lat[i], k = 20, radius_m = th$station_radius_m, ref = ref)
    pick_station(near, s$lon[i], s$lat[i], bearing, side, th$station_radius_m)
  }
  again <- function(i, bearing, side, exclude) {
    near <- station_candidates(activity, s$lon[i], s$lat[i], k = 20, radius_m = 2 * th$station_radius_m, ref = ref)
    pick_station(near, s$lon[i], s$lat[i], bearing, side, 2 * th$station_radius_m, strict = TRUE, exclude = exclude)
  }
  b_dep <- travel_bearing(s, oi, "forward")
  b_arr <- travel_bearing(s, di, "backward")
  jae <- is.null(dep_name) && !is.null(tap_station) && nrow(tap_station) == 1
  dep <- if (jae) {
    st <- tap_station
    st$dist_m <- geosphere::distHaversine(c(st$lon, st$lat), c(s$lon[oi], s$lat[oi]))
    st
  } else {
    choose(oi, dep_name, b_dep, "origin")
  }
  arr <- choose(di, arr_name, b_arr, "destination")
  if (!is.null(dep) && !is.null(arr) && identical(dep$key, arr$key)) {
    auto_dep <- is.null(dep_name) && !jae
    auto_arr <- is.null(arr_name)
    if (auto_dep && (!auto_arr || dep$dist_m >= arr$dist_m)) {
      dep <- again(oi, b_dep, "origin", arr$key)
    } else if (auto_arr) {
      arr <- again(di, b_arr, "destination", dep$key)
    }
  }
  if (!is.null(dep)) dep$source <- if (jae) "jae" else if (is.null(dep_name)) "auto" else "chosen"
  if (!is.null(arr)) arr$source <- if (is.null(arr_name)) "auto" else "chosen"

  # Select options: the 5 nearest stations of any rail system, plus the pick when it isn't one of them
  options_for <- function(i, picked) {
    cands <- station_candidates(activity, s$lon[i], s$lat[i], k = 5, ref = ref, all_systems = TRUE)
    if (!is.null(picked) && !picked$key %in% cands$key) cands <- rbind(picked[, names(cands)], cands)
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

# ---- Jaé gate tap, route along the OSM rails, the rail proposal ----

# The Jaé gate tap of a rail trip: a tap of its mode from th$rail_tap_lead_s
# before the start to the end, after `after` (epoch s: the end of the previous
# rail trip, whose own tap comes before it); the last one by the start +
# rail_gate_max_late_s, else the first. Zero or one row.
rail_trip_tap <- function(boardings, activity, start_utc, end_utc, after = -Inf, th = REVIEW_THRESHOLDS) {
  taps <- multimodal_taps(boardings, activity, as.numeric(start_utc) - th$rail_tap_lead_s, end_utc)
  t <- as.numeric(taps$datetime_utc)
  taps <- taps[t > after, , drop = FALSE]
  t <- t[t > after]
  if (nrow(taps) == 0) return(taps)
  early <- which(t <= as.numeric(start_utc) + th$rail_gate_max_late_s)
  taps[if (length(early)) max(early) else 1L, , drop = FALSE]
}

# The station of a Jaé rail tap: the nearest station of the activity's modes
# within th$rail_tap_station_m of it (a row of station_candidates(), with
# source "jae", tap_t and tap_dist_m), or NULL (no such station, no tap
# coordinates)
rail_tap_station <- function(tap, activity, th = REVIEW_THRESHOLDS, ref = rail_reference()) {
  if (is.null(tap) || nrow(tap) == 0 || is.na(tap$longitude[1]) || is.na(tap$latitude[1])) return(NULL)
  near <- station_candidates(activity, tap$longitude[1], tap$latitude[1], k = 1, radius_m = th$rail_tap_station_m, ref = ref)
  if (nrow(near) == 0) return(NULL)
  near$tap_dist_m <- near$dist_m
  near$source <- "jae"
  near$tap_t <- as.numeric(tap$datetime_utc[1])
  near
}

# System (short name, as in rail_stations()) and label ("Metrô L2",
# "SuperVia Deodoro") of line ids
rail_line_system <- function(ids) {
  net <- vapply(strsplit(ids, "|", fixed = TRUE), function(p) if (length(p) >= 2) p[2] else "", character(1))
  short <- unname(RAIL_SYSTEM_SHORT[net])
  ifelse(is.na(short), net, short)
}
rail_line_label <- function(ids) {
  p <- strsplit(ids, "|", fixed = TRUE)
  vapply(seq_along(ids), function(i) {
    ref <- if (length(p[[i]]) >= 3) p[[i]][3] else ""
    if (identical(p[[i]][1], "subway") && nzchar(ref)) ref <- paste0("L", ref)
    trimws(paste(rail_line_system(ids[i]), ref))
  }, character(1))
}

# Path along the OSM rails (the reference's rails) between two stations (rows
# of rail_stations()), for when Google has no transit leg. Stations of
# different systems get none. With the system's lines (rails$lines) the path
# rides them, changing line only where both pass within transfer_m of a
# station of the system; else (no line information, or the lines don't join
# the stations) any track of the activity's rail types or of the system's
# lines, a track of no line costing untagged_factor x. Tracks are undirected.
# Returns list(ok = TRUE, coords (lon, lat), length_m, lines (ids, in order),
# transfers (station names), mode = "lines" | "tracks") or list(ok = FALSE,
# reason = "same_station" | "systems" | "no_rails" | "not_connected", station).
rail_osm_route <- function(dep, arr, activity, ref = rail_reference(), th = REVIEW_THRESHOLDS) {
  o <- th$rail_osm
  if (identical(dep$key, arr$key)) return(list(ok = FALSE, reason = "same_station"))
  sys_dep <- dep$system %||% ""
  sys_arr <- arr$system %||% ""
  if (nzchar(sys_dep) && nzchar(sys_arr) && sys_dep != sys_arr) return(list(ok = FALSE, reason = "systems"))
  sys <- if (nzchar(sys_dep)) sys_dep else sys_arr
  if (is.null(ref) || nrow(ref$rails) == 0) return(list(ok = FALSE, reason = "no_rails", station = dep$name))

  lon <- c(dep$lon, arr$lon)
  lat <- c(dep$lat, arr$lat)
  pad <- max(o$pad_m, geosphere::distHaversine(c(lon[1], lat[1]), c(lon[2], lat[2])) / 2)
  pad_lat <- pad / 110540
  pad_lon <- pad / (111320 * cos(mean(lat) * pi / 180))
  b <- ref$bbox
  near <- b[, "xmax"] >= min(lon) - pad_lon & b[, "xmin"] <= max(lon) + pad_lon &
    b[, "ymax"] >= min(lat) - pad_lat & b[, "ymin"] <= max(lat) + pad_lat
  rails <- ref$rails[near, ]
  way_lines <- if ("lines" %in% names(rails)) {
    strsplit(ifelse(is.na(rails$lines), "", rails$lines), ";", fixed = TRUE)
  } else {
    rep(list(character(0)), nrow(rails))
  }
  sys_lines <- lapply(way_lines, function(l) l[nzchar(sys) & rail_line_system(l) == sys])
  types <- REVIEW_ACTIVITIES[[activity]]$rail_types
  usable <- lengths(sys_lines) > 0 | (rails$railway %in% types & (lengths(way_lines) == 0 | !nzchar(sys)))
  if (!any(usable)) return(list(ok = FALSE, reason = "no_rails", station = dep$name))

  # Track vertices (metric, densified), shared where the ways share an OSM node
  crs <- utm_epsg(lon, lat)
  xy <- sf::st_coordinates(sf::st_geometry(rails))
  m <- to_metric(xy[, "X"], xy[, "Y"], crs)
  pieces <- lapply(split(seq_len(nrow(m)), xy[, "L1"]), function(i) {
    d <- densify_xy(m[i, , drop = FALSE], o$step_m)
    cbind(d$x, d$y)
  })
  n_per <- vapply(pieces, nrow, integer(1))
  P <- do.call(rbind, pieces)
  way <- rep(seq_along(pieces), n_per)
  vkey <- paste(round(P[, 1], 1), round(P[, 2], 1))
  vid <- match(vkey, unique(vkey))
  V <- P[!duplicated(vkey), , drop = FALSE]
  i <- setdiff(seq_len(nrow(P)), cumsum(n_per))
  seg <- data.frame(a = vid[i], b = vid[i + 1L], len = sqrt(rowSums((P[i + 1L, , drop = FALSE] - P[i, , drop = FALSE])^2)),
                    way = way[i])
  seg <- seg[seg$a != seg$b & usable[seg$way], ]
  st_m <- to_metric(lon, lat, crs)

  # Edges from node `name` to the vertices (`states`, at vertices `verts`)
  # within radius of point p, at 3 x the distance (+ extra): the nearest track
  # wins, and each parallel track (one per direction, often not joined in
  # OSM) or line keeps its own way in
  join <- function(name, p, radius, verts, states = as.character(verts), extra = 0) {
    d <- sqrt((V[verts, 1] - p[1])^2 + (V[verts, 2] - p[2])^2)
    k <- d <= radius
    if (!any(k)) return(NULL)
    data.frame(a = name, b = states[k], w = 3 * d[k] + extra)
  }
  solve <- function(edges) {
    g <- igraph::graph_from_data_frame(edges[, c("a", "b")], directed = FALSE)
    if (!all(c("S", "T") %in% igraph::V(g)$name)) return(NULL)
    p <- suppressWarnings(igraph::shortest_paths(g, "S", "T", weights = edges$w, output = "vpath")$vpath[[1]])
    if (length(p) == 0) NULL else igraph::V(g)$name[p][-c(1, length(p))]
  }
  finish <- function(vertices, lines, transfers, mode) {
    ll <- from_metric(V[vertices, 1], V[vertices, 2], crs)
    ll <- ll[c(TRUE, rowSums(abs(diff(ll))) > 0), , drop = FALSE]
    list(ok = TRUE, coords = ll, length_m = path_cumulative_m(ll)[nrow(ll)], lines = lines, transfers = transfers, mode = mode)
  }

  # 1. The system's lines: states vertex#line, line changes at its stations
  lined <- seg[lengths(sys_lines[seg$way]) > 0, ]
  if (nrow(lined)) {
    k <- lengths(sys_lines[lined$way])
    e_line <- unlist(sys_lines[lined$way])
    e_a <- rep(lined$a, k)
    e_b <- rep(lined$b, k)
    edges <- data.frame(a = paste(e_a, e_line, sep = "#"), b = paste(e_b, e_line, sep = "#"), w = rep(lined$len, k))
    vl <- unique(data.frame(v = c(e_a, e_b), line = c(e_line, e_line)))
    states <- paste(vl$v, vl$line, sep = "#")
    src <- join("S", st_m[1, ], o$snap_m, vl$v, states)
    dst <- join("T", st_m[2, ], o$snap_m, vl$v, states)
    st <- rail_stations(ref)
    st <- st[st$system == sys & !st$key %in% c(dep$key, arr$key), ]
    hubs <- NULL
    if (nrow(st) && !is.null(src) && !is.null(dst)) {
      st_xy <- to_metric(st$lon, st$lat, crs)
      inside <- st_xy[, 1] >= min(V[, 1]) - o$transfer_m & st_xy[, 1] <= max(V[, 1]) + o$transfer_m &
        st_xy[, 2] >= min(V[, 2]) - o$transfer_m & st_xy[, 2] <= max(V[, 2]) + o$transfer_m
      hubs <- do.call(rbind, lapply(which(inside), function(j) {
        h <- join(paste0("H", j), st_xy[j, ], o$transfer_m, vl$v, states, extra = o$transfer_cost_m / 2)
        if (!is.null(h) && length(unique(sub("^[^#]*#", "", h$b))) >= 2) h
      }))
    }
    path <- if (!is.null(src) && !is.null(dst)) solve(rbind(edges, src, dst, hubs))
    if (!is.null(path)) {
      on_line <- grepl("#", path, fixed = TRUE)
      line_of <- sub("^[^#]*#", "", path)
      # a hub between two different lines is a transfer (same line: another track)
      hub_at <- which(!on_line)
      changed <- vapply(hub_at, function(h) line_of[h - 1] != line_of[h + 1], logical(1))
      transfers <- st$name[as.integer(sub("^H", "", path[hub_at[changed]]))]
      return(finish(as.integer(sub("#.*$", "", path[on_line])), rle(line_of[on_line])$values, transfers, "lines"))
    }
  }

  # 2. Any usable track (no line states)
  w <- seg$len * ifelse(lengths(sys_lines[seg$way]) > 0, 1, o$untagged_factor)
  verts <- unique(c(seg$a, seg$b))
  src <- join("S", st_m[1, ], o$snap_m, verts)
  dst <- join("T", st_m[2, ], o$snap_m, verts)
  if (is.null(src)) return(list(ok = FALSE, reason = "no_rails", station = dep$name))
  if (is.null(dst)) return(list(ok = FALSE, reason = "no_rails", station = arr$name))
  path <- solve(rbind(data.frame(a = as.character(seg$a), b = as.character(seg$b), w = w), src, dst))
  if (is.null(path)) return(list(ok = FALSE, reason = "not_connected"))
  verts <- as.integer(path)
  pair <- paste(pmin(utils::head(verts, -1), utils::tail(verts, -1)), pmax(utils::head(verts, -1), utils::tail(verts, -1)))
  used <- seg$way[match(pair, paste(pmin(seg$a, seg$b), pmax(seg$a, seg$b)))]
  finish(verts, unique(unlist(sys_lines[used[!is.na(used)]])), character(0), "tracks")
}

# Timed rail fix along a rail_osm_route() path: it departs at start_at (the
# Jaé tap rule), else at the trip's start (trip_t: the original item's
# start, end; epoch s), and arrives at the last sample within
# rail_osm$arrival_m of the end station, else at the trip's end; times are
# linear in distance (at most rail_osm$max_speed_kmh on average), plus the
# user's fine adjustments.
rail_osm_fix <- function(plan, route, s, trip_t, th = REVIEW_THRESHOLDS, start_offset_s = 0, end_offset_s = 0,
                         start_at = NULL) {
  coords <- route$coords
  cum <- path_cumulative_m(coords)
  total <- cum[length(cum)]
  t_start <- (start_at %||% trip_t[1]) + start_offset_s
  t_end <- trip_t[2]
  if (!is.null(plan$arr) && nrow(s)) {
    d <- geosphere::distHaversine(cbind(s$lon, s$lat), c(plan$arr$lon, plan$arr$lat))
    near <- which(d <= th$rail_osm$arrival_m)
    if (length(near)) t_end <- s$t[max(near)]
  }
  t_end <- t_end + end_offset_s
  min_s <- max(1, total / (th$rail_osm$max_speed_kmh / 3.6))
  if (t_end < t_start + min_s) t_end <- t_start + min_s
  path <- timed_path_from_anchors(coords, c(0, total), c(t_start, t_end))
  list(
    path = resample_timed_path(path, th$point_every_m, th$point_every_s),
    t_start = t_start, t_end = t_end, total_m = total, timing = "osm",
    method = "osm_rails", on_network = TRUE, route_reasons = list(), route = route
  )
}

# The rail proposal for a trip: stations (rail_plan(); a Jaé tap's station is
# the start), then Google transit between them (google_fn, with on_fail(code,
# detail)) or, when it has no transit leg, the route along the OSM rails.
# s: its samples; trip_t: the original item's start and end (epoch s); o: the
# overrides (origin_idx, dest_idx, dep, arr, start_offset, end_offset, split);
# tap: its Jaé gate tap (rail_trip_tap()); prev: the previous proposal (its
# route is reused for the same stations unless refresh_route). Timing: from a
# tap at the start station the train leaves at tap + rail_gate_to_train_s;
# from another tap, the rule of multimodal_rail_gate() (Google: applied while
# split). Returns the fix with plan, source ("google" | "osm_rails"),
# google_fail, gate, route_key; or, without a route, list(plan, path = NULL,
# no_path = reason, no_path_station, google_fail).
rail_proposal <- function(s, activity, trip_t, o = list(), tap = NULL, prev = NULL, refresh_route = FALSE,
                          google_fn = calculate_google_transit_route, ref = rail_reference(), th = REVIEW_THRESHOLDS) {
  trip_t <- as.numeric(trip_t)
  if (nrow(s) == 1) {
    s <- s[c(1L, 1L), ]
    s$t <- trip_t
    rownames(s) <- NULL
  }
  tap_st <- rail_tap_station(tap, activity, th, ref)
  plan <- rail_plan(s, activity, o$origin_idx, o$dest_idx, o$dep, o$arr, th, ref, tap_station = tap_st)
  none <- function(reason, station = NULL, fail = NULL) {
    list(plan = plan, path = NULL, no_path = reason, no_path_station = station, google_fail = fail, tap = tap,
         tap_station = tap_st)
  }
  if (!is.null(plan$dep) && !is.null(plan$arr) && identical(plan$dep$key, plan$arr$key)) return(none("same_station"))

  key <- paste(round(unlist(plan$points), 6), collapse = ",")
  route <- if (!refresh_route && !is.null(prev$route) && identical(prev$route_key, key)) prev$route
  if (is.null(route)) {
    fail <- NULL
    r <- google_fn(plan$points, REVIEW_ACTIVITIES[[activity]]$google, ref_time_utc = plan$ref_time_utc,
                   on_fail = function(code, detail = "") fail <<- list(code = code, detail = detail))
    if (!is.null(r)) {
      route <- list(coords = r$coords, steps = r$steps, source = "google")
    } else {
      fail <- fail %||% list(code = "request", detail = "")
      if (is.null(plan$dep) || is.null(plan$arr)) return(none("need_stations", fail = fail))
      osm <- rail_osm_route(plan$dep, plan$arr, activity, ref, th)
      if (!isTRUE(osm$ok)) return(none(osm$reason, osm$station, fail))
      route <- list(coords = osm$coords, osm = osm, source = "osm_rails", google_fail = fail)
    }
  }

  tap_t <- if (!is.null(tap) && nrow(tap)) as.numeric(tap$datetime_utc[1])
  at_tap <- !is.null(tap_st) && !is.null(plan$dep) && identical(plan$dep$key, tap_st$key)
  so <- o$start_offset %||% 0
  eo <- o$end_offset %||% 0
  station_gate <- function(base_start) {
    list(action = "station", late_s = tap_t - base_start, start_at = tap_t + th$rail_gate_to_train_s)
  }
  if (identical(route$source, "osm_rails")) {
    gate <- if (at_tap) station_gate(trip_t[1]) else if (!is.null(tap_t)) multimodal_rail_gate(tap_t, trip_t[1], th)
    start_at <- if (isTRUE(gate$action %in% c("station", "adjust"))) gate$start_at
    fx <- rail_osm_fix(plan, route, s, trip_t, th, so, eo, start_at = start_at)
    fx$start_at_tap <- !is.null(start_at)
  } else {
    fx <- rail_fix(plan, route, s, activity, th, so, eo, ref = ref)
    base_start <- fx$t_start - so
    gate <- if (at_tap) station_gate(base_start) else if (!is.null(tap_t)) multimodal_rail_gate(tap_t, base_start, th)
    if (identical(gate$action, "station") || (isTRUE(o$split$on) && identical(gate$action, "adjust"))) {
      fx <- rail_fix(plan, route, s, activity, th, so, eo, ref = ref, start_at = gate$start_at)
      fx$start_at_tap <- TRUE
    }
  }
  fx$gate <- gate
  fx$plan <- plan
  fx$route_key <- key
  fx$source <- route$source
  fx$google_fail <- route$google_fail
  fx$tap <- tap
  fx$tap_station <- tap_st
  fx
}

# Why a rail trip has no route (rail_proposal()'s no_path) or Google had no
# transit leg (google_fail), for the detail panel
rail_no_path_text <- function(reason, station = NULL, lang = APP_LANGUAGE) {
  switch(reason %||% "",
    same_station = tr_lang("Start and end are the same station.", lang),
    need_stations = tr_lang("Choose the start and end stations to route along the OSM rails.", lang),
    systems = tr_lang("The stations belong to different systems: no path along the rails.", lang),
    no_rails = sprintf(tr_lang("No rails within 300 m of %s.", lang), station %||% "?"),
    not_connected = tr_lang("The rails don't connect these stations.", lang),
    ""
  )
}
google_fail_text <- function(fail, mode_label, lang = APP_LANGUAGE) {
  if (is.null(fail)) return("")
  detail <- fail$detail %||% ""
  switch(fail$code,
    no_key = tr_lang("Google: no API key.", lang),
    request = tr_lang("Google: request failed or timed out.", lang),
    status = sprintf(tr_lang("Google: error %s.", lang), detail),
    zero_results = tr_lang("Google: no route.", lang),
    no_transit = if (nzchar(detail)) {
      sprintf(tr_lang("Google: no %s leg (only %s).", lang), mode_label, detail)
    } else {
      sprintf(tr_lang("Google: no %s leg (it suggests walking).", lang), mode_label)
    },
    no_polyline = sprintf(tr_lang("Google: no %s leg (no geometry).", lang), mode_label),
    fail$code
  )
}

# Rail fix from a plan and the transit route: calculate_google_transit_route()'s
# result (coords: transit steps only; steps: all of them, for the timing), or
# a bare n x 2 lon/lat matrix (distance timing). Timed path plus the route's
# own conformity against the rails (Google can return legs that don't follow
# them).
rail_fix <- function(plan, route, s, activity, th = REVIEW_THRESHOLDS,
                     start_offset_s = 0, end_offset_s = 0, ref = rail_reference(), start_at = NULL) {
  if (is.matrix(route)) route <- list(coords = route)
  route_coords <- route$coords
  oi <- plan$origin_idx
  di <- plan$dest_idx
  tp <- rail_timed_path(route_coords,
                        list(lon = s$lon[oi], lat = s$lat[oi], t = s$t[oi]),
                        list(lon = s$lon[di], lat = s$lat[di], t = s$t[di]),
                        th, start_offset_s, end_offset_s,
                        segments = if (length(route$steps)) rail_segments(route$steps), start_at = start_at)
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

# ---- Control pins (road trips) ----

# Distance along a polyline (cum: path_cumulative_m) of the nearest point of
# each segment to pt, and the distance (m) from pt to it
polyline_segment_projection <- function(coords, cum, pt) {
  n <- nrow(coords)
  lat0 <- mean(coords[, 2])
  kx <- 111320 * cos(lat0 * pi / 180)
  ky <- 110540
  ax <- coords[-n, 1] * kx
  ay <- coords[-n, 2] * ky
  dx <- coords[-1, 1] * kx - ax
  dy <- coords[-1, 2] * ky - ay
  len2 <- dx^2 + dy^2
  px <- pt[1] * kx
  py <- pt[2] * ky
  f <- ifelse(len2 > 0, ((px - ax) * dx + (py - ay) * dy) / len2, 0)
  f <- pmax(0, pmin(1, f))
  list(d = cum[-n] + f * (cum[-1] - cum[-n]), off = sqrt((px - ax - f * dx)^2 + (py - ay - f * dy)^2))
}

# Pins in travel order. pins: data.frame(id, lon, lat, after), after = NA or
# a placement by hand ("start", or the id of the pin it follows). Each pin is
# projected onto the recorded path (its nearest point anywhere along it) and
# sorted by that distance; a pin within near_m of two passes of the recording
# at least pass_gap_m apart along it (a loop, an out-and-back) is ambiguous.
# Placements by hand are applied after the sort. Returns the pins with d (m
# along the recording), off_m, ambiguous and bearing (the travel direction
# there; NA when the pin is farther than near_m from the recording), ordered.
order_pins <- function(s, pins, th = REVIEW_THRESHOLDS) {
  if (is.null(pins) || nrow(pins) == 0) return(pins)
  pt <- th$pins
  coords <- cbind(s$lon, s$lat)
  cum <- path_cumulative_m(coords)
  total <- cum[length(cum)]
  pins$after <- pins$after %||% NA_character_
  info <- lapply(seq_len(nrow(pins)), function(k) {
    sp <- polyline_segment_projection(coords, cum, c(pins$lon[k], pins$lat[k]))
    j <- which.min(sp$off)
    near <- sort(sp$d[sp$off <= pt$near_m])
    passes <- if (length(near)) 1L + sum(diff(near) > pt$pass_gap_m) else 0L
    bearing <- NA_real_
    if (sp$off[j] <= pt$near_m && total > 0) {
      ends <- path_points_at(coords, cum, c(max(0, sp$d[j] - 40), min(total, sp$d[j] + 40)))
      if (any(ends[1, ] != ends[2, ])) bearing <- geosphere::bearing(ends[1, ], ends[2, ])
    }
    c(d = sp$d[j], off_m = sp$off[j], ambiguous = passes >= 2, bearing = bearing)
  })
  info <- do.call(rbind, info)
  pins$d <- info[, "d"]
  pins$off_m <- info[, "off_m"]
  pins$ambiguous <- info[, "ambiguous"] == 1
  pins$bearing <- info[, "bearing"]
  ordered <- pins$id[order(pins$d)]
  # Placements by hand, in the sorted order
  for (id in ordered[ordered %in% pins$id[!is.na(pins$after)]]) {
    target <- pins$after[pins$id == id]
    if (!identical(target, "start") && !target %in% setdiff(ordered, id)) next
    ordered <- setdiff(ordered, id)
    pos <- if (identical(target, "start")) 0L else match(target, ordered)
    ordered <- append(ordered, id, after = pos)
  }
  out <- pins[match(ordered, pins$id), ]
  rownames(out) <- NULL
  out
}

# Along-route distance of each sample (s: lon, lat, t, by time) on the route
# coords, forward only: from the previous anchor, within max(min_reach_m,
# max_speed_mps x the time since it), the nearest point of the route; NA when
# farther than max_off_m from the route (an outlier, skipped). Non-decreasing.
project_samples_monotonic <- function(coords, cum, s, max_off_m = 100, max_speed_mps = 50, min_reach_m = 300) {
  n <- nrow(coords)
  out <- rep(NA_real_, nrow(s))
  if (n < 2 || nrow(s) == 0) return(out)
  last_d <- 0
  last_t <- s$t[1]
  for (k in seq_len(nrow(s))) {
    hi <- last_d + max(min_reach_m, max_speed_mps * (s$t[k] - last_t))
    segs <- which(cum[-1] >= last_d & cum[-n] <= hi)
    if (length(segs) == 0) next
    # the stretch of route between the window's first and last segments
    v <- c(segs, max(segs) + 1L)
    sp <- polyline_segment_projection(coords[v, , drop = FALSE], cum[v], c(s$lon[k], s$lat[k]))
    d <- pmin(pmax(sp$d, last_d), hi)
    j <- which.min(sp$off)
    if (sp$off[j] > max_off_m) next
    out[k] <- d[j]
    last_d <- d[j]
    last_t <- s$t[k]
  }
  out
}

# A timed path on whole seconds, strictly increasing (as the GPX writes them),
# keeping its shape: points that round into the same second are spaced one
# second apart (later points move on until there is slack, the end time
# stays), so no point is dropped; only where the points outnumber the seconds
# are some dropped, those where the path turns least (the ends stay). Times
# outside [t_start, t_end] dropped.
whole_second_path <- function(path, t_start, t_end) {
  p <- path[path$t >= t_start - 0.5 & path$t <= t_end + 0.5, c("lon", "lat", "t")]
  n <- nrow(p)
  if (n <= 2) return(multimodal_path(path, t_start, t_end))
  t0 <- round(t_start)
  t1 <- round(t_end)
  if (n > t1 - t0 + 1) {   # more points than seconds: keep the most turning ones
    xy <- cbind(p$lon, p$lat)
    b <- suppressWarnings(geosphere::bearing(xy[-n, , drop = FALSE], xy[-1, , drop = FALSE]))
    turn <- c(Inf, abs((b[-1] - b[-length(b)] + 180) %% 360 - 180), Inf)
    turn[is.na(turn)] <- 0
    p <- p[sort(order(-turn)[seq_len(t1 - t0 + 1)]), ]
    n <- nrow(p)
  }
  t <- round(p$t)
  t[1] <- t0
  t[n] <- t1
  for (i in 2:n) if (t[i] <= t[i - 1]) t[i] <- t[i - 1] + 1      # forward: one second apart
  if (t[n] > t1) {                                                  # the end stays: back from it
    t[n] <- t1
    for (i in (n - 1):1) if (t[i] >= t[i + 1]) t[i] <- t[i + 1] - 1
  }
  p$t <- t
  rownames(p) <- NULL
  p
}

# Timed path over coords (a pinned route) from the recording: each sample
# projected onto it (project_samples_monotonic) is a time anchor; the route's
# ends take the trip's first and last sample times; times in between are
# linear by distance (timed_path_from_anchors), then resampled, on whole
# seconds strictly increasing (as the GPX writes them; multimodal_path).
# angle_deg: bends kept by the resampling; keep_shape: the whole-second
# rounding keeps, of the points falling in the same second, the one where the
# path turns most (whole_second_path). A spliced path uses both (0, TRUE) so
# the base outside its windows keeps its geometry. Attribute "anchored": the
# samples used.
pinned_timed_path <- function(s, coords, th = REVIEW_THRESHOLDS, angle_deg = 2, keep_shape = FALSE) {
  pt <- th$pins
  coords <- as.matrix(coords[, 1:2])
  colnames(coords) <- c("lon", "lat")
  cum <- path_cumulative_m(coords)
  total <- cum[length(cum)]
  d <- project_samples_monotonic(coords, cum, s, pt$max_off_m, pt$max_speed_mps, pt$min_reach_m)
  ok <- which(!is.na(d) & s$t > s$t[1] & s$t < s$t[nrow(s)])
  anchor_d <- c(0, d[ok], total)
  anchor_t <- c(s$t[1], s$t[ok], s$t[nrow(s)])
  path <- timed_path_from_anchors(coords, anchor_d, anchor_t)
  rs <- resample_timed_path(path, th$point_every_m, th$point_every_s, angle_deg = angle_deg)
  out <- if (keep_shape) whole_second_path(rs, s$t[1], s$t[nrow(s)]) else multimodal_path(rs, s$t[1], s$t[nrow(s)])
  attr(out, "anchored") <- length(ok) + 2L
  out
}

# Road fix through control pins: OSRM /route (route_via_fn) on profile from the
# first sample (or start: c(lon, lat), a moved origin) through the pins
# (order_pins: with the recording's bearing where known, a radius_m snap) to
# the last sample (or end), timed by the recording (pinned_timed_path: the
# trip's start and end times stay). NULL when OSRM finds no route.
pinned_route_fix <- function(s, pins, profile, th = REVIEW_THRESHOLDS, route_via_fn = osrm_route_via,
                             start = NULL, end = NULL) {
  if (is.null(pins)) pins <- data.frame(id = character(0), lon = numeric(0), lat = numeric(0), after = character(0))
  op <- order_pins(s, pins, th)
  n <- nrow(s)
  pts <- rbind(start %||% c(s$lon[1], s$lat[1]), cbind(op$lon, op$lat), end %||% c(s$lon[n], s$lat[n]))
  r <- tryCatch(route_via_fn(pts, profile, bearings = c(NA, op$bearing, NA),
                             radiuses = c(NA, rep(th$pins$radius_m, nrow(op)), NA)), error = function(e) NULL)
  if (is.null(r$coords) || nrow(r$coords) < 2) return(NULL)
  path <- pinned_timed_path(s, r$coords, th)
  list(path = path, method = "pins", on_network = TRUE, n_pins = nrow(op), pins = op, anchored = attr(path, "anchored"))
}

# ---- Control pins as local splices (Batch review) ----
# The base path stays the match proposal: a pin replaces only a window of it
# around where its drag started, through the pin. Pins closer than two windows
# chain (the leg between them goes pin to pin); a moved start or end splices
# the same way up to an anchor along the path. Each piece between two control
# points is a leg: routed by OSRM (default), the recorded samples between the
# two points, or a straight line.

PIN_RULES_WW <- c("no", "minor", "all")     # wrong-way: none, minor streets (default), all but motorways / trunks
PIN_RULES_TURNS <- c("respect", "ignore")   # turn restrictions
LEG_MODES <- c("route", "recorded", "straight")

# Pins with the columns of the splice model: grab_lon / grab_lat (where the
# drag that made the pin started; NA: the pin itself), ww and turns (rules,
# used on official trips), after (placement by hand)
pins_normalize <- function(pins) {
  if (is.null(pins)) pins <- data.frame(id = character(0), lon = numeric(0), lat = numeric(0))
  n <- nrow(pins)
  if (is.null(pins$after)) pins$after <- rep(NA_character_, n)
  if (is.null(pins$grab_lon)) pins$grab_lon <- rep(NA_real_, n)
  if (is.null(pins$grab_lat)) pins$grab_lat <- rep(NA_real_, n)
  if (is.null(pins$ww)) pins$ww <- rep("minor", n)
  if (is.null(pins$turns)) pins$turns <- rep("respect", n)
  pins$ww[!pins$ww %in% PIN_RULES_WW] <- "minor"
  pins$turns[!pins$turns %in% PIN_RULES_TURNS] <- "respect"
  pins
}

# OSRM server and exclude for a routed leg. Official trips follow the rules:
# wrong-way no -> exclude=wwminor,wwmajor, minor -> wwmajor (the default),
# all -> none; restricted turns ignore -> the official-noturn server. Other
# profiles: their own server and default.
pin_osrm_options <- function(profile, ww = "minor", turns = "respect") {
  if (!identical(profile, "official")) return(list(profile = profile, exclude = NULL))
  if (!isTRUE(ww %in% PIN_RULES_WW)) ww <- "minor"
  list(profile = if (identical(turns, "ignore")) "official-noturn" else "official",
       exclude = switch(ww, no = "wwminor,wwmajor", minor = "wwmajor", all = ""))
}

# Rules of a leg between two control points (rows with ww, turns; anchors NA):
# the more permissive of the pins at its ends
leg_rules <- function(a_ww, a_turns, b_ww, b_turns) {
  ww <- c(a_ww, b_ww)
  ww <- ww[!is.na(ww)]
  turns <- c(a_turns, b_turns)
  list(ww = if (length(ww)) PIN_RULES_WW[max(match(ww, PIN_RULES_WW), na.rm = TRUE)] else "minor",
       turns = if (any(turns %in% "ignore")) "ignore" else "respect")
}

# Where each pin sits on the base path (coords, cum): the projection of its grab
# point (the pin when unknown), pins in travel order (op: order_pins()); with
# several passes near it (a loop), the first not before the previous pin.
# Non-decreasing.
pins_on_base <- function(coords, cum, op) {
  d <- numeric(nrow(op))
  last <- 0
  for (k in seq_len(nrow(op))) {
    g <- if (is.na(op$grab_lon[k]) || is.na(op$grab_lat[k])) c(op$lon[k], op$lat[k]) else c(op$grab_lon[k], op$grab_lat[k])
    sp <- polyline_segment_projection(coords, cum, g)
    near <- sp$d[sp$off <= min(sp$off) + 5]
    later <- near[near >= last]
    d[k] <- if (length(later)) min(later) else last
    last <- d[k]
  }
  d
}

# Bearing of a path at distance d along it (cum), over +-15 m
path_bearing_at <- function(coords, cum, d) {
  total <- cum[length(cum)]
  e <- path_points_at(coords, cum, c(max(0, d - 15), min(total, d + 15)))
  if (all(e[1, ] == e[2, ])) NA_real_ else geosphere::bearing(e[1, ], e[2, ])
}

# A routed leg: OSRM /route from xy[1, ] to xy[2, ] on opts$profile with
# opts$exclude; coords or NULL
splice_leg_route <- function(xy, opts, bearings = NULL, radiuses = NULL) {
  r <- tryCatch(osrm_route_via(xy, opts$profile, bearings = bearings, radiuses = radiuses, exclude = opts$exclude),
                error = function(e) NULL)
  r$coords
}

# The recorded samples between times t1 and t2 (exclusive), simplified, between
# points a and b (c(lon, lat))
recorded_leg <- function(s, t1, t2, a, b, tol_m = 10) {
  k <- which(s$t > min(t1, t2) & s$t < max(t1, t2))
  mid <- if (length(k)) cbind(s$lon[k], s$lat[k]) else NULL
  xy <- rbind(a, mid, b)
  colnames(xy) <- c("lon", "lat")
  if (nrow(xy) > 2) xy <- smooth_osrm_route(xy, tolerance_m = tol_m, angle_min = 3)
  xy[1, ] <- a
  xy[nrow(xy), ] <- b
  xy
}

# The base path between distances d1 and d2 (cut exactly there)
base_between <- function(coords, cum, d1, d2) {
  if (d2 <= d1) return(path_points_at(coords, cum, d1))
  inner <- coords[cum > d1 & cum < d2, , drop = FALSE]
  out <- rbind(path_points_at(coords, cum, d1), inner, path_points_at(coords, cum, d2))
  colnames(out) <- c("lon", "lat")
  out
}

# Where a route (coords: lon/lat, in travel order) joins the base path for
# good: walking along it from merge_from_m past its start, the first point
# from which it stays within merge_tol_m of the base (base_m: its vertices in
# crs; cum) heading the same way for merge_len_m, the base position there
# within d_range; from_end: walking back from the route's end instead (the
# last point where it leaves the base). Returns list(s = metres along the
# route from its start, d = base position) or NULL.
base_join <- function(coords, base_m, cum, crs, d_range, sp = REVIEW_THRESHOLDS$pins$splice, from_end = FALSE,
                      step_m = 5) {
  if (is.null(coords) || nrow(coords) < 2) return(NULL)
  m <- to_metric(coords[, 1], coords[, 2], crs)
  q <- densify_xy(m, step_m)
  n <- nrow(q)
  hx <- c(diff(q$x), NA)
  hy <- c(diff(q$y), NA)
  hx[n] <- hx[n - 1L]
  hy[n] <- hy[n - 1L]
  nb <- nrow(base_m)
  segs <- which(cum[-1] >= d_range[1] & cum[-nb] <= d_range[2] & diff(cum) > 0)
  if (n < 2 || !length(segs)) return(NULL)
  ax <- base_m[segs, 1]
  ay <- base_m[segs, 2]
  bx <- base_m[segs + 1L, 1] - ax
  by <- base_m[segs + 1L, 2] - ay
  bl <- sqrt(bx^2 + by^2)
  on <- logical(n)
  pos <- numeric(n)
  for (i in seq_len(n)) {
    f <- pmax(0, pmin(1, ((q$x[i] - ax) * bx + (q$y[i] - ay) * by) / bl^2))
    off <- sqrt((q$x[i] - ax - f * bx)^2 + (q$y[i] - ay - f * by)^2)
    off[(hx[i] * bx + hy[i] * by) < cos(pi / 4) * sqrt(hx[i]^2 + hy[i]^2) * bl] <- Inf   # heading the other way
    j <- which.min(off)
    on[i] <- off[j] <= sp$merge_tol_m
    pos[i] <- cum[segs[j]] + f[j] * (cum[segs[j] + 1L] - cum[segs[j]])
  }
  walked <- if (from_end) q$s[n] - q$s else q$s
  start <- NA_integer_
  for (i in if (from_end) rev(seq_len(n)) else seq_len(n)) {
    if (walked[i] < sp$merge_from_m) next
    if (!on[i] || pos[i] < d_range[1] || pos[i] > d_range[2]) {
      start <- NA_integer_
      next
    }
    if (is.na(start)) start <- i
    if (walked[i] - walked[start] >= sp$merge_len_m) return(list(s = q$s[start], d = pos[start]))
  }
  NULL
}

# The part of a route (lon/lat) before ("head") or after ("tail") s metres
# along it (measured in crs), cut exactly there
cut_route <- function(coords, crs, s, part = c("head", "tail")) {
  part <- match.arg(part)
  m <- to_metric(coords[, 1], coords[, 2], crs)
  sc <- c(0, cumsum(sqrt(rowSums(diff(m)^2))))
  s <- min(max(s, 0), sc[length(sc)])
  i <- min(max(which(sc <= s)), nrow(coords) - 1L)
  f <- if (sc[i + 1L] > sc[i]) (s - sc[i]) / (sc[i + 1L] - sc[i]) else 0
  pt <- coords[i, 1:2] + f * (coords[i + 1L, 1:2] - coords[i, 1:2])
  out <- if (part == "head") rbind(coords[seq_len(i), 1:2, drop = FALSE], pt) else
    rbind(pt, coords[(i + 1L):nrow(coords), 1:2, drop = FALSE])
  out <- matrix(as.numeric(out), ncol = 2, dimnames = list(NULL, c("lon", "lat")))
  out[c(TRUE, rowSums(abs(diff(out))) > 0), , drop = FALSE]
}

# Whether a path (lon/lat) turns back on itself, a U-turn or an out-and-back:
# two points over back_gap_m apart along it, within back_near_m of each other,
# heading over back_deg apart
path_backtracks <- function(coords, sp = REVIEW_THRESHOLDS$pins$splice, step_m = 10) {
  if (nrow(coords) < 3) return(FALSE)
  m <- to_metric(coords[, 1], coords[, 2], utm_epsg(coords[, 1], coords[, 2]))
  q <- densify_xy(m, step_m)
  n <- nrow(q)
  hx <- c(diff(q$x), NA)
  hy <- c(diff(q$y), NA)
  hx[n] <- hx[n - 1L]
  hy[n] <- hy[n - 1L]
  hl <- sqrt(hx^2 + hy^2)
  for (i in seq_len(n)) {
    j <- which(q$s - q$s[i] > sp$back_gap_m & (q$x - q$x[i])^2 + (q$y - q$y[i])^2 <= sp$back_near_m^2 & hl > 0)
    if (length(j) && hl[i] > 0 && any((hx[i] * hx[j] + hy[i] * hy[j]) / (hl[i] * hl[j]) <= cos(sp$back_deg * pi / 180))) {
      return(TRUE)
    }
  }
  FALSE
}

# Road fix with the pins as local splices of the base path (base_path:
# data.frame lon, lat, t, the match proposal's path). pins: pins_normalize()
# columns; ends: moved start / end, c(lon, lat) each; legs: leg modes by key
# ("<from>><to>", control point keys: a pin's id, "start", "end", and the
# group's anchors "a:<first>" / "b:<last>"; default "route"). route_fn(xy,
# opts, bearings, radiuses): a routed leg's coords (opts: pin_osrm_options).
# Each splice ends where the routes rejoin the base by themselves (see
# th$pins$splice and base_join()). Timed by the recording (pinned_timed_path).
# Returns the fix (method "pins") with pins (op: d along the recording,
# base_d, warn), splices (lo, hi, keys, legs: key, mode, coords, failed; ok),
# warn (pin ids whose splice is over warn_ratio x its base stretch and turns
# back on itself), coords (the untimed geometry); NULL without any pin or
# moved end.
splice_route_fix <- function(s, base_path, pins, profile, ends = NULL, legs = NULL, th = REVIEW_THRESHOLDS,
                             route_fn = splice_leg_route) {
  sp <- th$pins$splice
  base <- as.matrix(base_path[, c("lon", "lat")])
  colnames(base) <- c("lon", "lat")
  cum <- path_cumulative_m(base)
  total <- cum[length(cum)]
  base_t <- base_path$t
  t_at <- function(d) if (is.null(base_t)) NA_real_ else stats::approx(cum, base_t, xout = d, rule = 2, ties = "ordered")$y
  pins <- pins_normalize(pins)
  op <- if (nrow(pins)) order_pins(s, pins, th) else pins
  op$base_d <- if (nrow(op)) pins_on_base(base, cum, op) else numeric(0)
  cp <- data.frame(key = character(0), kind = character(0), lon = numeric(0), lat = numeric(0), d = numeric(0),
                   bearing = numeric(0), radius = numeric(0), ww = character(0), turns = character(0))
  add_cp <- function(cp, key, kind, lon, lat, d, bearing, radius, ww, turns) {
    rbind(cp, data.frame(key = key, kind = kind, lon = lon, lat = lat, d = d, bearing = bearing, radius = radius,
                         ww = ww, turns = turns))
  }
  if (!is.null(ends$start)) cp <- add_cp(cp, "start", "start", ends$start[1], ends$start[2], 0, NA, NA, NA, NA)
  if (nrow(op)) {
    cp <- add_cp(cp, as.character(op$id), "pin", op$lon, op$lat, op$base_d, op$bearing, th$pins$radius_m, op$ww, op$turns)
  }
  if (!is.null(ends$end)) cp <- add_cp(cp, "end", "end", ends$end[1], ends$end[2], total, NA, NA, NA, NA)
  if (nrow(cp) == 0 || total <= 0) return(NULL)
  cp$d <- cummax(cp$d)
  crs <- utm_epsg(base[, 1], base[, 2])
  base_m <- to_metric(base[, 1], base[, 2], crs)
  anchor <- function(key, d) {
    xy <- path_points_at(base, cum, d)
    data.frame(key = key, kind = "anchor", lon = xy[1, 1], lat = xy[1, 2], d = d, bearing = path_bearing_at(base, cum, d),
               radius = sp$anchor_radius_m, ww = NA, turns = NA)
  }
  # A routed leg. At a pin, the recording's bearing picks the carriageway; when
  # the route then ends farther than pin_snap_m from the pin (a pin dropped on
  # a side street at a junction, where the recording still runs along the
  # avenue) it is asked again without that bearing, and that one is kept when
  # it ends under a third as far (a pin dropped between two carriageways keeps
  # the one going the recording's way).
  # a_bearing: the departure bearing (at a pin, how the previous leg arrived)
  route_leg <- function(a, b, a_bearing, opts) {
    xy <- rbind(c(a$lon, a$lat), c(b$lon, b$lat))
    ends_off <- function(r) if (is.null(r) || nrow(r) < 2) Inf else geosphere::distHaversine(r[nrow(r), 1:2], xy[2, ])
    r <- route_fn(xy, opts, bearings = c(a_bearing, b$bearing), radiuses = c(a$radius, b$radius))
    if (b$kind == "pin" && !is.na(b$bearing) && ends_off(r) > sp$pin_snap_m) {
      r2 <- route_fn(xy, opts, bearings = c(a_bearing, NA), radiuses = c(a$radius, b$radius))
      if (ends_off(r2) < ends_off(r) / 3) r <- r2
    }
    r
  }
  # OSRM route a -> b (once per pair: the route between two pins serves the
  # search out of the first and into the second)
  routes <- list()
  routed <- function(a, b, a_bearing) {
    key <- paste0(a$key, ">", b$key)
    if (is.null(routes[[key]])) {
      rl <- leg_rules(a$ww, a$turns, b$ww, b$turns)
      r <- route_leg(a, b, a_bearing, pin_osrm_options(profile, rl$ww, rl$turns))
      routes[[key]] <<- list(r = if (is.null(r) || nrow(r) < 2) NULL else
        matrix(as.numeric(as.matrix(r[, 1:2])), ncol = 2, dimnames = list(NULL, c("lon", "lat"))))
    }
    routes[[key]]$r
  }
  # A leg a -> b by its mode: routed (coords: the route's piece; NULL: OSRM
  # failed, a straight line), the recording or a straight line
  make_leg <- function(a, b, coords) {
    key <- paste0(a$key, ">", b$key)
    mode <- unname(legs[key])
    if (length(mode) != 1 || is.na(mode) || !mode %in% LEG_MODES) mode <- "route"
    xa <- c(a$lon, a$lat)
    xb <- c(b$lon, b$lat)
    failed <- mode == "route" && is.null(coords)
    xy <- if (mode == "straight" || failed) {
      rbind(xa, xb)
    } else if (mode == "recorded") {
      recorded_leg(s, t_at(a$d), t_at(b$d), xa, xb, sp$recorded_tol_m)
    } else {
      # anchors are on the base path: the leg starts / ends exactly there
      if (a$kind == "anchor") coords[1, ] <- xa
      if (b$kind == "anchor") coords[nrow(coords), ] <- xb
      coords
    }
    xy <- matrix(as.numeric(xy), ncol = 2, dimnames = list(NULL, c("lon", "lat")))
    list(key = key, mode = mode, coords = xy, failed = failed, length_m = path_cumulative_m(xy)[nrow(xy)])
  }
  # How a routed leg arrives at its end: the next one leaves a pin that way (no U-turn there)
  arrival <- function(l) {
    lc <- path_cumulative_m(l$coords)
    if (l$mode == "route" && !l$failed && lc[length(lc)] > 0) path_bearing_at(l$coords, lc, lc[length(lc)]) else NA_real_
  }
  # The far points of the searches: the neighbouring control point, else the
  # base far_m away (or its start / end)
  far_ahead <- function(k) {
    lim <- min(total, cp$d[k] + sp$far_m)
    if (k < nrow(cp) && cp$d[k + 1L] <= lim) cp[k + 1L, ] else anchor(sprintf("f:%.0f", lim), lim)
  }
  far_behind <- function(k) {
    lim <- max(0, cp$d[k] - sp$far_m)
    if (k > 1 && cp$d[k - 1L] >= lim) cp[k - 1L, ] else anchor(sprintf("f:%.0f", lim), lim)
  }

  splices <- list()
  cur <- NULL                 # the splice being built: lo, keys, legs, arrival
  finish <- function(hi) {
    cur$hi <- hi
    splices[[length(splices) + 1L]] <<- cur
    cur <<- NULL
  }
  for (k in seq_len(nrow(cp))) {
    c0 <- cp[k, ]
    if (is.null(cur)) {
      if (c0$kind == "start") {
        cur <- list(lo = 0, keys = c0$key, legs = list(), arrival = NA_real_)
      } else {
        # Into c0: where the route from the far point behind last leaves the base
        prev_hi <- if (length(splices)) splices[[length(splices)]]$hi else -Inf
        fb <- far_behind(k)
        r <- routed(fb, c0, fb$bearing)
        jn <- base_join(r, base_m, cum, crs, c(fb$d, c0$d), sp, from_end = TRUE)
        lo <- if (!is.null(jn)) jn$d else if (is.null(r)) max(fb$d, c0$d - sp$window_m) else if (fb$kind == "anchor") fb$d else -Inf
        if (lo <= prev_hi) {
          # no way back onto the base before the previous splice ends: one splice through both
          cur <- splices[[length(splices)]]
          splices[[length(splices)]] <- NULL
          cur$legs <- cur$legs[-length(cur$legs)]
          lm <- make_leg(cp[k - 1L, ], c0, routed(cp[k - 1L, ], c0, cur$dep))
          cur$legs <- c(cur$legs, list(lm))
          cur$keys <- c(cur$keys, c0$key)
          cur$arrival <- arrival(lm)
        } else {
          a <- anchor(paste0("a:", c0$key), lo)
          li <- make_leg(a, c0, if (!is.null(jn)) cut_route(r, crs, jn$s, "tail") else if (fb$kind == "anchor") r)
          cur <- list(lo = lo, keys = c0$key, legs = list(li), arrival = arrival(li))
        }
      }
    }
    if (c0$kind == "end") {
      finish(total)
      next
    }
    # Out of c0: where the route to the far point ahead first joins the base for good
    dep <- if (c0$kind == "pin" && !is.na(cur$arrival)) cur$arrival else c0$bearing
    fa <- far_ahead(k)
    r <- routed(c0, fa, dep)
    jn <- base_join(r, base_m, cum, crs, c(c0$d, fa$d), sp)
    cur$dep <- dep
    if (!is.null(jn) || fa$kind == "anchor" || is.null(r)) {
      hi <- if (!is.null(jn)) jn$d else if (is.null(r)) min(fa$d, c0$d + sp$window_m) else fa$d
      b <- anchor(paste0("b:", c0$key), hi)
      cur$legs <- c(cur$legs, list(make_leg(c0, b, if (!is.null(jn)) cut_route(r, crs, jn$s, "head") else r)))
      finish(hi)
    } else {
      # no join before the next control point: the splice runs on through it
      lm <- make_leg(c0, fa, r)
      cur$legs <- c(cur$legs, list(lm))
      cur$keys <- c(cur$keys, fa$key)
      cur$arrival <- arrival(lm)
    }
  }
  # A warning: much longer than the base it replaces and turning back on itself
  warn <- character(0)
  for (g in seq_along(splices)) {
    sg <- splices[[g]]
    len <- sum(vapply(sg$legs, `[[`, numeric(1), "length_m"))
    routed_any <- any(vapply(sg$legs, function(l) l$mode == "route" && !l$failed, logical(1)))
    xy <- do.call(rbind, lapply(sg$legs, `[[`, "coords"))
    sg$ok <- !(routed_any && len > sp$warn_ratio * max(sg$hi - sg$lo, 1) && path_backtracks(xy, sp))
    sg$arrival <- NULL
    sg$dep <- NULL
    if (!sg$ok) warn <- c(warn, intersect(sg$keys, as.character(op$id)))
    splices[[g]] <- sg
  }

  # The path: the base, each splice in its place, the base between them. Times:
  # the base keeps its own (a splice doesn't re-time the rest of the trip); a
  # splice is timed by the samples between its ends' times, projected onto it
  # (as pinned_timed_path); then whole seconds keeping the shape
  dedup <- function(xy) xy[c(TRUE, rowSums(abs(diff(xy))) > 0), , drop = FALSE]
  base_part <- function(d1, d2) {
    xy <- base_between(base, cum, d1, d2)
    data.frame(lon = xy[, 1], lat = xy[, 2], t = t_at(c(d1, cum[cum > d1 & cum < d2], d2)))
  }
  # At most one point per whole second inside a splice (the whole-second
  # rounding below only respaces a few): of the points sharing a second, the
  # one where it turns most; its ends stay
  per_second <- function(p) {
    n <- nrow(p)
    if (n <= 2) return(p)
    xy <- cbind(p$lon, p$lat)
    b <- suppressWarnings(geosphere::bearing(xy[-n, , drop = FALSE], xy[-1, , drop = FALSE]))
    turn <- c(Inf, abs((b[-1] - b[-length(b)] + 180) %% 360 - 180), Inf)
    turn[is.na(turn)] <- 0
    keep <- logical(n)
    keep[c(1L, n)] <- TRUE
    for (g in split(seq_len(n), round(p$t))) if (!any(keep[g])) keep[g[which.max(turn[g])]] <- TRUE
    p[keep, , drop = FALSE]
  }
  splice_part <- function(sg) {
    xy <- dedup(do.call(rbind, lapply(sg$legs, `[[`, "coords")))
    t1 <- if (sg$lo <= 0 && !is.null(ends$start)) s$t[1] else t_at(sg$lo)
    t2 <- if (sg$hi >= total && !is.null(ends$end)) s$t[nrow(s)] else t_at(sg$hi)
    sc <- path_cumulative_m(xy)
    inside <- s[s$t > t1 & s$t < t2, , drop = FALSE]
    d <- if (nrow(inside)) project_samples_monotonic(xy, sc, inside, th$pins$max_off_m, th$pins$max_speed_mps, th$pins$min_reach_m) else numeric(0)
    ok <- !is.na(d)
    tp <- timed_path_from_anchors(xy, c(0, d[ok], sc[length(sc)]), c(t1, inside$t[ok], t2))
    rs <- per_second(resample_timed_path(tp, th$point_every_m, th$point_every_s, angle_deg = 0))
    attr(rs, "anchored") <- sum(ok)
    rs[, c("lon", "lat", "t")]
  }
  parts <- list()
  anchored <- 0L
  pos <- 0
  for (sg in splices) {
    if (sg$lo > pos) parts[[length(parts) + 1L]] <- base_part(pos, sg$lo)
    sp_t <- splice_part(sg)
    anchored <- anchored + attr(sp_t, "anchored")
    parts[[length(parts) + 1L]] <- sp_t
    pos <- sg$hi
  }
  if (pos < total) parts[[length(parts) + 1L]] <- base_part(pos, total)
  timed <- do.call(rbind, parts)
  coords <- dedup(as.matrix(timed[, c("lon", "lat")]))
  colnames(coords) <- c("lon", "lat")
  timed$t <- cummax(timed$t)
  path <- if (is.null(base_t)) pinned_timed_path(s, coords, th, angle_deg = 0, keep_shape = TRUE) else
    whole_second_path(timed, s$t[1], s$t[nrow(s)])
  if (!is.null(base_t)) attr(path, "anchored") <- anchored
  op$warn <- as.character(op$id) %in% warn
  failed <- any(unlist(lapply(splices, function(sg) vapply(sg$legs, `[[`, logical(1), "failed"))))
  list(path = path, method = "pins", on_network = !failed, n_pins = nrow(op), pins = op, anchored = attr(path, "anchored"),
       splices = splices, warn = warn, coords = coords)
}

# New positions for samples (s: lon, lat, t, by time) on a route (Edit Samples
# snap through pins): the projection of each sample on the route; samples
# without one (outliers) by time between their neighbours. n x 2 lon/lat.
snap_positions_to_route <- function(coords, s, th = REVIEW_THRESHOLDS) {
  pt <- th$pins
  coords <- as.matrix(coords[, 1:2])
  cum <- path_cumulative_m(coords)
  d <- project_samples_monotonic(coords, cum, s, pt$max_off_m, pt$max_speed_mps, pt$min_reach_m)
  d[1] <- 0
  d[length(d)] <- cum[length(cum)]
  known <- which(!is.na(d))
  d <- stats::approx(s$t[known], d[known], xout = s$t, rule = 2, ties = "ordered")$y
  d <- cummax(d)
  path_points_at(coords, cum, d)
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


# ---- Multimodal split: walk -> wait -> bus / rail -> walk / visit ----

# A trip Arc started at the front door is split at the Jaé tap (time and place
# of the boarding) into separate tracks: the recorded walk to the stop, a
# stationary wait there, the ride, and what follows the alighting.

# Jaé tap profile that marks the boarding of each activity: bus/BRT taps for
# bus and car trips (Arc often classifies a bus ride as car), Metrô gate taps
# for metro and train trips. VLT taps happen on board: no wait to split.
MULTIMODAL_TAP_PROFILES <- list(bus = "bus", car = "bus", metro = "metro", train = "metro")

# Map / detail colours by segment type (the rail leg uses its activity colour)
MULTIMODAL_COLORS <- c(walking = "#D81B60", stationary = "#00ACC1", bus = "#2E7D32", visit = "#546E7A")

# Jaé taps of the activity's mode inside [start_utc, end_utc], by time. Bus
# taps need a place (the boarding point); a Metrô tap without one stands at
# the rail leg's departure station.
multimodal_taps <- function(boardings, activity, start_utc, end_utc) {
  prof <- MULTIMODAL_TAP_PROFILES[[activity]]
  if (is.null(prof) || is.null(boardings) || nrow(boardings) == 0) return(boardings[0, , drop = FALSE])
  t <- as.numeric(boardings$datetime_utc)
  placed <- !is.na(boardings$longitude) & !is.na(boardings$latitude)
  k <- which(boardings$profile %in% prof & t >= as.numeric(start_utc) & t <= as.numeric(end_utc) &
               (placed | REVIEW_ACTIVITIES[[activity]]$family == "rail"))
  boardings[k[order(t[k])], , drop = FALSE]
}

# Bus trip split at the Jaé boarding (time and place of the tap) and the
# alighting point clicked on the map. trip_samples: data.frame(lon, lat, t,
# acc); boarding: lon, lat, t (epoch s or POSIXct), optional label / line (a
# jae_boardings row works); alighting_point: c(lon, lat), snapped to the
# recorded sample nearest to it along the recording after the boarding.
#   walk to the stop: the recorded samples from the first one to the arrival
#     at the stop (multimodal_arrival_index), typed walking;
#   wait: stationary at the boarding point from the arrival until the tap;
#   bus: boarding -> alighting sample, OSRM /match on the bus profile;
#   after_mode: "walk" (the recorded samples after the alighting, typed
#     walking), "visit" (stationary at the last sample until the trip's end)
#     or "none" (the segments end at the alighting).
# walk_method "osrm" is the manual fallback: both walks routed with OSRM foot,
# the first one arriving after its duration (capped at the tap).
# Returns a list of segments, each list(role, type, name, t_start, t_end,
# path = data.frame(lon, lat, t), info): in time order, touching (each starts
# when the previous ends), whole-second times strictly increasing within each.
# Zero-length walks and waits are left out.
build_multimodal_segments <- function(trip_samples, boarding, alighting_point, after_mode = c("walk", "visit", "none"),
                                      walk_method = c("recorded", "osrm"), th = REVIEW_THRESHOLDS,
                                      route_fn = calculate_osrm_route, match_fn = osrm_match,
                                      wait_point_s = 30, walk_mps = 1.3, arrive_radius_m = 40) {
  after_mode <- match.arg(after_mode)
  walk_method <- match.arg(walk_method)
  s <- multimodal_samples(trip_samples)
  b <- multimodal_point(boarding)
  t_end <- s$t[nrow(s)]
  if (!isTRUE(b$t >= s$t[1] && b$t < t_end)) stop("the boarding time is outside the trip")

  ib <- which(s$t >= b$t)[1]
  ia <- multimodal_alighting_index(s, ib, alighting_point)
  t_alight <- s$t[ia]
  if (t_alight <= b$t) stop("the alighting point must come after the boarding along the recording")
  alight_xy <- c(s$lon[ia], s$lat[ia])

  segs <- multimodal_access(s, b, c(b$lon, b$lat), b$t, walk_method, "stop", th, route_fn, wait_point_s, walk_mps,
                            arrive_radius_m)
  segs[[length(segs) + 1L]] <- multimodal_bus(s, b, alight_xy, t_alight, th, route_fn, match_fn)

  if (t_end > t_alight && after_mode != "none") {
    dest <- c(s$lon[nrow(s)], s$lat[nrow(s)])
    segs[[length(segs) + 1L]] <- if (after_mode == "visit") {
      multimodal_stationary("at_destination", "At the destination", dest, t_alight, t_end, wait_point_s)
    } else if (walk_method == "recorded") {
      multimodal_recorded("walk_from_stop", "Walk from the stop", s, ia, nrow(s))
    } else {
      multimodal_moving("walk_from_stop", "Walk from the stop", multimodal_walk(alight_xy, dest, route_fn, walk_mps),
                        t_alight, t_end, th)
    }
  }
  segs
}

# Rail trip with a Jaé gate tap: the recorded walk to the station (arrival by
# the tap's place and time; a tap without a place stands at the departure
# station), a wait at the rail leg's departure point until its departure,
# then the rail leg. rail_fx: rail_fix() result (path, t_start, t_end,
# method). The tap must not be after the departure (multimodal_rail_tap_ok).
build_rail_multimodal_segments <- function(trip_samples, tap, rail_fx, activity, walk_method = c("recorded", "osrm"),
                                           th = REVIEW_THRESHOLDS, route_fn = calculate_osrm_route,
                                           wait_point_s = 30, walk_mps = 1.3, arrive_radius_m = 40) {
  walk_method <- match.arg(walk_method)
  s <- multimodal_samples(trip_samples)
  b <- multimodal_point(tap)
  p <- rail_fx$path
  t_dep <- round(p$t[1])
  t_arr <- round(p$t[nrow(p)])
  if (!multimodal_rail_tap_ok(b$t, rail_fx)) stop("the Jaé tap is after the rail leg's start")
  if (!isTRUE(b$t >= s$t[1])) stop("the Jaé tap is before the trip")
  if (is.na(b$lon) || is.na(b$lat)) {
    b$lon <- p$lon[1]
    b$lat <- p$lat[1]
  }

  segs <- multimodal_access(s, b, c(p$lon[1], p$lat[1]), t_dep, walk_method, "station", th, route_fn, wait_point_s,
                            walk_mps, arrive_radius_m)
  rail <- multimodal_path(p[, c("lon", "lat", "t")], t_dep, t_arr)
  segs[[length(segs) + 1L]] <- list(
    role = "rail", type = activity, name = REVIEW_ACTIVITIES[[activity]]$label, t_start = t_dep, t_end = t_arr,
    path = rail, info = list(method = rail_fx$method %||% NA_character_,
                             distance_m = path_cumulative_m(cbind(rail$lon, rail$lat))[nrow(rail)])
  )
  segs
}

# The tap can start a rail split only if it is not after the rail leg's departure
multimodal_rail_tap_ok <- function(tap_t, rail_fx) {
  isTRUE(as.numeric(tap_t) <= rail_fx$path$t[1])
}

# Jaé gate tap vs the rail leg's computed start (base_start: without manual
# offsets). "before": the wait runs until the start; "adjust": the tap is
# after it by at most th$rail_gate_max_late_s, so the gate is trusted and the
# leg starts at tap + th$rail_gate_to_train_s (start_at); "ignore": later
# still, likely another trip. late_s: tap - base_start.
multimodal_rail_gate <- function(tap_t, base_start, th = REVIEW_THRESHOLDS) {
  late <- as.numeric(tap_t) - base_start
  if (late <= 0) return(list(action = "before", late_s = late))
  if (late <= th$rail_gate_max_late_s) {
    return(list(action = "adjust", late_s = late, start_at = as.numeric(tap_t) + th$rail_gate_to_train_s))
  }
  list(action = "ignore", late_s = late)
}

# Arrival at the stop / station: the first recorded sample within radius_m of
# the tap point at or before the tap; if none, the sample nearest to it before
# the tap. Returns a row of s.
multimodal_arrival_index <- function(s, b, radius_m = 40) {
  before <- which(s$t <= b$t)
  if (length(before) == 0) return(1L)
  d <- geosphere::distHaversine(cbind(s$lon[before], s$lat[before]), c(b$lon, b$lat))
  near <- which(d <= radius_m)
  before[if (length(near)) near[1] else which.min(d)]
}

# Walk to the stop / station and wait there until t_depart (see
# build_multimodal_segments; place: "stop" or "station" for the names)
multimodal_access <- function(s, b, wait_xy, t_depart, walk_method, place, th, route_fn, wait_point_s, walk_mps,
                              arrive_radius_m) {
  names <- if (place == "stop") c("Walk to the stop", "Wait at the stop") else c("Walk to the station", "Wait at the station")
  t0 <- s$t[1]
  walk <- NULL
  if (walk_method == "recorded") {
    ia <- multimodal_arrival_index(s, b, arrive_radius_m)
    t_arrive <- s$t[ia]
    if (ia > 1L) walk <- multimodal_recorded("walk_to_stop", names[1], s, 1L, ia)
  } else {
    w <- multimodal_walk(c(s$lon[1], s$lat[1]), c(b$lon, b$lat), route_fn, walk_mps)
    t_arrive <- min(round(t0 + w$duration_s), b$t)
    if (t_arrive > t0) walk <- multimodal_moving("walk_to_stop", names[1], w, t0, t_arrive, th)
  }
  wait <- if (t_depart > t_arrive) {
    multimodal_stationary("wait", names[2], wait_xy, t_arrive, t_depart, wait_point_s, list(label = b$label))
  }
  Filter(Negate(is.null), list(walk, wait))
}

# Cleaned samples (at least 2), acc filled
multimodal_samples <- function(trip_samples) {
  s <- review_clean_samples(trip_samples)
  if (is.null(s$acc)) s$acc <- NA_real_
  if (nrow(s) < 2) stop("the trip needs at least 2 samples")
  s
}

# lon, lat, t (epoch s), label, line from a list / one-row data.frame, or a
# jae_boardings row. Exact names: `$` on a data.frame would partially match t
# to tipo_transacao.
multimodal_point <- function(b) {
  field <- function(...) {
    for (k in c(...)) if (!is.null(b[[k]])) return(b[[k]])
    NULL
  }
  list(lon = as.numeric(field("lon", "longitude"))[1], lat = as.numeric(field("lat", "latitude"))[1],
       t = as.numeric(field("t", "datetime_utc"))[1], label = as.character(field("label") %||% NA_character_)[1],
       line = as.character(field("line", "linha_ou_estacao") %||% NA_character_)[1])
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

# Recorded samples from_i..to_i as they are, typed walking
multimodal_recorded <- function(role, name, s, from_i, to_i) {
  p <- s[from_i:to_i, c("lon", "lat", "t")]
  list(role = role, type = "walking", name = name, t_start = s$t[from_i], t_end = s$t[to_i],
       path = multimodal_path(p, s$t[from_i], s$t[to_i]),
       info = list(method = "recorded", distance_m = path_cumulative_m(cbind(p$lon, p$lat))[nrow(p)], samples = nrow(p)))
}

# Walking geometry from -> to: OSRM foot, joined to the exact end points;
# straight line at walk_mps when OSRM has no route (walk_method "osrm" only)
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
multimodal_moving <- function(role, name, walk, t_start, t_end, th) {
  coords <- walk$coords
  colnames(coords) <- c("lon", "lat")
  total <- path_cumulative_m(coords)[nrow(coords)]
  path <- if (total > 0) {
    resample_timed_path(timed_path_from_anchors(coords, c(0, total), c(t_start, t_end)), th$point_every_m, th$point_every_s)
  } else {
    data.frame(lon = coords[1, 1], lat = coords[1, 2], t = c(t_start, t_end))
  }
  list(role = role, type = "walking", name = name, t_start = t_start, t_end = t_end,
       path = multimodal_path(path, t_start, t_end),
       info = list(method = walk$method, distance_m = total, osrm_duration_s = walk$duration_s))
}

# Stationary segment at point: one point every every_s, the last at t_end
multimodal_stationary <- function(role, name, point, t_start, t_end, every_s, info = list()) {
  t <- unique(c(seq(t_start, t_end, by = every_s), t_end))
  list(role = role, type = "stationary", name = name, t_start = t_start, t_end = t_end,
       path = multimodal_path(data.frame(lon = point[1], lat = point[2], t = t), t_start, t_end), info = info)
}

# Bus segment: boarding point (tap time), the samples in between, the
# alighting sample; OSRM /match on the bus profile, /route when matching
# fails, a straight line as a last resort
multimodal_bus <- function(s, b, alight_xy, t_alight, th, route_fn, match_fn) {
  inner <- s[s$t > b$t & s$t < t_alight, c("lon", "lat", "t", "acc")]
  pts <- rbind(data.frame(lon = b$lon, lat = b$lat, t = b$t, acc = 10), inner,
               data.frame(lon = alight_xy[1], lat = alight_xy[2], t = t_alight, acc = 10))
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
  line <- if (!is.na(b$line) && nzchar(trimws(b$line))) paste("Bus", trimws(b$line)) else "Bus"
  list(role = "bus", type = "bus", name = line, t_start = b$t, t_end = t_alight, path = p,
       info = list(method = fx$method, distance_m = path_cumulative_m(cbind(p$lon, p$lat))[nrow(p)],
                   samples = nrow(inner)))
}

# lon, lat, t from t_start to t_end exactly, on whole seconds (as the GPX
# writes them) and strictly increasing: points that would repeat a second are
# dropped, the ends kept
multimodal_path <- function(path, t_start, t_end) {
  p <- path[path$t >= t_start - 0.5 & path$t <= t_end + 0.5, c("lon", "lat", "t")]
  if (nrow(p) == 0) p <- path[c(1, nrow(path)), c("lon", "lat", "t")]
  if (nrow(p) == 1) p <- p[c(1, 1), ]
  p$t <- round(p$t)
  p$t[1] <- round(t_start)
  p$t[nrow(p)] <- round(t_end)
  keep <- c(TRUE, diff(p$t) > 0)
  keep[nrow(p)] <- TRUE
  p <- p[keep, ]
  while (nrow(p) > 2 && p$t[nrow(p)] <= p$t[nrow(p) - 1L]) p <- p[-(nrow(p) - 1L), ]
  rownames(p) <- NULL
  p
}

# Names for a split's GPX: the wait takes the stop's name (stop: list(name,
# source), see stop_name_for) as its track name; the ride (bus / rail leg)
# takes leg_text (jae_leg_text) as its <desc>
multimodal_label_segments <- function(segs, stop = NULL, leg_text = NULL) {
  lapply(segs, function(g) {
    if (identical(g$role, "wait") && !is.null(stop$name) && nzchar(stop$name)) {
      g$gpx_name <- stop$name
      g$stop <- stop
    }
    if (g$role %in% c("bus", "rail") && !is.null(leg_text) && nzchar(leg_text)) g$desc <- leg_text
    g
  })
}

# Accepted fix for a split trip: the segments as separate tracks (name, type,
# path, gpx_name, desc; role, times and stop for the notes), plus their joined
# path for the batch file name and the map. tap: the Jaé boarding (a
# read_jae_boardings() row) for the ride's note.
multimodal_accepted <- function(segs, activity, tap = NULL) {
  paths <- lapply(segs, `[[`, "path")
  joined <- do.call(rbind, paths)
  joined <- joined[c(TRUE, diff(joined$t) > 0), ]
  list(activity = activity, path = joined, method = "split", tap = tap,
       segments = lapply(segs, function(g) list(name = g$name, type = g$type, path = g$path, gpx_name = g$gpx_name,
                                                desc = g$desc, role = g$role, t_start = g$t_start, t_end = g$t_end,
                                                stop = g$stop)))
}

# Arc notes for the accepted split trips (see multimodal_notes), by time.
# ride_body(tap): the ride leg's note body (e.g. smtr_note_body), NULL to
# keep its leg text.
review_notes <- function(accepted, wait_note = TRUE, ride_body = function(tap) NULL) {
  notes <- list()
  for (a in accepted) {
    if (is.null(a$segments)) next
    body <- if (!is.null(a$tap)) ride_body(a$tap)
    segs <- lapply(a$segments, function(g) {
      if (g$role %in% c("bus", "rail") && !is.null(body) && nzchar(body)) g$note_body <- body
      g
    })
    notes <- c(notes, multimodal_notes(segs, wait_note))
  }
  notes[order(vapply(notes, function(n) as.numeric(n$start_utc), numeric(1)))]
}

# Arc notes for a split, for write_notes_package_dir(): list(body, start_utc,
# end_utc, role). Arc attaches a note to every item its time range touches
# (and doesn't show instant notes), so each note is a range strictly inside
# its segment (see multimodal_note_range): a ride leg (bus / rail; body =
# g$note_body, e.g. the SMTR bus note, else the leg text g$desc) and, with
# wait_note, the wait (body = the stop name).
multimodal_notes <- function(segs, wait_note = TRUE, margin_s = 15, min_leg_s = 40) {
  out <- list()
  for (g in segs) {
    role <- g$role %||% ""
    body <- if (role %in% c("bus", "rail")) {
      g$note_body %||% g$desc
    } else if (wait_note && role == "wait") {
      g$stop$name
    }
    if (is.null(body) || !nzchar(body)) next
    r <- multimodal_note_range(g$t_start, g$t_end, margin_s, min_leg_s)
    if (!is.null(r)) out[[length(out) + 1L]] <- list(body = body, start_utc = r[1], end_utc = r[2], role = role)
  }
  out
}

# Note range inside a segment: start + margin_s .. end - margin_s, on whole
# seconds inside those bounds. NULL for a segment shorter than min_leg_s.
multimodal_note_range <- function(t_start, t_end, margin_s = 15, min_leg_s = 40) {
  if (t_end - t_start < min_leg_s) return(NULL)
  c(ceiling(t_start + margin_s), floor(t_end - margin_s))
}


# ---- Coverage of the original item ----

# Arc replaces only the time range a GPX covers, so the tracks of an accepted
# trip must cover its original item exactly (span: start, end; epoch s).
# segments: the main tracks in time order (each with path = data.frame(lon,
# lat, t), and t_start / t_end); s: the recorded samples (lon, lat, t). A main
# track reaching outside the span (e.g. a station time extrapolated before the
# first sample) is scaled into it, unless keep_start (the departure from a
# Jaé tap: the span starts there). Then each gap, by fill$before / fill$after:
#   under cover$stretch_s: the first (last) point moved to the span's end;
#   "wait": a stationary track at the main track's first (last) point;
#   "walk": the recorded samples in the gap (no OSRM), ending at the main
#     track's start (starting at its end);
#   "extend": the main tracks' times scaled to start (end) at the span's end;
#   "auto" (default): "wait" when the recorded samples in the gap stay within
#     cover$wait_m of that point (or there are none), else "walk".
# places$before / $after: the wait's name (the station). Returns
# list(segments (the fills with role "cover"), gaps = list(before, after:
# list(side, gap_s, mode, auto, place, from, to)), span (the covered range),
# raw (the main tracks' range before any change)).
review_cover <- function(segments, span, s = NULL, fill = list(), places = list(), keep_start = FALSE,
                         th = REVIEW_THRESHOLDS) {
  cv <- th$cover
  span <- as.numeric(span)
  n <- length(segments)
  last_row <- function(p) p[nrow(p), ]
  retime <- function(a0, a1, b0, b1) {
    if (a1 <= a0) return(invisible(NULL))
    f <- function(t) b0 + (t - a0) * (b1 - b0) / (a1 - a0)
    segments <<- lapply(segments, function(g) {
      g$path$t <- f(g$path$t)
      if (!is.null(g$t_start)) g$t_start <- f(g$t_start)
      if (!is.null(g$t_end)) g$t_end <- f(g$t_end)
      g
    })
  }
  t0 <- segments[[1]]$path$t[1]
  t1 <- last_row(segments[[n]]$path)$t
  raw <- c(t0, t1)
  lo <- if (keep_start) min(span[1], t0) else span[1]
  hi <- span[2]
  if (t0 < lo || t1 > hi) {
    b0 <- max(t0, lo)
    b1 <- min(t1, hi)
    if (b1 <= b0) {
      b0 <- lo
      b1 <- hi
    }
    retime(t0, t1, b0, b1)
    t0 <- b0
    t1 <- b1
  }
  pick <- function(x, side, default) {
    v <- if (!is.null(x) && side %in% names(x)) x[[side]] else NULL
    if (is.null(v) || length(v) != 1 || is.na(v)) default else v
  }
  first_xy <- unlist(segments[[1]]$path[1, c("lon", "lat")])
  last_xy <- unlist(last_row(segments[[n]]$path)[c("lon", "lat")])
  side_info <- function(side) {
    before <- side == "before"
    from <- if (before) lo else t1
    to <- if (before) t0 else hi
    anchor <- if (before) first_xy else last_xy
    pts <- if (is.null(s) || nrow(s) == 0) {
      data.frame(lon = numeric(0), lat = numeric(0), t = numeric(0))
    } else {
      s[s$t > from & s$t < to, c("lon", "lat", "t")]
    }
    stays <- nrow(pts) == 0 || max(geosphere::distHaversine(cbind(pts$lon, pts$lat), anchor)) <= cv$wait_m
    auto <- if (stays) "wait" else "walk"
    want <- pick(fill, side, "auto")
    if (!want %in% c("wait", "walk", "extend")) want <- auto
    gap <- to - from
    mode <- if (gap <= 0) "none" else if (gap < cv$stretch_s) "stretch" else want
    list(side = side, gap_s = max(gap, 0), mode = mode, auto = auto, place = pick(places, side, ""),
         from = from, to = to, anchor = anchor, samples = pts)
  }
  gaps <- list(before = side_info("before"), after = side_info("after"))

  e0 <- if (gaps$before$mode == "extend") lo else t0
  e1 <- if (gaps$after$mode == "extend") hi else t1
  if (e0 != t0 || e1 != t1) retime(t0, t1, e0, e1)
  if (gaps$before$mode == "stretch") {
    segments[[1]]$path$t[1] <- lo
    if (!is.null(segments[[1]]$t_start)) segments[[1]]$t_start <- lo
  }
  if (gaps$after$mode == "stretch") {
    segments[[n]]$path$t[nrow(segments[[n]]$path)] <- hi
    if (!is.null(segments[[n]]$t_end)) segments[[n]]$t_end <- hi
  }
  fill_track <- function(g) {
    if (g$mode == "wait") {
      w <- multimodal_stationary("cover", "Wait", g$anchor, g$from, g$to, cv$wait_every_s)
      if (nzchar(g$place)) w$gpx_name <- g$place
      return(w)
    }
    if (g$mode != "walk") return(NULL)
    pts <- g$samples
    path <- if (g$side == "before") {
      xy <- if (nrow(pts)) c(pts$lon[1], pts$lat[1]) else g$anchor
      rbind(data.frame(lon = xy[1], lat = xy[2], t = g$from), pts, data.frame(lon = g$anchor[1], lat = g$anchor[2], t = g$to))
    } else {
      xy <- if (nrow(pts)) c(pts$lon[nrow(pts)], pts$lat[nrow(pts)]) else g$anchor
      rbind(data.frame(lon = g$anchor[1], lat = g$anchor[2], t = g$from), pts, data.frame(lon = xy[1], lat = xy[2], t = g$to))
    }
    list(role = "cover", type = "walking", name = "Walk", t_start = g$from, t_end = g$to,
         path = multimodal_path(path, g$from, g$to))
  }
  before <- fill_track(gaps$before)
  after <- fill_track(gaps$after)
  list(segments = c(if (!is.null(before)) list(before), segments, if (!is.null(after)) list(after)),
       gaps = lapply(gaps, function(g) g[c("side", "gap_s", "mode", "auto", "place", "from", "to")]),
       span = c(lo, hi), raw = raw)
}

# The main tracks of an accepted fix: its segments (a split) or its path as
# one track of its activity
review_main_segments <- function(a) {
  if (!is.null(a$segments)) return(a$segments)
  p <- a$path[, c("lon", "lat", "t")]
  list(list(role = "main", name = REVIEW_ACTIVITIES[[a$activity]]$label, code = REVIEW_ACTIVITIES[[a$activity]]$code,
            path = p, t_start = p$t[1], t_end = p$t[nrow(p)]))
}

# An accepted fix covering its original item (review_cover()): its tracks as
# segments, its joined path, span, covered = TRUE and the gaps
review_cover_entry <- function(a, span, s = NULL, fill = list(), places = list(), keep_start = FALSE,
                               th = REVIEW_THRESHOLDS) {
  cv <- review_cover(review_main_segments(a), span, s, fill, places, keep_start, th)
  joined <- do.call(rbind, lapply(cv$segments, function(g) g$path[, c("lon", "lat", "t")]))
  a$path <- joined[c(TRUE, diff(joined$t) > 0), ]
  rownames(a$path) <- NULL
  a$segments <- cv$segments
  a$span <- cv$span
  a$cover_gaps <- cv$gaps
  a$covered <- TRUE
  a
}

# ---- Export and state ----

# build_gpx() inputs for accepted fixes (named list by trip id; each with
# activity and path = data.frame(lon, lat, t)). A split trip (segments: name,
# type, path, gpx_name, desc; see multimodal_accepted) gives one track per
# segment.
# A fix with its original span not yet covered (an older saved state) is
# covered here by extending its main tracks (no samples needed).
review_gpx_input <- function(accepted) {
  items <- list()
  samples <- list()
  for (id in names(accepted)) {
    a <- accepted[[id]]
    if (!isTRUE(a$covered) && length(a$span) == 2) a <- review_cover_entry(a, a$span, fill = list(before = "extend", after = "extend"))
    parts <- a$segments %||% list(list(name = REVIEW_ACTIVITIES[[a$activity]]$label, path = a$path,
                                       code = REVIEW_ACTIVITIES[[a$activity]]$code))
    for (g in parts) {
      p <- g$path
      item_id <- toupper(uuid::UUIDgenerate(use.time = TRUE))
      tz <- tz_from_coords(p$lat[1], p$lon[1])
      t0 <- as.POSIXct(p$t[1], origin = "1970-01-01", tz = "UTC")
      sec_gmt <- seconds_from_gmt(t0, if (is.na(tz)) "UTC" else tz)
      items[[length(items) + 1L]] <- list(
        .internalId = item_id,
        base = list(id = item_id, isVisit = FALSE),
        trip = list(confirmedActivityType = g$code %||% activity_type_code(g$type)),
        description = g$name,
        .gpxName = g$gpx_name,
        .gpxDesc = g$desc
      )
      dates <- format(as.POSIXct(round(p$t), origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      samples <- c(samples, lapply(seq_len(nrow(p)), function(i) list(
        id = paste0(item_id, "-", i), date = dates[i], latitude = p$lat[i], longitude = p$lon[i],
        altitude = 0, secondsFromGMT = sec_gmt, timelineItemId = item_id
      )))
    }
  }
  list(items = items, samples = samples)
}

# Tracks in the batch GPX: one per accepted trip, one per segment of a split
review_gpx_track_count <- function(accepted) {
  sum(vapply(accepted, function(a) if (is.null(a$segments)) 1L else length(a$segments), integer(1)))
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
