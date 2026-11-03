# ---- Period review: every trip and visit of a period on the map ----
# From the local backup only (items/, weeks/, places/; "Fetch everything from
# iCloud" brings them up to date). Two views of the same period:
#   now:      the timeline as Arc shows it (items neither deleted nor
#             disabled): the GPX imports and the recordings they didn't replace;
#   recorded: what was recorded (items not from a GPX, not deleted), also the
#             ones a GPX import disabled, with their disabled samples.
# Trip paths are simplified to PERIOD_SIMPLIFY_M for the map.

PERIOD_MAX_DAYS   <- if (exists("PERIOD_MAX_DAYS")) PERIOD_MAX_DAYS else 366
PERIOD_SIMPLIFY_M <- 4
PERIOD_CACHE_VERSION <- 2L   # bump when period_timeline()'s output changes (2: place_id)

# Path simplification for a period of `days` days: finer for short periods
period_tolerance <- function(days) if (days <= 31) PERIOD_SIMPLIFY_M else if (days <= 92) 8 else 15

# Modes outside the review's tables (icon, colour); the rest: a grey dot
PERIOD_EXTRA_MODES <- list(
  airplane  = list(icon = "✈️", color = "#0277BD"),
  boat      = list(icon = "⛴️", color = "#01579B"),
  hiking    = list(icon = "\U0001F97E", color = "#6D4C41"),
  scooter   = list(icon = "\U0001F6F4", color = "#7B1FA2"),
  cable_car = list(icon = "\U0001F6A1", color = "#BF360C"),
  funicular = list(icon = "\U0001F69E", color = "#BF360C")
)
PERIOD_OTHER_MODE <- list(icon = "•", color = "#757575")

# Arc's places (places/*.json) as a data.frame (id, name); the latest save of
# a place wins. Cached while the files don't change.
.period_places <- new.env(parent = emptyenv())
load_places <- function(dir = PLACES_BACKUP_DIR) {
  empty <- data.frame(id = character(0), name = character(0), stringsAsFactors = FALSE)
  files <- list.files(dir, "\\.json$", full.names = TRUE)
  if (length(files) == 0) return(empty)
  key <- paste(files, as.numeric(file.mtime(files)), collapse = "|")
  if (identical(.period_places$key, key)) return(.period_places$df)
  rows <- lapply(files, function(f) {
    df <- tryCatch(jsonlite::fromJSON(f, flatten = TRUE), error = function(e) NULL)
    if (!is.data.frame(df) || !all(c("id", "name") %in% names(df))) return(NULL)
    data.frame(id = as.character(df$id), name = as.character(df$name),
               saved = as.character(if ("lastSaved" %in% names(df)) df$lastSaved else NA_character_),
               stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, rows)
  if (is.null(df)) return(empty)
  df <- df[!is.na(df$id), ]
  df <- df[order(df$id, df$saved, decreasing = TRUE), ]
  df <- df[!duplicated(df$id), c("id", "name")]
  rownames(df) <- NULL
  .period_places$key <- key
  .period_places$df <- df
  df
}

# Samples of many items, each weekly file read once: a list by item id of
# data.frames (t, lat, lon) sorted by time. Disabled samples count only for
# the ids in keep_disabled (recordings a GPX import disabled).
# progress(fraction, detail) is called once per weekly file.
period_item_samples <- function(items, weekly_dir = WEEKLY_BACKUP_DIR, keep_disabled = character(0), progress = NULL) {
  if (nrow(items) == 0) return(list())
  weeks <- sort(unique(unlist(lapply(seq_len(nrow(items)), function(i) trip_weeks(items$start_utc[i], items$end_utc[i])))))
  parts <- list()
  for (w in seq_along(weeks)) {
    if (is.function(progress)) progress(w / length(weeks), weeks[w])
    wk <- read_week_samples(weeks[w], weekly_dir, keep_disabled = TRUE)
    if (is.null(wk)) next
    want <- wk$item_id %in% items$id & (!wk$disabled | wk$item_id %in% keep_disabled)
    if (any(want)) parts[[length(parts) + 1L]] <- wk[want, c("id", "item_id", "t", "lat", "lon")]
  }
  all <- do.call(rbind, parts)
  if (is.null(all)) return(list())
  all <- all[!duplicated(all$id), ]
  all <- all[order(all$item_id, all$t), ]
  lapply(split(all[, c("t", "lat", "lon")], all$item_id), function(d) { rownames(d) <- NULL; d })
}

# Douglas-Peucker in metres (a local equirectangular projection): the indices
# of the points kept, the first and the last always among them
simplify_idx <- function(lon, lat, tol_m = PERIOD_SIMPLIFY_M) {
  n <- length(lon)
  if (n <= 2) return(seq_len(n))
  x <- lon * 111320 * cos(mean(lat) * pi / 180)
  y <- lat * 110540
  keep <- logical(n)
  keep[c(1L, n)] <- TRUE
  stack <- list(c(1L, n))
  while (length(stack)) {
    ab <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    a <- ab[1]
    b <- ab[2]
    if (b - a < 2L) next
    i <- (a + 1L):(b - 1L)
    dx <- x[b] - x[a]
    dy <- y[b] - y[a]
    l2 <- dx^2 + dy^2
    tt <- if (l2 > 0) pmin(1, pmax(0, ((x[i] - x[a]) * dx + (y[i] - y[a]) * dy) / l2)) else 0
    d <- sqrt((x[i] - x[a] - tt * dx)^2 + (y[i] - y[a] - tt * dy)^2)
    j <- which.max(d)
    if (d[j] > tol_m) {
      m <- i[j]
      keep[m] <- TRUE
      stack[[length(stack) + 1L]] <- c(a, m)
      stack[[length(stack) + 1L]] <- c(m, b)
    }
  }
  which(keep)
}

# Mode name of an Arc activity code ("unknown" when none)
period_mode_of_code <- function(code) {
  m <- unname(ACTIVITY_TYPE_NAMES[as.character(code)])
  m[is.na(m)] <- "unknown"
  m
}

# A mode's label (translation key), icon and colour
period_mode_style <- function(mode) {
  st <- REVIEW_ACTIVITIES[[mode]] %||% REVIEW_SET_MODES[[mode]] %||% PERIOD_EXTRA_MODES[[mode]] %||%
    (if (mode %in% c("unknown", "bogus")) REVIEW_UNTYPED_INFO) %||% PERIOD_OTHER_MODE
  label <- if (exists("ACTIVITY_CHOICES")) names(ACTIVITY_CHOICES)[match(mode, ACTIVITY_CHOICES)] else NA_character_
  if (is.na(label)) label <- st$label %||% if (mode %in% c("unknown", "bogus")) REVIEW_UNTYPED_INFO$label else mode
  list(label = label, icon = st$icon, color = st$color)
}

# The period's trips and visits in both views. items: load_items(visits =
# TRUE) rows; samples: period_item_samples() by item id; places: load_places().
# One row per item shown in either view: id, kind ("trip" / "visit"),
# start_utc, end_utc, mode, gpx, now, recorded, place (visits: the place's
# name, else the address), place_id, lat, lon (visits), km, path (trips: n x 2 lat / lon,
# simplified; NULL under 2 samples), n_samples. Trips under a minute with
# under 2 samples (Arc's zero-length bridges) are left out.
period_timeline <- function(items, samples = list(), places = load_places(), tol_m = PERIOD_SIMPLIFY_M) {
  gpx <- items$source %in% "GPX"
  now <- !items$deleted & !items$disabled
  recorded <- !items$deleted & !gpx
  tl <- items[now | recorded, , drop = FALSE]
  tl$gpx <- gpx[now | recorded]
  tl$now <- now[now | recorded]
  tl$recorded <- recorded[now | recorded]
  n <- nrow(tl)
  tl$kind <- ifelse(tl$is_visit, "visit", "trip")
  tl$mode <- ifelse(tl$is_visit, "visit", period_mode_of_code(tl$activity_code))
  pid <- tl[["place_id"]] %||% rep(NA_character_, n)
  name <- places$name[match(pid, places$id)]
  addr <- tl[["address"]] %||% rep(NA_character_, n)
  tl$place <- ifelse(tl$is_visit, ifelse(!is.na(name) & nzchar(name), name, addr), NA_character_)
  tl$place_id <- ifelse(tl$is_visit, pid, NA_character_)
  if (is.null(tl[["lat"]])) tl$lat <- rep(NA_real_, n)
  if (is.null(tl[["lon"]])) tl$lon <- rep(NA_real_, n)
  tl$path <- rep(list(NULL), n)
  tl$n_samples <- integer(n)
  for (i in which(!tl$is_visit)) {
    s <- samples[[tl$id[i]]]
    if (is.null(s) || nrow(s) == 0) next
    tl$n_samples[i] <- nrow(s)
    if (nrow(s) < 2) next
    k <- simplify_idx(s$lon, s$lat, tol_m)
    tl$path[[i]] <- cbind(round(s$lat[k], 6), round(s$lon[k], 6))
  }
  bridge <- !tl$is_visit & tl$n_samples < 2 & as.numeric(tl$end_utc - tl$start_utc, units = "secs") < 60
  tl <- tl[!bridge, , drop = FALSE]
  tl$km <- ifelse(tl$is_visit, NA_real_, tl$distance_m / 1000)
  cols <- c("id", "kind", "start_utc", "end_utc", "mode", "gpx", "now", "recorded", "place", "place_id", "lat", "lon", "km",
            "path", "n_samples")
  tl <- tl[order(tl$start_utc), cols, drop = FALSE]
  rownames(tl) <- NULL
  tl
}

# The timeline as the client's message, by column (a year is thousands of
# items: far faster to encode than one object each): id, k ("t" / "v"), s / e
# (epoch s), m (mode), g (GPX), now, rec, nm (place), pid (place id), lat / lon (visits), km,
# p (paths as flat lat, lon, lat, lon, ... vectors; null without one) and
# styles (label key, icon, colour by mode). I(): a one-item period still
# sends arrays. The client (PERIOD_JS) rebuilds the items.
period_payload <- function(tl) {
  modes <- unique(tl$mode[tl$kind != "visit"])
  styles <- stats::setNames(lapply(modes, function(m) {
    st <- period_mode_style(m)
    list(label = st$label, icon = st$icon, color = st$color)
  }), modes)
  list(id = I(tl$id), k = I(ifelse(tl$kind == "visit", "v", "t")),
       s = I(as.numeric(tl$start_utc)), e = I(as.numeric(tl$end_utc)), m = I(tl$mode),
       g = I(tl$gpx %in% TRUE), now = I(tl$now %in% TRUE), rec = I(tl$recorded %in% TRUE),
       nm = I(tl$place), pid = I(tl$place_id), lat = I(tl$lat), lon = I(tl$lon), km = I(round(tl$km, 2)),
       p = lapply(tl$path, function(p) if (is.null(p)) NULL else as.vector(t(p))),
       styles = styles)
}

# The Jaé taps of [from, to] (local dates) for the client, by column: t
# (epoch s), p (profile: bus / metro / tram), lb (label), lat, lon
period_taps <- function(from, to, taps = read_jae_boardings(), tz = Sys.timezone()) {
  b <- period_bounds_utc(from, to, tz)
  t <- as.numeric(taps$datetime_utc)
  k <- which(!is.na(t) & t >= as.numeric(b[1]) & t <= as.numeric(b[2]))
  list(t = I(t[k]), p = I(as.character(taps$profile[k])), lb = I(as.character(taps$label[k])),
       lat = I(taps$latitude[k]), lon = I(taps$longitude[k]))
}

# ---- A period of up to a year: month by month, cached on disk ----
# Each month's timeline (period_timeline() of the items overlapping it) is kept
# in REVIEW_SCRATCH_DIR/period_cache/<YYYY-MM>_<tol>m.rds with a signature of
# the files it came from (the month's items files, its weekly sample files, the
# places): unchanged files, no work. A period joins its months' rows.

period_cache_dir <- function() file.path(REVIEW_SCRATCH_DIR, "period_cache")

# Local day bounds of [from, to] as UTC instants
period_bounds_utc <- function(from, to, tz = Sys.timezone()) {
  c(lubridate::force_tz(as.POSIXct(paste(as.Date(from), "00:00:00"), tz = "UTC"), tz),
    lubridate::force_tz(as.POSIXct(paste(as.Date(to), "23:59:59"), tz = "UTC"), tz))
}

# The files a month's timeline is made from, with their times and sizes
period_month_signature <- function(month, items_dir = ITEMS_BACKUP_DIR, weekly_dir = WEEKLY_BACKUP_DIR,
                                   places_dir = PLACES_BACKUP_DIR, tz = Sys.timezone()) {
  month <- lubridate::floor_date(as.Date(month), "month")
  last <- lubridate::ceiling_date(month + 1, "month") - 1
  b <- period_bounds_utc(month, last, tz)
  files <- c(file.path(items_dir, paste0(format(c(lubridate::floor_date(month - 1, "month"), month), "%Y-%m"), ".json")),
             file.path(weekly_dir, paste0(weeks_for_interval(b[1] - 14 * 3600, b[2] + 14 * 3600), ".json.gz")),
             list.files(places_dir, "\\.json$", full.names = TRUE))
  files <- files[file.exists(files)]
  info <- file.info(files)
  paste(c(PERIOD_CACHE_VERSION, sprintf("%s:%.0f:%.0f", basename(files), as.numeric(info$mtime), info$size)), collapse = "|")
}

# One month's timeline (every item overlapping it), from the cache when its files are unchanged
period_month_timeline <- function(month, tol = PERIOD_SIMPLIFY_M, items_dir = ITEMS_BACKUP_DIR, weekly_dir = WEEKLY_BACKUP_DIR,
                                  places_dir = PLACES_BACKUP_DIR, progress = NULL, tz = Sys.timezone()) {
  month <- lubridate::floor_date(as.Date(month), "month")
  last <- lubridate::ceiling_date(month + 1, "month") - 1
  sig <- period_month_signature(month, items_dir, weekly_dir, places_dir, tz)
  file <- file.path(period_cache_dir(), sprintf("%s_%gm.rds", format(month, "%Y-%m"), tol))
  if (file.exists(file)) {
    cached <- tryCatch(readRDS(file), error = function(e) NULL)
    if (identical(cached$sig, sig)) return(cached$tl)
  }
  items <- load_items(items_dir, month, last, tz = tz, visits = TRUE)
  trips <- items[!items$is_visit & !items$deleted, , drop = FALSE]
  samples <- period_item_samples(trips, weekly_dir, keep_disabled = trips$id[trips$disabled], progress = progress)
  tl <- period_timeline(items, samples, load_places(places_dir), tol)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  tryCatch(saveRDS(list(sig = sig, tl = tl), file), error = function(e) NULL)
  tl
}

# The timeline of [from, to] (local dates), month by month. progress(fraction,
# detail) once per month (and per weekly file of a month built anew).
period_load <- function(from, to, items_dir = ITEMS_BACKUP_DIR, weekly_dir = WEEKLY_BACKUP_DIR, places_dir = PLACES_BACKUP_DIR,
                        progress = NULL, tz = Sys.timezone()) {
  from <- as.Date(from)
  to <- as.Date(to)
  tol <- period_tolerance(as.integer(to - from) + 1L)
  months <- seq(lubridate::floor_date(from, "month"), lubridate::floor_date(to, "month"), by = "month")
  parts <- lapply(seq_along(months), function(i) {
    step <- function(f, d) if (is.function(progress)) progress((i - 1 + f) / length(months), paste(format(months[i], "%m/%Y"), d))
    step(0, "")
    period_month_timeline(months[i], tol, items_dir, weekly_dir, places_dir, progress = step, tz = tz)
  })
  tl <- do.call(rbind, parts)
  if (is.null(tl) || nrow(tl) == 0) return(period_timeline(load_items(items_dir, from, to, tz = tz, visits = TRUE)))
  b <- period_bounds_utc(from, to, tz)
  tl <- tl[!duplicated(tl$id) & tl$start_utc <= b[2] & tl$end_utc >= b[1], , drop = FALSE]
  tl <- tl[order(tl$start_utc), , drop = FALSE]
  rownames(tl) <- NULL
  tl
}
