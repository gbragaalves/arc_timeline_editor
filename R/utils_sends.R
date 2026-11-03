# ---- Sends: the GPX files the app sent to Arc, and what Arc shows now ----
# The send log (gpx_send_log_read: file, sent_utc, size) and the local copies
# (GPX_ARCHIVE_DIR; the last one sent of each file name). Every track of a copy
# (its type, start, end, simplified path) is checked against the items backup:
#   in_arc    GPX items (neither deleted nor disabled) cover at least half of it;
#   replaced  GPX items there, but disabled (a later import went over it);
#   missing   no GPX item there (not imported yet, or deleted in Arc);
#   absorbed  a walk or a wait with no GPX item: Arc keeps those short legs in
#             the visit or the recorded walk next to them (expected).
# orig_visible: a recorded trip (not GPX, not disabled) still covers at least
# half of it (the import didn't hide it). Shown in Review > Sends.

SENDS_PATH_TOL_M <- 15

# The tracks of a GPX file: data.frame(k, name, type, t0, t1, n) and their
# paths (flat lat, lon, ... vectors, simplified); NULL when it can't be read.
# Kept in memory while the file doesn't change.
.sends_tracks <- new.env(parent = emptyenv())
gpx_tracks_of <- function(path, tol_m = SENDS_PATH_TOL_M) {
  key <- paste(normalizePath(path, mustWork = FALSE), as.numeric(file.mtime(path)), file.size(path), tol_m)
  if (!is.null(.sends_tracks[[key]])) return(.sends_tracks[[key]])
  out <- gpx_tracks_read(path, tol_m)
  if (!is.null(out)) assign(key, out, envir = .sends_tracks)
  out
}
gpx_tracks_read <- function(path, tol_m) {
  doc <- tryCatch(xml2::read_xml(path), error = function(e) NULL)
  if (is.null(doc)) return(NULL)
  xml2::xml_ns_strip(doc)
  trks <- xml2::xml_find_all(doc, "//trk")
  rows <- list()
  paths <- list()
  for (k in seq_along(trks)) {
    pts <- xml2::xml_find_all(trks[[k]], ".//trkpt")
    if (length(pts) == 0) next
    lat <- as.numeric(xml2::xml_attr(pts, "lat"))
    lon <- as.numeric(xml2::xml_attr(pts, "lon"))
    t <- as.numeric(parse_timestamp_utc(xml2::xml_text(xml2::xml_find_first(pts, "time"))))
    ok <- !is.na(lat) & !is.na(lon) & !is.na(t)
    if (!any(ok)) next
    lat <- lat[ok]; lon <- lon[ok]; t <- t[ok]
    keep <- simplify_idx(lon, lat, tol_m)
    rows[[length(rows) + 1L]] <- data.frame(
      k = length(rows) + 1L,
      name = xml2::xml_text(xml2::xml_find_first(trks[[k]], "name")),
      type = xml2::xml_text(xml2::xml_find_first(trks[[k]], "type")),
      t0 = min(t), t1 = max(t), n = length(t), stringsAsFactors = FALSE)
    paths[[length(paths) + 1L]] <- as.vector(rbind(round(lat[keep], 6), round(lon[keep], 6)))
  }
  if (!length(rows)) return(list(tracks = data.frame(k = integer(0), name = character(0), type = character(0), t0 = numeric(0),
                                                     t1 = numeric(0), n = integer(0)), paths = list()))
  list(tracks = do.call(rbind, rows), paths = paths)
}

# Seconds of [t0, t1] covered by the items' spans (overlaps not double counted)
covered_s <- function(t0, t1, starts, ends) {
  k <- which(starts < t1 & ends > t0)
  if (!length(k)) return(0)
  a <- pmax(starts[k], t0)
  b <- pmin(ends[k], t1)
  o <- order(a)
  a <- a[o]; b <- b[o]
  total <- 0; cur_a <- a[1]; cur_b <- b[1]
  for (i in seq_along(a)[-1]) {
    if (a[i] <= cur_b) cur_b <- max(cur_b, b[i]) else { total <- total + cur_b - cur_a; cur_a <- a[i]; cur_b <- b[i] }
  }
  total + cur_b - cur_a
}

# A track's status against the items (load_items() rows); type: its GPX <type>
track_status <- function(t0, t1, items, type = "") {
  len <- max(t1 - t0, 1)
  s <- as.numeric(items$start_utc)
  e <- as.numeric(items$end_utc)
  gpx <- items$source %in% "GPX" & !items$deleted
  share <- function(sel) covered_s(t0, max(t1, t0 + 1), s[sel], e[sel]) / len
  status <- if (share(gpx & !items$disabled) >= 0.5) "in_arc" else if (share(gpx & items$disabled) >= 0.5) "replaced" else "missing"
  if (status == "missing" && type %in% c("stationary", "walking")) status <- "absorbed"
  orig <- share(!gpx & !items$deleted & !items$disabled & !items$is_visit) >= 0.5
  list(status = status, orig_visible = orig)
}

# Every sent file (latest send of each name first) with its tracks' statuses
gpx_sends_status <- function(log = gpx_send_log_read(), archive_dir = GPX_ARCHIVE_DIR, items_dir = ITEMS_BACKUP_DIR,
                             tz = Sys.timezone(), progress = NULL) {
  log <- log[order(log$sent_utc, decreasing = TRUE), , drop = FALSE]
  log <- log[!duplicated(log$file), , drop = FALSE]
  parsed <- lapply(seq_len(nrow(log)), function(i) {
    if (is.function(progress)) progress(0.6 * i / max(1, nrow(log)), log$file[i])
    path <- file.path(archive_dir, log$file[i])
    if (file.exists(path)) gpx_tracks_of(path) else NULL
  })
  t_all <- unlist(lapply(parsed, function(p) if (!is.null(p)) c(p$tracks$t0, p$tracks$t1)))
  items <- if (length(t_all)) {
    if (is.function(progress)) progress(0.7, "items")
    rng <- as.Date(as.POSIXct(range(t_all), origin = "1970-01-01", tz = tz), tz = tz)
    load_items(items_dir, rng[1], rng[2], tz = tz)
  } else NULL
  tracks <- list()
  paths <- list()
  files <- lapply(seq_len(nrow(log)), function(i) {
    p <- parsed[[i]]
    has_copy <- !is.null(p)
    n <- c(in_arc = 0L, replaced = 0L, missing = 0L, absorbed = 0L, orig = 0L)
    if (has_copy && nrow(p$tracks)) {
      for (j in seq_len(nrow(p$tracks))) {
        st <- track_status(p$tracks$t0[j], p$tracks$t1[j], items, p$tracks$type[j])
        n[st$status] <- n[st$status] + 1L
        if (st$orig_visible) n["orig"] <- n["orig"] + 1L
        tracks[[length(tracks) + 1L]] <<- data.frame(file = log$file[i], k = p$tracks$k[j], name = p$tracks$name[j],
                                                    type = p$tracks$type[j], t0 = p$tracks$t0[j], t1 = p$tracks$t1[j],
                                                    status = st$status, orig_visible = st$orig_visible, stringsAsFactors = FALSE)
        paths[[length(paths) + 1L]] <<- p$paths[[j]]
      }
    }
    data.frame(file = log$file[i], sent_utc = log$sent_utc[i], has_copy = has_copy,
               n_tracks = if (has_copy) nrow(p$tracks) else NA_integer_, n_in = n[["in_arc"]], n_replaced = n[["replaced"]],
               n_missing = n[["missing"]], n_absorbed = n[["absorbed"]], n_orig = n[["orig"]], stringsAsFactors = FALSE)
  })
  files <- do.call(rbind, files)
  tracks <- if (length(tracks)) do.call(rbind, tracks) else
    data.frame(file = character(0), k = integer(0), name = character(0), type = character(0), t0 = numeric(0), t1 = numeric(0),
               status = character(0), orig_visible = logical(0))
  list(files = files, tracks = tracks, paths = paths)
}

# The status as the client's message, by column
sends_payload <- function(st) {
  f <- st$files %||% data.frame()
  t <- st$tracks
  list(files = list(file = I(f$file), sent = I(as.numeric(parse_timestamp_utc(f$sent_utc))), copy = I(f$has_copy),
                    n = I(f$n_tracks), n_in = I(f$n_in), n_rep = I(f$n_replaced), n_mis = I(f$n_missing), n_abs = I(f$n_absorbed),
                    n_orig = I(f$n_orig)),
       tracks = list(file = I(t$file), k = I(t$k), type = I(t$type), t0 = I(t$t0), t1 = I(t$t1), status = I(t$status),
                     orig = I(t$orig_visible), p = st$paths))
}
