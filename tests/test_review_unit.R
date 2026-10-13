# Unit tests on synthetic data (metric CRS: UTM 23S): review checks, rail/road fixes, splits, notes,
# SMTR notes, pins, iCloud cleanup, jitter / spur fixes, map layers.
# Run from the project root: Rscript tests/test_review_unit.R
exprs <- parse("app.R")
for (e in exprs) {
  if (is.call(e) && identical(e[[1]], as.name("shinyApp"))) next
  suppressPackageStartupMessages(eval(e, globalenv()))
}
fails <- 0L
check <- function(label, ok) { cat(sprintf("[%s] %s\n", if (isTRUE(ok)) "PASS" else "FAIL", label)); if (!isTRUE(ok)) fails <<- fails + 1L }

CRS <- 32723
X0 <- 685000; Y0 <- 7460000                          # around Rio de Janeiro
ll <- function(x, y) from_metric(X0 + x, Y0 + y, CRS)
line_sfc <- function(...) sf::st_sfc(lapply(list(...), function(m) sf::st_linestring(cbind(X0 + m[, 1], Y0 + m[, 2]))), crs = CRS)
samples_at <- function(x, y, t, acc = 10) { p <- ll(x, y); data.frame(lon = p[, 1], lat = p[, 2], t = t, acc = acc) }
TH <- REVIEW_THRESHOLDS

# ---- 1. Densification catches a straight chord through a block ----
# Road: along y = 0 up to x = 212, then up x = 212. Recorded: on the road, then a
# 300 m straight jump from (0,0) to (212,212) between two on-road samples.
road_ref <- line_sfc(rbind(c(-200, 0), c(212, 0), c(212, 412)))
s <- samples_at(c(-200, -100, 0, 212, 212, 212), c(0, 0, 0, 212, 312, 412), seq(0, 250, by = 50))
road_ll <- ll(c(-200, 212, 212), c(0, 0, 412))
fake_match <- list(ok = TRUE, pieces = list(list(coords = road_ll, idx = 1:6)), tracepoints = as.matrix(s[, c("lon", "lat")]),
                   sent = rep(TRUE, 6), confidence = 0.9, n_dropped = 0L)
cf <- conformity(s, "car", match = fake_match)
check(sprintf("chord: longest_off_m = %.0f > 120 (off_share %.2f, max offset %.0f m)", cf$metrics$longest_off_m,
              cf$metrics$off_share, cf$metrics$max_offset_m), cf$metrics$longest_off_m > 120)
xy <- to_metric(s$lon, s$lat, CRS)
undense <- offset_metrics(xy, road_ref, tol_m = 25, step_m = 1e6)
check(sprintf("same chord tested only at the samples: longest_off_m = %.0f (would be missed)", undense$longest_off_m),
      undense$longest_off_m < 120)
check("chord is flagged (road)", length(review_flags(cf$metrics, "car")) > 0)

# ---- 2. A path that follows the reference within 10 m: off_share ~ 0 ----
rail_ref <- line_sfc(rbind(c(0, 0), c(2000, 0)))
xs <- seq(0, 2000, by = 100)
s2 <- samples_at(xs, rep(c(10, -10), length.out = length(xs)), seq(0, by = 15, length.out = length(xs)))
cr <- conformity(s2, "metro", reference = rail_ref)
check(sprintf("within 10 m of the rails: off_share = %.3f, max offset %.1f m", cr$metrics$off_share, cr$metrics$max_offset_m),
      cr$metrics$off_share < 0.01 && cr$metrics$max_offset_m <= 10.5)
check("within 10 m: not flagged (rail)", length(review_flags(cr$metrics, "metro")) == 0)
road_line <- ll(c(0, 2000), c(0, 0))
cr2 <- conformity(s2, "bus", match = list(ok = TRUE, pieces = list(list(coords = road_line, idx = seq_along(xs))),
                                          tracepoints = as.matrix(s2[, c("lon", "lat")]), confidence = 0.95, n_dropped = 0L))
check(sprintf("within 10 m of the matched road: off_share = %.3f (road tol %d m)", cr2$metrics$off_share, TH$road$tol_m),
      cr2$metrics$off_share < 0.01 && length(review_flags(cr2$metrics, "bus")) == 0)
s2b <- samples_at(xs, rep(30, length(xs)), seq_along(xs) * 10)  # 30 m off the road all along (urban canyon)
cr2b <- conformity(s2b, "car", match = list(ok = TRUE, pieces = list(list(coords = road_line, idx = seq_along(xs))),
                                            tracepoints = as.matrix(s2b[, c("lon", "lat")]), confidence = 0.95, n_dropped = 0L))
check(sprintf("road tol is 35 m: 30 m off all along -> off_share %.2f, not flagged", cr2b$metrics$off_share),
      TH$road$tol_m == 35 && cr2b$metrics$off_share < 0.01 && length(review_flags(cr2b$metrics, "car")) == 0)

# ---- 3. Thresholds ----
base_m <- list(n_samples = 50, path_km = 6, straight_km = 5, off_share = 0, longest_off_m = 0, max_offset_m = 0,
               match_confidence = 0.9, no_match = FALSE)
with <- function(...) modifyList(base_m, list(...))
codes <- function(m, a) vapply(review_flags(m, a), `[[`, "", "code")
check("rail: off_share 0.16 flagged, 0.14 not", identical(codes(with(off_share = 0.16), "metro"), "off_share") &&
        length(codes(with(off_share = 0.14), "metro")) == 0)
check("rail: longest_off 401 flagged, 399 not", identical(codes(with(longest_off_m = 401), "train"), "longest_off") &&
        length(codes(with(longest_off_m = 399), "train")) == 0)
check("rail: max_offset 601 flagged, 599 not", identical(codes(with(max_offset_m = 601), "tram"), "max_offset") &&
        length(codes(with(max_offset_m = 599), "tram")) == 0)
check("rail: match confidence ignored", length(codes(with(match_confidence = 0.1), "metro")) == 0)
check("road: off_share 0.11 flagged, 0.09 not", identical(codes(with(off_share = 0.11), "car"), "off_share") &&
        length(codes(with(off_share = 0.09), "car")) == 0)
check("road: longest_off 121 flagged, 119 not", identical(codes(with(longest_off_m = 121), "taxi"), "longest_off") &&
        length(codes(with(longest_off_m = 119), "taxi")) == 0)
check("road: match confidence is informational (0.00 alone not flagged)", length(codes(with(match_confidence = 0), "bus")) == 0 &&
        is.null(TH$road$match_confidence))
check("road: no_match flagged", "no_match" %in% codes(with(no_match = TRUE, match_confidence = NA, off_share = NA), "car"))
check("road: max_offset alone not a road trigger", length(codes(with(max_offset_m = 5000), "car")) == 0)
check("fragment: path_km 0.49 flagged (car and metro), 0.50 not",
      identical(codes(with(path_km = 0.49, straight_km = 0.4), "car"), "fragment") &&
        identical(codes(with(path_km = 0.49, straight_km = 0.4), "metro"), "fragment") &&
        length(codes(with(path_km = 0.50, straight_km = 0.4), "car")) == 0 && TH$min_path_km == 0.5 && is.null(TH$min_straight_km))
check(sprintf("reasons text: \"%s\"", format_reasons(review_flags(with(off_share = 0.32, longest_off_m = 640), "metro"), "en")),
      grepl("32% of the path off the network", format_reasons(review_flags(with(off_share = 0.32), "metro"), "en"), fixed = TRUE))
check(sprintf("reasons text PT: \"%s\"", format_reasons(review_flags(with(longest_off_m = 300), "car"), "pt")),
      grepl("300 m seguidos fora da rede", format_reasons(review_flags(with(longest_off_m = 300), "car"), "pt"), fixed = TRUE))

# ---- 4. Road timestamps come from the matched tracepoints ----
gx <- seq(0, 1000, by = 50)
geom <- ll(gx, rep(0, length(gx)))
tp_x <- c(0, 150, 400, 900, 1000)
tp_t <- c(0, 30, 50, 120, 125) + 1.79e9
s4 <- samples_at(tp_x, rep(3, 5), tp_t)              # recorded 3 m off the road
m4 <- list(ok = TRUE, pieces = list(list(coords = geom, idx = 1:5)), tracepoints = ll(tp_x, rep(0, 5)), confidence = 0.9)
r4 <- road_timed_path(s4, m4)
p4 <- r4$path
check(sprintf("road path: %d points, times monotonic", nrow(p4)), !is.unsorted(p4$t))
at_tp <- vapply(tp_x, function(x) { i <- which.min(abs(p4$d - x)); p4$t[i] }, numeric(1))
check("road path: time at each tracepoint equals its sample time", isTRUE(all.equal(at_tp, tp_t, tolerance = 1e-9)) &&
        all(tp_t %in% p4$t))
t275 <- approx(p4$d, p4$t, 275)$y - 1.79e9
check(sprintf("road path: speed follows the samples (t at 275 m = %.1f s, constant speed would give %.1f s)", t275, 275 / 1000 * 125),
      abs(t275 - 40) < 0.1)
check(sprintf("road path density: max step %.0f m / %.1f s (<= 100 m and <= 10 s)", max(diff(p4$d)), max(diff(p4$t))),
      max(diff(p4$d)) <= 100 + 1e-6 && max(diff(p4$t)) <= 10 + 1e-6)

# ---- 5. Gap handling ----
s5 <- samples_at(c(-50, 0, 200, 400, 500, 600, 800, 1000), rep(2, 8), c(0, 10, 40, 70, 90, 110, 140, 170) + 1.79e9)
g1 <- ll(seq(0, 400, by = 50), 0); g2 <- ll(seq(600, 1000, by = 50), 0)
m5 <- list(ok = TRUE, pieces = list(list(coords = g1, idx = 2:4), list(coords = g2, idx = 6:8)),
           tracepoints = rbind(c(NA, NA), ll(c(0, 200, 400), 0), c(NA, NA), ll(c(600, 800, 1000), 0)), confidence = 0.8)
dogleg <- ll(c(400, 500, 600), c(0, 60, 0))
route_calls <- 0L
fake_route <- function(points, profile) { route_calls <<- route_calls + 1L; list(coords = dogleg, distance_m = 233, duration_s = 30) }
r5 <- road_timed_path(s5, m5, route_fn = fake_route)
p5 <- r5$path
check(sprintf("gap: routed once between the pieces (%d call), on_network = %s, method %s", route_calls, r5$on_network, r5$method),
      route_calls == 1 && isTRUE(r5$on_network) && r5$method == "match+gaps")
dog_pt <- which.min(geosphere::distHaversine(cbind(p5$lon, p5$lat), dogleg[2, ]))
check("gap: the path goes through the routed dogleg", geosphere::distHaversine(c(p5$lon[dog_pt], p5$lat[dog_pt]), dogleg[2, ]) < 1)
gp <- p5[p5$t >= 70 + 1.79e9 & p5$t <= 110 + 1.79e9, ]
lin <- (gp$t - gp$t[1]) / (gp$t[nrow(gp)] - gp$t[1]) - (gp$d - gp$d[1]) / (gp$d[nrow(gp)] - gp$d[1])
check(sprintf("gap: time linear in distance across the gap (max deviation %.2e)", max(abs(lin))), max(abs(lin)) < 1e-6)
check("gap: leading dropped sample becomes a dwell at the first matched point from its time",
      p5$t[1] == s5$t[1] && p5$d[2] == 0 && !is.unsorted(p5$t))
r5b <- road_timed_path(s5, m5, route_fn = function(points, profile) NULL)
check("gap: OSRM /route failure -> straight line, on_network = FALSE", isFALSE(r5b$on_network) && !is.unsorted(r5b$path$t))

# ---- 6. Stations ----
st_xy <- ll(c(0, 400, 1500), c(0, 0, 0))
stations <- data.frame(name = c("A", "B", "C"), lon = st_xy[, 1], lat = st_xy[, 2])
cands <- function(x, y) { p <- ll(x, y); d <- geosphere::distHaversine(cbind(stations$lon, stations$lat), p); cbind(stations, dist_m = d)[order(d), ] }
east <- 90
o <- ll(200, 0)
pick <- pick_station(cands(200, 0), o[1], o[2], east, "origin")
check(sprintf("origin 200 m past A heading east, B 200 m ahead: picks %s (not B)", pick$name), identical(pick$name, "A"))
o2 <- ll(550, 0)
check("origin 150 m past B: picks B", identical(pick_station(cands(550, 0), o2[1], o2[2], east, "origin")$name, "B"))
d1 <- ll(1300, 0)
check("destination 200 m before C heading east: picks C",
      identical(pick_station(cands(1300, 0), d1[1], d1[2], east, "destination")$name, "C"))
far <- ll(900, 900)
check("no station within 600 m: keeps the sample point (NULL)", is.null(pick_station(cands(900, 900), far[1], far[2], east, "origin")))

route <- ll(seq(0, 1500, by = 50), 0)
tr_o <- list(lon = o[1], lat = o[2], t = 1000)
tr_d <- list(lon = d1[1], lat = d1[2], t = 1300)
rt <- rail_timed_path(route, tr_o, tr_d)
v <- TH$rail_speed_kmh / 3.6
check(sprintf("t_start = origin - %.1f s (200 m at %d km/h); t_end = dest + %.1f s", 1000 - rt$t_start, TH$rail_speed_kmh, rt$t_end - 1300),
      abs((1000 - rt$t_start) - 200 / v) < 0.5 && abs((rt$t_end - 1300) - 200 / v) < 0.5)
check("rail path starts at the departure station at t_start and ends at the arrival station at t_end",
      abs(rt$path$t[1] - rt$t_start) < 1e-6 && abs(rt$path$t[nrow(rt$path)] - rt$t_end) < 1e-6 &&
        geosphere::distHaversine(c(rt$path$lon[1], rt$path$lat[1]), st_xy[1, ]) < 1)
long_route <- ll(seq(0, 4000, by = 50), 0)
o3 <- ll(3000, 0)
rt3 <- rail_timed_path(long_route, list(lon = o3[1], lat = o3[2], t = 5000), list(lon = long_route[81, 1], lat = long_route[81, 2], t = 5100))
check(sprintf("cap: 3000 m from the station -> adjustment %.0f s (cap %d s)", 5000 - rt3$t_start, TH$max_time_adjust_s),
      abs((5000 - rt3$t_start) - TH$max_time_adjust_s) < 1e-6)
rt4 <- rail_timed_path(route, tr_o, tr_d, start_offset_s = -60, end_offset_s = 30)
check("fine adjustments shift t_start/t_end", abs(rt4$t_start - (rt$t_start - 60)) < 1e-6 && abs(rt4$t_end - (rt$t_end + 30)) < 1e-6 &&
        !is.unsorted(rt4$path$t))
rt5 <- rail_timed_path(route, tr_o, tr_d, start_offset_s = 120)
check("adjustment past the origin sample time keeps times monotonic", !is.unsorted(rt5$path$t) && rt5$path$t[1] == rt5$t_start)

# ---- 7. Multi-track GPX ----
accepted <- list(
  T1 = list(activity = "metro", path = rt$path),
  T2 = list(activity = "car", path = transform(p4, t = t))
)
accepted$T1$path$t <- accepted$T1$path$t + 1.79e9
inp <- review_gpx_input(accepted)
gpx <- build_gpx(inp$items, inp$samples, "2026-10-11T12:00:00Z")
doc <- tryCatch(xml2::read_xml(paste(gpx, collapse = "\n")), error = function(e) NULL)
ns <- if (!is.null(doc)) xml2::xml_ns(doc)
types <- if (!is.null(doc)) xml2::xml_text(xml2::xml_find_all(doc, "//d1:trk/d1:type", ns))
names_ <- if (!is.null(doc)) xml2::xml_text(xml2::xml_find_all(doc, "//d1:trk/d1:name", ns))
check(sprintf("batch GPX valid, %d <trk>, types %s, names %s", length(types), paste(types, collapse = "/"), paste(names_, collapse = " | ")),
      !is.null(doc) && length(types) == 2 && setequal(types, c("metro", "car")) && all(grepl("^(Metro|Car) \\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}$", names_)))
fn <- review_batch_filename(accepted)
check(sprintf("batch filename %s", fn), grepl("^arc_batch_car-metro_\\d{4}-\\d{2}-\\d{2}_\\d{4}-\\d{2}-\\d{2}\\.gpx$", fn))

# ---- 8. Secondary metrics ----
s8 <- samples_at(c(0, 100, 101, 300), c(0, 0, 0, 0), c(0, 10, 11, 31))   # 1 m in 1 s ignored (< 5 s apart)
m8 <- trip_secondary_metrics(s8)
check(sprintf("max speed ignores pairs < 5 s apart: %.1f km/h (36 expected)", m8$max_speed_kmh), abs(m8$max_speed_kmh - 36) < 0.5)

# ---- 9. Detour guard and bulk criteria ----
ok_p <- list(method = "match", on_network = TRUE, len_ratio = 1.05, rematch_off_share = 0, recorded_km = 6)
st <- function(...) road_proposal_status(modifyList(ok_p, list(...)))
check("status: 1.05x, on the roads -> bulk ok, no detour", isTRUE(st()$bulk_ok) && isFALSE(st()$detour))
check("detour: 1.26x flagged, 1.25x not", isTRUE(st(len_ratio = 1.26)$detour) && isFALSE(st(len_ratio = 1.25)$detour))
check("bulk: 0.90x and 1.20x in, 0.89x and 1.21x out",
      isTRUE(st(len_ratio = 0.90)$bulk_ok) && isTRUE(st(len_ratio = 1.20)$bulk_ok) &&
        isFALSE(st(len_ratio = 0.89)$bulk_ok) && isFALSE(st(len_ratio = 1.21)$bulk_ok))
check("bulk: re-match off share 1% in, 2% out, NA out",
      isTRUE(st(rematch_off_share = 0.01)$bulk_ok) && isFALSE(st(rematch_off_share = 0.02)$bulk_ok) &&
        isFALSE(st(rematch_off_share = NA_real_)$bulk_ok))
check("bulk: no confidence condition; route fallback / via / straight-line gaps out",
      isTRUE(st(confidence = 0)$bulk_ok) && isTRUE(st(method = "match+gaps")$bulk_ok) &&
        isFALSE(st(method = "route_fallback")$bulk_ok) && isFALSE(st(method = "via")$bulk_ok) && isFALSE(st(on_network = FALSE)$bulk_ok))

xs9 <- seq(0, 2000, by = 100)
s9 <- samples_at(xs9, rep(0, length(xs9)), xs9 / 10 + 1.79e9)
same_match <- function(coords, timestamps, radiuses, profile) {
  list(ok = TRUE, pieces = list(list(coords = coords, idx = seq_len(nrow(coords)))), tracepoints = coords,
       sent = rep(TRUE, nrow(coords)), confidence = 0, n_dropped = 0L)
}
mk_fx <- function(x, y) { p <- ll(x, y); list(path = data.frame(lon = p[, 1], lat = p[, 2], t = seq_along(x) * 5 + 1.79e9),
                                              method = "match", on_network = TRUE) }
c9 <- road_proposal_check(mk_fx(xs9, 0 * xs9), s9, "car", match_fn = same_match)
check(sprintf("check: straight proposal %.2f km vs %.2f km recorded -> %.2fx, re-match %.0f%% off, bulk %s",
              c9$proposed_km, c9$recorded_km, c9$len_ratio, 100 * c9$rematch_off_share, c9$bulk_ok),
      abs(c9$len_ratio - 1) < 1e-3 && c9$rematch_off_share == 0 && isTRUE(c9$bulk_ok))
spur_x <- c(xs9[1:11], 1000, 1000, xs9[11:21]); spur_y <- c(rep(0, 11), 300, 0, rep(0, 11))  # 600 m spur at x = 1000
c9b <- road_proposal_check(mk_fx(spur_x, spur_y), s9, "car", match_fn = same_match)
check(sprintf("check: proposal with a 600 m spur -> %.2fx, detour %s, bulk %s", c9b$len_ratio, c9b$detour, c9b$bulk_ok),
      abs(c9b$len_ratio - 1.3) < 0.01 && isTRUE(c9b$detour) && isFALSE(c9b$bulk_ok))
half_off <- function(coords, timestamps, radiuses, profile) {  # the re-match lands 100 m away on the second half
  g <- ll(xs9, ifelse(xs9 > 1000, 100, 0))
  list(ok = TRUE, pieces = list(list(coords = g, idx = seq_len(nrow(coords)))), tracepoints = coords,
       sent = rep(TRUE, nrow(coords)), confidence = 0.9, n_dropped = 0L)
}
c9c <- road_proposal_check(mk_fx(xs9, 0 * xs9), s9, "car", match_fn = half_off)
check(sprintf("check: re-match strays on half the path -> %.0f%% off, bulk %s", 100 * c9c$rematch_off_share, c9c$bulk_ok),
      c9c$rematch_off_share > 0.4 && isFALSE(c9c$bulk_ok))

# ---- 9b. Junk = path length; fragments and proposals under 0.9x never in bulk ----
# Round trip: 3 km out and back along the same road, start = end
xs_rt <- c(seq(0, 3000, by = 100), seq(2900, 0, by = -100))
s_rt <- samples_at(xs_rt, c(rep(0, 31), rep(8, 30)), seq_along(xs_rt) * 12 + 1.79e9)
m_rt <- trip_secondary_metrics(s_rt)
check(sprintf("round trip: straight %.2f km, path %.2f km -> not a fragment (old rule would flag it)", m_rt$straight_km, m_rt$path_km),
      m_rt$straight_km < 0.3 && m_rt$path_km > 5 && !"fragment" %in% codes(modifyList(base_m, m_rt), "car"))
s_fr <- samples_at(c(0, 60, 120, 180, 240, 300, 360, 420), c(0, 30, 0, 30, 0, 30, 0, 30), (1:8) * 20 + 1.79e9)
m_fr <- trip_secondary_metrics(s_fr)
check(sprintf("fragment from samples: path %.2f km -> %s", m_fr$path_km, paste(codes(modifyList(base_m, m_fr), "taxi"), collapse = ",")),
      m_fr$path_km < 0.5 && identical(codes(modifyList(base_m, m_fr), "taxi"), "fragment"))
check(sprintf("reasons text: \"%s\" / \"%s\"", format_reasons(review_flags(with(path_km = 0.3), "car"), "en"),
              format_reasons(review_flags(with(path_km = 0.3), "car"), "pt")),
      identical(format_reasons(review_flags(with(path_km = 0.3), "car"), "en"), "fragment: 0.30 km recorded") &&
        identical(format_reasons(review_flags(with(path_km = 0.3), "car"), "pt"), "fragmento: 0.30 km gravados"))
check("fragment never in bulk: 0.4 km recorded, 1.00x, on the roads -> bulk FALSE",
      isFALSE(st(recorded_km = 0.4, len_ratio = 1)$bulk_ok) && isTRUE(st(recorded_km = 0.5, len_ratio = 1)$bulk_ok))
check("under 0.9x: 0.89x short (skips part?) and out of bulk; 0.90x neither short nor out; 1.05x not short",
      isTRUE(st(len_ratio = 0.89)$short) && isFALSE(st(len_ratio = 0.89)$bulk_ok) &&
        isFALSE(st(len_ratio = 0.90)$short) && isTRUE(st(len_ratio = 0.90)$bulk_ok) && isFALSE(st()$short) &&
        isTRUE(TH$road$short_ratio == 0.9))
c9v <- road_proposal_check(modifyList(mk_fx(xs9[1:14], 0 * xs9[1:14]), list(method = "via")), s9, "car", match_fn = same_match)
check(sprintf("via candidate at %.2fx: short %s, detour %s, bulk %s", c9v$len_ratio, c9v$short, c9v$detour, c9v$bulk_ok),
      abs(c9v$len_ratio - 0.65) < 0.01 && isTRUE(c9v$short) && isFALSE(c9v$detour) && isFALSE(c9v$bulk_ok))

# The inventory columns from a cache (no samples loaded: every trip is cached)
mk_trip <- function(id) data.frame(id = id, last_saved = "2026-09-01T00:00:00Z", activity = "car", already_fixed = FALSE,
                                   start_utc = as.POSIXct("2026-09-01 12:00", tz = "UTC"), stringsAsFactors = FALSE)
trips_b <- rbind(mk_trip("FRAG"), mk_trip("ROUND"), mk_trip("SHORT"), mk_trip("OK"))
summ <- function(ratio, rec) list(method = "match", on_network = TRUE, proposed_km = ratio * rec, recorded_km = rec,
                                   len_ratio = ratio, rematch_off_share = 0, rematch_longest_off_m = 0)
met <- function(path, straight, off) modifyList(base_m, list(path_km = path, straight_km = straight, off_share = off))
entries <- list(FRAG = list(met(0.3, 0.2, 0.3), summ(1.0, 0.3)), ROUND = list(met(7.6, 0.01, 0.2), summ(0.99, 7.6)),
                SHORT = list(met(3.5, 1.9, 0.2), summ(0.67, 3.5)), OK = list(met(5, 4, 0.02), NULL))
cache_b <- list()
for (i in seq_len(nrow(trips_b))) {
  e <- entries[[trips_b$id[i]]]
  cache_b[[review_cache_key(trips_b[i, ])]] <- list(metrics = e[[1]], match = NULL, proposal = e[[2]])
}
cf_b <- tempfile(fileext = ".rds"); saveRDS(cache_b, cf_b)
rb <- review_trips(trips_b, cache_file = cf_b)
show <- function(nm) paste(rb$id[rb[[nm]]], collapse = ",")
check(sprintf("inventory: flagged %s | fragment %s | skips part %s | bulk %s", show("flagged"), show("fragment"),
              show("short_proposal"), show("bulk_ok")),
      identical(show("flagged"), "FRAG,ROUND,SHORT") && identical(show("fragment"), "FRAG") &&
        identical(show("short_proposal"), "SHORT") && identical(show("bulk_ok"), "ROUND"))

# ---- 10. Re-route through the recorded path (via waypoints) ----
xs10 <- seq(0, 3000, by = 50)
t10 <- 1.79e9 + c(xs10[xs10 <= 1500] / 10, 150 + (xs10[xs10 > 1500] - 1500) / 5)  # 10 m/s, then 5 m/s
s10 <- samples_at(xs10, rep(0, length(xs10)), t10)
s10$acc[xs10 == 300] <- 80                                         # a poor sample
wp10 <- via_waypoints(s10, 1L, nrow(s10), 300)
gap10 <- geosphere::distHaversine(cbind(s10$lon[wp10[-length(wp10)]], s10$lat[wp10[-length(wp10)]]),
                                  cbind(s10$lon[wp10[-1]], s10$lat[wp10[-1]]))
check(sprintf("waypoints: %d, spacing %.0f-%.0f m, ends kept, poor sample at 300 m skipped (next: %.0f m)",
              length(wp10), min(gap10), max(gap10), xs10[wp10[2]]),
      wp10[1] == 1 && wp10[length(wp10)] == nrow(s10) && all(gap10[-length(gap10)] >= 300) && min(gap10) >= 150 &&
        !(which(xs10 == 300) %in% wp10) && xs10[wp10[2]] == 350)

via_calls <- list()
fake_via <- function(coords, profile, bearings = NULL, radiuses = NULL) {
  via_calls[[length(via_calls) + 1L]] <<- list(coords = coords, bearings = bearings, radiuses = radiuses)
  geom <- coords[1, , drop = FALSE]
  legs <- numeric(0)
  for (k in 2:nrow(coords)) {
    seg <- coords[k, , drop = FALSE]
    # A waypoint snapped onto the wrong carriageway: a 1 km loop to reach it
    if (abs(coords[k, 1] - ll(1500, 0)[1]) < 1e-7) {
      a <- coords[k - 1, ]; b <- coords[k, ]
      up <- ll(c(0, 0), c(500, 500))[, 2] - ll(0, 0)[2]
      seg <- rbind(c(a[1], a[2] + up[1]), c(b[1], b[2] + up[1]), b)
    }
    leg <- rbind(geom[nrow(geom), ], seg)
    legs <- c(legs, sum(geosphere::distHaversine(leg[-nrow(leg), , drop = FALSE], leg[-1, , drop = FALSE])))
    geom <- rbind(geom, seg)
  }
  list(coords = geom, legs_m = legs, distance_m = sum(legs))
}
v10 <- via_timed_path(s10, 3L, nrow(s10) - 2L, route_via_fn = fake_via)
p10 <- v10$path
last_call <- via_calls[[length(via_calls)]]
check(sprintf("via: %d calls, loop around the waypoint at 1500 m pruned (%d dropped), final route %.0f m",
              length(via_calls), v10$n_pruned, max(path_cumulative_m(cbind(p10$lon, p10$lat)))),
      length(via_calls) == 2 && v10$n_pruned == 2 && !any(abs(last_call$coords[, 1] - ll(1500, 0)[1]) < 1e-7) &&
        abs(max(path_cumulative_m(cbind(p10$lon, p10$lat))) - 2800) < 5)  # samples 3 .. n-2: 100 m .. 2900 m
check(sprintf("via: bearings east inside (%.0f-%.0f), none at the ends; radius 100 m inside",
              min(last_call$bearings, na.rm = TRUE), max(last_call$bearings, na.rm = TRUE)),
      is.na(last_call$bearings[1]) && is.na(last_call$bearings[length(last_call$bearings)]) &&
        all(abs(last_call$bearings[2:(length(last_call$bearings) - 1)] - 90) < 1) &&
        all(last_call$radiuses[2:(length(last_call$radiuses) - 1)] == 100) && is.na(last_call$radiuses[1]))
wp_t <- vapply(seq_len(nrow(last_call$coords)), function(k) {
  j <- which.min(geosphere::distHaversine(cbind(p10$lon, p10$lat), last_call$coords[k, ]))
  p10$t[j]
}, numeric(1))
wp_idx <- match(round(last_call$coords[, 1], 9), round(s10$lon, 9))
check(sprintf("via: each waypoint keeps its sample time (max error %.2e s); speed change kept", max(abs(wp_t[-1] - s10$t[wp_idx][-1]))),
      max(abs(wp_t[-1] - s10$t[wp_idx][-1])) < 1e-6)
check(sprintf("via: dwell at the start from the first sample (%s), ends at the last sample time, monotonic",
              format(p10$t[1] - s10$t[1])),
      p10$t[1] == s10$t[1] && p10$t[nrow(p10)] == s10$t[nrow(s10)] && !is.unsorted(p10$t))
dd <- diff(path_cumulative_m(cbind(p10$lon, p10$lat)))
check(sprintf("via: density max step %.0f m / %.1f s", max(dd), max(diff(p10$t))), max(dd) <= 100.5 && max(diff(p10$t)) <= 10.01)
check("via: OSRM failure -> NULL", is.null(via_timed_path(s10, 1L, nrow(s10), route_via_fn = function(...) NULL)))

# ---- 11. Rail timing from Google's steps ----
enc_num <- function(v) {
  v <- as.integer(v)
  v <- if (v < 0) bitwNot(bitwShiftL(v, 1L)) else bitwShiftL(v, 1L)
  out <- ""
  while (v >= 32L) { out <- paste0(out, intToUtf8(bitwOr(32L, bitwAnd(v, 31L)) + 63L)); v <- bitwShiftR(v, 5L) }
  paste0(out, intToUtf8(v + 63L))
}
encode_polyline <- function(m) {
  la <- round(m[, 2] * 1e5); lo <- round(m[, 1] * 1e5)
  paste0(mapply(function(a, b) paste0(enc_num(a), enc_num(b)), diff(c(0, la)), diff(c(0, lo))), collapse = "")
}
G0 <- 1.80e9                                   # Google's (future) timetable
leg1 <- ll(seq(0, 6000, by = 200), 0)           # Linha 2: 6 km east, 10 min
walk <- ll(c(6000, 6000), c(0, 60))             # 60 m between platforms, 1 min
leg2 <- ll(6000, seq(60, 3060, length.out = 16)) # Linha 1: 3 km north, 5 min
g_transit <- function(m, line, from, to, dep, arr, dur, times = TRUE, with_dur = TRUE) {
  td <- list(line = list(name = line, short_name = sub("Linha ", "", line), vehicle = list(type = "SUBWAY")),
             departure_stop = list(name = from), arrival_stop = list(name = to))
  if (times) { td$departure_time <- list(value = dep); td$arrival_time <- list(value = arr) }
  st <- list(travel_mode = "TRANSIT", polyline = list(points = encode_polyline(m)),
             distance = list(value = round(sum(geosphere::distHaversine(m[-nrow(m), ], m[-1, ])))), transit_details = td)
  if (with_dur) st$duration <- list(value = dur)
  st
}
g_walk <- function(m, dur) list(travel_mode = "WALKING", polyline = list(points = encode_polyline(m)),
                                duration = list(value = dur), distance = list(value = 60))
g_route <- function(times = TRUE, with_dur = TRUE) list(legs = list(list(steps = list(
  g_walk(ll(c(-30, 0), c(0, 0)), 30),
  g_transit(leg1, "Linha 2", "Botafogo", "Estácio", G0, G0 + 600, 600, times, with_dur),
  g_walk(walk, 60),
  g_transit(leg2, "Linha 1", "Estácio", "Uruguai", G0 + 840, G0 + 1140, 300, times, with_dur),
  g_walk(ll(c(6000, 6000), c(3060, 3100)), 40)))))
steps <- transit_route_steps(g_route(), GOOGLE_ACCEPTED_VEHICLE_TYPES$subway)
check(sprintf("parser: kinds %s; leg 1 %s %s→%s dep %+.0f arr %+.0f; polyline round trip %.1e°",
              paste(vapply(steps, `[[`, "", "kind"), collapse = "/"), steps[[2]]$line, steps[[2]]$from_stop, steps[[2]]$to_stop,
              steps[[2]]$departure_t - G0, steps[[2]]$arrival_t - G0, max(abs(steps[[2]]$coords - leg1))),
      identical(vapply(steps, `[[`, "", "kind"), c("walk", "transit", "walk", "transit", "walk")) &&
        identical(steps[[2]]$line, "Linha 2") && steps[[4]]$departure_t == G0 + 840 && max(abs(steps[[2]]$coords - leg1)) < 1e-5)

# The route mode's output is unchanged: old and new parser on the same (mocked) response
key_before <- Sys.getenv("GOOGLE_MAPS_API_KEY")
Sys.setenv(GOOGLE_MAPS_API_KEY = "unit-test")
mock_get <- function(params) list(status = "OK", routes = list(g_route()))
google_directions_get <- mock_get
# OLD_GOOGLE_UTILS: a copy of utils_google.R from before the step timing (optional)
old_file <- Sys.getenv("OLD_GOOGLE_UTILS")
pts <- data.frame(lat = c(leg1[1, 2], leg2[16, 2]), lng = c(leg1[1, 1], leg2[16, 1]))
r_new <- suppressMessages(calculate_google_transit_route(pts, "subway"))
r_old <- r_new
if (nzchar(old_file) && file.exists(old_file)) {
  old_env <- new.env()
  sys.source(old_file, envir = old_env)
  assign("google_directions_get", mock_get, envir = old_env)
  r_old <- suppressMessages(old_env$calculate_google_transit_route(pts, "subway"))
}
Sys.setenv(GOOGLE_MAPS_API_KEY = key_before)
check(sprintf("route mode%s: coords %d x 2 identical %s, %.0f m / %.0f s, + %d steps",
              if (nzchar(old_file) && file.exists(old_file)) " unchanged vs the old parser" else "",
              nrow(r_new$coords), identical(r_new$coords, r_old$coords), r_new$distance_m, r_new$duration_s, length(r_new$steps)),
      nrow(r_new$coords) > 2 && identical(r_new$coords, r_old$coords) && identical(r_new$distance_m, r_old$distance_m) &&
        identical(r_new$duration_s, r_old$duration_s) && length(r_new$steps) == 5)

segs <- rail_segments(steps)
check(sprintf("segments: %s; transfer at %s: walk %.0f s + wait %.0f s",
              paste(vapply(segs, `[[`, "", "kind"), collapse = "/"), segs[[2]]$stop, segs[[2]]$move_s, segs[[2]]$wait_s),
      identical(vapply(segs, `[[`, "", "kind"), c("leg", "transfer", "leg")) && segs[[2]]$move_s == 60 && segs[[2]]$wait_s == 180)

R0 <- 1.75e9                                    # the real trip
route_c <- r_new$coords
at <- function(m, i) list(lon = m[i, 1], lat = m[i, 2])
stop_pt <- steps[[4]]$coords[1, ]                # Estácio, Linha 1 platform
transfer_stats <- function(p) {
  near <- geosphere::distHaversine(cbind(p$lon, p$lat), colMeans(rbind(steps[[2]]$coords[nrow(leg1), ], stop_pt))) <= 70
  moved <- geosphere::distHaversine(cbind(p$lon[-nrow(p)], p$lat[-nrow(p)]), cbind(p$lon[-1], p$lat[-1]))
  still <- moved < 0.01 & diff(p$t) > 0
  list(window = diff(range(p$t[near])), still_s = sum(diff(p$t)[still]),
       still_pts = sum(geosphere::distHaversine(cbind(p$lon, p$lat), stop_pt) < 0.01),
       arrive = min(p$t[geosphere::distHaversine(cbind(p$lon, p$lat), steps[[2]]$coords[nrow(leg1), ]) < 0.5]))
}
o_s <- c(at(route_c, 1), t = R0); d_s <- c(at(route_c, nrow(route_c)), t = R0 + 1140)
r11 <- rail_timed_path(route_c, o_s, d_s, segments = segs)
ts <- transfer_stats(r11$path)
check(sprintf("two legs, 4-min transfer: t_start %+.6f / t_end %+.6f s off; scale %.2f; arrives at Estácio +%.0f s",
              r11$path$t[1] - R0, r11$path$t[nrow(r11$path)] - (R0 + 1140), r11$scale, ts$arrive - R0),
      r11$timing == "steps" && r11$path$t[1] == R0 && r11$path$t[nrow(r11$path)] == R0 + 1140 && abs(ts$arrive - (R0 + 600)) < 1e-6 &&
        !is.unsorted(r11$path$t))
check(sprintf("transfer: %.0f s within 70 m of Estácio, %.0f s fully still (wait) on %d points, walk 60 m in between",
              ts$window, ts$still_s, ts$still_pts),
      ts$window >= 240 && ts$window <= 260 && abs(ts$still_s - 180) < 1e-6 && ts$still_pts == 3)
dd <- diff(r11$path$t)[geosphere::distHaversine(cbind(r11$path$lon[-1], r11$path$lat[-1]), cbind(r11$path$lon[-nrow(r11$path)], r11$path$lat[-nrow(r11$path)])) > 0.01]
check(sprintf("legs: moving points at most %.1f s apart (10 s rule kept outside the wait)", max(dd)), max(dd) <= 10 + 1e-6)

r11b <- rail_timed_path(route_c, o_s, modifyList(d_s, list(t = R0 + 912)), segments = segs)
tsb <- transfer_stats(r11b$path)
check(sprintf("trip 20%% faster than Google: scale %.2f, wait %.0f s, t_start/t_end exact", r11b$scale, tsb$still_s),
      abs(r11b$scale - 0.8) < 1e-9 && abs(tsb$still_s - 144) < 1e-6 && r11b$path$t[1] == R0 && r11b$path$t[nrow(r11b$path)] == R0 + 912)
r11c <- rail_timed_path(route_c, o_s, d_s, start_offset_s = -60, end_offset_s = 30, segments = segs)
check(sprintf("±time controls: start %+.0f s, end %+.0f s, still exact; wait %.0f s",
              r11c$path$t[1] - R0, r11c$path$t[nrow(r11c$path)] - (R0 + 1140), transfer_stats(r11c$path)$still_s),
      r11c$path$t[1] == R0 - 60 && r11c$path$t[nrow(r11c$path)] == R0 + 1170 &&
        abs(transfer_stats(r11c$path)$still_s - 180 * 1230 / 1140) < 1e-6)
o200 <- c(at(route_c, 2), t = R0 + 20)          # first sample 200 m past the departure station
r11d <- rail_timed_path(route_c, o200, d_s, segments = segs)
check(sprintf("station snapping: sample 200 m in -> t_start = sample - %.1f s (35 km/h), path starts there", R0 + 20 - r11d$t_start),
      abs((R0 + 20 - r11d$t_start) - 200 / (TH$rail_speed_kmh / 3.6)) < 0.5 && r11d$path$t[1] == r11d$t_start)
check(sprintf("legs text: \"%s\" | \"%s\"", format_rail_legs(r11$legs, "en"), format_rail_legs(r11$legs, "pt")),
      identical(format_rail_legs(r11$legs, "en"), "Linha 2 · 10 min → transfer Estácio 4 min → Linha 1 · 5 min") &&
        identical(format_rail_legs(r11$legs, "pt"), "Linha 2 · 10 min → baldeação Estácio 4 min → Linha 1 · 5 min"))
check(sprintf("Google tooltip: \"%s\"", format_rail_legs(r11$legs, "en", google = TRUE)),
      grepl("transfer Estácio 4 min (walk 1 min, wait 3 min)", format_rail_legs(r11$legs, "en", google = TRUE), fixed = TRUE))

notimes <- rail_segments(transit_route_steps(g_route(times = FALSE), GOOGLE_ACCEPTED_VEHICLE_TYPES$subway))
check(sprintf("no timetable: transfer = walk only (%.0f s + %.0f s wait), legs from step durations", notimes[[2]]$move_s, notimes[[2]]$wait_s),
      notimes[[2]]$move_s == 60 && notimes[[2]]$wait_s == 0 && notimes[[1]]$move_s == 600 && isFALSE(notimes[[2]]$timetable))
nodur <- transit_route_steps(g_route(times = FALSE, with_dur = FALSE), GOOGLE_ACCEPTED_VEHICLE_TYPES$subway)
s11 <- data.frame(lon = c(route_c[1, 1], route_c[nrow(route_c), 1]), lat = c(route_c[1, 2], route_c[nrow(route_c), 2]),
                  t = c(R0, R0 + 1140), acc = 10)
plan11 <- list(origin_idx = 1L, dest_idx = 2L)
fx_steps <- rail_fix(plan11, r_new[c("coords", "steps")], s11, "metro")
fx_nodur <- rail_fix(plan11, list(coords = route_c, steps = nodur), s11, "metro")
fx_matrix <- rail_fix(plan11, route_c, s11, "metro")
check(sprintf("rail_fix timing: with steps %s, steps without durations %s (no segments: %s), bare coords %s",
              fx_steps$timing, fx_nodur$timing, is.null(rail_segments(nodur)), fx_matrix$timing),
      fx_steps$timing == "steps" && fx_nodur$timing == "distance" && is.null(rail_segments(nodur)) && fx_matrix$timing == "distance" &&
        fx_nodur$path$t[1] == R0 && fx_nodur$path$t[nrow(fx_nodur$path)] == R0 + 1140)

# ---- 12. Multimodal split: walk -> wait -> bus / rail -> walk / visit / as is ----
# Home (0,0) at T0; recorded walk east to the stop (300,0) at +270, standing at
# the stop until the Jaé tap at +600 (tap point (300,5)); bus east to (3300,0);
# alighting sample at +1200; recorded walk north to the destination (3300,200),
# trip end +1500.
T0 <- 1.75e9
stub_route <- function(points, profile) {
  coords <- cbind(lon = points$lng, lat = points$lat)
  d <- path_cumulative_m(coords)[2]
  list(coords = coords, distance_m = d, duration_s = d / if (profile == "foot") 1.4 else 8)
}
stub_match <- function(coords, timestamps, radiuses, profile) {
  colnames(coords) <- c("lon", "lat")
  list(ok = TRUE, pieces = list(list(coords = coords, idx = seq_len(nrow(coords)))), tracepoints = coords,
       sent = rep(TRUE, nrow(coords)), confidence = 0.9, n_dropped = 0L)
}
fail_match <- function(...) list(ok = FALSE, pieces = list(), confidence = NA_real_, n_dropped = 0L)
no_route <- function(...) stop("OSRM must not be called")
mm_x <- c(0, 100, 200, 300, 300, 300, 300, seq(600, 3300, by = 300), 3300, 3300, 3300)
mm_y <- c(0, 0, 0, 0, 3, -2, 1, rep(0, 10), 100, 200, 200)
mm_t <- T0 + c(0, 90, 180, 270, 360, 450, 540, seq(660, 1200, by = 60), 1350, 1440, 1500)
trip_mm <- samples_at(mm_x, mm_y, mm_t)
stop_ll <- ll(300, 5)
board <- list(lon = stop_ll[1, 1], lat = stop_ll[1, 2], t = T0 + 600, label = "Ponto teste", line = "371")
alight <- ll(3290, 15)[1, ]                       # click 18 m from the alighting sample (3300,0) at +1200
seg_roles <- function(segs) vapply(segs, `[[`, "", "role")
seg_types <- function(segs) vapply(segs, `[[`, "", "type")
# Continuity: starts at the first sample, each segment starts when the previous
# ends, whole-second times strictly increasing inside each, from t_start to t_end
chain_ok <- function(segs, t_first, t_last) {
  st <- vapply(segs, `[[`, 0, "t_start"); en <- vapply(segs, `[[`, 0, "t_end")
  paths <- all(vapply(segs, function(g) {
    t <- g$path$t
    nrow(g$path) >= 2 && all(diff(t) > 0) && all(t == round(t)) && t[1] == g$t_start && t[length(t)] == g$t_end
  }, logical(1)))
  st[1] == t_first && all(st[-1] == en[-length(en)]) && en[length(en)] == t_last && all(en > st) && paths
}
same_xy <- function(p, s, rows) isTRUE(all.equal(p$lon, s$lon[rows])) && isTRUE(all.equal(p$lat, s$lat[rows]))

# Arrival rule
s_mm <- review_clean_samples(trip_mm)
check(sprintf("arrival: FIRST sample within 40 m before the tap (+%.0f s), not the nearest (+360 s, 2 m)",
              s_mm$t[multimodal_arrival_index(s_mm, multimodal_point(board))] - T0),
      multimodal_arrival_index(s_mm, multimodal_point(board)) == 4)
far <- modifyList(board, list(lon = ll(300, 200)[1, 1], lat = ll(300, 200)[1, 2]))
check(sprintf("arrival: none within 40 m -> the sample nearest the tap point before the tap (+%.0f s, y = 3)",
              s_mm$t[multimodal_arrival_index(s_mm, multimodal_point(far))] - T0),
      multimodal_arrival_index(s_mm, multimodal_point(far)) == 5)
ahead <- modifyList(board, list(lon = ll(600, 0)[1, 1], lat = ll(600, 0)[1, 2]))
check(sprintf("arrival: a sample on the tap point AFTER the tap is not used (+%.0f s, not +660 s)",
              s_mm$t[multimodal_arrival_index(s_mm, multimodal_point(ahead))] - T0),
      multimodal_arrival_index(s_mm, multimodal_point(ahead)) == 4)

# Default: recorded walks (OSRM foot never called for them), after = walk
segs <- build_multimodal_segments(trip_mm, board, alight, route_fn = no_route, match_fn = stub_match)
check(sprintf("segments: %s / types %s", paste(seg_roles(segs), collapse = " > "), paste(seg_types(segs), collapse = ",")),
      identical(seg_roles(segs), c("walk_to_stop", "wait", "bus", "walk_from_stop")) &&
        identical(seg_types(segs), c("walking", "stationary", "bus", "walking")))
check(sprintf("walk to the stop = recorded samples 1-4 as they are (%d points, +0..+%.0f s, %s)", nrow(segs[[1]]$path),
              segs[[1]]$t_end - T0, segs[[1]]$info$method),
      same_xy(segs[[1]]$path, trip_mm, 1:4) && all(segs[[1]]$path$t == mm_t[1:4]) && segs[[1]]$info$method == "recorded")
w <- segs[[2]]
check(sprintf("wait: arrival (+%.0f s) -> tap (+%.0f s) at the tap point, a point every 30 s (%d points, max gap %.0f s)",
              w$t_start - T0, w$t_end - T0, nrow(w$path), max(diff(w$path$t))),
      w$t_start == T0 + 270 && w$t_end == board$t && all(w$path$lon == board$lon & w$path$lat == board$lat) &&
        max(diff(w$path$t)) <= 30)
b3 <- segs[[3]]
check(sprintf("bus: tap -> alighting sample (+%.0f s), ends ON the sample (click snapped), %s, named '%s'", b3$t_end - T0,
              b3$info$method, b3$name),
      b3$t_start == board$t && b3$t_end == T0 + 1200 && b3$path$lon[nrow(b3$path)] == trip_mm$lon[17] &&
        b3$path$lat[nrow(b3$path)] == trip_mm$lat[17] && b3$info$method == "match" && b3$name == "Bus 371")
check(sprintf("walk from the stop = recorded samples 17-20 (alighting -> end), %d points", nrow(segs[[4]]$path)),
      same_xy(segs[[4]]$path, trip_mm, 17:20) && all(segs[[4]]$path$t == mm_t[17:20]) && segs[[4]]$type == "walking")
check("after = walk: continuous chain, first sample -> trip end, monotonic whole seconds", chain_ok(segs, T0, T0 + 1500))

# after = visit: stationary at the last sample until the trip's end
segs_v <- build_multimodal_segments(trip_mm, board, alight, "visit", route_fn = no_route, match_fn = stub_match)
v <- segs_v[[length(segs_v)]]
check(sprintf("after = visit: %s/%s +%.0f..+%.0f s at the last sample, %d points; chain ok", v$role, v$type,
              v$t_start - T0, v$t_end - T0, nrow(v$path)),
      v$role == "at_destination" && v$type == "stationary" && v$t_start == T0 + 1200 && v$t_end == T0 + 1500 &&
        all(v$path$lon == trip_mm$lon[20] & v$path$lat == trip_mm$lat[20]) && max(diff(v$path$t)) <= 30 &&
        chain_ok(segs_v, T0, T0 + 1500))

# after = none: the segments end at the alighting
segs_n <- build_multimodal_segments(trip_mm, board, alight, "none", route_fn = no_route, match_fn = stub_match)
check(sprintf("after = none: %s, ends at the alighting (+%.0f s); chain ok", paste(seg_roles(segs_n), collapse = " > "),
              segs_n[[length(segs_n)]]$t_end - T0),
      identical(seg_roles(segs_n), c("walk_to_stop", "wait", "bus")) && chain_ok(segs_n, T0, T0 + 1200))

# Trip starting at the stop: no walk; tap on a sample near the stop: no wait
board_at_start <- modifyList(board, list(lon = trip_mm$lon[1], lat = trip_mm$lat[1], t = T0 + 90))
segs_s <- build_multimodal_segments(trip_mm, board_at_start, alight, route_fn = no_route, match_fn = stub_match)
check(sprintf("trip starts at the stop: %s; chain ok", paste(seg_roles(segs_s), collapse = " > ")),
      identical(seg_roles(segs_s), c("wait", "bus", "walk_from_stop")) && chain_ok(segs_s, T0, T0 + 1500))
board_on_sample <- modifyList(board, list(lon = trip_mm$lon[4], lat = trip_mm$lat[4], t = T0 + 270))
segs_o <- build_multimodal_segments(trip_mm, board_on_sample, alight, route_fn = no_route, match_fn = stub_match)
check(sprintf("tap on the arrival sample: %s (no zero-length wait); chain ok", paste(seg_roles(segs_o), collapse = " > ")),
      identical(seg_roles(segs_o), c("walk_to_stop", "bus", "walk_from_stop")) && chain_ok(segs_o, T0, T0 + 1500))

# Manual fallback: OSRM foot walks (the old rule: arrival after the foot duration)
segs_f <- build_multimodal_segments(trip_mm, board, alight, "walk", walk_method = "osrm", route_fn = stub_route,
                                    match_fn = stub_match)
walk_s <- path_cumulative_m(rbind(c(trip_mm$lon[1], trip_mm$lat[1]), c(board$lon, board$lat)))[2] / 1.4
check(sprintf("walk_method = osrm: %s / %s, arrival +%.0f s (foot duration %.1f s, rounded); chain ok",
              segs_f[[1]]$info$method, segs_f[[4]]$info$method, segs_f[[1]]$t_end - T0, walk_s),
      segs_f[[1]]$info$method == "osrm_foot" && segs_f[[4]]$info$method == "osrm_foot" &&
        segs_f[[1]]$t_end == T0 + round(walk_s) && chain_ok(segs_f, T0, T0 + 1500))

# Matching fails: /route on the bus profile, same times
segs_r <- build_multimodal_segments(trip_mm, board, alight, "walk", route_fn = stub_route, match_fn = fail_match)
check(sprintf("no match: bus via %s, +%.0f..+%.0f s", segs_r[[3]]$info$method, segs_r[[3]]$t_start - T0, segs_r[[3]]$t_end - T0),
      segs_r[[3]]$info$method == "route" && segs_r[[3]]$t_start == T0 + 600 && segs_r[[3]]$t_end == T0 + 1200)

# A jae_boardings row works as the boarding
# (all the read_jae_boardings() columns: `$` would partially match t to tipo_transacao)
jrow <- read_jae_boardings(tempfile())[0, ]
jrow[1, "longitude"] <- board$lon; jrow[1, "latitude"] <- board$lat; jrow[1, "label"] <- "123 · Teste"
jrow[1, "linha_ou_estacao"] <- "123"; jrow[1, "tipo_transacao"] <- "Integral"; jrow[1, "profile"] <- "bus"
jrow$datetime_utc <- as.POSIXct(board$t, origin = "1970-01-01", tz = "UTC")
segs_j <- build_multimodal_segments(trip_mm, jrow, alight, route_fn = no_route, match_fn = stub_match)
check(sprintf("a Jaé row is accepted as the boarding: wait ends at the tap, bus '%s'", segs_j[[3]]$name),
      segs_j[[2]]$t_end == board$t && segs_j[[3]]$name == "Bus 123")
check("boarding outside the trip is an error",
      inherits(tryCatch(build_multimodal_segments(trip_mm, modifyList(board, list(t = T0 - 10)), alight, route_fn = no_route,
                                                  match_fn = stub_match), error = function(e) e), "error"))

# Alighting from the nearest sample ALONG the recording: home (1500,30) is 10 m
# from the click (1500,20) but before the boarding; the way back passes
# (1500,60), 40 m away; the outbound samples are at x = 1400 (+T 800) and
# x = 1700 (+T 860). The click projects onto the outbound leg at x = 1500, so
# the alighting sample is x = 1400.
ax <- c(1500, 900, 300, 300, 700, 1100, 1400, 1700, 2100, 2500, 2500, 2000, 1500, 1000)
ay <- c(30, 15, 0, 0, 0, 0, 0, 0, 0, 0, 60, 60, 60, 60)
at <- T0 + c(0, 300, 560, 600, 680, 740, 800, 860, 940, 1020, 1080, 1180, 1280, 1380)
trip_a <- samples_at(ax, ay, at)
click <- ll(1500, 20)[1, ]
map_nearest <- which.min(geosphere::distHaversine(cbind(trip_a$lon, trip_a$lat), click))
after_b <- 4:nrow(trip_a)
map_nearest_after <- after_b[which.min(geosphere::distHaversine(cbind(trip_a$lon, trip_a$lat)[after_b, ], click))]
segs_a <- build_multimodal_segments(trip_a, modifyList(board, list(lon = trip_a$lon[4], lat = trip_a$lat[4], t = T0 + 600)),
                                    click, "walk", route_fn = no_route, match_fn = stub_match)
bus_a <- Filter(function(g) g$role == "bus", segs_a)[[1]]
check(sprintf("alighting along the recording: +%.0f s (sample x=1400); map-nearest sample overall +%.0f s (home), after the boarding +%.0f s (way back)",
              bus_a$t_end - T0, at[map_nearest] - T0, at[map_nearest_after] - T0),
      bus_a$t_end == T0 + 800 && map_nearest == 1 && map_nearest_after == 13)

# Rail: recorded walk to the station, wait at the rail leg's start until its
# departure (+700, not the tap), then the rail leg
rail_path <- data.frame(lon = ll(c(310, 1500, 3000), c(0, 0, 0))[, 1], lat = ll(c(310, 1500, 3000), c(0, 0, 0))[, 2],
                        t = T0 + c(700.4, 1000, 1300.2))
rail_fx <- list(path = rail_path, t_start = T0 + 700.4, t_end = T0 + 1300.2, method = "google")
segs_rl <- build_rail_multimodal_segments(trip_mm, board, rail_fx, "metro", route_fn = no_route)
wr <- segs_rl[[2]]
check(sprintf("rail: %s / %s; wait +%.0f..+%.0f s at the rail leg's start; rail +%.0f..+%.0f s, 3 points kept",
              paste(seg_roles(segs_rl), collapse = " > "), paste(seg_types(segs_rl), collapse = ","), wr$t_start - T0,
              wr$t_end - T0, segs_rl[[3]]$t_start - T0, segs_rl[[3]]$t_end - T0),
      identical(seg_roles(segs_rl), c("walk_to_stop", "wait", "rail")) && identical(seg_types(segs_rl), c("walking", "stationary", "metro")) &&
        wr$t_start == T0 + 270 && wr$t_end == T0 + 700 && all(wr$path$lon == rail_path$lon[1]) &&
        nrow(segs_rl[[3]]$path) == 3 && segs_rl[[3]]$path$lon[1] == rail_path$lon[1] && chain_ok(segs_rl, T0, T0 + 1300))
check("rail: a tap after the rail leg's start is not usable (and is an error if forced)",
      !multimodal_rail_tap_ok(T0 + 750, rail_fx) && multimodal_rail_tap_ok(T0 + 600, rail_fx) &&
        inherits(tryCatch(build_rail_multimodal_segments(trip_mm, modifyList(board, list(t = T0 + 750)), rail_fx, "metro",
                                                         route_fn = no_route), error = function(e) e), "error"))

# Jaé gate tap vs the computed rail start: route (310,0) -> (3300,0); the first
# sample (x = 600, +660) is 290 m along, so the computed start is +660 - 290 m
# at 35 km/h = +630.2 s. Taps 1 s and 2 min after it are trusted (the leg
# starts at the tap + 60 s, the backward extrapolation shortened); 4 min after
# it is ignored; a tap before it waits for the computed start.
rroute <- ll(c(310, 3300), c(0, 0))
r_origin <- list(lon = trip_mm$lon[8], lat = trip_mm$lat[8], t = trip_mm$t[8])
r_dest <- list(lon = trip_mm$lon[17], lat = trip_mm$lat[17], t = trip_mm$t[17])
tp0 <- rail_timed_path(rroute, r_origin, r_dest)
base0 <- tp0$t_start
gate_case <- function(late_s) {
  tap_t <- base0 + late_s
  g <- multimodal_rail_gate(tap_t, base0)
  tp <- if (identical(g$action, "adjust")) rail_timed_path(rroute, r_origin, r_dest, start_at = g$start_at) else tp0
  list(g = g, tp = tp, tap_t = tap_t)
}
g1 <- gate_case(1); g120 <- gate_case(120); g240 <- gate_case(240); gm60 <- gate_case(-60)
check(sprintf("gate: computed start +%.1f s (extrapolated %.1f s back from the first sample)", base0 - T0, tp0$adj_start_s),
      abs(base0 - (T0 + 660 - 290 / (35 / 3.6))) < 0.5)
check(sprintf("gate 1 s after: %s, start +%.1f s = tap + %d s, backward extrapolation %.1f s -> %.1f s, path from it, increasing",
              g1$g$action, g1$tp$t_start - T0, REVIEW_THRESHOLDS$rail_gate_to_train_s, tp0$adj_start_s, g1$tp$adj_start_s),
      g1$g$action == "adjust" && g1$tp$t_start == g1$tap_t + 60 && g1$tp$path$t[1] == g1$tp$t_start &&
        g1$tp$adj_start_s < tp0$adj_start_s && all(diff(g1$tp$path$t) > 0))
check(sprintf("gate 2 min after: %s, start +%.1f s = tap + 60 s (after the first sample: extrapolation %.1f s), increasing",
              g120$g$action, g120$tp$t_start - T0, g120$tp$adj_start_s),
      g120$g$action == "adjust" && g120$tp$t_start == g120$tap_t + 60 && g120$tp$adj_start_s < 0 &&
        g120$tp$path$t[1] == g120$tp$t_start && all(diff(g120$tp$path$t) > 0))
check(sprintf("gate 4 min after: %s (more than %d s late: another trip)", g240$g$action, REVIEW_THRESHOLDS$rail_gate_max_late_s),
      g240$g$action == "ignore" && is.null(g240$g$start_at))
check(sprintf("gate 1 min before: %s (the wait runs until the computed start)", gm60$g$action), gm60$g$action == "before")
check("gate: exactly 3 min after is still trusted", identical(multimodal_rail_gate(base0 + 180, base0)$action, "adjust"))
tp_off <- rail_timed_path(rroute, r_origin, r_dest, start_offset_s = 30, start_at = g1$g$start_at)
check(sprintf("gate: manual offsets still apply on top (+30 s -> start +%.1f s)", tp_off$t_start - T0),
      tp_off$t_start == g1$tap_t + 60 + 30)
# Rail split with the 2 min case: wait until tap + 60 s, then the leg
fx120 <- c(g120$tp, list(method = "google"))
tap120 <- modifyList(board, list(t = g120$tap_t))
segs_g <- build_rail_multimodal_segments(trip_mm, tap120, fx120, "metro", route_fn = no_route)
check(sprintf("rail split, tap 2 min late: %s; wait +%.0f..+%.0f s, rail from +%.0f s; chain ok",
              paste(seg_roles(segs_g), collapse = " > "), segs_g[[2]]$t_start - T0, segs_g[[2]]$t_end - T0, segs_g[[3]]$t_start - T0),
      identical(seg_roles(segs_g), c("walk_to_stop", "wait", "rail")) && segs_g[[2]]$t_end == round(g120$tap_t + 60) &&
        segs_g[[3]]$t_start == round(g120$tap_t + 60) && chain_ok(segs_g, T0, round(g120$tp$t_end)))
check("real use keeps OpenFreeMap: MAP_RASTER_BASE is off unless the test launcher sets it", identical(MAP_RASTER_BASE, FALSE))

# Rail tap without a place: it stands at the rail leg's departure station
segs_np <- build_rail_multimodal_segments(trip_mm, modifyList(board, list(lon = NA_real_, lat = NA_real_)), rail_fx, "metro",
                                          route_fn = no_route)
check(sprintf("rail tap without a place: arrival by the departure station (+%.0f s); %s; chain ok",
              segs_np[[1]]$t_end - T0, paste(seg_roles(segs_np), collapse = " > ")),
      segs_np[[1]]$t_end == T0 + 270 && identical(seg_roles(segs_np), c("walk_to_stop", "wait", "rail")) &&
        chain_ok(segs_np, T0, T0 + 1300))

# Taps per trip: the activity's mode, inside the time range; bus taps need a place, rail taps don't
taps_df <- read_jae_boardings(tempfile())[0, ]
taps_df[1:4, "profile"] <- c("bus", "metro", "bus", "bus")
taps_df$datetime_utc <- as.POSIXct(T0 + c(600, 650, 2000, 700), origin = "1970-01-01", tz = "UTC")
taps_df$longitude <- c(board$lon, board$lon, board$lon, NA); taps_df$latitude <- c(board$lat, board$lat, board$lat, NA)
tt <- function(act) as.numeric(multimodal_taps(taps_df, act, T0, T0 + 1500)$datetime_utc) - T0
taps_df[5, "profile"] <- "metro"; taps_df$datetime_utc[5] <- as.POSIXct(T0 + 660, origin = "1970-01-01", tz = "UTC")
check(sprintf("taps: bus %s, car %s (bus taps, the one without a place left out), metro %s, train %s (Metrô gates, with or without a place), tram %s, taxi %s",
              paste(tt("bus"), collapse = ","), paste(tt("car"), collapse = ","), paste(tt("metro"), collapse = ","),
              paste(tt("train"), collapse = ","), length(tt("tram")), length(tt("taxi"))),
      identical(tt("bus"), 600) && identical(tt("car"), 600) && identical(tt("metro"), c(650, 660)) &&
        identical(tt("train"), c(650, 660)) && length(tt("tram")) == 0 && length(tt("taxi")) == 0)

# Batch GPX: one track per segment, in time order, touching, typed
acc_split <- list(trip1 = multimodal_accepted(segs, "car"))
inp <- review_gpx_input(acc_split)
doc <- xml2::read_xml(paste(build_gpx(inp$items, inp$samples), collapse = "\n"))
trks <- xml2::xml_find_all(doc, "//*[local-name()='trk']")
trk_types <- xml2::xml_text(xml2::xml_find_first(trks, "./*[local-name()='type']"))
trk_times <- lapply(trks, function(k) as.numeric(as.POSIXct(xml2::xml_text(xml2::xml_find_all(k, ".//*[local-name()='time']")),
                                                          format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))
firsts <- vapply(trk_times, `[`, 0, 1); lasts <- vapply(trk_times, function(x) x[length(x)], 0)
check(sprintf("GPX: %d tracks %s; touching %s; strictly increasing inside %s; %d tracks counted",
              length(trks), paste(trk_types, collapse = ","), all(firsts[-1] == lasts[-length(lasts)]),
              all(vapply(trk_times, function(x) all(diff(x) > 0), logical(1))), review_gpx_track_count(acc_split)),
      length(trks) == 4 && identical(trk_types, c("walking", "stationary", "bus", "walking")) &&
        all(firsts[-1] == lasts[-length(lasts)]) && firsts[1] == T0 && lasts[4] == T0 + 1500 &&
        all(vapply(trk_times, function(x) all(diff(x) > 0), logical(1))) && review_gpx_track_count(acc_split) == 4 &&
        identical(acc_split$trip1$activity, "car") && !is.unsorted(acc_split$trip1$path$t, strictly = TRUE))

# ---- 13. OSRM profile of road trips (official) ----
check(sprintf("car trips default to the official profile (%s); taxi stays on car, bus on bus",
              REVIEW_ACTIVITIES$car$osrm),
      REVIEW_ACTIVITIES$car$osrm == "official" && REVIEW_ACTIVITIES$taxi$osrm == "car" && REVIEW_ACTIVITIES$bus$osrm == "bus")
check("official is a configured, enabled driving profile",
      "official" %in% osrm_enabled_profiles() && OSRM_PROFILES$official == "driving")
osrm_fns <- c("calculate_osrm_route", "recalculate_segment", "osrm_match", "osrm_route_via")
check("every OSRM function accepts profile = 'official' (match.arg lists)",
      all(vapply(osrm_fns, function(f) "official" %in% eval(formals(get(f))$profile), logical(1))))
check("every configured OSRM profile is accepted by every OSRM function",
      all(vapply(osrm_fns, function(f) all(names(OSRM_SERVERS) %in% eval(formals(get(f))$profile)), logical(1))))
check("Batch review profile select: Car / Official / Bus", identical(unname(profile_choices(REVIEW_PROFILE_CHOICES)), c("car", "official", "bus")))
# Without the official server enabled, car trips fall back to the car profile
fallback <- local({
  OSRM_ENABLED_PROFILES <- c("car", "foot", "bus")
  environment(osrm_enabled_profiles) <- environment()
  if ("official" %in% osrm_enabled_profiles()) "official" else "car"
})
check("official not enabled: car trips use car", fallback == "car")
trip13 <- data.frame(id = "T13", last_saved = "2026-06-13T09:00:00Z", activity = "car")
k_off <- review_cache_key(trip13)
REVIEW_ACTIVITIES$car$osrm <- "car"; k_car <- review_cache_key(trip13); REVIEW_ACTIVITIES$car$osrm <- "official"
check("road cache key includes the profile", k_off != k_car && grepl("|official|", k_off, fixed = TRUE))
check("rail cache key unchanged by profiles", grepl("|1$", review_cache_key(data.frame(id = "R", last_saved = "x", activity = "metro"), rails_mtime = 1)))
seen <- character(0)
spy_match <- function(coords, timestamps, radiuses, profile) { seen <<- c(seen, paste0("match:", profile)); same_match(coords, timestamps, radiuses, profile) }
spy_route <- function(points, profile = "car", ...) {
  seen <<- c(seen, paste0("route:", profile))
  list(coords = cbind(points$lng, points$lat), distance_m = 1, duration_s = 1)
}
p13 <- road_proposal(s9, same_match(cbind(s9$lon, s9$lat), s9$t, NULL, "x"), "car", route_fn = spy_route, match_fn = spy_match,
                     profile = "bus")
check(sprintf("road_proposal(profile = 'bus') uses it for the re-match: %s", paste(unique(seen), collapse = ", ")),
      !is.null(p13) && length(seen) > 0 && all(grepl(":bus$", seen)))
seen <- character(0)
p13b <- road_proposal(s9, same_match(cbind(s9$lon, s9$lat), s9$t, NULL, "x"), "car", route_fn = spy_route, match_fn = spy_match)
check(sprintf("road_proposal default for car: %s", paste(unique(seen), collapse = ", ")), all(grepl(":official$", seen)))

# ---- 14. Stop names, Jaé leg text, GPX names / desc, notes (ranges), notes package ----
# GTFS: stops at 0 m (platform of station S1), 30 m (named stop) and 90 m (too far)
st_ll <- ll(c(300, 300, 330, 390, 5000), c(5, 5, 5, 5, 5))
gtfs_df <- data.frame(stop_id = c("S1", "P1", "A", "B", "Z"), stop_name = c("Estação Teste", "Plataforma 2", "Ponto A", "Ponto B", "Longe"),
                      stop_lat = st_ll[, 2], stop_lon = st_ll[, 1], location_type = c("1", "0", "0", "0", "0"),
                      parent_station = c("", "S1", "", "", ""), stringsAsFactors = FALSE)
gtfs_df <- gtfs_df[c(1, 3, 4, 5), ]                     # station + named stops (no platform at the same spot yet)
g_near <- gtfs_stop_name(ll(320, 5)[1, 1], ll(320, 5)[1, 2], stops = gtfs_df)
check(sprintf("GTFS: nearest within 60 m: '%s' (%.0f m)", g_near$name, g_near$distance_m), g_near$name == "Ponto A" && g_near$source == "gtfs")
check("GTFS: nothing within 60 m -> NULL", is.null(gtfs_stop_name(ll(1000, 5)[1, 1], ll(1000, 5)[1, 2], stops = gtfs_df)))
plat <- data.frame(stop_id = c("S1", "P1"), stop_name = c("Terminal Teste (Centro)", "Plataforma B"), stop_lat = ll(c(0, 400), c(0, 0))[, 2],
                   stop_lon = ll(c(0, 400), c(0, 0))[, 1], location_type = c("1", "0"), parent_station = c("", "S1"), stringsAsFactors = FALSE)
g_par <- gtfs_stop_name(ll(410, 0)[1, 1], ll(410, 0)[1, 2], stops = plat)
check(sprintf("GTFS: a platform with a parent_station gives the parent's name: '%s'", g_par$name),
      g_par$name == "Terminal Teste (Centro)" && g_par$stop_id == "P1")
real_gtfs <- gtfs_stops()
check(sprintf("GTFS file: %d stops, all named", nrow(real_gtfs %||% data.frame())), !is.null(real_gtfs) && nrow(real_gtfs) > 7000 && all(nzchar(real_gtfs$stop_name)))

# Google: stubbed requests, temporary cache
cache_tmp <- tempfile(fileext = ".rds")
calls <- 0L
ok_req <- function(lon, lat, r, key) { calls <<- calls + 1L; list(status = "ok", name = "Ponto Google", distance_m = 20, api = "new") }
g1 <- google_stop_name(-43.2, -22.9, key = "x", cache_file = cache_tmp, request_fn = ok_req)
g2 <- google_stop_name(-43.20001, -22.90001, key = "x", cache_file = cache_tmp, request_fn = ok_req)
check(sprintf("Google: '%s', second call within ~11 m from the cache (%d request)", g1$name, calls),
      g1$name == "Ponto Google" && g1$source == "google" && identical(g2$name, "Ponto Google") && calls == 1L)
none_req <- function(...) { calls <<- calls + 1L; list(status = "none", api = "new") }
calls <- 0L
n1 <- google_stop_name(-43.3, -22.95, key = "x", cache_file = cache_tmp, request_fn = none_req)
n2 <- google_stop_name(-43.3, -22.95, key = "x", cache_file = cache_tmp, request_fn = none_req)
check("Google: 'none' is cached too", is.null(n1) && is.null(n2) && calls == 1L)
err_req <- function(...) { calls <<- calls + 1L; list(status = "error", api = "new") }
calls <- 0L
e1 <- google_stop_name(-43.4, -22.96, key = "x", cache_file = cache_tmp, request_fn = err_req)
e2 <- google_stop_name(-43.4, -22.96, key = "x", cache_file = cache_tmp, request_fn = err_req)
check("Google: a transient error is not cached", is.null(e1) && is.null(e2) && calls == 2L)
dis_req <- function(...) { calls <<- calls + 1L; list(status = "disabled", api = "legacy") }
calls <- 0L
msgs <- character(0)
withCallingHandlers({
  d1 <- google_stop_name(-43.5, -22.97, key = "x", cache_file = cache_tmp, request_fn = dis_req)
  d2 <- google_stop_name(-43.6, -22.98, key = "x", cache_file = cache_tmp, request_fn = dis_req)
}, message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
check(sprintf("Google disabled: said once (%d message), then skipped (%d request)", length(msgs), calls),
      is.null(d1) && is.null(d2) && length(msgs) == 1 && calls == 1L)
.stops_state$places_disabled <- NULL
check("Google: no key -> skipped", is.null(google_stop_name(-43.2, -22.9, key = "", cache_file = cache_tmp, request_fn = ok_req)))

# Chain: GTFS > Google > generic
no_hit <- function(...) NULL
c1 <- stop_name_for(0, 0, gtfs_fn = function(...) list(name = "G", source = "gtfs"), google_fn = function(...) stop("not called"))
c2 <- stop_name_for(0, 0, gtfs_fn = no_hit, google_fn = function(...) list(name = "P", source = "google"))
c3 <- stop_name_for(0, 0, lang = "pt", gtfs_fn = no_hit, google_fn = no_hit)
c4 <- stop_name_for(0, 0, lang = "en", gtfs_fn = no_hit, google_fn = no_hit)
check(sprintf("chain: %s/%s, %s/%s, '%s'/%s (pt), '%s' (en); labels %s, %s, %s", c1$name, c1$source, c2$name, c2$source, c3$name, c3$source,
              c4$name, stop_source_label("gtfs", "pt"), stop_source_label("google", "pt"), stop_source_label("generic", "pt")),
      c1$source == "gtfs" && c2$source == "google" && c3$name == "Ponto de Ônibus" && c3$source == "generic" && c4$name == "Bus stop" &&
        stop_source_label("generic", "pt") == "genérico")

# Jaé leg text: empty parts left out
jl <- function(...) { r <- read_jae_boardings(tempfile())[0, ]; v <- list(...); for (k in names(v)) r[1, k] <- v[[k]]; r }
t_full <- jae_leg_text(jl(modo = "Ônibus", linha_ou_estacao = "371", descricao = "Praça Seca - Centro", id_veiculo = "32719"))
t_novh <- jae_leg_text(jl(modo = "BRT", linha_ou_estacao = "50", descricao = "", id_veiculo = ""))
t_metro <- jae_leg_text(jl(modo = "Metrô", linha_ou_estacao = "BOTAFOGO", descricao = "", id_veiculo = " "))
check(sprintf("leg text: '%s' | '%s' | '%s'", t_full, t_novh, t_metro),
      t_full == "Ônibus 371 · Praça Seca - Centro · veículo 32719" && t_novh == "BRT 50" && t_metro == "Metrô BOTAFOGO")

# GPX: the wait's track named by the stop, <desc> on the bus, no <wpt>
segs_l <- multimodal_label_segments(segs, stop = list(name = "Silva Rosa", source = "gtfs"), leg_text = t_full)
tap_l <- jl(modo = "Ônibus", linha_ou_estacao = "371", descricao = "Praça Seca - Centro", id_veiculo = "32719")
acc_l <- list(trip1 = multimodal_accepted(segs_l, "bus", tap = tap_l))
inp_l <- review_gpx_input(acc_l)
doc_l <- xml2::read_xml(paste(build_gpx(inp_l$items, inp_l$samples), collapse = "\n"))
nsx <- function(x, q) xml2::xml_find_all(x, q)
trk_l <- nsx(doc_l, "//*[local-name()='trk']")
trk_names <- xml2::xml_text(xml2::xml_find_first(trk_l, "./*[local-name()='name']"))
trk_desc <- xml2::xml_text(xml2::xml_find_first(trk_l, "./*[local-name()='desc']"))
child_order <- xml2::xml_name(xml2::xml_children(trk_l[[3]]))
check(sprintf("GPX: tracks %s | desc %s | wpt %d | bus children %s", paste(trk_names, collapse = " / "),
              paste(ifelse(is.na(trk_desc), "-", trk_desc), collapse = " / "), length(nsx(doc_l, "//*[local-name()='wpt']")),
              paste(child_order, collapse = ",")),
      trk_names[2] == "Silva Rosa" && grepl("^Walk to the stop \\d{4}-\\d\\d-\\d\\d \\d\\d:\\d\\d$", trk_names[1]) &&
        grepl("^Bus 371 ", trk_names[3]) && identical(trk_desc[3], t_full) && all(is.na(trk_desc[c(1, 2, 4)])) &&
        length(nsx(doc_l, "//*[local-name()='wpt']")) == 0 && identical(child_order, c("name", "desc", "type", "trkseg")) &&
        identical(xml2::xml_name(xml2::xml_children(doc_l))[1:2], c("metadata", "trk")) && is.null(inp_l$waypoints))
a_seg <- acc_l$trip1$segments
check(sprintf("accepted split keeps the notes' data: roles %s, stop '%s', tap %s",
              paste(vapply(a_seg, `[[`, "", "role"), collapse = ","), a_seg[[2]]$stop$name, acc_l$trip1$tap$id_veiculo),
      identical(vapply(a_seg, `[[`, "", "role"), vapply(segs_l, `[[`, "", "role")) &&
        all(vapply(seq_along(a_seg), function(k) a_seg[[k]]$t_start == segs_l[[k]]$t_start && a_seg[[k]]$t_end == segs_l[[k]]$t_end, TRUE)) &&
        a_seg[[2]]$stop$name == "Silva Rosa" && acc_l$trip1$tap$id_veiculo == "32719")

# Note ranges: start + 15 s .. end - 15 s; none under 40 s
nr <- function(d, t0 = T0) { r <- multimodal_note_range(t0, t0 + d); if (is.null(r)) "none" else sprintf("+%g..+%g", r[1] - t0, r[2] - t0) }
check(sprintf("note range: 375 s -> %s; 60 s -> %s; 40 s -> %s; 39 s -> %s; 10 s -> %s; fractional 100.4 s from .4 -> %s",
              nr(375), nr(60), nr(40), nr(39), nr(10), nr(100, T0 + 0.4)),
      nr(375) == "+15..+360" && nr(60) == "+15..+45" && nr(40) == "+15..+25" && nr(39) == "none" && nr(10) == "none" &&
        nr(100, T0 + 0.4) == "+15.6..+84.6")
range_ok <- all(vapply(40:900, function(d) {
  r <- multimodal_note_range(T0, T0 + d)
  !is.null(r) && r[1] == T0 + 15 && r[2] == T0 + d - 15 && r[2] - r[1] >= 10 && all(r == round(r))
}, logical(1)))
check("note range: every leg of 40-900 s gets whole seconds 15 s inside both ends, at least 10 s long", range_ok)

# Notes of the labelled split: ranges strictly inside the bus leg and the wait
nn <- multimodal_notes(segs_l)
nn_off <- multimodal_notes(segs_l, wait_note = FALSE)
inside_one <- function(n, segs) {
  k <- which(vapply(segs, function(g) n$start_utc < g$t_end && n$end_utc > g$t_start, logical(1)))  # segments the range touches
  length(k) == 1 && n$start_utc - segs[[k]]$t_start >= 15 && segs[[k]]$t_end - n$end_utc >= 15
}
inside_ok <- all(vapply(nn, inside_one, logical(1), segs = segs_l))
check(sprintf("notes: %s | wait note off -> %s | each range touches exactly one segment, 15 s inside it: %s",
              paste(sprintf("%s '%s' +%g..+%g s", vapply(nn, `[[`, "", "role"), vapply(nn, `[[`, "", "body"),
                            vapply(nn, function(n) n$start_utc - T0, 0), vapply(nn, function(n) n$end_utc - T0, 0)), collapse = " | "),
              paste(vapply(nn_off, `[[`, "", "role"), collapse = ","), inside_ok),
      length(nn) == 2 && nn[[1]]$role == "wait" && nn[[1]]$body == "Silva Rosa" &&
        nn[[1]]$start_utc == segs_l[[2]]$t_start + 15 && nn[[1]]$end_utc == segs_l[[2]]$t_end - 15 &&
        nn[[2]]$role == "bus" && nn[[2]]$body == t_full && nn[[2]]$start_utc == segs_l[[3]]$t_start + 15 &&
        nn[[2]]$end_utc == segs_l[[3]]$t_end - 15 && inside_ok && length(nn_off) == 1 && nn_off[[1]]$role == "bus")
mkseg <- function(role, t0, t1, ...) c(list(role = role, t_start = T0 + t0, t_end = T0 + t1), list(...))
short_segs <- list(mkseg("wait", 0, 20, stop = list(name = "Curto")), mkseg("bus", 20, 59, desc = "Ônibus 1"),
                   mkseg("wait", 59, 99, stop = list(name = "Médio")), mkseg("bus", 99, 139, desc = "Ônibus 2"),
                   mkseg("rail", 139, 400, desc = "Metrô BOTAFOGO", note_body = "Nota rica"), mkseg("walk_from_stop", 400, 900))
sn <- multimodal_notes(short_segs)
check(sprintf("short segments (wait 20 s, bus 39 s, wait 40 s, bus 40 s, rail with note_body, walk): notes %s",
              paste(sprintf("'%s' +%g..+%g", vapply(sn, `[[`, "", "body"), vapply(sn, function(n) n$start_utc - T0, 0),
                            vapply(sn, function(n) n$end_utc - T0, 0)), collapse = ", ")),
      length(sn) == 3 && sn[[1]]$body == "Médio" && sn[[1]]$start_utc == T0 + 74 && sn[[1]]$end_utc == T0 + 84 &&
        sn[[2]]$body == "Ônibus 2" && sn[[2]]$start_utc == T0 + 114 && sn[[3]]$body == "Nota rica" &&
        all(vapply(sn, inside_one, logical(1), segs = short_segs)))
legacy <- list(list(name = "Bus 1", type = "bus", path = NULL, desc = "x"))  # accepted before the notes data existed
check("old accepted splits (no role / times) give no notes", length(multimodal_notes(legacy)) == 0 &&
        length(review_notes(list(a = list(segments = legacy)))) == 0)

# review_notes: the ride body from ride_body(tap), else the leg text; sorted by time; plain fixes skipped
acc_mix <- list(b = acc_l$trip1, plain = list(activity = "car", path = data.frame(lon = 0, lat = 0, t = T0)))
rn_default <- review_notes(acc_mix)
rn_rich <- review_notes(acc_mix, ride_body = function(tap) paste("Linha", tap$linha_ou_estacao, "rica"))
rn_nowait <- review_notes(acc_mix, wait_note = FALSE, ride_body = function(tap) NULL)
check(sprintf("review_notes: default %s | rich %s | no wait %s", paste(vapply(rn_default, `[[`, "", "body"), collapse = " / "),
              paste(vapply(rn_rich, `[[`, "", "body"), collapse = " / "), paste(vapply(rn_nowait, `[[`, "", "body"), collapse = " / ")),
      length(rn_default) == 2 && rn_default[[2]]$body == t_full && rn_rich[[2]]$body == "Linha 371 rica" &&
        rn_rich[[1]]$body == "Silva Rosa" && length(rn_nowait) == 1 && rn_nowait[[1]]$body == t_full &&
        rn_default[[1]]$start_utc < rn_default[[2]]$start_utc)

# Notes-only import package: ranges
pkg_dir <- file.path(tempdir(), "notes_pkg_test"); unlink(pkg_dir, recursive = TRUE)
nts <- c(nn, list(list(body = "Metrô BOTAFOGO\nsegunda linha", start_utc = as.POSIXct("2026-07-01 12:00:00", tz = "UTC"),
                       end_utc = as.POSIXct("2026-07-01 12:05:00", tz = "UTC"))))
write_notes_package_dir(nts, pkg_dir, now_utc = "2026-10-11T12:00:00Z")
meta <- jsonlite::fromJSON(file.path(pkg_dir, "metadata.json"))
files <- sort(list.files(pkg_dir, recursive = TRUE))
iso0 <- function(x) format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
n_jun <- jsonlite::fromJSON(file.path(pkg_dir, "notes", format(as.POSIXct(T0 + 600, origin = "1970-01-01", tz = "UTC"), "%Y-%m.json", tz = "UTC")), simplifyVector = FALSE)
n_jul <- jsonlite::fromJSON(file.path(pkg_dir, "notes", "2026-07.json"), simplifyVector = FALSE)
gz <- readLines(gzfile(file.path(pkg_dir, files[grepl("^samples/", files)])))
check(sprintf("notes package: files %s | metadata %s/%s schema %s, notes recordCount %d, stats %d/%d/%d", paste(files, collapse = ", "),
              meta$exportType, meta$exportMode, meta$schemaVersion, meta$extensions$notes$recordCount, meta$stats$itemCount,
              meta$stats$sampleCount, meta$stats$placeCount),
      meta$exportType == "incremental" && meta$exportMode == "bucketed" && meta$extensions$notes$recordCount == 3 &&
        meta$stats$itemCount == 0 && all(c("places/0.json", "metadata.json", "notes/2026-07.json") %in% files) &&
        any(grepl("^items/\\d{4}-\\d\\d\\.json$", files)) && any(grepl("^samples/\\d{4}-W\\d\\d\\.json\\.gz$", files)) &&
        identical(trimws(gz), "[]") && identical(jsonlite::fromJSON(file.path(pkg_dir, "places", "0.json")), list()) &&
        !any(startsWith(basename(files), ".")))
check(sprintf("notes in the package are ranges: %s", paste(vapply(c(n_jun, n_jul), function(n) sprintf("'%s' %s..%s", gsub("\n", "\\\\n", n$body), n$startDate, n$endDate), ""), collapse = " | ")),
      length(n_jun) == 2 && all(vapply(c(n_jun, n_jul), function(n) n$startDate < n$endDate, logical(1))) &&
        n_jun[[2]]$startDate == iso0(segs_l[[3]]$t_start + 15) && n_jun[[2]]$endDate == iso0(segs_l[[3]]$t_end - 15) &&
        n_jun[[2]]$body == t_full && n_jun[[1]]$body == "Silva Rosa" && n_jul[[1]]$body == "Metrô BOTAFOGO\nsegunda linha" &&
        setequal(names(n_jun[[1]]), c("id", "timelineItemId", "body", "source", "startDate", "endDate", "creationDate", "lastSaved", "deleted")) &&
        is.null(n_jun[[1]]$timelineItemId) && grepl("^[0-9A-F-]{36}$", n_jun[[1]]$id) && n_jun[[1]]$source == "LocoKit2")

# ---- 14b. SMTR bus note (synthetic cache; no BigQuery) ----
check(sprintf("operator names: '%s' | '%s' | '%s' | '%s'", smtr_title("SANCETUR - SANTA CECILIA TURISMO LTDA"), smtr_title("VIACAO NOVACAP S/A."),
              smtr_title("TRANSPORTES BARRA LTDA."), smtr_title("AUTO VIACAO TRES AMIGOS S.A.")),
      smtr_title("SANCETUR - SANTA CECILIA TURISMO LTDA") == "Sancetur \u2013 Santa Cecília Turismo" &&
        smtr_title("VIACAO NOVACAP S/A.") == "Viação Novacap" && smtr_title("TRANSPORTES BARRA LTDA.") == "Transportes Barra" &&
        smtr_title("AUTO VIACAO TRES AMIGOS S.A.") == "Auto Viação Três Amigos" && smtr_title("CAIO") == "Caio" &&
        smtr_title("EXPRESSO DA BARRA") == "Expresso da Barra" && is.na(smtr_title(NA)) && is.na(smtr_title("")))
check(sprintf("chassis: '%s' | '%s' | '%s'", smtr_chassis("MBB OF 1721L EURO VI"), smtr_chassis("VOLKSWAGEN 17.230 OD"), smtr_chassis("MERCEDES-BENZ O-500U")),
      smtr_chassis("MBB OF 1721L EURO VI") == "MBB OF 1721L Euro VI" && smtr_chassis("VOLKSWAGEN 17.230 OD") == "Volkswagen 17.230 OD" &&
        smtr_chassis("MERCEDES-BENZ O-500U") == "Mercedes-Benz O-500U")

tap_at <- function(local, line = "753", veh = "33103", modo = "Ônibus", cons = "Santa Cruz") {
  r <- jl(modo = modo, linha_ou_estacao = line, descricao = "", id_veiculo = veh, consorcio = cons)
  r$datetime_utc <- lubridate::with_tz(as.POSIXct(local, tz = "America/Sao_Paulo"), "UTC")
  r
}
trips_14 <- data.frame(id_viagem = c("v1", "v2", "v3"), id_veiculo = "D33103", servico = c("753", "756", "753"), sentido = c("I", "V", "V"),
                       trip_id = c("gt1", "gt2", NA), datetime_partida = c("2026-06-14 13:46:16", "2026-06-14 15:40:00", "2026-06-14 15:20:00"),
                       datetime_chegada = c("2026-06-14 15:18:43", "2026-06-14 17:01:16", "2026-06-14 16:50:00"),
                       consorcio = "Santa Cruz", quantidade_transacao = c(67L, 53L, 20L), quantidade_transacao_riocard = c(11L, 4L, 3L),
                       stringsAsFactors = FALSE)
trips_14$id_validador <- list("VAL1", "VAL1", "VAL1")
heads_14 <- data.frame(servico = c("753", "753", "756", "756"), direction_id = c("0", "1", "0", "1"),
                       trip_headsign = c("Metrô Coelho Neto", "Santa Cruz", "Metrô Coelho Neto", "Santa Cruz"), n = c(26, 27, 29, 29))
heads_14$trip_ids <- list("gt1", character(0), character(0), "gt2")
ops_14 <- data.frame(id_validador = c("VAL1", "VAL1"), datetime_inicio_validade = c("2024-06-07 16:20:11", "2025-12-10 15:55:02"),
                     datetime_fim_validade = c("2025-12-10 15:52:49", NA), operadora = c("TRANSPORTES BARRA LTDA.", "SANCETUR - SANTA CECILIA TURISMO LTDA"))
entry <- function(...) {
  e <- list(date = "2026-06-14", vehicle = "33103", fetched_at = as.numeric(Sys.time()),
            coverage = list(trips = TRUE, transacao = TRUE), trips = trips_14,
            vehicle_info = list(id_veiculo = "D33103", placa = "QSO5D51", carroceria = "CAIO",
                                nome_chassi = "MBB OF 1721L EURO VI", ano_fabricacao = 2025L),
            operators = ops_14, headsigns = heads_14)
  v <- list(...)
  for (k in names(v)) e[k] <- list(v[[k]])
  e
}
cache_14 <- list(`2026-06-14|33103` = entry())
expected_14 <- paste("Linha 753: Metrô Coelho Neto", "Ônibus D33103 - Placa QSO5D51",
                     "Consórcio Santa Cruz, operadora Sancetur \u2013 Santa Cecília Turismo", "Caio · MBB OF 1721L Euro VI · 2025",
                     "Início da viagem: 13:46 | Fim da viagem: 15:18", "Embarques totais na viagem: 67 Jaé + 11 RioCard", sep = "\n")
b_full <- smtr_note_body(tap_at("2026-06-14 14:24:00"), cache_14)
check("SMTR note: the 14/06 format line by line", identical(b_full, expected_14))
# 15:45: inside both the 756 trip (15:40-17:01) and a 753 trip (15:20-16:50, no trip_id): the Jaé line wins, headsign by sentido V
b_pref <- smtr_note_body(tap_at("2026-06-14 15:45:00"), cache_14)
b_other <- smtr_note_body(tap_at("2026-06-14 16:55:00"), cache_14)
check(sprintf("SMTR note: overlapping trips -> '%s'; only the 756 trip -> '%s'", strsplit(b_pref, "\n")[[1]][1], strsplit(b_other, "\n")[[1]][1]),
      startsWith(b_pref, "Linha 753: Santa Cruz\n") && grepl("Início da viagem: 15:20 \\| Fim da viagem: 16:50", b_pref) &&
        startsWith(b_other, "Linha 756: Santa Cruz\n"))
b_old_op <- smtr_note_body(tap_at("2025-11-01 10:00:00"), list(`2025-11-01|33103` = entry(
  date = "2025-11-01", trips = transform(trips_14[1, ], datetime_partida = "2025-11-01 09:30:00", datetime_chegada = "2025-11-01 11:00:00"))))
check("SMTR note: the validator's operator valid at the tap (before the 2025-12-10 switch)", grepl("operadora Transportes Barra\n", b_old_op))
b_notrip <- smtr_note_body(tap_at("2026-06-14 12:00:00"), cache_14)
check(sprintf("SMTR note: no trip containing the tap -> lines from Jaé / licensing only:\n      %s", gsub("\n", " | ", b_notrip)),
      identical(b_notrip, paste("Linha 753", "Ônibus D33103 - Placa QSO5D51", "Consórcio Santa Cruz", "Caio · MBB OF 1721L Euro VI · 2025", sep = "\n")))
b_pend <- smtr_note_body(tap_at("2026-10-10 08:00:00"), list(`2026-10-10|33103` = entry(
  date = "2026-10-10", coverage = list(trips = FALSE, transacao = FALSE), trips = trips_14[0, ], operators = NULL, headsigns = NULL,
  vehicle_info = NULL)))
b_nocache <- smtr_note_body(tap_at("2026-10-11 08:00:00"), list())
b_part <- smtr_note_body(tap_at("2026-06-14 14:24:00"), list(`2026-06-14|33103` = entry(
  coverage = list(trips = TRUE, transacao = FALSE), trips = transform(trips_14, quantidade_transacao = NA_integer_))))
check(sprintf("SMTR note: date not covered -> '%s' | no cache entry -> same | boardings missing -> last lines '%s'",
              gsub("\n", " | ", b_pend), paste(tail(strsplit(b_part, "\n")[[1]], 2), collapse = " | ")),
      identical(b_pend, paste("Linha 753", "Ônibus 33103", "Consórcio Santa Cruz", SMTR_NOTE_PENDING, sep = "\n")) &&
        identical(b_nocache, b_pend) && grepl("Fim da viagem: 15:18\n\\(dados SMTR ainda não disponíveis\\)$", b_part) &&
        !grepl("Embarques", b_part))
check("SMTR note: not a bus tap / no vehicle -> NULL",
      is.null(smtr_note_body(tap_at("2026-06-14 14:24:00", modo = "Metrô"), cache_14)) &&
        is.null(smtr_note_body(tap_at("2026-06-14 14:24:00", veh = ""), cache_14)))

# Prefetch: one batch per date, cache hits, incomplete entries fetched again after an hour, errors reported
fc <- tempfile(fileext = ".rds")
fetched <- list()
fake_fetch <- function(date, vehicles) {
  fetched[[length(fetched) + 1L]] <<- list(date = date, vehicles = sort(vehicles))
  stats::setNames(lapply(vehicles, function(v) entry(date = date, vehicle = v, coverage = list(trips = date != "2026-10-10", transacao = TRUE))), vehicles)
}
taps_pf <- rbind(tap_at("2026-06-14 14:24:00"), tap_at("2026-06-14 18:00:00", veh = "51561"), tap_at("2026-10-10 08:00:00"),
                 tap_at("2026-06-14 14:30:00", modo = "Metrô", veh = "4103"))
p1 <- smtr_prefetch(taps_pf, cache_file = fc, fetch_fn = fake_fetch, auth_fn = function() NULL)
n1 <- length(fetched)
p2 <- smtr_prefetch(taps_pf, cache_file = fc, fetch_fn = fake_fetch, auth_fn = function() NULL)
n2 <- length(fetched)
cc <- smtr_cache(fc); cc[["2026-10-10|33103"]]$fetched_at <- cc[["2026-10-10|33103"]]$fetched_at - 2 * SMTR_REFETCH_S; smtr_cache_save(cc, fc)
p3 <- smtr_prefetch(taps_pf, cache_file = fc, fetch_fn = fake_fetch, auth_fn = function() NULL)
p4 <- smtr_prefetch(tap_at("2026-06-20 08:00:00"), cache_file = fc, fetch_fn = fake_fetch, auth_fn = function() stop("no token"))
p5 <- smtr_prefetch(tap_at("2026-06-21 08:00:00"), cache_file = fc, fetch_fn = function(...) stop("SMTR query refused: it would read 9.00 GB (limit 2 GB)"),
                    auth_fn = function() NULL)
check(sprintf("prefetch: first %d batches (%s), second %d (cache), stale incomplete refetched: %s, auth error '%s', query error '%s'",
              n1, paste(vapply(fetched[1:n1], function(f) paste0(f$date, "[", paste(f$vehicles, collapse = ","), "]"), ""), collapse = " "),
              n2 - n1, paste(vapply(fetched[-(1:n2)], `[[`, "", "date"), collapse = ","), p4$error, p5$error),
      n1 == 2 && identical(fetched[[1]]$vehicles, c("33103", "51561")) && p1$fetched == 2L && n2 == n1 && p2$fetched == 0L &&
        length(fetched) == n2 + 1 && fetched[[n2 + 1]]$date == "2026-10-10" && identical(p4$error, "no token") &&
        grepl("refused", p5$error) && length(smtr_cache(fc)) == 3)

# ---- 15. Control pins: ordering, monotonic projection, pinned timing ----
# Recording: east along y = 0 from x = 0 to 2000 (+0..+400 s), back west along y = 60 to x = 0 (+400..+800 s)
px <- c(seq(0, 2000, by = 100), seq(2000, 0, by = -100)); py <- c(rep(0, 21), rep(60, 21))
pt_t <- T0 + c(seq(0, 400, by = 20), seq(400, 800, by = 20)); pt_t[22:42] <- pt_t[22:42] + 1
rec <- samples_at(px, py, pt_t)
mkpins <- function(x, y, ids = seq_along(x), after = NA_character_) {
  p <- ll(x, y); data.frame(id = as.character(ids), lon = p[, 1], lat = p[, 2], after = after, stringsAsFactors = FALSE)
}
op <- order_pins(rec, mkpins(c(1500, 500, 1000), c(-200, -200, -200)))
check(sprintf("pins ordered by projection along the recording: %s (clicked 1, 2, 3)", paste(op$id, collapse = ",")),
      identical(op$id, c("2", "3", "1")) && all(!op$ambiguous))
op2 <- order_pins(rec, mkpins(c(1500, 500), c(30, 30)))
check(sprintf("a pin between the two passes is ambiguous: %s; bearing %s", paste(op2$ambiguous, collapse = ","),
              paste(round(op2$bearing), collapse = ",")), all(op2$ambiguous))
op3 <- order_pins(rec, mkpins(c(1500, 500, 1000), c(-200, -200, -200), after = c("start", NA, NA)))
op4 <- order_pins(rec, mkpins(c(1500, 500, 1000), c(-200, -200, -200), after = c(NA, "1", NA)))
check(sprintf("placement by hand: pin 1 after start -> %s; pin 2 after pin 1 -> %s", paste(op3$id, collapse = ","), paste(op4$id, collapse = ",")),
      identical(op3$id, c("1", "2", "3")) && identical(op4$id, c("3", "1", "2")))
check("pin far from the recording: no bearing", is.na(order_pins(rec, mkpins(1000, -500))$bearing))

# Monotonic projection on a route that goes out and back on the same street
route_xy <- ll(c(0, 2000, 2000, 0), c(0, 0, 60, 60))
rcum <- path_cumulative_m(route_xy)
proj <- project_samples_monotonic(route_xy, rcum, rec)
check(sprintf("projection: non-decreasing %s, way back matched on the way back (sample 30 at %.0f m, > 2060), the first pass not reused",
              !is.unsorted(proj[!is.na(proj)]), proj[30]),
      !is.unsorted(proj[!is.na(proj)]) && proj[30] > 2060 && proj[10] < 2000 && all(!is.na(proj)))
noisy <- rec; noisy$lat[15] <- ll(1400, 400)[1, 2]       # one sample 400 m off the route
proj_n <- project_samples_monotonic(route_xy, rcum, noisy)
check(sprintf("projection: an outlier 400 m off is skipped (NA), its neighbours kept (%.0f, %.0f m)", proj_n[14], proj_n[16]),
      is.na(proj_n[15]) && !is.na(proj_n[14]) && !is.na(proj_n[16]) && !is.unsorted(proj_n[!is.na(proj_n)]))
stay <- rbind(rec[1:10, ], transform(rec[rep(10, 3), ], t = rec$t[10] + c(30, 60, 90)), transform(rec[11:42, ], t = t + 100))
proj_s <- project_samples_monotonic(route_xy, rcum, stay)
check("projection: a stop (same place, later times) keeps the same distance (a dwell, not a step back)",
      all(abs(proj_s[11:13] - proj_s[10]) < 1) && !is.unsorted(proj_s[!is.na(proj_s)]))

# Pinned timing: ends at the trip's first / last times, increasing, the dwell kept
stay$t <- stay$t + 0.4 * (seq_len(nrow(stay)) %% 3)          # fractional sample times (as some recordings have)
tp <- pinned_timed_path(stay, route_xy)
tp_d <- project_along(route_xy, rcum, cbind(tp$lon, tp$lat))
check(sprintf("pinned path: %s..%s s, %d points, %d anchors, whole seconds strictly increasing %s, dwell kept %s", tp$t[1] - T0, tp$t[nrow(tp)] - T0,
              nrow(tp), attr(tp, "anchored"), !is.unsorted(tp$t, strictly = TRUE) && all(tp$t == round(tp$t)), any(diff(round(tp_d)) == 0)),
      tp$t[1] == round(stay$t[1]) && tp$t[nrow(tp)] == round(stay$t[nrow(stay)]) && !is.unsorted(tp$t, strictly = TRUE) && all(tp$t == round(tp$t)) &&
        any(diff(round(tp_d)) == 0))
# A route that differs from the recording (a detour around one block): the time spreads over it
stub_via <- function(coords, profile, bearings = NULL, radiuses = NULL, ...) {
  seen_via <<- list(coords = coords, profile = profile, bearings = bearings, radiuses = radiuses)
  list(coords = route_xy, distance_m = rcum[length(rcum)], legs_m = rep(rcum[length(rcum)] / (nrow(coords) - 1), nrow(coords) - 1))
}
seen_via <- NULL
pf <- pinned_route_fix(rec, mkpins(c(1500, 500), c(-20, -20)), "official", route_via_fn = stub_via)
check(sprintf("pinned fix: %s, %d pins in order, via %d points on %s, bearings %s, radiuses %s", pf$method, pf$n_pins, nrow(seen_via$coords),
              seen_via$profile, paste(ifelse(is.na(seen_via$bearings), "-", round(seen_via$bearings)), collapse = "/"),
              paste(ifelse(is.na(seen_via$radiuses), "-", seen_via$radiuses), collapse = "/")),
      pf$method == "pins" && pf$n_pins == 2 && nrow(seen_via$coords) == 4 && seen_via$profile == "official" &&
        is.na(seen_via$bearings[1]) && is.na(seen_via$bearings[4]) && abs(seen_via$bearings[2] - 90) < 5 &&
        identical(pf$pins$id, c("2", "1")) && pf$path$t[1] == rec$t[1] && pf$path$t[nrow(pf$path)] == rec$t[nrow(rec)])
check("pinned fix: OSRM failure -> NULL", is.null(pinned_route_fix(rec, mkpins(500, 0), "car", route_via_fn = function(...) NULL)))
# Edit Samples snap through pins: every sample on the route, in order
sn <- snap_positions_to_route(route_xy, noisy)
sn_d <- project_along(route_xy, rcum, sn)
check(sprintf("snap through pins: %d positions, the outlier placed between its neighbours (%.0f < %.0f < %.0f m)", nrow(sn),
              sn_d[14], sn_d[15], sn_d[16]),
      nrow(sn) == nrow(noisy) && sn_d[14] <= sn_d[15] && sn_d[15] <= sn_d[16] && !is.unsorted(sn_d))

# ---- 16. official.lua: wrong-way driving (profile diff); car.lua / bus.lua untouched ----
lua <- readLines(path.expand("~/osrm-data/official.lua"))
cfg_line <- grep("^OFFICIAL_WRONG_WAY_SPEED = 10", lua)
handlers <- trimws(lua[grep("^    (wrong_way_before|WayHandlers.oneway|wrong_way_open|WayHandlers.surface|wrong_way_speed|WayHandlers.penalties),", lua)])
check(sprintf("official.lua: OFFICIAL_WRONG_WAY_SPEED = 10 at line %s (top), handlers %s", paste(cfg_line, collapse = ","), paste(handlers, collapse = " ")),
      length(cfg_line) == 1 && cfg_line < 30 && any(grepl("^OFFICIAL_WRONG_WAY_PENALTY = ", lua[1:30])) && any(grepl("^OFFICIAL_WRONG_WAY_EXCLUDED = ", lua[1:30])) &&
        identical(handlers, c("wrong_way_before,", "WayHandlers.oneway,", "wrong_way_open,", "WayHandlers.surface,", "wrong_way_speed,", "WayHandlers.penalties,")))
turn <- paste(lua[grep("^function process_turn", lua):length(lua)], collapse = "\n")
check("official.lua: entering a wrong-way stretch adds OFFICIAL_WRONG_WAY_PENALTY; other restricted roads keep max_turn_weight",
      grepl("turn.target_access_turn_classification == OFFICIAL_WRONG_WAY_CLASS then\\s+turn.weight = turn.weight \\+ OFFICIAL_WRONG_WAY_PENALTY\\s+else\\s+turn.weight = constants.max_turn_weight", turn))
lua_txt <- paste(lua, collapse = "\n")
noturn <- readLines(path.expand("~/osrm-data/official-noturn.lua"))
check("official.lua: wrong-way classes wwminor / wwmajor (primary, secondary and links), excludable {wwmajor} and {wwminor, wwmajor}, set after WayHandlers.classes; motorways / trunks stay one-way; official-noturn.lua = official without turn restrictions",
      grepl("'wwminor', 'wwmajor'", lua_txt, fixed = TRUE) && grepl("Set {'wwmajor'},", lua_txt, fixed = TRUE) &&
        grepl("Set {'wwminor', 'wwmajor'}", lua_txt, fixed = TRUE) &&
        grepl("OFFICIAL_WRONG_WAY_MAJOR = { primary = true, primary_link = true, secondary = true, secondary_link = true }", lua_txt, fixed = TRUE) &&
        grepl("OFFICIAL_WRONG_WAY_EXCLUDED = { motorway = true, motorway_link = true, trunk = true, trunk_link = true }", lua_txt, fixed = TRUE) &&
        grepl("WayHandlers.classes,\\s+wrong_way_classes,", lua_txt) &&
        any(grepl("require('official')", noturn, fixed = TRUE)) && any(grepl("use_turn_restrictions = false", noturn, fixed = TRUE)))
md5 <- unname(tools::md5sum(path.expand(c("~/osrm-data/car.lua", "~/osrm-data/bus.lua"))))
check(sprintf("car.lua unchanged, bus.lua as of the busway rule (9be4917) (md5 %s / %s)", substr(md5[1], 1, 8), substr(md5[2], 1, 8)),
      identical(md5, c("1b9d7b7a49525e51619892c7c49cefac", "2e29abf91e49862210793bad7d4e4223")))

# ---- 17. OSRM data refresh status (last_refresh.json) in the status-dot tooltip ----
rs_ok <- list(date = "2026-10-12T07:40:00Z", type = "patch", status = "OK", base_osm_timestamp = "2026-10-10T20:21:21Z",
              last_changeset_id = 190376845, last_changeset_time = "2026-10-12T02:47:54Z")
t_pt <- osrm_refresh_text(rs_ok, "pt"); t_en <- osrm_refresh_text(rs_ok, "en")
rs_nochg <- rs_ok; rs_nochg$last_changeset_time <- NULL
rs_fail <- utils::modifyList(rs_ok, list(status = "FAILED", reason = "smoke tests failed on bus: previous dataset restored"))
t_fail <- osrm_refresh_text(rs_fail, "pt")
check(sprintf("refresh tooltip: PT '%s' | EN '%s' | no edits '%s' | FAILED '%s'", t_pt, t_en, osrm_refresh_text(rs_nochg, "en"), t_fail),
      t_pt == "Base OSM: 10/10/2026 · minhas edições até 11/10 23:47" && t_en == "OSM base: 2026-10-10 · my edits up to 2026-10-11 23:47" &&
        osrm_refresh_text(rs_nochg, "en") == "OSM base: 2026-10-10" &&
        t_fail == "Base OSM: 10/10/2026 · minhas edições até 11/10 23:47. A última atualização dos dados FALHOU (12/10 04:40): smoke tests failed on bus: previous dataset restored")
st_file <- tempfile(fileext = ".json")
jsonlite::write_json(rs_fail, st_file, auto_unbox = TRUE)
check("refresh status file: read, FAILED -> orange; missing / broken file -> no text, not failed",
      osrm_refresh_failed(osrm_refresh_status(st_file)) && !osrm_refresh_failed(osrm_refresh_status(tempfile())) &&
        identical(osrm_refresh_text(osrm_refresh_status(tempfile()), "pt"), "") &&
        { writeLines("{broken", st_file); is.null(osrm_refresh_status(st_file)) } && !osrm_refresh_failed(rs_ok))

# ---- 18. iCloud cleanup: guard, send log, plans, end to end on a fake iCloud (rclone :local:) ----
# The real exports/ and logs/ (the app's own send log and cleanup logs) must come out unchanged
real_snapshot <- function() {
  f <- list.files(c("exports", "logs"), recursive = TRUE, full.names = TRUE, all.files = TRUE)
  i <- file.info(f)
  data.frame(file = f, size = i$size, mtime = as.numeric(i$mtime))
}
real_before <- real_snapshot()
ok_paths <- c("icloud:Arc Timeline/GPX/a.gpx", "icloud:Arc Timeline/Import/notes/2026-06.json", "icloud:Arc Timeline/Import/metadata.json")
bad_paths <- c("icloud:Arc Timeline/Backup/X/notes/2026-06.json", "icloud:Arc Timeline/GPX", "icloud:Arc Timeline/GPX/",
               "icloud:Arc Timeline/GPX/sub/a.gpx", "icloud:Arc Timeline/Import", "icloud:Arc Timeline", "icloud:",
               "icloud:Arc Timeline/GPX/../Backup/x", "icloud:Arc Timeline/Import/../Backup/notes", "other:Arc Timeline/GPX/a.gpx",
               "icloud:Arc Timeline/GPXX/a.gpx", "icloud:Arc Timeline/Import/Backup/x", "icloud:Arc Timeline/Import/./x",
               "icloud:Arc Timeline/Exports/a.gpx", NA_character_)
calls <- character(0)
rec_run <- function(args, timeout = 0) { calls <<- c(calls, paste(args, collapse = " ")); list(ok = TRUE, stdout = character(0), output = character(0)) }
refused <- vapply(bad_paths, function(p) inherits(tryCatch(icloud_cleanup_delete(p, rec_run), error = function(e) e), "error"), TRUE)
no_calls <- length(calls) == 0
check(sprintf("guard: %d allowed, %d/%d refused (no rclone call for them: %s); Import itself only for rmdirs",
              sum(vapply(ok_paths, icloud_cleanup_allowed, TRUE)), sum(refused), length(bad_paths), no_calls),
      all(vapply(ok_paths, icloud_cleanup_allowed, TRUE)) && all(refused) && no_calls &&
        !icloud_cleanup_allowed(c(ok_paths[1], ok_paths[2])) &&
        icloud_cleanup_allowed("icloud:Arc Timeline/Import", "import_root") &&
        !icloud_cleanup_allowed("icloud:Arc Timeline/GPX", "import_root") &&
        { icloud_cleanup_rmdirs_import(rec_run); identical(calls, "rmdirs icloud:Arc Timeline/Import --leave-root") })

# Send log + archive (temp files)
tl <- tempfile(); ta <- tempfile(); g1 <- tempfile(fileext = ".gpx"); writeLines("<gpx>1</gpx>", g1)
gpx_send_log_add(g1, file = tl, archive_dir = ta, now = as.POSIXct("2026-09-01 12:00:00", tz = "UTC"))
writeLines("<gpx>longer 2</gpx>", g1)
gpx_send_log_add(g1, file = tl, archive_dir = ta, now = as.POSIXct("2026-10-01 12:00:00", tz = "UTC"))
sl <- gpx_send_log_read(tl)
check(sprintf("send log: %d rows (%s), archive copy %s; missing log -> %d rows", nrow(sl), paste(sl$sent_utc, collapse = ", "),
              file.exists(file.path(ta, basename(g1))), nrow(gpx_send_log_read(tempfile()))),
      nrow(sl) == 2 && sl$file[1] == basename(g1) && sl$size[2] == file.size(g1) &&
        identical(readLines(file.path(ta, basename(g1))), "<gpx>longer 2</gpx>") && nrow(gpx_send_log_read(tempfile())) == 0)

# GPX plan
now18 <- as.POSIXct("2026-10-20 12:00:00", tz = "UTC")
log18 <- data.frame(file = c("old.gpx", "recent.gpx", "changed.gpx", "twice.gpx", "twice.gpx"),
                    sent_utc = c("2026-09-30T12:00:00Z", "2026-10-15T12:00:00Z", "2026-09-01T12:00:00Z", "2026-09-01T12:00:00Z", "2026-10-19T12:00:00Z"),
                    size = c(100, 200, 300, 400, 400), stringsAsFactors = FALSE)
remote18 <- data.frame(Name = c("old.gpx", "recent.gpx", "changed.gpx", "foreign.gpx", "twice.gpx"), Size = c(100, 200, 999, 50, 400))
plan18 <- gpx_cleanup_plan(remote18, log18, now18, 14)
check(sprintf("GPX plan: %s", paste(sprintf("%s=%s (%s)", plan18$file, plan18$action, plan18$reason), collapse = " | ")),
      identical(plan18$action, c("delete", "keep", "keep", "keep", "keep")) && grepl("not in the send log", plan18$reason[4]) &&
        grepl("differs", plan18$reason[3]) && grepl("^sent 1.0 days ago", plan18$reason[5]))

# Import package kind and decision
fl <- function(paths, sizes) data.frame(Path = paths, Size = sizes)
pk <- c(import_package_kind(fl(character(0), numeric(0))),
        import_package_kind(fl(c("metadata.json", "notes/2026-06.json", "items/2026-06.json", "places/0.json", "samples/2026-W24.json.gz"), c(509, 892, 3, 3, 23))),
        import_package_kind(fl(c("metadata.json", "items/2026-06.json", "places/0.json", "samples/2026-W24.json.gz"), c(509, 5000, 3, 9000))),
        import_package_kind(fl(c("metadata.json", "notes/2026-06.json", "items/2026-06.json", "samples/2026-W24.json.gz"), c(509, 892, 5000, 23))))
d_abs <- import_cleanup_decision(c("A", "B"), c("A", "B", "C"), now18 - 3600, now18)
d_new <- import_cleanup_decision(c("A", "B"), c("A"), now18 - 86400, now18)
d_old <- import_cleanup_decision(c("A", "B"), c("A"), now18 - 3 * 86400, now18)
check(sprintf("Import: kinds %s | absorbed -> %s | 1 day, 1 missing -> %s (pending %s) | 3 days -> pending since %s",
              paste(pk, collapse = "/"), d_abs$action, d_new$action, is.null(d_new$pending_since) == FALSE, d_old$pending_since),
      identical(pk, c("empty", "notes", "other", "other")) && d_abs$action == "delete" && d_new$action == "keep" &&
        is.null(d_new$pending_since) && d_old$action == "keep" && d_old$pending_since == "2026-10-17T12:00:00Z")
check("auth expired: trust token / 401 / reconnect yes; a timeout no",
      icloud_auth_expired(c("ERROR : trust token expired")) && icloud_auth_expired("HTTP error 401") &&
        icloud_auth_expired("please run rclone reconnect icloud:") && !icloud_auth_expired("rclone timed out after 120 s"))

# ---- End to end: fake iCloud tree, real rclone on the :local: backend ----
fake <- file.path(tempfile("fake_icloud_"), "Arc Timeline")
bk <- file.path(fake, "Backup", "AAAA-BBBB")
for (d in c("GPX", "Import", "Exports", file.path("Backup", "AAAA-BBBB", "samples"), file.path("Backup", "AAAA-BBBB", "notes"))) dir.create(file.path(fake, d), recursive = TRUE)
writeLines("[]", file.path(bk, "samples", "2026-W42.json.gz"))
writeLines("x", file.path(fake, "Exports", "keep.gpx"))
pkg18 <- file.path(tempdir(), "pkg18"); unlink(pkg18, recursive = TRUE)
write_notes_package_dir(list(list(body = "Ponto X", start_utc = now18 - 86400, end_utc = now18 - 86000),
                             list(body = "Linha 1", start_utc = now18 - 85000, end_utc = now18 - 84000)), pkg18)
file.copy(list.files(pkg18, full.names = TRUE), file.path(fake, "Import"), recursive = TRUE)
pkg_ids18 <- notes_ids_in_dir(file.path(pkg18, "notes"))
write_backup_notes <- function(ids) jsonlite::write_json(lapply(ids, function(i) list(id = i, body = "b", deleted = FALSE)),
                                                         file.path(bk, "notes", "2026-10.json"), auto_unbox = TRUE)
write_backup_notes(c("OTHER", pkg_ids18))                       # absorbed
for (f in c("old.gpx", "recent.gpx", "foreign.gpx")) writeLines(strrep("g", 100), file.path(fake, "GPX", f))
log_e2e <- tempfile(fileext = ".csv"); arch_e2e <- tempfile("arch_"); stat_e2e <- tempfile(fileext = ".json")
jsonlite::write_json(NULL, stat_e2e); unlink(stat_e2e)
utils::write.csv(data.frame(file = c("old.gpx", "recent.gpx"), sent_utc = c(format(Sys.time() - 20 * 86400, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                                                                        format(Sys.time() - 2 * 86400, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
                            size = file.size(file.path(fake, "GPX", "old.gpx"))), log_e2e, row.names = FALSE)
tree <- function() { f <- list.files(dirname(fake), recursive = TRUE, all.files = TRUE); paste(sort(f), tools::md5sum(file.path(dirname(fake), sort(f)))) }
keep_globals <- list(ICLOUD_REMOTE = ICLOUD_REMOTE, ICLOUD_ARC_DIR = ICLOUD_ARC_DIR)
ICLOUD_REMOTE <- ":local"; ICLOUD_ARC_DIR <- fake; .icloud_state$backup_id <- NULL
logs18 <- character(0); lg <- function(m) logs18 <<- c(logs18, m)
run_e2e <- function(...) icloud_cleanup(send_log = log_e2e, archive_dir = arch_e2e, status_file = stat_e2e, log = lg, ...)
before <- tree()
s_dry <- run_e2e(dry_run = TRUE, gpx_enabled = TRUE)
check(sprintf("e2e dry run: tree unchanged %s, no status file %s; says %s", identical(tree(), before), !file.exists(stat_e2e),
              paste(grep("would|keep", logs18, value = TRUE), collapse = " / ")),
      identical(tree(), before) && !file.exists(stat_e2e) && sum(grepl("would delete Import/", logs18)) == 5 &&
        any(grepl("would delete old.gpx: sent by the app", logs18)) && any(grepl("keep foreign.gpx: not in the send log", logs18)) &&
        any(grepl("keep recent.gpx: sent 2.0 days ago", logs18)))
backup_before <- grep("^Arc Timeline/(Backup|Exports)/", before, value = TRUE)
logs18 <- character(0)
s1 <- run_e2e(dry_run = FALSE, gpx_enabled = FALSE)
t1 <- tree()
check(sprintf("e2e real, GPX cleanup off: Import emptied %s (folder kept %s), GPX untouched %s, Backup/Exports untouched %s, status import '%s'",
              length(list.files(file.path(fake, "Import"), recursive = TRUE)) == 0, dir.exists(file.path(fake, "Import")),
              length(list.files(file.path(fake, "GPX"))) == 3, identical(grep("^Arc Timeline/(Backup|Exports)/", t1, value = TRUE), backup_before),
              jsonlite::fromJSON(stat_e2e)$import$state),
      length(list.files(file.path(fake, "Import"), recursive = TRUE, all.files = TRUE)) == 0 && length(list.dirs(file.path(fake, "Import"))) == 1 &&
        length(list.files(file.path(fake, "GPX"))) == 3 && identical(grep("^Arc Timeline/(Backup|Exports)/", t1, value = TRUE), backup_before) &&
        jsonlite::fromJSON(stat_e2e)$import$state == "deleted" && any(grepl("would delete when enabled old.gpx", logs18)))
s2 <- run_e2e(dry_run = FALSE, gpx_enabled = TRUE)
check(sprintf("e2e real, GPX cleanup on: GPX now %s; archived copy %s (same size %s); Backup/Exports untouched %s",
              paste(sort(list.files(file.path(fake, "GPX"))), collapse = ","), file.exists(file.path(arch_e2e, "old.gpx")),
              isTRUE(file.size(file.path(arch_e2e, "old.gpx")) == 101), identical(grep("^Arc Timeline/(Backup|Exports)/", tree(), value = TRUE), backup_before)),
      identical(sort(list.files(file.path(fake, "GPX"))), c("foreign.gpx", "recent.gpx")) && file.exists(file.path(arch_e2e, "old.gpx")) &&
        identical(grep("^Arc Timeline/(Backup|Exports)/", tree(), value = TRUE), backup_before) && isTRUE(s2$ok))
# A package Arc hasn't absorbed, sent 3 days ago: kept, pending -> orange
file.copy(list.files(pkg18, full.names = TRUE), file.path(fake, "Import"), recursive = TRUE)
for (f in list.files(file.path(fake, "Import"), recursive = TRUE, full.names = TRUE)) Sys.setFileTime(f, Sys.time() - 3 * 86400)
write_backup_notes(c("OTHER", pkg_ids18[1]))
s3 <- run_e2e(dry_run = FALSE, gpx_enabled = FALSE)
ICLOUD_REMOTE <- keep_globals$ICLOUD_REMOTE
st3 <- icloud_cleanup_status(stat_e2e)
check(sprintf("e2e not absorbed after 3 days: Import kept %s, state '%s', tooltip PT '%s' / EN '%s', orange %s",
              length(list.files(file.path(fake, "Import"), recursive = TRUE)) == 5, st3$import$state,
              icloud_cleanup_text(st3, "pt"), icloud_cleanup_text(st3, "en"), icloud_cleanup_warn(st3)),
      length(list.files(file.path(fake, "Import"), recursive = TRUE)) == 5 && st3$import$state == "waiting" &&
        grepl("^Pacote de notas aguardando Restore desde \\d\\d/\\d\\d$", icloud_cleanup_text(st3, "pt")) &&
        grepl("^Notes package waiting for Restore since \\d{4}-\\d\\d-\\d\\d$", icloud_cleanup_text(st3, "en")) && icloud_cleanup_warn(st3))
# Expired login: skipped at the first call, reconnect -> orange
expired <- function(args, timeout = 0) list(ok = FALSE, stdout = character(0), output = "ERROR : iclouddrive: trust token expired, please run rclone reconnect")
calls_after <- 0L
s4 <- icloud_cleanup(dry_run = FALSE, run = function(...) { calls_after <<- calls_after + 1L; expired(...) }, send_log = log_e2e,
                     archive_dir = arch_e2e, status_file = stat_e2e, log = lg)
st4 <- icloud_cleanup_status(stat_e2e)
check(sprintf("expired login: %d rclone call, reconnect %s, tooltip PT '%s' / EN '%s', orange %s", calls_after, st4$reconnect,
              icloud_cleanup_text(st4, "pt"), icloud_cleanup_text(st4, "en"), icloud_cleanup_warn(st4)),
      calls_after == 1L && isTRUE(st4$reconnect) && identical(icloud_cleanup_text(st4, "pt"), "Reconecte: rclone reconnect icloud:") &&
        identical(icloud_cleanup_text(st4, "en"), "Reconnect: rclone reconnect icloud:") && icloud_cleanup_warn(st4) &&
        !icloud_cleanup_warn(list(ok = TRUE, reconnect = FALSE, import = list(state = "empty"))) && identical(icloud_cleanup_text(NULL), ""))
# icloud_push_file logs every GPX it sends (temp send log / archive)
ICLOUD_REMOTE <- ":local"
keep_log <- list(GPX_SEND_LOG_FILE = GPX_SEND_LOG_FILE, GPX_ARCHIVE_DIR = GPX_ARCHIVE_DIR)
GPX_SEND_LOG_FILE <- tempfile(fileext = ".csv"); GPX_ARCHIVE_DIR <- tempfile("arch_push_")
gp <- file.path(tempdir(), "arc_batch_bus_2026-06-11_2026-06-11.gpx"); writeLines("<gpx/>", gp)
pr <- icloud_push_file(gp, subdir = "GPX")
pl <- gpx_send_log_read()
check(sprintf("push hook: sent %s, send log %d row (%s), archived %s", pr$ok, nrow(pl), pl$file[1] %||% "-",
              file.exists(file.path(GPX_ARCHIVE_DIR, basename(gp)))),
      isTRUE(pr$ok) && nrow(pl) == 1 && pl$file == basename(gp) && file.exists(file.path(fake, "GPX", basename(gp))) &&
        file.exists(file.path(GPX_ARCHIVE_DIR, basename(gp))))
GPX_SEND_LOG_FILE <- keep_log$GPX_SEND_LOG_FILE; GPX_ARCHIVE_DIR <- keep_log$GPX_ARCHIVE_DIR
ICLOUD_REMOTE <- keep_globals$ICLOUD_REMOTE; ICLOUD_ARC_DIR <- keep_globals$ICLOUD_ARC_DIR; .icloud_state$backup_id <- NULL
check(sprintf("real exports/ and logs/ untouched by the tests (%d files, same sizes and times)", nrow(real_before)),
      identical(real_snapshot(), real_before))

# ---- 19. Stationary jitter, spurs, lasso exclusions, moved route ends, map layers ----
# A street along x (UTM metres around Rio); a stop at x = 500 with GPS jitter, 70 s
T19 <- 1.76e9
jit <- c(0, 8, -6, 10, -9, 5, -7, 9)
x19 <- c(seq(0, 480, by = 40), 500 + jit, seq(540, 1000, by = 40))
y19 <- c(rep(0, 13), c(0, 6, -8, 4, 9, -5, 7, -3), rep(0, 12))
t19 <- c(T19 + (0:12) * 4, T19 + 52 + (1:8) * 10, T19 + 132 + (1:12) * 4)
s19 <- samples_at(x19, y19, t19)
cl <- stationary_clusters(s19, speed_mps = 2, radius_m = 30, min_n = 3)
walk <- samples_at(seq(0, 500, by = 25), 0, T19 + (0:20) * 10)        # 2.5 m/s: not slow
crawl <- samples_at(seq(0, 300, by = 12), 0, T19 + (0:25) * 10)       # 1.2 m/s, 300 m: slow runs fitting in 30 m only
two <- samples_at(c(0, 1, 100, 200), 0, T19 + c(0, 10, 15, 20))        # 2 slow samples: under min_n
crawl_cl <- stationary_clusters(crawl)
crawl_ext <- vapply(crawl_cl, function(m) max(abs(diff(range(seq(0, 300, by = 12)[m])))), 0)
check(sprintf("stop clusters: %d cluster of %d samples (%s); 2.5 m/s -> %d; crawl at 1.2 m/s -> %d runs of %s samples, each within 30 m of its centroid (extent <= %.0f m); 2 slow samples -> %d",
              length(cl), length(cl[[1]] %||% 0), paste(range(cl[[1]] %||% 0), collapse = "-"), length(stationary_clusters(walk)),
              length(crawl_cl), paste(unique(lengths(crawl_cl)), collapse = ","), max(crawl_ext), length(stationary_clusters(two))),
      length(cl) == 1 && all(14:20 %in% cl[[1]]) && length(stationary_clusters(walk)) == 0 && max(crawl_ext) <= 60 &&
        length(stationary_clusters(two)) == 0)

# Spurs: J -> tip -> J (a U-turn into a side street), J -> G1 -> G2 -> G1 -> J, a real loop, a long spur
pt19 <- function(x, y) ll(x, y)
A <- pt19(0, 0); B <- pt19(100, 0); J <- pt19(200, 0); C <- pt19(300, 0); TIP <- pt19(200, 60)
G1 <- pt19(200, 50); G2 <- pt19(200, 100); L1 <- pt19(250, 80); FAR1 <- pt19(200, 120); FAR2 <- pt19(200, 240)
r1 <- rbind(A, B, J, TIP, J, C)
r2 <- rbind(A, B, J, G1, G2, G1, J, C)
K <- pt19(250, 20)
r3 <- rbind(A, B, J, G1, L1, K, J, C)          # around a block back to J: not a palindrome
r4 <- rbind(A, J, FAR1, FAR2, FAR1, J, C)      # 240 m out: longer than 150
r5 <- rbind(J, TIP, J)                          # nothing would be left
same <- function(a, b) nrow(a) == nrow(b) && max(abs(a - b)) < 1e-9
check(sprintf("spurs: U-turn tip cut %s, 2-node spur cut %s, block loop kept %s, 240 m spur kept %s (cut at 300 m: %s), lone spur kept %s",
              same(remove_route_spurs(r1), rbind(A, B, J, C)), same(remove_route_spurs(r2), rbind(A, B, J, C)),
              same(remove_route_spurs(r3), r3), same(remove_route_spurs(r4, 150), r4), same(remove_route_spurs(r4, 300), rbind(A, J, C)),
              same(remove_route_spurs(r5), r5)),
      same(remove_route_spurs(r1), rbind(A, B, J, C)) && same(remove_route_spurs(r2), rbind(A, B, J, C)) &&
        same(remove_route_spurs(r3), r3) && same(remove_route_spurs(r4, 150), r4) && same(remove_route_spurs(r4, 300), rbind(A, J, C)) &&
        same(remove_route_spurs(r5), r5))

# review_match with a stub /match: it echoes the points sent on the street (y = 0), with a spur into a side
# street at the stop (as the GPS jitter does); records what was sent
sent19 <- NULL
stub19 <- function(coords, ts, rad, profile) {
  sent19 <<- list(coords = coords, t = ts)
  n <- nrow(coords)
  xy <- to_metric(coords[, 1], coords[, 2], CRS)
  tp <- from_metric(xy[, 1], rep(Y0, n), CRS)
  geom <- rbind(ll(0, 0), ll(480, 0), ll(500, 0), ll(500, 40), ll(500, 0), ll(1000, 0))
  colnames(geom) <- c("lon", "lat"); colnames(tp) <- c("lon", "lat")
  list(ok = TRUE, pieces = list(list(coords = geom, idx = seq_len(n))), tracepoints = tp, sent = rep(TRUE, n),
       confidence = 0.9, n_dropped = 0L)
}
th_off <- TREVIEW <- REVIEW_THRESHOLDS
th_off$match_fixes$collapse_stops <- FALSE; th_off$match_fixes$remove_spurs <- FALSE
th_on <- th_off; th_on$match_fixes$collapse_stops <- TRUE; th_on$match_fixes$remove_spurs <- TRUE
m_off <- review_match(s19, "car", th_off, match_fn = stub19); n_off <- nrow(sent19$coords)
m_on <- review_match(s19, "car", th_on, match_fn = stub19); n_on <- nrow(sent19$coords)
rep_xy <- to_metric(sent19$coords[, 1], sent19$coords[, 2], CRS)
cen <- c(mean(x19[cl[[1]]]), mean(y19[cl[[1]]]))
k_rep <- which(abs(rep_xy[, 1] - X0 - cen[1]) < 0.5)
geom_len <- function(m) path_cumulative_m(m$pieces[[1]]$coords)[nrow(m$pieces[[1]]$coords)]
check(sprintf("review_match: sent %d points without fixes, %d with the stop collapsed (one at its centroid: %s); every stop sample matched (%s) at one place; spur cut %s (route %.0f -> %.0f m)",
              n_off, n_on, length(k_rep) == 1, all(cl[[1]] %in% m_on$pieces[[1]]$idx),
              m_on$spurs_removed > 0, geom_len(m_off), geom_len(m_on)),
      n_off == nrow(s19) && n_on == nrow(s19) - length(cl[[1]]) + 1 && length(k_rep) == 1 &&
        all(cl[[1]] %in% m_on$pieces[[1]]$idx) && nrow(unique(round(m_on$tracepoints[cl[[1]], ], 9))) == 1 &&
        m_on$spurs_removed == 2 && abs(geom_len(m_off) - geom_len(m_on) - 80) < 1 &&
        identical(review_match_signature(th_off), "stops0;spurs0;tidy1") && grepl("^stops2,30,3;spurs150;tidy1$", review_match_signature(th_on)))
# Timing still uses all the samples: the path dwells at the stop for its duration
fx19 <- road_timed_path(s19, m_on, "car", th_on, route_fn = function(...) NULL)
p19 <- fx19$path
pxy <- to_metric(p19$lon, p19$lat, CRS)
at_stop <- abs(pxy[, 1] - X0 - 500) < 6
check(sprintf("timing with the stop collapsed: %.0f s within 6 m of the stop (recorded stop %.0f s), path %s..%s",
              diff(range(p19$t[at_stop])), diff(range(t19[cl[[1]]])), format(min(p19$t) - T19), format(max(p19$t) - T19)),
      diff(range(p19$t[at_stop])) >= diff(range(t19[cl[[1]]])) - 1 && min(p19$t) == t19[1] && max(p19$t) == t19[length(t19)])
# Lasso exclusions: the samples are not sent, then placed on the piece between their neighbours
ex_t <- t19[16:18]
m_ex <- review_match(s19, "car", th_off, exclude = ex_t, match_fn = stub19)
ex_xy <- to_metric(m_ex$tracepoints[16:18, 1], m_ex$tracepoints[16:18, 2], CRS)
nb_x <- to_metric(m_ex$tracepoints[c(15, 19), 1], m_ex$tracepoints[c(15, 19), 2], CRS)[, 1]
check(sprintf("exclusions: %d sent of %d, excluded flagged %s, placed on the piece (%s) between their neighbours (%s)",
              nrow(sent19$coords), nrow(s19), identical(which(m_ex$excluded), 16:18), all(16:18 %in% m_ex$pieces[[1]]$idx),
              all(ex_xy[, 1] >= min(nb_x) - 0.01 & ex_xy[, 1] <= max(nb_x) + 0.01)),
      nrow(sent19$coords) == nrow(s19) - 3 && identical(which(m_ex$excluded), 16:18) && all(16:18 %in% m_ex$pieces[[1]]$idx) &&
        all(ex_xy[, 1] >= min(nb_x) - 0.01 & ex_xy[, 1] <= max(nb_x) + 0.01) && !any(is.na(m_ex$tracepoints[16:18, ])))

# Moved route ends: the route starts at the dragged point; the times stay the trip's
via_pts <- NULL
via_stub <- function(pts, profile, bearings = NULL, radiuses = NULL) {
  via_pts <<- pts
  list(coords = rbind(pts[1, ], ll(500, 0), pts[nrow(pts), ]), distance_m = 1000)
}
bldg <- as.numeric(ll(-30, 25))
pf19 <- pinned_route_fix(s19, NULL, "car", start = bldg, route_via_fn = via_stub)
check(sprintf("moved start: first route point is the dragged one %s, no pins %d, path %s..%s s",
              isTRUE(all.equal(as.numeric(via_pts[1, ]), bldg)), pf19$n_pins, format(pf19$path$t[1] - T19), format(pf19$path$t[nrow(pf19$path)] - T19)),
      isTRUE(all.equal(as.numeric(via_pts[1, ]), bldg)) && nrow(via_pts) == 2 && pf19$n_pins == 0 &&
        pf19$path$t[1] == t19[1] && pf19$path$t[nrow(pf19$path)] == t19[length(t19)] &&
        isTRUE(all.equal(as.numeric(via_pts[2, ]), c(s19$lon[nrow(s19)], s19$lat[nrow(s19)]))))

# Layers: bus infrastructure by bounds, router tiles URL
bi <- list(features = list(
  list(properties = list(kind = "busway")), list(properties = list(kind = "blocked")), list(properties = list(kind = "bus_lane"))),
  bbox = rbind(c(-43.30, -22.90, -43.29, -22.89), c(-43.295, -22.895, -43.294, -22.894), c(-43.10, -22.80, -43.09, -22.79)))
b_in <- bus_infra_in_bounds(-43.31, -22.91, -43.28, -22.88, 15, data = bi)
b_cap <- bus_infra_in_bounds(-43.31, -22.91, -43.28, -22.88, 15, max_ways = 1, data = bi)
check(sprintf("bus layer: %d ways in view (kinds %s), capped -> %s, zoom 11 -> %d; router URL %s",
              length(b_in), paste(vapply(b_in, function(f) f$properties$kind, ""), collapse = ","),
              b_cap[[1]]$properties$kind, length(bus_infra_in_bounds(-43.31, -22.91, -43.28, -22.88, 11, data = bi)), router_tile_url("official")),
      length(b_in) == 2 && identical(b_cap[[1]]$properties$kind, "busway") &&
        length(bus_infra_in_bounds(-43.31, -22.91, -43.28, -22.88, 11, data = bi)) == 0 &&
        identical(router_tile_url("official"), "http://127.0.0.1:5005/tile/v1/driving/tile({x},{y},{z}).mvt"))
real_bi <- bus_infra_data()
check(sprintf("bus layer file: %d ways, kinds %s", length(real_bi$features %||% list()),
              paste(names(table(vapply(real_bi$features %||% list(), function(f) f$properties$kind, ""))), collapse = ",")),
      length(real_bi$features) > 1000 && all(c("busway", "bus_only", "bus_lane", "blocked") %in%
                                              vapply(real_bi$features, function(f) f$properties$kind, "")))
pyc <- system2("python3", c("-I", "-c", shQuote(paste(
  "import sys; sys.path.insert(0, 'tools'); import refresh_bus_infra as b;",
  "assert b.kind_of({'highway': 'busway'}) == 'busway';",
  "assert b.kind_of({'highway': 'service', 'access': 'no', 'bus': 'designated'}) == 'bus_only';",
  "assert b.kind_of({'highway': 'primary', 'busway:right': 'lane'}) == 'bus_lane';",
  "assert b.kind_of({'highway': 'primary', 'lanes:bus': '1'}) == 'bus_lane';",
  "assert b.kind_of({'highway': 'service', 'access': 'private'}) == 'blocked';",
  "assert b.kind_of({'highway': 'residential'}) is None;",
  "assert b.oneway_of({'oneway': 'yes'}) == 1 and b.oneway_of({'oneway': 'yes', 'oneway:bus': 'no'}) == 0 and b.oneway_of({'oneway': '-1'}) == -1;",
  "s = b.simplify([[0, 0], [0.0001, 0.000001], [0.0002, 0]]); assert len(s) == 2;",
  "print('ok')"))), stdout = TRUE, stderr = TRUE)
check(sprintf("bus layer builder (Python): kinds, oneway, simplification: %s", paste(pyc, collapse = " ")), identical(pyc, "ok"))

# ---- Pins as local splices, per-pin rules, free legs, maintenance flag ----
# Fake routers: a straight line between the leg's ends (calls recorded), and a
# small road network (edges in metres, one-way unless both = TRUE): both ends
# snap to its nearest node (every 5 m), the shortest path (igraph)
legs_seen <- list()
straight_route <- function(xy, opts, bearings = NULL, radiuses = NULL) {
  legs_seen[[length(legs_seen) + 1L]] <<- opts
  xy
}
road <- function(x1, y1, x2, y2, both = FALSE) data.frame(x1 = x1, y1 = y1, x2 = x2, y2 = y2, both = both)
net_router <- function(edges, step = 5) {
  key <- function(x, y) paste(round(x, 2), round(y, 2))
  E <- do.call(rbind, lapply(seq_len(nrow(edges)), function(i) {
    e <- edges[i, ]
    L <- sqrt((e$x2 - e$x1)^2 + (e$y2 - e$y1)^2)
    f <- seq(0, 1, length.out = max(1, ceiling(L / step)) + 1)
    x <- e$x1 + f * (e$x2 - e$x1); y <- e$y1 + f * (e$y2 - e$y1); n <- length(f)
    d <- data.frame(a = key(x[-n], y[-n]), b = key(x[-1], y[-1]), w = L / (n - 1), ax = x[-n], ay = y[-n], bx = x[-1], by = y[-1])
    if (e$both) d <- rbind(d, data.frame(a = d$b, b = d$a, w = d$w, ax = d$bx, ay = d$by, bx = d$ax, by = d$ay))
    d
  }))
  V <- rbind(data.frame(k = E$a, x = E$ax, y = E$ay), data.frame(k = E$b, x = E$bx, y = E$by))
  V <- V[!duplicated(V$k), ]
  g <- igraph::graph_from_data_frame(E[, c("a", "b")], directed = TRUE, vertices = V$k)
  function(xy, opts, bearings = NULL, radiuses = NULL) {
    legs_seen[[length(legs_seen) + 1L]] <<- opts
    m <- to_metric(xy[, 1], xy[, 2], CRS)
    near <- function(i) V$k[which.min((V$x - (m[i, 1] - X0))^2 + (V$y - (m[i, 2] - Y0))^2)]
    p <- suppressWarnings(igraph::shortest_paths(g, near(1), near(2), weights = E$w, output = "vpath")$vpath[[1]])
    if (length(p) < 2) return(NULL)
    v <- V[match(igraph::V(g)$name[p], V$k), ]
    ll(v$x, v$y)
  }
}
pin_at <- function(id, x, y, gx = x, gy = 0, ...) {
  p <- ll(x, y); g <- ll(gx, gy)
  data.frame(id = id, lon = p[1, 1], lat = p[1, 2], after = NA_character_, grab_lon = g[1, 1], grab_lat = g[1, 2], ...)
}
keys_of <- function(sg) vapply(sg$legs, `[[`, "", "key")
same_legs <- function(x, y) isTRUE(all.equal(lapply(x$legs, `[[`, "coords"), lapply(y$legs, `[[`, "coords")))
# The base: 6 km east along y = 0 (a vertex every 50 m), driven at 10 m/s, on a two-way road. A parallel
# one-way carriageway at y = 30 from x = 1000 to 3000 (ramps from 900 and down to 3100), a two-way dead-end spur
# north at 3500, a one-way bulge north of 4000-5000 (600 m up), side streets at both ends
nx <- seq(0, 6000, by = 50)
n_base <- samples_at(nx, rep(0, length(nx)), nx / 10)
n_path <- n_base[, c("lon", "lat", "t")]
n_s <- n_base[seq(1, nrow(n_base), by = 2), ]
net <- net_router(rbind(
  road(0, 0, 6000, 0, TRUE),
  road(900, 0, 1000, 30), road(1000, 30, 3000, 30), road(3000, 30, 3100, 0),
  road(3500, 0, 3500, 400, TRUE),
  road(4000, 0, 4000, 600), road(4000, 600, 5000, 600), road(5000, 600, 5000, 0),
  road(0, -200, 0, 0, TRUE), road(6000, 0, 6000, 200, TRUE)
))
nb_xy <- as.matrix(n_path[, c("lon", "lat")])
nb_cum <- path_cumulative_m(nb_xy)
# rows of the result equal to base vertices before lo / after hi, exactly
outside_same <- function(fx, lo, hi, bxy = nb_xy, bcum = nb_cum) {
  co <- fx$coords
  before <- bxy[bcum < lo - 0.5, , drop = FALSE]
  after <- bxy[bcum > hi + 0.5, , drop = FALSE]
  isTRUE(all.equal(unname(co[seq_len(nrow(before)), ]), unname(before), tolerance = 0)) &&
    isTRUE(all.equal(unname(co[nrow(co) - rev(seq_len(nrow(after))) + 1L, ]), unname(after), tolerance = 0))
}
local_xy <- function(co) { m <- to_metric(co[, 1], co[, 2], CRS); cbind(x = m[, 1] - X0, y = m[, 2] - Y0) }
splice_xy <- function(sg) local_xy(do.call(rbind, lapply(sg$legs, `[[`, "coords")))
splice_len <- function(sg) sum(vapply(sg$legs, `[[`, numeric(1), "length_m"))

f1 <- splice_route_fix(n_s, n_path, pin_at("1", 2000, 30), "car", route_fn = net)
g1 <- f1$splices[[1]]
x1 <- splice_xy(g1)
mid1 <- x1[x1[, "x"] > 1100 & x1[, "x"] < 2900, , drop = FALSE]
check(sprintf("splice, pin on the parallel carriageway: replaces [%.0f, %.0f] m (it leaves the base at the ramp ~915, rejoins ~3085), on the parallel road between them %s, %.0f m for %.0f m of base, warning %s, outside identical %s",
              g1$lo, g1$hi, all(abs(mid1[, "y"] - 30) < 1), splice_len(g1), g1$hi - g1$lo, length(f1$warn) > 0, outside_same(f1, g1$lo, g1$hi)),
      length(f1$splices) == 1 && abs(g1$lo - 916) < 10 && abs(g1$hi - 3084) < 10 && nrow(mid1) > 10 && all(abs(mid1[, "y"] - 30) < 1) &&
        length(f1$warn) == 0 && isTRUE(g1$ok) && outside_same(f1, g1$lo, g1$hi) && identical(keys_of(g1), c("a:1>1", "1>b:1")) &&
        min(geosphere::distHaversine(f1$coords, ll(2000, 30))) < 3)
f2 <- splice_route_fix(n_s, n_path, rbind(pin_at("1", 1500, 30), pin_at("2", 2500, 30)), "car", route_fn = net)
check(sprintf("no join before the next pin: one splice through both [%.0f, %.0f] m, legs %s",
              f2$splices[[1]]$lo, f2$splices[[1]]$hi, paste(keys_of(f2$splices[[1]]), collapse = " ")),
      length(f2$splices) == 1 && identical(keys_of(f2$splices[[1]]), c("a:1>1", "1>2", "2>b:2")) &&
        abs(f2$splices[[1]]$lo - 916) < 10 && abs(f2$splices[[1]]$hi - 3084) < 10)
f3 <- splice_route_fix(n_s, n_path, pin_at("3", 4600, 0), "car", route_fn = net)
g3 <- f3$splices[[1]]
check(sprintf("a pin on the base's own road: a short local splice [%.0f, %.0f] m, outside identical %s",
              g3$lo, g3$hi, outside_same(f3, g3$lo, g3$hi)),
      length(f3$splices) == 1 && g3$hi - g3$lo < 100 && g3$lo < 4600 && g3$hi > 4600 && outside_same(f3, g3$lo, g3$hi) &&
        identical(keys_of(g3), c("a:3>3", "3>b:3")) && length(f3$warn) == 0)
f4 <- splice_route_fix(n_s, n_path, pin_at("4", 3500, 300), "car", route_fn = net)
f5 <- splice_route_fix(n_s, n_path, pin_at("5", 4500, 600), "car", route_fn = net)
g5 <- f5$splices[[1]]
check(sprintf("warning: a dead-end spur (in and back out, %.0f m for %.0f m of base) -> %s; the 600 m bulge (%.0f m for %.0f m, x%.1f, no turning back) -> %s",
              splice_len(f4$splices[[1]]), f4$splices[[1]]$hi - f4$splices[[1]]$lo, paste(f4$warn, collapse = ",") %||% "-",
              splice_len(g5), g5$hi - g5$lo, splice_len(g5) / (g5$hi - g5$lo), if (length(f5$warn)) paste(f5$warn, collapse = ",") else "none"),
      identical(f4$warn, "4") && isTRUE(f4$pins$warn[1]) && length(f5$warn) == 0 && isTRUE(g5$ok) &&
        splice_len(g5) / (g5$hi - g5$lo) > 1.5 && abs(g5$lo - 4000) < 10 && abs(g5$hi - 5000) < 10)
fa <- splice_route_fix(n_s, n_path, pin_at("1", 2000, 30), "car", route_fn = net)
fb <- splice_route_fix(n_s, n_path, pin_at("2", 4500, 600), "car", route_fn = net)
f6 <- splice_route_fix(n_s, n_path, rbind(pin_at("1", 2000, 30), pin_at("2", 4500, 600)), "car", route_fn = net)
check(sprintf("two pins that each rejoin the base: two splices [%s], each equal to that pin alone, the base between them identical",
              paste(vapply(f6$splices, function(g) sprintf("%.0f-%.0f", g$lo, g$hi), ""), collapse = ", ")),
      length(f6$splices) == 2 && same_legs(f6$splices[[1]], fa$splices[[1]]) && same_legs(f6$splices[[2]], fb$splices[[1]]))
wig_base <- samples_at(nx, ifelse(nx %% 100 == 50, 0.3, 0), nx / 10)   # a base with sub-2-degree bends
wb_path <- wig_base[, c("lon", "lat", "t")]
fw <- splice_route_fix(wig_base[seq(1, nrow(wig_base), 2), ], wb_path, pin_at("1", 2000, 30), "car", route_fn = net)
wb_xy <- as.matrix(wb_path[, c("lon", "lat")]); wb_cum <- path_cumulative_m(wb_xy)
out_v <- wb_xy[wb_cum < fw$splices[[1]]$lo - 1 | wb_cum > fw$splices[[1]]$hi + 1, , drop = FALSE]
fp <- as.matrix(fw$path[, c("lon", "lat")]); fp_cum <- path_cumulative_m(fp)
off_v <- vapply(seq_len(nrow(out_v)), function(k) min(polyline_segment_projection(fp, fp_cum, out_v[k, ])$off), numeric(1))
check(sprintf("splice: the timed path keeps every base vertex outside the splice, small bends included (max off %.1e m over %d vertices)",
              max(off_v), length(off_v)), max(off_v) < 0.01 && length(off_v) > 50)
# Moved start / end: from the new start to where its route joins the base; a pin near the moved end runs on to it
st <- ll(0, -200)
f7 <- splice_route_fix(n_s, n_path, NULL, "car", ends = list(start = st[1, ]), route_fn = net)
check(sprintf("start drag: moved start -> joins the base at %.0f m, the rest of the base identical, starts at the new start",
              f7$splices[[1]]$hi),
      f7$splices[[1]]$hi < 10 && outside_same(f7, 0, f7$splices[[1]]$hi) && all(abs(f7$coords[1, ] - st[1, ]) < 1e-9) &&
        identical(keys_of(f7$splices[[1]]), "start>b:start"))
# A moved start and a pin farther on, where the route from the start straight to the pin skips the base (a
# diagonal street from the start's side street to the parallel carriageway): two splices, the pin's the same as
# with the pin alone
net_diag <- net_router(rbind(
  road(0, 0, 6000, 0, TRUE),
  road(900, 0, 1000, 30), road(1000, 30, 3000, 30), road(3000, 30, 3100, 0),
  road(0, -200, 0, 0, TRUE), road(0, -200, 1000, 30), road(6000, 0, 6000, 200, TRUE)
))
f7p <- splice_route_fix(n_s, n_path, pin_at("1", 2000, 30), "car", ends = list(start = st[1, ]), route_fn = net_diag)
f7q <- splice_route_fix(n_s, n_path, pin_at("1", 2000, 30), "car", route_fn = net_diag)
check(sprintf("start drag + a pin 2 km on (start -> pin would skip the base): splices %s; the pin's [%.0f, %.0f] m, the same as the pin alone %s",
              paste(vapply(f7p$splices, function(g) paste(keys_of(g), collapse = " "), ""), collapse = " | "),
              f7p$splices[[length(f7p$splices)]]$lo, f7p$splices[[length(f7p$splices)]]$hi,
              length(f7p$splices) == 2 && same_legs(f7p$splices[[2]], f7q$splices[[1]])),
      length(f7p$splices) == 2 && identical(keys_of(f7p$splices[[1]]), "start>b:start") && f7p$splices[[1]]$hi < 10 &&
        identical(keys_of(f7p$splices[[2]]), c("a:1>1", "1>b:1")) && same_legs(f7p$splices[[2]], f7q$splices[[1]]))
# A fixed point's start looks for the base only near the trip's start (reach): the route toward the base far ahead
# would take the diagonal and replace ~3 km of the recording; a dragged start keeps the far search
fr0 <- splice_route_fix(n_s, n_path, NULL, "car", ends = list(start = st[1, ]), route_fn = net_diag)
fr1 <- splice_route_fix(n_s, n_path, NULL, "car", ends = list(start = st[1, ]), route_fn = net_diag, reach = list(start = 500))
te_r <- review_trip_ends(list(end = unname(st[1, ])), list(start = list(name = "A", lon = st[1, 1], lat = st[1, 2]), end = list(name = "B", lon = 0, lat = 0)))
check(sprintf("fixed point's reach: dragged start joins the base at %.0f m (splice %.0f m, short %s); within %s m of the start it joins at %.0f m (%s, %.0f m, short %s); reach only for the anchored end (%s)",
              fr0$splices[[1]]$hi, splice_len(fr0$splices[[1]]), anchor_splices_short(fr0), REVIEW_THRESHOLDS$pins$splice$anchor_reach_m,
              fr1$splices[[1]]$hi, paste(keys_of(fr1$splices[[1]]), collapse = " "), splice_len(fr1$splices[[1]]), anchor_splices_short(fr1),
              paste(names(te_r$reach), collapse = ",")),
      fr0$splices[[1]]$hi > 2500 && !anchor_splices_short(fr0) && fr1$splices[[1]]$hi < 10 &&
        identical(keys_of(fr1$splices[[1]]), "start>b:start") && anchor_splices_short(fr1) &&
        identical(names(te_r$reach), "start") && te_r$reach$start == REVIEW_THRESHOLDS$pins$splice$anchor_reach_m)
en <- ll(6000, 200)
fr2 <- splice_route_fix(n_s, n_path, NULL, "car", ends = list(end = en[1, ]), route_fn = net, reach = list(end = 500))
check(sprintf("fixed point's reach at the end: splice %s from %.0f m of %.0f", paste(keys_of(fr2$splices[[1]]), collapse = " "), fr2$splices[[1]]$lo, nb_cum[length(nb_cum)]),
      identical(keys_of(fr2$splices[[1]]), "a:end>end") && fr2$splices[[1]]$lo >= nb_cum[length(nb_cum)] - 500)
f8 <- splice_route_fix(n_s, n_path, pin_at("1", 5900, 0), "car", ends = list(end = en[1, ]), route_fn = net)
check(sprintf("end drag with a pin 100 m before the end: one splice, legs %s", paste(keys_of(f8$splices[[1]]), collapse = " ")),
      length(f8$splices) == 1 && identical(keys_of(f8$splices[[1]]), c("a:1>1", "1>end")) &&
        geosphere::distHaversine(f8$coords[nrow(f8$coords), ], en[1, ]) < 3)
# Free legs: the recorded samples / a straight line, timing monotonic on whole seconds
wig <- samples_at(nx, ifelse(nx > 1700 & nx < 2000, 60, 0), nx / 10)
f9 <- splice_route_fix(wig, n_path, pin_at("1", 2000, 30), "car", legs = c("a:1>1" = "recorded", "1>b:1" = "straight"), route_fn = net)
lg9 <- f9$splices[[1]]$legs
p9 <- f9$path
check(sprintf("free legs: %s; recorded leg has %d points through the samples; times whole seconds, strictly increasing, %s-%s",
              paste(vapply(lg9, `[[`, "", "mode"), collapse = "/"), nrow(lg9[[1]]$coords), p9$t[1], p9$t[nrow(p9)]),
      identical(vapply(lg9, `[[`, "", "mode"), c("recorded", "straight")) && nrow(lg9[[1]]$coords) > 2 &&
        nrow(lg9[[2]]$coords) == 2 && all(diff(p9$t) > 0) && all(p9$t == round(p9$t)) &&
        p9$t[1] == round(wig$t[1]) && p9$t[nrow(p9)] == round(wig$t[nrow(wig)]) && isTRUE(f9$on_network))
f9b <- splice_route_fix(wig, n_path, pin_at("1", 2000, 30), "car", route_fn = function(...) NULL)
check(sprintf("no route for a pin: no straight line, the previous path kept (%d splices, identical %s), pin %s flagged (%s)",
              length(f9b$splices), outside_same(f9b, -1, -1), paste(f9b$warn, collapse = ","), f9b$fail[["1"]] %||% "-"),
      length(f9b$splices) == 0 && outside_same(f9b, -1, -1) && nrow(f9b$coords) == nrow(nb_xy) && identical(f9b$warn, "1") &&
        identical(f9b$fail[["1"]], "route") && isTRUE(f9b$pins$warn[1]) && identical(f9b$pins$warn_text[1], "route"))
# Dragged ends: the route from / to the drop (never a straight line), the trip's start / end moved by the drive
# time of the stretch added; the regression: a start whose route joins the path right at the old start (0 s of
# base to time it) used to collapse into a straight line
st0 <- ll(0, -200)[1, ]
fe1 <- splice_route_fix(n_s, n_path, NULL, "car", ends = list(start = st0), route_fn = net)
p1 <- local_xy(as.matrix(fe1$path[, c("lon", "lat")]))
head1 <- fe1$path[p1[, "y"] < -1, ]
gap1 <- max(sqrt(rowSums(diff(local_xy(as.matrix(head1[, c("lon", "lat")])))^2)))
check(sprintf("start dragged 200 m up a side street (the route joins the path at the old start): %d points on the side street, largest gap %.0f m, starts %+.0f s (200 m at 8 m/s), times increasing %s",
              nrow(head1), gap1, fe1$path$t[1], all(diff(fe1$path$t) > 0)),
      nrow(head1) >= 5 && gap1 < 60 && fe1$path$t[1] == -25 && all(diff(fe1$path$t) > 0) && isTRUE(abs(fe1$t_start_new + 25) < 0.5) &&
        !isTRUE(fe1$start_clamped))
net_dur <- function(xy, opts, bearings = NULL, radiuses = NULL) {
  r <- net(xy, opts, bearings, radiuses)
  if (!is.null(r)) attr(r, "duration_s") <- path_cumulative_m(r)[nrow(r)] / 5
  r
}
fe2 <- splice_route_fix(n_s, n_path, NULL, "car", ends = list(start = st0), route_fn = net_dur, start_offset_s = -30)
en0 <- ll(6000, 200)[1, ]
fe3 <- splice_route_fix(n_s, n_path, NULL, "car", ends = list(end = en0), route_fn = net)
fe4 <- splice_route_fix(n_s, n_path, NULL, "car", ends = list(start = st0, end = en0), route_fn = net, bounds = c(-10, 610))
check(sprintf("times: OSRM's drive time (5 m/s) and -30 s -> start %+.0f s; end dragged 200 m -> end %+.0f s (ends %+.0f); clamped to the neighbouring items: start %+.0f (%s), end %+.0f (%s)",
              fe2$path$t[1], fe3$path$t[nrow(fe3$path)] - 600, fe3$t_end_new - 600, fe4$path$t[1], fe4$start_clamped,
              fe4$path$t[nrow(fe4$path)] - 600, fe4$end_clamped),
      fe2$path$t[1] == -70 && fe3$path$t[nrow(fe3$path)] == 625 && fe4$path$t[1] == -10 && isTRUE(fe4$start_clamped) &&
        fe4$path$t[nrow(fe4$path)] == 610 && isTRUE(fe4$end_clamped) && all(diff(fe4$path$t) > 0))
fe5 <- splice_route_fix(n_s, n_path, NULL, "car", ends = list(start = st0), route_fn = function(...) NULL)
off <- ll(40, -150)[1, ]
fe6 <- splice_route_fix(n_s, n_path, NULL, "car", ends = list(start = off), route_fn = net)
far_off <- ll(-400, -600)[1, ]
fe7 <- splice_route_fix(n_s, n_path, NULL, "car", ends = list(start = far_off), route_fn = net)
check(sprintf("no straight line for a dragged end: no route -> %s, path from the old start %s; dropped 40 m off the road -> connector %s, path from the drop %s; 500 m from any road -> %s",
              fe5$fail$start %||% "-", isTRUE(all.equal(unname(fe5$coords[1, ]), unname(nb_xy[1, ]))), !is.null(fe6$splices[[1]]$connector),
              isTRUE(geosphere::distHaversine(fe6$coords[1, ], off) < 0.01), fe7$fail$start %||% "-"),
      identical(fe5$fail$start, "route") && length(fe5$splices) == 0 && isTRUE(all.equal(unname(fe5$coords[1, ]), unname(nb_xy[1, ]))) &&
        is.null(fe5$t_start_new) && !is.null(fe6$splices[[1]]$connector) && geosphere::distHaversine(fe6$coords[1, ], off) < 0.01 &&
        nrow(fe6$splices[[1]]$connector) == 2 && identical(fe7$fail$start, "no_road") && length(fe7$splices) == 0)
# Pin snapping: with the recorded bearing the router ends off_m from the pin (another road); without it, at the pin
snap_router <- function(off_with, off_without) {
  calls <- list()
  f <- function(xy, opts, bearings = NULL, radiuses = NULL) {
    calls[[length(calls) + 1L]] <<- bearings
    off <- if (!is.na(bearings[2])) off_with else off_without
    m <- to_metric(xy[, 1], xy[, 2], CRS)
    rbind(xy[1, ], from_metric(m[2, 1] + off, m[2, 2], CRS))
  }
  list(f = f, calls = function() calls)
}
pin_near <- pin_at("1", 2000, 40)                       # near the recording: it gets the recording's bearing
r1 <- snap_router(9, 0.5)
fs1 <- splice_route_fix(n_s, n_path, pin_near, "car", route_fn = r1$f)
r2 <- snap_router(6, 4)
fs2 <- splice_route_fix(n_s, n_path, pin_near, "car", route_fn = r2$f)
leg2_start <- r1$calls()[[length(r1$calls())]][1]
leg1_end <- function(fx) { co <- fx$splices[[1]]$legs[[1]]$coords; geosphere::distHaversine(co[nrow(co), ], ll(2000, 40)[1, ]) }
check(sprintf("pin snap: 9 m off with the recorded bearing, 0.5 m without -> without (leg ends %.1f m from the pin); 6 m vs 4 m -> keeps the bearing (%.1f m); the next leg leaves the pin as the first arrived (bearing %.0f)",
              leg1_end(fs1), leg1_end(fs2), leg2_start),
      abs(leg1_end(fs1) - 0.5) < 0.1 && abs(leg1_end(fs2) - 6) < 0.1 && !is.na(fs1$pins$bearing[1]) && !is.na(leg2_start) && abs(leg2_start - 90) < 10)
# A splice doesn't re-time the base outside it
tb <- f1
far <- n_path[nb_cum < tb$splices[[1]]$lo - 50 | nb_cum > tb$splices[[1]]$hi + 50, ]
t_far <- vapply(seq_len(nrow(far)), function(k) tb$path$t[which.min(geosphere::distHaversine(as.matrix(tb$path[, c("lon", "lat")]), c(far$lon[k], far$lat[k])))], numeric(1))
check(sprintf("splice timing: outside the splice the base keeps its times (max difference %.1f s over %d vertices)", max(abs(t_far - round(far$t))), nrow(far)),
      max(abs(t_far - round(far$t))) <= 1)
# The searches' helpers on their own
bj_m <- to_metric(nb_xy[, 1], nb_xy[, 2], CRS)
r_on <- ll(c(1000, 1100, 1300, 1500, 2000), c(30, 2, 0, 0, 0))    # comes down onto the base at ~1100 and stays
j_on <- base_join(r_on, bj_m, nb_cum, CRS, c(0, 6000))
r_short <- ll(c(1000, 1100, 1200, 1210), c(30, 0, 0, 30))          # touches the base for 100 m only
j_short <- base_join(r_short, bj_m, nb_cum, CRS, c(0, 6000))
r_back <- ll(c(1000, 1100, 1300, 1500), c(0, 0, 0, 0))[4:1, ]       # on the base but heading the other way
j_back <- base_join(r_back, bj_m, nb_cum, CRS, c(0, 6000))
check(sprintf("base_join: joins at %.0f m (route), %.0f m (base); a 100 m touch -> %s; on the base heading the other way -> %s; backtracks: spur %s, bulge %s, straight %s",
              j_on$s %||% NA, j_on$d %||% NA, if (is.null(j_short)) "none" else "a join", if (is.null(j_back)) "none" else "a join",
              path_backtracks(do.call(rbind, lapply(f4$splices[[1]]$legs, `[[`, "coords"))),
              path_backtracks(do.call(rbind, lapply(g5$legs, `[[`, "coords"))), path_backtracks(ll(c(0, 1000, 2000), c(0, 0, 0)))),
      abs(j_on$d - 1100) < 15 && is.null(j_short) && is.null(j_back) &&
        path_backtracks(do.call(rbind, lapply(f4$splices[[1]]$legs, `[[`, "coords"))) &&
        !path_backtracks(do.call(rbind, lapply(g5$legs, `[[`, "coords"))) && !path_backtracks(ll(c(0, 1000, 2000), c(0, 0, 0))))

# The rules tests below: a 4 km base along y = 0, the straight router
sx <- seq(0, 4000, by = 50)
sp_base <- samples_at(sx, rep(0, length(sx)), sx / 10)
sp_path <- sp_base[, c("lon", "lat", "t")]
sp_s <- sp_base[seq(1, nrow(sp_base), by = 2), ]

# Per-pin rules -> server and exclude
r_no <- pin_osrm_options("official", "no", "respect"); r_mi <- pin_osrm_options("official", "minor", "respect")
r_all <- pin_osrm_options("official", "all", "ignore"); r_car <- pin_osrm_options("car", "all", "ignore")
check(sprintf("rules: no -> %s/%s, minor -> %s/%s, all+ignore -> %s/'%s', car -> %s/default",
              r_no$profile, r_no$exclude, r_mi$profile, r_mi$exclude, r_all$profile, r_all$exclude, r_car$profile),
      identical(r_no, list(profile = "official", exclude = "wwminor,wwmajor")) &&
        identical(r_mi, list(profile = "official", exclude = "wwmajor")) &&
        identical(r_all, list(profile = "official-noturn", exclude = "")) && identical(r_car, list(profile = "car", exclude = NULL)) &&
        identical(osrm_exclude_for("official"), "wwmajor") && identical(osrm_exclude_for("official", ""), "") &&
        identical(osrm_exclude_for("car"), "") && identical(osrm_exclude_for("official-noturn"), "wwmajor"))
lr <- leg_rules("minor", "respect", "all", "ignore")
check("rules: a leg between two pins takes the more permissive (all, ignore); anchors (NA) don't count",
      identical(lr, list(ww = "all", turns = "ignore")) && identical(leg_rules(NA, NA, "no", "respect"), list(ww = "no", turns = "respect")))
legs_seen <- list()
invisible(splice_route_fix(sp_s, sp_path, pin_at("1", 2000, 120, ww = "all", turns = "ignore"), "official", route_fn = straight_route))
check(sprintf("official trip: a pin's legs are asked with %s",
              paste(unique(vapply(legs_seen, function(o) paste0(o$profile, " exclude='", o$exclude, "'"), "")), collapse = "; ")),
      length(legs_seen) == 2 && all(vapply(legs_seen, function(o) identical(o, list(profile = "official-noturn", exclude = "")), TRUE)))
legs_seen <- list()
invisible(splice_route_fix(sp_s, sp_path, pin_at("1", 2000, 120, ww = "all", turns = "ignore"), "bus", route_fn = straight_route))
check("bus trip: the rules don't apply (bus server, its default)",
      all(vapply(legs_seen, function(o) identical(o, list(profile = "bus", exclude = NULL)), TRUE)))
check("old pins (no grab, no rules) get the defaults", {
  pn <- pins_normalize(data.frame(id = "1", lon = 0, lat = 0))
  identical(pn$ww, "minor") && identical(pn$turns, "respect") && is.na(pn$grab_lon) && is.na(pn$after)
})
# Maintenance flag
mf <- tempfile()
writeLines('{"since": "2026-10-12T18:05:00Z", "reason": "nightly", "profiles": ["car", "official"], "pid": 1}', mf)
m_on <- osrm_maintenance(mf, pid_alive = function(pid) TRUE)
m_dead <- osrm_maintenance(mf, pid_alive = function(pid) FALSE)
check(sprintf("maintenance: PT \"%s\" / EN \"%s\"", osrm_maintenance_text(m_on, "pt"), osrm_maintenance_text(m_on, "en")),
      !is.null(m_on) && identical(osrm_maintenance_text(m_on, "pt"), "Roteamento em manutenção desde 15:05 (atualização noturna dos dados: car, official)") &&
        identical(osrm_maintenance_text(m_on, "en"), "Routing under maintenance since 15:05 (nightly data refresh: car, official)"))
writeLines("{not json", mf)
m_bad <- osrm_maintenance(mf)
check("maintenance: its process gone -> stale (no maintenance); no file -> none; half-written file -> maintenance since its mtime",
      is.null(m_dead) && is.null(osrm_maintenance(tempfile())) && !is.null(m_bad) && inherits(m_bad$since, "POSIXct") &&
        grepl("^Routing under maintenance since", osrm_maintenance_notice(m_bad, "en")))
unlink(mf)

# ---- Period stepping (Batch review) ----
st1 <- review_step_range("2026-05-07", "2026-05-07", 1); st2 <- review_step_range("2026-05-07", "2026-05-07", -1)
stw <- review_step_range("2026-05-04", "2026-05-10", 1); stm <- review_step_range("2026-02-26", "2026-03-01", -1)
check(sprintf("period step: 1 day +1 -> %s..%s, -1 -> %s..%s; a week +1 -> %s..%s; 4 days across a month end -1 -> %s..%s; invalid -> NULL",
              st1$from, st1$to, st2$from, st2$to, stw$from, stw$to, stm$from, stm$to),
      identical(c(st1$from, st1$to), as.Date(c("2026-05-08", "2026-05-08"))) && identical(c(st2$from, st2$to), as.Date(c("2026-05-06", "2026-05-06"))) &&
        identical(c(stw$from, stw$to), as.Date(c("2026-05-11", "2026-05-17"))) && identical(c(stm$from, stm$to), as.Date(c("2026-02-22", "2026-02-25"))) &&
        is.null(review_step_range("2026-05-08", "2026-05-07", 1)) && is.null(review_step_range("2026-05-07", "2026-05-07", 0)) &&
        is.null(review_step_range(NA, "2026-05-07", 1)))

# ---- Rail stations of every system: lines, labels, candidates, picks by key ----
sp <- ll(c(0, 40, 800, 3000, 0, 5000), c(0, 30, 0, 0, 10, 0))
st_ref <- data.frame(name = c("Maracanã", "Maracanã", "Estácio", "Alfa", "Maracanã", "Rodoviária"),
                     modes = c("subway", "train", "subway", "train", "subway", ""),
                     lon = sp[, 1], lat = sp[, 2], osm = c("n1", "n2", "n3", NA, "w9", "n6"),
                     operator = c("MetrôRio", "SuperVia", NA, NA, "MetrôRio", NA), network = NA_character_)
stops <- data.frame(route = c("subway", "subway", "train", "subway"), network = c("Metrô Rio", "Metrô Rio", "SuperVia", "Metrô Rio"),
                    operator = "", ref = c("2", "1", "Deodoro", "2"), lon = ll(c(10, 790, 50, 810), 0)[, 1], lat = ll(c(10, 790, 50, 810), 0)[, 2])
st_ref$lines <- rail_station_lines(st_ref, stops)
check(sprintf("station lines by stops within 300 m of a compatible route: %s", paste(st_ref$lines, collapse = " / ")),
      identical(st_ref$lines[1], "subway|Metrô Rio|2") && identical(st_ref$lines[2], "train|SuperVia|Deodoro") &&
        setequal(strsplit(st_ref$lines[3], ";")[[1]], c("subway|Metrô Rio|1", "subway|Metrô Rio|2")) && identical(st_ref$lines[4], ""))
ref_t <- list(stations = st_ref, mtime = "unit-test-1")
rs <- rail_stations(ref_t)
lab_pt <- rail_station_label(rs, "pt"); lab_en <- rail_station_label(rs, "en")
check(sprintf("rail stations: %d (bus station out, area copy of a station out); labels PT %s | EN for one without a system: %s",
              nrow(rs), paste(lab_pt, collapse = " | "), lab_en[rs$name == "Alfa"]),
      nrow(rs) == 4 && !"Rodoviária" %in% rs$name && !"w9" %in% rs$key &&
        identical(lab_pt[rs$key == "n1"], "Maracanã — Metrô L2") && identical(lab_pt[rs$key == "n2"], "Maracanã — SuperVia") &&
        identical(lab_pt[rs$key == "n3"], "Estácio — Metrô L1/L2") && identical(lab_pt[rs$name == "Alfa"], "Alfa — Trem") &&
        identical(lab_en[rs$name == "Alfa"], "Alfa — Train") && startsWith(rs$key[rs$name == "Alfa"], "s"))
c_metro <- station_candidates("metro", sp[1, 1], sp[1, 2], k = 5, ref = ref_t)
c_all <- station_candidates("metro", sp[1, 1], sp[1, 2], k = 5, ref = ref_t, all_systems = TRUE)
check(sprintf("candidates: metro trip %s; all systems %s", paste(c_metro$key, collapse = ","), paste(c_all$key, collapse = ",")),
      identical(c_metro$key, c("n1", "n3")) && identical(c_all$key, c("n1", "n2", "n3", rs$key[rs$name == "Alfa"])))
s_rt <- samples_at(c(20, 400, 900), c(0, 0, 0), c(0, 60, 120))
pl_auto <- rail_plan(s_rt, "metro", ref = ref_t)
pl_key <- rail_plan(s_rt, "metro", dep_name = "n2", arr_name = "", ref = ref_t)
check(sprintf("rail plan: automatic start %s; picked by key (SuperVia station on a metro trip) %s, end '' -> recorded point %s; start options %s",
              pl_auto$dep$key, pl_key$dep$key, is.null(pl_key$arr), paste(pl_key$dep_options$key, collapse = ",")),
      identical(pl_auto$dep$key, "n1") && identical(pl_key$dep$key, "n2") && is.null(pl_key$arr) && "n2" %in% pl_key$dep_options$key &&
        abs(pl_key$points$lng[1] - sp[2, 1]) < 1e-9)

# ---- Rail trips always fixable: route along the OSM rails, Jaé start, same-station guard ----
# Metrô L1 on two parallel tracks (y = 0 and y = 8, never joined, as OSM maps
# the two directions); L2 from near Central up x = 2000, ending on a SuperVia
# track (a shared node) that runs along y = 3000. Stations: A and B on L1 (one
# stop), Central (L1 + L2), D on L2, E on SuperVia, F a Metrô station with
# only the SuperVia track near it.
osm_line <- function(x, y) sf::st_linestring(ll(x, y))
osm_rails <- sf::st_sf(
  railway = c("subway", "subway", "subway", "rail"), name = NA_character_, osm = paste0("w", 1:4),
  lines = c("subway|Metrô Rio|1", "subway|Metrô Rio|1", "subway|Metrô Rio|2", "train|SuperVia|Deodoro"),
  geometry = sf::st_sfc(osm_line(c(0, 1000, 2000, 4000), c(0, 0, 0, 0)), osm_line(c(0, 4000), c(8, 8)),
                        osm_line(c(2000, 2000), c(60, 3000)), osm_line(c(0, 2000, 4000), c(3000, 3000, 3000)), crs = 4326))
osm_st_xy <- ll(c(500, 1500, 2000, 2000, 3500, 3500), c(-3, 11, 25, 2500, 3000, 3020))
osm_st <- data.frame(name = c("A", "B", "Central", "D", "E", "F"), modes = c("subway", "subway", "subway", "subway", "train", "subway"),
                     lon = osm_st_xy[, 1], lat = osm_st_xy[, 2], osm = paste0("n", 1:6), operator = NA_character_,
                     network = c(NA, NA, NA, NA, NA, "Metrô Rio"),
                     lines = c("subway|Metrô Rio|1", "subway|Metrô Rio|1", "subway|Metrô Rio|1;subway|Metrô Rio|2", "subway|Metrô Rio|2",
                               "train|SuperVia|Deodoro", ""))
make_ref <- function(rails, stations, tag) {
  bb <- t(vapply(sf::st_geometry(rails), function(g) as.numeric(sf::st_bbox(g)), numeric(4)))
  colnames(bb) <- c("xmin", "ymin", "xmax", "ymax")
  list(rails = rails, bbox = bb, stations = stations, mtime = tag)
}
oref <- make_ref(osm_rails, osm_st, "unit-osm-1")
ost <- rail_stations(oref)
stn <- function(n) ost[ost$name == n, ]
r_ab <- rail_osm_route(stn("A"), stn("B"), "metro", oref)
check(sprintf("OSM rails, one stop A -> B (parallel tracks never joined, the stations nearer different ones): ok %s, %.0f m, lines %s, mode %s, transfers %d",
              r_ab$ok, r_ab$length_m %||% NA, paste(rail_line_label(r_ab$lines %||% character(0)), collapse = ","), r_ab$mode %||% "-",
              length(r_ab$transfers)),
      isTRUE(r_ab$ok) && abs(r_ab$length_m - 1000) < 40 && identical(r_ab$lines, "subway|Metrô Rio|1") && r_ab$mode == "lines" &&
        length(r_ab$transfers) == 0)
r_ad <- rail_osm_route(stn("A"), stn("D"), "metro", oref)
check(sprintf("OSM rails, a transfer within one system A -> D: ok %s, %.0f m, lines %s, transfer at %s",
              r_ad$ok, r_ad$length_m %||% NA, paste(rail_line_label(r_ad$lines %||% character(0)), collapse = " -> "),
              paste(r_ad$transfers, collapse = ",")),
      isTRUE(r_ad$ok) && identical(r_ad$lines, c("subway|Metrô Rio|1", "subway|Metrô Rio|2")) && identical(r_ad$transfers, "Central") &&
        r_ad$length_m > 3900 && r_ad$length_m < 4200)
r_de <- rail_osm_route(stn("D"), stn("E"), "metro", oref)
r_af <- rail_osm_route(stn("A"), stn("F"), "metro", oref)
r_aa <- rail_osm_route(stn("A"), stn("A"), "metro", oref)
check(sprintf("OSM rails, no transfer across systems: Metrô D -> SuperVia E %s; Metrô A -> F (only the SuperVia track near it, joined to L2) %s at %s; same station %s",
              r_de$reason %||% "path", r_af$reason %||% "path", r_af$station %||% "-", r_aa$reason %||% "path"),
      identical(r_de$reason, "systems") && identical(r_af$reason, "no_rails") && identical(r_af$station, "F") &&
        identical(r_aa$reason, "same_station"))
oref_old <- make_ref(osm_rails[, c("railway", "name")], osm_st[, c("name", "modes", "lon", "lat")], "unit-osm-2")
ost_old <- rail_stations(oref_old)
r_old <- rail_osm_route(ost_old[ost_old$name == "A", ], ost_old[ost_old$name == "B", ], "metro", oref_old)
check(sprintf("OSM rails without line information (older reference): A -> B ok %s, mode %s, %.0f m", r_old$ok, r_old$mode %||% "-",
              r_old$length_m %||% NA),
      isTRUE(r_old$ok) && r_old$mode == "tracks" && abs(r_old$length_m - 1000) < 40)

# Jaé tap -> station; the trip's tap
tap_at <- function(x, y, t) {
  p <- if (is.na(x)) cbind(NA_real_, NA_real_) else ll(x, y)
  data.frame(profile = "metro", datetime_utc = as.POSIXct(t, origin = "1970-01-01", tz = "UTC"), longitude = p[, 1], latitude = p[, 2],
             label = "Metrô", linha_ou_estacao = "", stringsAsFactors = FALSE)
}
ts_near <- rail_tap_station(tap_at(530, 30, 900), "metro", ref = oref)
ts_far <- rail_tap_station(tap_at(900, 700, 900), "metro", ref = oref)
ts_none <- rail_tap_station(tap_at(NA, NA, 900), "metro", ref = oref)
taps <- rbind(tap_at(500, 0, 1000 - 1200), tap_at(500, 0, 1000 - 500), tap_at(500, 0, 1000 + 100), tap_at(500, 0, 1000 + 400))
pick_t <- function(x) if (nrow(x)) as.numeric(x$datetime_utc) - 1000 else NA
tp1 <- rail_trip_tap(taps, "metro", 1000, 2000)
tp2 <- rail_trip_tap(taps[1:2, ], "metro", 1000, 2000)
tp3 <- rail_trip_tap(taps[1:2, ], "metro", 1000, 2000, after = 1000 - 450)
check(sprintf("Jaé tap -> station: 41 m from A -> %s (%s, %.0f m); far -> %s; no coordinates -> %s; the trip's tap: %+.0f s (last by start + 3 min), %+.0f s (15 min lead), after the previous trip -> %s",
              ts_near$name %||% "-", ts_near$source %||% "-", ts_near$tap_dist_m %||% NA, if (is.null(ts_far)) "none" else ts_far$name,
              if (is.null(ts_none)) "none" else ts_none$name, pick_t(tp1), pick_t(tp2), if (nrow(tp3)) "a tap" else "none"),
      identical(ts_near$name, "A") && identical(ts_near$source, "jae") && ts_near$tap_t == 900 && is.null(ts_far) && is.null(ts_none) &&
        pick_t(tp1) == 100 && pick_t(tp2) == -500 && nrow(tp3) == 0)

# Same station at both ends: the farther automatic end looks again (behind / ahead only)
s_same <- samples_at(c(1300, 1350, 1400, 1450, 1500), c(0, 0, 0, 0, 5), c(0, 10, 20, 30, 40))
pl_same <- rail_plan(s_same, "metro", ref = oref)
pl_jae <- rail_plan(s_same, "metro", ref = oref, tap_station = ts_near)
check(sprintf("same-station guard: start %s (%s), end %s; with a Jaé tap at A: start %s (%s)",
              pl_same$dep$name %||% "-", pl_same$dep$source %||% "-", pl_same$arr$name %||% "-", pl_jae$dep$name %||% "-", pl_jae$dep$source %||% "-"),
      identical(pl_same$dep$name, "A") && identical(pl_same$arr$name, "B") && identical(pl_same$dep$source, "auto") &&
        identical(pl_jae$dep$name, "A") && identical(pl_jae$dep$source, "jae"))

# The proposal: Google has no transit leg -> OSM rails; the Jaé start departs at tap + 60 s even before the trip
no_google <- function(points, mode, ref_time_utc, on_fail) { on_fail("no_transit", ""); NULL }
# Google with one transit leg A -> B (its stops at the stations; Google's polyline itself isn't used)
straight_google <- function(points, mode, ref_time_utc, on_fail) {
  f <- seq(0, 1, length.out = 30)
  co <- cbind(lon = points$lng[1] + f * diff(points$lng), lat = points$lat[1] + f * diff(points$lat))
  list(coords = co, steps = list(list(kind = "transit", coords = co, duration_s = 100, line = "Linha 1", short = "1",
                                      from_stop = "A", to_stop = "B", from_xy = unname(ll(500, -3)[1, ]), to_xy = unname(ll(1500, 11)[1, ]),
                                      departure_t = 2000, arrival_t = 2100)))
}
s_trip <- samples_at(c(700, 900, 1100, 1300, 1480), c(0, 0, 0, 0, 5), c(1010, 1040, 1070, 1100, 1130))
trip_t <- c(1000, 1150)
fx_osm <- rail_proposal(s_trip, "metro", trip_t, tap = tap_at(530, 30, 900), google_fn = no_google, ref = oref)
mono <- function(fx) !is.null(fx$path) && all(diff(fx$path$t) > 0)
check(sprintf("fallback: Google %s -> source %s; start %s (%s) at %+.0f s (tap + 60), end %s at %+.0f s (last sample near it); %d points, times increasing %s",
              fx_osm$google_fail$code %||% "-", fx_osm$source %||% "none", fx_osm$plan$dep$name %||% "-", fx_osm$gate$action %||% "-",
              fx_osm$t_start - 1000, fx_osm$plan$arr$name %||% "-", fx_osm$t_end - 1000, nrow(fx_osm$path %||% data.frame()), mono(fx_osm)),
      identical(fx_osm$source, "osm_rails") && identical(fx_osm$google_fail$code, "no_transit") && fx_osm$t_start == 960 &&
        fx_osm$t_end == 1130 && identical(fx_osm$gate$action, "station") && mono(fx_osm) && fx_osm$path$t[1] == 960 &&
        abs(fx_osm$total_m - 1000) < 40)
fx_g <- rail_proposal(s_trip, "metro", trip_t, tap = tap_at(530, 30, 900), google_fn = straight_google, ref = oref)
gate_case <- function(tap) {
  fx <- rail_proposal(s_trip, "metro", trip_t, tap = tap, google_fn = no_google, ref = oref)
  sprintf("%s %+.0f", fx$gate$action %||% "none", fx$t_start - 1000)
}
g_adj <- gate_case(tap_at(900, 900, 1100))
g_late <- gate_case(tap_at(900, 900, 1300))
g_before <- gate_case(tap_at(900, 900, 800))
check(sprintf("Jaé timing: Google route start %+.0f s (%s); taps away from a station (3-min window): +100 s -> %s, +300 s -> %s, -200 s -> %s",
              fx_g$t_start - 1000, fx_g$gate$action %||% "-", g_adj, g_late, g_before),
      identical(fx_g$source, "google") && fx_g$t_start == 960 && identical(fx_g$gate$action, "station") &&
        g_adj == "adjust +160" && g_late == "ignore +0" && g_before == "before +0")
fx_rev <- rail_osm_fix(fx_osm$plan, fx_osm$route, s_trip, trip_t, end_offset_s = -600)
fx_one <- rail_proposal(s_trip[3, ], "metro", trip_t, o = list(dep = stn("A")$key, arr = stn("B")$key), google_fn = no_google, ref = oref)
check(sprintf("fallback timing monotonic: end moved before the start -> %.0f s for %.0f m, increasing %s; one-sample trip with both stations chosen -> %s %+.0f..%+.0f s",
              fx_rev$t_end - fx_rev$t_start, fx_rev$total_m, mono(fx_rev), fx_one$source %||% "none", fx_one$t_start - 1000, fx_one$t_end - 1000),
      mono(fx_rev) && fx_rev$t_end > fx_rev$t_start && mono(fx_one) && identical(fx_one$source, "osm_rails") &&
        fx_one$t_start == 1000 && fx_one$t_end == 1150)
fx_none <- rail_proposal(s_trip, "metro", trip_t, o = list(arr = ""), google_fn = no_google, ref = oref)
fx_same <- rail_proposal(s_trip, "metro", trip_t, o = list(dep = stn("B")$key, arr = stn("B")$key), google_fn = no_google, ref = oref)
check(sprintf("no route: end at the recorded point -> %s (rows: start %s); same station -> %s; text '%s'",
              fx_none$no_path %||% "-", fx_none$plan$dep$name %||% "-", fx_same$no_path %||% "-",
              paste(rail_no_path_text(fx_none$no_path, lang = "pt"), google_fail_text(fx_none$google_fail, "metrô", "pt"))),
      is.null(fx_none$path) && identical(fx_none$no_path, "need_stations") && !is.null(fx_none$plan$dep) &&
        identical(fx_same$no_path, "same_station"))

# Google's failure reasons
key_before <- Sys.getenv("GOOGLE_MAPS_API_KEY")
Sys.setenv(GOOGLE_MAPS_API_KEY = "unit-test")
walk_route <- list(legs = list(list(steps = list(list(travel_mode = "WALKING", polyline = list(points = ""), duration = list(value = 600),
                                                      distance = list(value = 800))))))
bus_route <- list(legs = list(list(steps = list(list(travel_mode = "TRANSIT", polyline = list(points = ""), duration = list(value = 600),
                                                     transit_details = list(line = list(vehicle = list(type = "BUS"))))))))
reason_for <- function(js) {
  google_directions_get <<- function(params) js
  why <- "none"
  suppressMessages(calculate_google_transit_route(pts, "subway", on_fail = function(code, detail = "") why <<- paste(code, detail)))
  trimws(why)
}
reasons <- c(walk = reason_for(list(status = "OK", routes = list(walk_route))), bus = reason_for(list(status = "OK", routes = list(bus_route))),
             zero = reason_for(list(status = "ZERO_RESULTS", routes = list())), err = reason_for(list(status = "OVER_QUERY_LIMIT", routes = list())),
             req = reason_for(NULL))
google_directions_get <- mock_get
Sys.setenv(GOOGLE_MAPS_API_KEY = key_before)
check(sprintf("Google failure reasons: %s", paste(names(reasons), reasons, sep = " = ", collapse = "; ")),
      identical(unname(reasons), c("no_transit", "no_transit BUS", "zero_results", "status OVER_QUERY_LIMIT", "request")))

# ---- GPX coverage of the original item (every export path) ----
C0 <- 1760000000
# Tracks of a GPX: start, end (epoch s), type, in time order
gpx_ranges <- function(gpx) {
  doc <- xml2::read_xml(paste(gpx, collapse = "\n"))
  ns <- xml2::xml_ns(doc)
  trks <- xml2::xml_find_all(doc, ".//d1:trk", ns)
  out <- do.call(rbind, lapply(trks, function(tk) {
    tt <- as.numeric(as.POSIXct(xml2::xml_text(xml2::xml_find_all(tk, ".//d1:time", ns)), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    ty <- xml2::xml_text(xml2::xml_find_first(tk, "./d1:type", ns))
    data.frame(start = min(tt), end = max(tt), type = ty, n = length(tt))
  }))
  out[order(out$start), ]
}
covers <- function(r, span) {
  isTRUE(r$start[1] == round(span[1]) && r$end[nrow(r)] == round(span[2]) && all(r$start[-1] == r$end[-nrow(r)]))
}
gpx_of <- function(acc) { inp <- review_gpx_input(acc); build_gpx(inp$items, inp$samples, "2026-10-12T12:00:00Z") }
types <- function(r) paste(r$type, collapse = "/")
# Main: (0,0) at +120 s to (2000,0) at +480 s; recorded: waiting near the start until +110, walking away after +480
main_path <- function(t0 = 120, t1 = 480) {
  x <- seq(0, 2000, by = 200)
  p <- ll(x, 0 * x)
  data.frame(lon = p[, 1], lat = p[, 2], t = C0 + seq(t0, t1, length.out = length(x)))
}
s_cov <- rbind(samples_at(c(0, 30, 60, 20, 10), c(0, 20, 40, 30, 10), C0 + c(5, 30, 60, 90, 110)),
               samples_at(c(2050, 2200, 2400, 2600), c(0, 0, 0, 0), C0 + c(500, 530, 560, 590)))
span1 <- C0 + c(0, 600)
a1 <- review_cover_entry(list(activity = "bus", path = main_path(), method = "match"), span1, s_cov)
r1 <- gpx_ranges(gpx_of(list(t1 = a1)))
check(sprintf("coverage, single trip: %s; tracks %s over %+.0f..%+.0f s, contiguous %s",
              paste(vapply(a1$cover_gaps, function(g) sprintf("%s %.0f s -> %s", g$side, g$gap_s, g$mode), ""), collapse = ", "),
              types(r1), r1$start[1] - C0, r1$end[nrow(r1)] - C0, covers(r1, span1)),
      covers(r1, span1) && identical(r1$type, c("stationary", "bus", "walking")) &&
        identical(vapply(a1$cover_gaps, `[[`, "", "mode"), c(before = "wait", after = "walk")))
# Batch: two trips, one accepted before the rule (span only: covered at export by extending)
span2 <- C0 + c(3000, 3400)
old <- list(activity = "car", path = transform(main_path(3100, 3350), t = t), method = "match", span = span2)
r2 <- gpx_ranges(gpx_of(list(t1 = a1, t2 = old)))
in_span <- function(r, sp) r[r$start >= round(sp[1]) & r$end <= round(sp[2]), ]
check(sprintf("coverage, batch: %d tracks; trip 1 %s, trip 2 (older state, extended at export) %s %s",
              nrow(r2), covers(in_span(r2, span1), span1), covers(in_span(r2, span2), span2), types(in_span(r2, span2))),
      nrow(r2) == 4 && covers(in_span(r2, span1), span1) && covers(in_span(r2, span2), span2) && identical(in_span(r2, span2)$type, "car"))
# Multimodal split (walk -> wait -> bus, "leave as is" after the alighting): the rest filled
seg_walk <- list(role = "walk_to_stop", type = "walking", name = "Walk to the stop", t_start = C0 + 10, t_end = C0 + 60,
                 path = multimodal_path(samples_at(c(0, 30), c(0, 20), C0 + c(10, 60))[, c("lon", "lat", "t")], C0 + 10, C0 + 60))
seg_wait <- multimodal_stationary("wait", "Wait at the stop", unlist(ll(30, 20)), C0 + 60, C0 + 120, 30)
seg_bus <- list(role = "bus", type = "bus", name = "Bus", t_start = C0 + 120, t_end = C0 + 480, path = main_path())
a3 <- review_cover_entry(multimodal_accepted(list(seg_walk, seg_wait, seg_bus), "bus"), span1, s_cov)
r3 <- gpx_ranges(gpx_of(list(t3 = a3)))
check(sprintf("coverage, multimodal split: %s; tracks %s, contiguous %s",
              paste(vapply(a3$cover_gaps, function(g) sprintf("%s %.0f s -> %s", g$side, g$gap_s, g$mode), ""), collapse = ", "),
              types(r3), covers(r3, span1)),
      covers(r3, span1) && identical(r3$type, c("stationary", "walking", "stationary", "bus", "walking")))
# Fragment: a 40 s trip, the proposal 2 s short at the start and 3 s at the end: stretched, no extra track
span4 <- C0 + c(5000, 5040)
a4 <- review_cover_entry(list(activity = "car", path = main_path(5002, 5037), method = "match"), span4, s_cov)
r4 <- gpx_ranges(gpx_of(list(t4 = a4)))
check(sprintf("coverage, fragment (gaps 2 s / 3 s): %s; %d track %+.0f..%+.0f s",
              paste(vapply(a4$cover_gaps, `[[`, "", "mode"), collapse = "/"), nrow(r4), r4$start[1] - span4[1], r4$end[1] - span4[1]),
      nrow(r4) == 1 && covers(r4, span4) && identical(unname(vapply(a4$cover_gaps, `[[`, "", "mode")), c("stretch", "stretch")))

# Stationary vs walking; explicit choices; tiny gaps; reaching outside the span
mode_with <- function(s, fill = list(), path = main_path(), span = span1) {
  vapply(review_cover(review_main_segments(list(activity = "bus", path = path)), span, s, fill)$gaps, `[[`, "", "mode")
}
s_near <- samples_at(c(0, 100, 250), c(0, 50, 0), C0 + c(20, 60, 100))
s_away <- samples_at(c(0, 200, 400), c(0, 0, 0), C0 + c(20, 60, 100))
m_near <- mode_with(s_near)[["before"]]
m_away <- mode_with(s_away)[["before"]]
m_none <- mode_with(s_near[0, ])[["before"]]
m_walk <- mode_with(s_near, list(before = "walk"))[["before"]]
ext <- review_cover(review_main_segments(list(activity = "bus", path = main_path())), span1, s_near, list(before = "extend", after = "extend"))
check(sprintf("gap fill: samples within 250 m -> %s, reaching 400 m -> %s, none -> %s, chosen walk -> %s; extend -> %d track %+.0f..%+.0f s, increasing %s",
              m_near, m_away, m_none, m_walk, length(ext$segments), ext$segments[[1]]$path$t[1] - C0,
              utils::tail(ext$segments[[1]]$path$t, 1) - C0, all(diff(ext$segments[[1]]$path$t) > 0)),
      m_near == "wait" && m_away == "walk" && m_none == "wait" && m_walk == "walk" && length(ext$segments) == 1 &&
        ext$segments[[1]]$path$t[1] == span1[1] && utils::tail(ext$segments[[1]]$path$t, 1) == span1[2] &&
        all(diff(ext$segments[[1]]$path$t) > 0))
tiny <- mode_with(s_near, path = main_path(4, 595))
small <- mode_with(s_near, path = main_path(6, 594))
over <- review_cover(review_main_segments(list(activity = "metro", path = main_path(-150, 480))), span1, s_cov)
kept <- review_cover(review_main_segments(list(activity = "metro", path = main_path(-150, 480))), span1, s_cov, keep_start = TRUE)
check(sprintf("tiny gaps: 4 s / 5 s -> %s; 6 s / 6 s -> %s; a start 150 s before the trip -> fitted from %+.0f s (raw %+.0f); from a Jaé tap -> kept, span from %+.0f s",
              paste(tiny, collapse = "/"), paste(small, collapse = "/"), over$segments[[1]]$path$t[1] - C0, over$raw[1] - C0, kept$span[1] - C0),
      identical(unname(tiny), c("stretch", "wait")) && identical(unname(small), c("wait", "wait")) &&
        over$segments[[1]]$path$t[1] == span1[1] && over$raw[1] == C0 - 150 && kept$span[1] == C0 - 150 &&
        kept$segments[[1]]$path$t[1] == C0 - 150)

# Edit Samples (single trip): the snapped stretch inside the original item, gaps filled from the original samples
arc_sample <- function(x, y, t) {
  p <- ll(x, y)
  list(date = format(as.POSIXct(t, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
       location = list(latitude = p[1, 2], longitude = p[1, 1]), confirmedActivityType = 21L)
}
orig <- c(lapply(seq(0, 110, by = 10), function(k) arc_sample(k %% 40, 0, C0 + k)),
          lapply(seq(120, 480, by = 30), function(k) arc_sample((k - 120) * 5.5, 0, C0 + k)),
          lapply(seq(490, 600, by = 10), function(k) arc_sample(2000 + (k - 480) * 5, 0, C0 + k)))
snapped <- lapply(seq(150, 450, by = 30), function(k) arc_sample((k - 120) * 5.5, 3, C0 + k))
pkg <- build_edit_package(snapped, orig, NULL, "additive", "2026-10-12T12:00:00Z")
r5 <- gpx_ranges(build_edit_gpx(pkg, "2026-10-12T12:00:00Z", orig))
r5_old <- gpx_ranges(build_edit_gpx(pkg, "2026-10-12T12:00:00Z"))
check(sprintf("coverage, Edit Samples GPX: %s over %+.0f..%+.0f s, contiguous %s (without the originals: %s %+.0f..%+.0f s)",
              types(r5), r5$start[1] - C0, r5$end[nrow(r5)] - C0, covers(r5, C0 + c(0, 600)), types(r5_old),
              r5_old$start[1] - C0, r5_old$end[1] - C0),
      covers(r5, C0 + c(0, 600)) && identical(r5$type, c("stationary", "bus", "walking")) && nrow(r5_old) == 1)

# ---- Rail legs along OSM route relations by direction, transfers, Google as a hint ----
# A small reference: a VLT one-way pair (A -> B on the north street y = 60, B -> A on the south one y = -60);
# a metro line with stations X, Y, Z (one relation each way); metro lines 1 (P0, P1, Q along y = 2000) and 2 (Q2,
# 180 m from Q, to R2 north); a SuperVia station 100 m from Q
rel_route <- function(rel, line, name, x, y, stops) {
  xy <- ll(x, y); colnames(xy) <- c("lon", "lat")
  cum <- path_cumulative_m(xy)
  sp <- lapply(seq_len(nrow(stops)), function(k) {
    p <- ll(stops$x[k], stops$y[k])
    pr <- polyline_segment_projection(xy, cum, p[1, ])
    j <- which.min(pr$off)
    data.frame(rel = rel, seq = k, ref = paste0("n", k), name = stops$name[k], along_m = pr$d[j], off_m = pr$off[j], lon = p[1, 1], lat = p[1, 2])
  })
  list(meta = data.frame(rel = rel, route = sub("\\|.*$", "", line), network = strsplit(line, "|", fixed = TRUE)[[1]][2], operator = "",
                         ref = sub("^.*\\|", "", line), name = name, from = "", to = "", line = line, ptv2 = TRUE, gaps = 0L,
                         directed = TRUE, n_stops = nrow(stops)),
       xy = xy, stops = do.call(rbind, sp))
}
stp <- function(name, x, y) data.frame(name = name, x = x, y = y)
vlt <- "tram|VLT Carioca|1"; m1 <- "subway|Metrô Rio|1"; m2 <- "subway|Metrô Rio|2"; mx <- "subway|Metrô Rio|9"
R <- list(
  rel_route("r1", vlt, "VLT Carioca 1: A → B", c(0, 200, 200, 800, 800, 1000), c(0, 0, 60, 60, 0, 0), rbind(stp("A", 0, 0), stp("B", 1000, 0))),
  rel_route("r2", vlt, "VLT Carioca 1: B → A", c(1000, 800, 800, 200, 200, 0), c(0, 0, -60, -60, 0, 0), rbind(stp("B", 1000, 0), stp("A", 0, 0))),
  rel_route("r3", mx, "Linha 9: X → Z", c(0, 2000), c(1000, 1000), rbind(stp("X", 0, 1000), stp("Y", 1000, 1000), stp("Z", 2000, 1000))),
  rel_route("r4", mx, "Linha 9: Z → X", c(2000, 0), c(1005, 1005), rbind(stp("Z", 2000, 1005), stp("Y", 1000, 1005), stp("X", 0, 1005))),
  rel_route("r5", m1, "Linha 1: P0 → Q", c(-500, 0, 1000), c(2000, 2000, 2000), rbind(stp("P0", -500, 2000), stp("P1", 0, 2000), stp("Q", 1000, 2000))),
  rel_route("r6", m2, "Linha 2: Q2 → R2", c(1150, 1150), c(2100, 3000), rbind(stp("Q2", 1150, 2100), stp("R2", 1150, 3000)))
)
st_xy <- ll(c(0, 1000, 0, 1000, 2000, -500, 0, 1000, 1150, 1150, 1000), c(0, 0, 1000, 1000, 1000, 2000, 2000, 2000, 2100, 3000, 1900))
r_st <- data.frame(name = c("A", "B", "X", "Y", "Z", "P0", "P1", "Q", "Q2", "R2", "Qsv"),
                   modes = c("tram", "tram", rep("subway", 8), "train"), lon = st_xy[, 1], lat = st_xy[, 2], osm = paste0("n", 101:111),
                   operator = NA_character_, network = NA_character_,
                   lines = c(vlt, vlt, mx, mx, mx, m1, m1, m1, m2, m2, "train|SuperVia|Deodoro"))
r_rails <- sf::st_sf(
  railway = c("tram", "tram", "tram", "tram", rep("subway", 3), "rail"), name = NA_character_, osm = paste0("w", 1:8),
  lines = c(vlt, vlt, vlt, vlt, mx, m1, m2, "train|SuperVia|Deodoro"),
  oneway = c(NA, "yes", "yes", NA, NA, NA, NA, NA), pref_dir = NA_character_,
  geometry = sf::st_sfc(osm_line(c(0, 200), c(0, 0)), osm_line(c(200, 200, 800, 800), c(0, 60, 60, 0)),
                        osm_line(c(800, 800, 200, 200), c(0, -60, -60, 0)), osm_line(c(800, 1000), c(0, 0)),
                        osm_line(c(0, 2000), c(1000, 1000)), osm_line(c(-500, 1000), c(2000, 2000)), osm_line(c(1150, 1150), c(2100, 3000)),
                        osm_line(c(900, 1100), c(1900, 1900)), crs = 4326))
rref <- make_ref(r_rails, r_st, "unit-rel-1")
rref$routes <- do.call(rbind, lapply(R, `[[`, "meta"))
rref$route_xy <- stats::setNames(lapply(R, `[[`, "xy"), rref$routes$rel)
rref$route_stops <- do.call(rbind, lapply(R, `[[`, "stops"))
rst <- rail_stations(rref)
S <- function(n) rst[rst$name == n, ]
ymax <- function(co) max(local_xy(co)[, "y"]); ymin <- function(co) min(local_xy(co)[, "y"])
l_ab <- rail_relation_leg(S("A"), S("B"), rref); l_ba <- rail_relation_leg(S("B"), S("A"), rref)
rref_nr <- rref; rref_nr$routes <- rref_nr$routes[0, ]
g_ab <- rail_osm_route(S("A"), S("B"), "tram", rref_nr); g_ba <- rail_osm_route(S("B"), S("A"), "tram", rref_nr)
check(sprintf("one-way pair: A -> B along %s (north street %s), B -> A along %s (south street %s); without relations, the rails graph by oneway: A -> B north %s, B -> A south %s",
              l_ab$rel, ymax(l_ab$coords) > 55, l_ba$rel, ymin(l_ba$coords) < -55, ymax(g_ab$coords) > 55, ymin(g_ba$coords) < -55),
      identical(l_ab$rel, "r1") && identical(l_ba$rel, "r2") && ymax(l_ab$coords) > 55 && ymin(l_ab$coords) > -1 &&
        ymin(l_ba$coords) < -55 && ymax(l_ba$coords) < 1 && isTRUE(g_ab$ok) && ymax(g_ab$coords) > 55 && ymin(g_ab$coords) > -1 &&
        isTRUE(g_ba$ok) && ymin(g_ba$coords) < -55 && ymax(g_ba$coords) < 1)
l_xy <- rail_relation_leg(S("X"), S("Y"), rref); l_yx <- rail_relation_leg(S("Y"), S("X"), rref); l_zy <- rail_relation_leg(S("Z"), S("Y"), rref)
check(sprintf("stop order picks the relation: X -> Y %s (%.0f m), Y -> X %s, Z -> Y %s; the cut ends at the stops",
              l_xy$rel, l_xy$length_m, l_yx$rel, l_zy$rel),
      identical(l_xy$rel, "r3") && identical(l_yx$rel, "r4") && identical(l_zy$rel, "r4") && abs(l_xy$length_m - 1000) < 5 &&
        abs(local_xy(l_xy$coords)[1, "x"]) < 2 && abs(local_xy(l_xy$coords)[nrow(l_xy$coords), "x"] - 1000) < 2)
# A trip P1 -> R2: metro 1 to Q, walk to Q2, metro 2 to R2 (samples along it, 10 m/s; 600 s)
tr_s <- samples_at(c(0, 500, 1000, 1150, 1150, 1150), c(2000, 2000, 2000, 2100, 2500, 3000), C0 + c(0, 50, 100, 300, 400, 600))
tr_t <- C0 + c(0, 600)
key <- function(n) S(n)$key
no_google <- function(points, mode, ref_time_utc, on_fail) { on_fail("no_transit", ""); NULL }
fw <- rail_proposal(tr_s, "metro", tr_t, o = list(dep = key("P1"), xfers = c(key("Q"), key("Q2")), arr = key("R2")), google_fn = no_google, ref = rref)
kinds <- vapply(fw$rail_legs %||% list(), function(l) paste(l$kind, l$source), "")
check(sprintf("walking transfer: P1 > Q > Q2 > R2 set -> %s; legs %s; walk %.0f m between stations %.0f m apart, %.0f s (3 min per 250 m), timed %+.0f..%+.0f s",
              fw$source %||% fw$no_path, paste(kinds, collapse = " / "), fw$rail_legs[[2]]$length_m %||% NA, fw$rail_legs[[2]]$dist_m %||% NA,
              fw$rail_legs[[2]]$walk_s %||% NA, (fw$walk_times$t0 %||% NA) - C0, (fw$walk_times$t1 %||% NA) - C0),
      identical(fw$source, "osm_rails") && identical(kinds, c("ride relation", "walk walk", "ride relation")) &&
        abs(fw$rail_legs[[2]]$walk_s - 180 * fw$rail_legs[[2]]$dist_m / 250) < 1 && nrow(fw$walk_times) == 1 &&
        abs(fw$walk_times$t1 - fw$walk_times$t0 - fw$rail_legs[[2]]$walk_s) < 1 && all(diff(fw$path$t) > 0))
fsys <- rail_proposal(tr_s, "metro", tr_t, o = list(dep = key("P1"), xfers = c(key("Q"), key("Qsv")), arr = key("R2")), google_fn = no_google, ref = rref)
check(sprintf("no transfer across systems: Q (Metrô) > Qsv (SuperVia, 100 m) -> %s", fsys$no_path %||% "a route"),
      is.null(fsys$path) && identical(fsys$no_path, "systems"))
# Google as a hint: its route P1 -> Q (walk) Q2 -> R2 fills the transfers and times the plan; a start set elsewhere wins
g_steps <- list(
  list(kind = "transit", coords = ll(c(0, 1000), c(2050, 2050)), duration_s = 120, line = "Linha 1", short = "1", from_stop = "P1", to_stop = "Q",
       from_xy = unname(ll(0, 2000)[1, ]), to_xy = unname(ll(1000, 2000)[1, ]), departure_t = 1000, arrival_t = 1120),
  list(kind = "walk", coords = NULL, duration_s = 150),
  list(kind = "transit", coords = ll(c(1150, 1150), c(2100, 3000)), duration_s = 90, line = "Linha 2", short = "2", from_stop = "Q2", to_stop = "R2",
       from_xy = unname(ll(1150, 2100)[1, ]), to_xy = unname(ll(1150, 3000)[1, ]), departure_t = 1300, arrival_t = 1390))
fake_google <- function(points, mode, ref_time_utc, on_fail) list(coords = ll(c(0, 1150), c(2000, 3000)), steps = g_steps)
fg <- rail_proposal(tr_s, "metro", tr_t, o = list(arr = key("R2")), google_fn = fake_google, ref = rref)
fd <- rail_proposal(tr_s, "metro", tr_t, o = list(dep = key("P0"), arr = key("R2")), google_fn = fake_google, ref = rref)
check(sprintf("Google as a hint: nothing against it -> %s, transfers %s (%s), legs %s; Início set to P0 -> %s",
              fg$source %||% fg$no_path, paste(vapply(fg$plan$xfers, `[[`, "", "name"), collapse = ">"), paste(unique(vapply(fg$plan$xfers, `[[`, "", "source")), collapse = ","),
              paste(vapply(fg$rail_legs, function(l) paste(l$kind, l$source), ""), collapse = " / "), fd$source %||% fd$no_path),
      identical(fg$source, "google") && identical(vapply(fg$plan$xfers, `[[`, "", "name"), c("Q", "Q2")) &&
        all(vapply(fg$plan$xfers, `[[`, "", "source") == "google") && identical(fg$timing, "steps") && nrow(fg$walk_times) == 1 &&
        identical(fd$source, "osm_rails") && identical(fd$plan$dep$name, "P0"))

# One transfer station set, the next line boarding at another station 180 m away (or the line in alighting at one):
# that station is added, the change becomes a walk
f_one <- rail_proposal(tr_s, "metro", tr_t, o = list(dep = key("P1"), xfers = key("Q"), arr = key("R2")), google_fn = no_google, ref = rref)
f_mir <- rail_proposal(tr_s, "metro", tr_t, o = list(dep = key("P1"), xfers = key("Q2"), arr = key("R2")), google_fn = no_google, ref = rref)
xf <- function(fx) paste(vapply(fx$plan$xfers, function(x) sprintf("%s (%s)", x$name, x$source), ""), collapse = " > ")
lk <- function(fx) paste(vapply(fx$rail_legs %||% list(), `[[`, "", "kind"), collapse = "/")
check(sprintf("one transfer row: Q set -> transfers %s, legs %s; Q2 set -> %s, legs %s",
              xf(f_one), lk(f_one), xf(f_mir), lk(f_mir)),
      identical(lk(f_one), "ride/walk/ride") && identical(xf(f_one), "Q (chosen) > Q2 (auto)") &&
        identical(lk(f_mir), "ride/walk/ride") && identical(xf(f_mir), "Q (auto) > Q2 (chosen)") &&
        abs(f_one$rail_legs[[2]]$walk_s - fw$rail_legs[[2]]$walk_s) < 1e-6)

# ---- Fixed anchor points for car / taxi trip ends (synthetic anchors only) ----
pt_anchor <- function(name, x, y, radius = 300, from = "2000-01-01", to = NA) {
  p <- ll(x, y)
  list(name = name, kind = "point", lon = p[1, 1], lat = p[1, 2], radius_m = radius, valid_from = from, valid_to = to)
}
road_anchor <- list(name = "R", kind = "road", coords = ll(c(-500, 500), c(500, 500)), radius_m = 300, valid_from = "2000-01-01", valid_to = NA)
s_a <- samples_at(c(50, 400, 1000), c(0, 300, 1000), c(0, 60, 120))
an2 <- list(pt_anchor("A1", 0, 0), pt_anchor("A2", 200, 0))
e1 <- trip_anchor_ends(s_a, "2026-06-01", an2)
e2 <- trip_anchor_ends(s_a, "2026-06-01", list(pt_anchor("A1", 0, 0, to = "2026-01-01"), pt_anchor("A2", 200, 0)))
e3 <- trip_anchor_ends(s_a, "2026-06-01", list(pt_anchor("A1", 0, 0, radius = 30)))
e4 <- trip_anchor_ends(samples_at(c(100, 2000), c(450, 2000), c(0, 60)), "2026-06-01", list(road_anchor))
check(sprintf("anchors: start 50 m from A1, 150 m from A2 -> %s; A1 expired -> %s; radius 30 m -> %s; end far away -> %s; a road anchor -> %s %.0f m (on the road at the start's nearest point %s)",
              e1$start$name %||% "-", e2$start$name %||% "-", if (is.null(e3$start)) "none" else e3$start$name, if (is.null(e1$end)) "none" else e1$end$name,
              e4$start$name %||% "-", e4$start$dist_m %||% NA, isTRUE(geosphere::distHaversine(c(e4$start$lon, e4$start$lat), ll(100, 500)[1, ]) < 0.5)),
      identical(e1$start$name, "A1") && abs(e1$start$dist_m - 50) < 1 && identical(e2$start$name, "A2") && is.null(e3$start) &&
        is.null(e1$end) && identical(e4$start$name, "R") && abs(e4$start$dist_m - 50) < 1 &&
        geosphere::distHaversine(c(e4$start$lon, e4$start$lat), ll(100, 500)[1, ]) < 0.5)
# Resolving: project_onto, a building (entrance, or the nearest routable road), a road
rd <- function(name, highway, x, y) list(name = name, highway = highway, coords = ll(x, y))
roads <- list(rd("Rua X", "residential", c(-200, 200), c(0, 0)), rd("", "service", c(-200, 200), c(20, 20)),
              rd("", "footway", c(0, 300), c(150, 150)), rd("Rua Y", "residential", c(0, 300), c(60, 60)))
node <- list(type = "node", lon = ll(0, 30)[1, 1], lat = ll(0, 30)[1, 2])
r_po <- anchor_point_from_osm(list(name = "H", project_onto = "Rua X"), node, roads)
r_node <- anchor_point_from_osm(list(name = "N"), node, roads)
sq <- ll(c(100, 140, 140, 100, 100), c(100, 100, 140, 140, 100))
bld <- function(ent) list(type = "way", tags = list(building = "yes"), coords = sq,
                          nodes = data.frame(lon = sq[, 1], lat = sq[, 2], entrance = ent))
r_ent <- anchor_point_from_osm(list(name = "B"), bld(c("yes", NA, "main", NA, "yes")), roads)
r_bld <- anchor_point_from_osm(list(name = "B"), bld(rep(NA, 5)), roads)
r_way <- anchor_point_from_osm(list(name = "W"), list(type = "way", tags = list(highway = "service"), coords = ll(c(0, 100), c(0, 0)), nodes = data.frame()), roads)
loc <- function(r) round(local_xy(cbind(r$lon, r$lat)))
check(sprintf("resolving: project_onto Rua X -> %s at (%s) (not the nearer internal road); a node -> %s; a building with entrances -> %s at (%s) (the main one); without -> %s at (%s) (Rua Y, not the nearer footway); a road way -> %s",
              r_po$how, paste(loc(r_po), collapse = ","), r_node$how, r_ent$how, paste(loc(r_ent), collapse = ","), r_bld$how,
              paste(loc(r_bld), collapse = ","), r_way$kind),
      identical(r_po$how, "project_onto") && all(abs(loc(r_po) - c(0, 0)) <= 1) && identical(r_node$how, "node") &&
        identical(r_ent$how, "entrance") && all(abs(loc(r_ent) - c(140, 140)) <= 1) && identical(r_bld$how, "nearest_road") &&
        all(abs(loc(r_bld) - c(120, 60)) <= 1) && identical(r_way$kind, "road"))
# Override and restore; the anchored end through the splice; the feature off
anch <- list(start = list(name = "A1", lon = ll(0, -200)[1, 1], lat = ll(0, -200)[1, 2]), end = NULL)
te_a <- review_trip_ends(NULL, anch)
te_d <- review_trip_ends(list(start = unname(ll(30, -150)[1, ])), anch)
fa1 <- splice_route_fix(n_s, n_path, NULL, "car", ends = te_a$ends, route_fn = net)
check(sprintf("override / restore: anchored -> %s %s; dragged -> %s (restore button for %s); restored (drag removed) -> %s; the anchored start routes from it (%.0f m), %+.0f s",
              te_a$info$start$source, te_a$info$start$anchor, te_d$info$start$source, te_d$info$start$anchor,
              review_trip_ends(list(), anch)$info$start$source, geosphere::distHaversine(fa1$coords[1, ], c(anch$start$lon, anch$start$lat)), fa1$path$t[1]),
      identical(te_a$info$start$source, "anchor") && identical(te_d$info$start$source, "drag") && identical(te_d$info$start$anchor, "A1") &&
        identical(review_trip_ends(list(), anch)$info$start$source, "anchor") && is.null(te_a$ends$end) &&
        geosphere::distHaversine(fa1$coords[1, ], c(anch$start$lon, anch$start$lat)) < 0.01 && fa1$path$t[1] == -25)
saved <- get0("ROUTE_ANCHORS", envir = globalenv(), inherits = FALSE)
if (!is.null(saved)) rm("ROUTE_ANCHORS", envir = globalenv())
off_cfg <- route_anchors_config(); off_an <- route_anchors(); off_e <- trip_anchor_ends(s_a, "2026-06-01", off_an)
if (!is.null(saved)) assign("ROUTE_ANCHORS", saved, envir = globalenv())
tmp_rds <- tempfile(fileext = ".rds")
saveRDS(list(key = route_anchors_key(list(list(name = "Z", osm = "node/1"))), anchors = an2), tmp_rds)
other <- route_anchors(tmp_rds, cfg = list(list(name = "Y", osm = "node/2")))
same <- route_anchors(tmp_rds, cfg = list(list(name = "Z", osm = "node/1")))
check(sprintf("feature off without ROUTE_ANCHORS: config %s, anchors %s, ends %s; a file made for other anchors -> %s, for these -> %d",
              if (is.null(off_cfg)) "none" else "set", if (is.null(off_an)) "none" else "some", if (is.null(off_e$start) && is.null(off_e$end)) "none" else "some",
              if (is.null(other)) "unused" else "used", length(same)),
      is.null(off_cfg) && is.null(off_an) && is.null(off_e$start) && is.null(off_e$end) && is.null(other) && length(same) == 2)

# An anchor projected onto a street keeps its node: a wait filled at that end goes to the node (merging into
# the visit there), while the route still ends at the street point
po <- anchor_point_from_osm(list(name = "H", project_onto = "Rua X"), node, roads)
h_rec <- c(list(name = "H", radius_m = 300, valid_from = "2000-01-01", valid_to = NA), po)
s_h <- samples_at(c(-1000, -500, -10, 2, 0), c(0, 0, 5, 28, 31), c(0, 250, 500, 550, 600))
e_h <- trip_anchor_ends(s_h, "2026-06-01", list(h_rec))
te_h <- review_trip_ends(NULL, e_h)
route_h <- ll(c(-1000, 0), c(0, 0))
main_h <- review_main_segments(list(activity = "car", path = data.frame(lon = route_h[, 1], lat = route_h[, 2], t = c(0, 500))))
cv_node <- review_cover(main_h, c(0, 600), s_h, wait_at = list(after = te_h$info$end$wait), places = list(after = "H"))
cv_street <- review_cover(main_h, c(0, 600), s_h)
wait_of <- function(cv) Filter(function(g) identical(g$role, "cover") && identical(g$type, "stationary"), cv$segments)[[1]]$path
off_node <- max(geosphere::distHaversine(as.matrix(wait_of(cv_node)[, c("lon", "lat")]), ll(0, 30)[1, ]))
off_street <- max(geosphere::distHaversine(as.matrix(wait_of(cv_street)[, c("lon", "lat")]), ll(0, 0)[1, ]))
main_end <- utils::tail(cv_node$segments[[1]]$path, 1)
check(sprintf("anchor with project_onto: node kept %s, the end's wait point is the node %s; the 100 s wait after the route goes to the node (max %.1f m off), named %s; the route still ends at the street point (%.1f m); without it the wait stays at the street point (%.1f m)",
              !is.null(po$node_lon), isTRUE(geosphere::distHaversine(te_h$info$end$wait, ll(0, 30)[1, ]) < 0.01), off_node,
              wait_of(cv_node) |> nrow() > 0 && identical(Filter(function(g) identical(g$role, "cover"), cv_node$segments)[[1]]$gpx_name, "H"),
              geosphere::distHaversine(c(main_end$lon, main_end$lat), ll(0, 0)[1, ]), off_street),
      !is.null(po$node_lon) && geosphere::distHaversine(te_h$info$end$wait, ll(0, 30)[1, ]) < 0.01 &&
        identical(cv_node$gaps$after$mode, "wait") && off_node < 0.5 &&
        identical(Filter(function(g) identical(g$role, "cover"), cv_node$segments)[[1]]$gpx_name, "H") &&
        geosphere::distHaversine(c(main_end$lon, main_end$lat), ll(0, 0)[1, ]) < 0.5 && off_street < 0.5 &&
        is.null(review_trip_ends(list(end = unname(ll(5, 5)[1, ])), e_h)$info$end$wait))

cat(if (fails == 0) "\nALL PASS\n" else sprintf("\n%d FAILED\n", fails))
quit(status = if (fails == 0) 0 else 1)
