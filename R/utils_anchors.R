# ---- Fixed anchor points for car / taxi trip ends ----
# ROUTE_ANCHORS (config_local.R; absent: the feature is off) lists places a
# car or taxi trip often starts or ends at: list(name, osm = "node/<id>" |
# "way/<id>", project_onto (optional: a street name), radius_m, valid_from,
# valid_to (NA: open)). resolve_route_anchors() turns them into points from the
# local pbf (osmium) and writes ROUTE_ANCHORS_FILE (git-ignored; the nightly
# OSM refresh rebuilds it, so edits in OSM carry over). In Batch review a car /
# taxi trip whose first (last) sample lies within an anchor's radius_m on a
# date it's valid starts (ends) exactly there, as if its start (end) marker
# had been dragged there; a drag of that marker overrides it for the trip.

ROUTE_ANCHORS_FILE <- if (exists("ROUTE_ANCHORS_FILE")) ROUTE_ANCHORS_FILE else file.path("reference", "route_anchors.rds")
ANCHOR_ACTIVITIES <- c("car", "taxi")
# Roads a car can route from: an anchor never sits on a footway or a path
ANCHOR_ROAD_TYPES <- c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential",
                       "living_street", "service", "road", "motorway_link", "trunk_link", "primary_link",
                       "secondary_link", "tertiary_link")

# The configured anchors; NULL when ROUTE_ANCHORS isn't set (the feature is off)
route_anchors_config <- function() {
  a <- get0("ROUTE_ANCHORS", envir = globalenv(), ifnotfound = NULL)
  if (!is.list(a) || !length(a)) NULL else a
}

# What the resolved file must match: the anchors' names, elements and projections
route_anchors_key <- function(cfg = route_anchors_config()) {
  if (is.null(cfg)) return("")
  paste(vapply(cfg, function(a) paste(a$name, a$osm, a$project_onto %||% "", sep = "|"), character(1)), collapse = ";")
}

# The pbf to resolve them from: my edited extract (~/osrm-data/clipped-mine.osm.pbf)
# when there, else OSM_PBF_FILE
route_anchors_pbf <- function() {
  mine <- file.path(dirname(OSM_PBF_FILE), "clipped-mine.osm.pbf")
  if (file.exists(mine)) mine else OSM_PBF_FILE
}

# Nodes and ways of an OPL dump (ways with node locations, as from
# osmium add-locations-to-ways): nodes = data.frame(id, lon, lat, name,
# entrance), ways = list of list(id, tags, coords (lon, lat), nodes (ids))
osm_opl_parse <- function(lines) {
  unescape <- function(x) {
    m <- gregexpr("%[0-9a-fA-F]+%", x)
    regmatches(x, m) <- lapply(regmatches(x, m), function(h) {
      vapply(h, function(code) intToUtf8(strtoi(gsub("%", "", code, fixed = TRUE), 16L)), character(1))
    })
    x
  }
  field <- function(l, f) {
    hit <- regexpr(paste0(" ", f, "[^ ]*"), l)
    out <- rep("", length(l))
    out[hit > 0] <- substring(regmatches(l, hit), 3)
    out
  }
  tags_of <- function(t) {
    if (!nzchar(t)) return(list())
    kv <- strsplit(t, ",", fixed = TRUE)[[1]]
    stats::setNames(as.list(unescape(sub("^[^=]*=", "", kv))), unescape(sub("=.*$", "", kv)))
  }
  kind <- substr(lines, 1, 1)
  nl <- lines[kind == "n"]
  ntags <- lapply(field(nl, "T"), tags_of)
  nodes <- data.frame(id = sub("^n([0-9]+) .*$", "n\\1", nl), lon = suppressWarnings(as.numeric(field(nl, "x"))),
                      lat = suppressWarnings(as.numeric(field(nl, "y"))),
                      name = vapply(ntags, function(t) t$name %||% "", character(1)),
                      entrance = vapply(ntags, function(t) t$entrance %||% NA_character_, character(1)))
  wl <- lines[kind == "w"]
  ways <- lapply(seq_along(wl), function(i) {
    p <- strsplit(field(wl[i], "N"), ",", fixed = TRUE)[[1]]
    co <- cbind(lon = suppressWarnings(as.numeric(sub("^[^x]*x([-0-9.]+)y.*$", "\\1", p))),
                lat = suppressWarnings(as.numeric(sub("^.*y", "", p))))
    list(id = sub("^w([0-9]+) .*$", "w\\1", wl[i]), tags = tags_of(field(wl[i], "T")), coords = co, nodes = sub("x.*$", "", p))
  })
  list(nodes = nodes, ways = ways)
}

# The point of one anchor (cfg: a ROUTE_ANCHORS entry) from its OSM element
# and the roads around it. el: list(type = "node", lon, lat) or list(type =
# "way", tags, coords, nodes = data.frame(lon, lat, entrance) of its nodes);
# roads: list of list(name, highway, coords). A node: itself; with
# project_onto, its projection onto the nearest road of that name (never a road
# of another name, e.g. a condominium's internal one), keeping the node
# (node_lon, node_lat: a wait there merges into the visit). A way that is a road:
# the road (kind "road": the point on it nearest each trip's end). A building
# or area: its entrance node (entrance=main first), else the point of the
# nearest routable road nearest to its centroid. list(kind = "point" | "road",
# lon, lat, coords, how) or NULL.
anchor_point_from_osm <- function(cfg, el, roads) {
  routable <- Filter(function(r) isTRUE(r$highway %in% ANCHOR_ROAD_TYPES) && nrow(r$coords) >= 2, roads)
  nearest_on <- function(p, rs) {
    best <- NULL
    for (r in rs) {
      cum <- path_cumulative_m(r$coords)
      sp <- polyline_segment_projection(r$coords, cum, p)
      j <- which.min(sp$off)
      if (is.null(best) || sp$off[j] < best$off) best <- list(off = sp$off[j], pt = path_points_at(r$coords, cum, sp$d[j])[1, ])
    }
    best
  }
  point <- function(p, how) list(kind = "point", lon = unname(p[1]), lat = unname(p[2]), how = how)
  if (identical(el$type, "node")) {
    p <- c(el$lon, el$lat)
    if (is.null(cfg$project_onto) || !nzchar(cfg$project_onto)) return(point(p, "node"))
    hit <- nearest_on(p, Filter(function(r) identical(r$name, cfg$project_onto), routable))
    if (is.null(hit)) return(point(p, "node (no road named like project_onto)"))
    return(c(point(hit$pt, "project_onto"), list(node_lon = unname(p[1]), node_lat = unname(p[2]))))
  }
  if (isTRUE(el$tags$highway %in% ANCHOR_ROAD_TYPES)) return(list(kind = "road", coords = el$coords, how = "road"))
  ent <- el$nodes[!is.na(el$nodes$entrance), , drop = FALSE]
  if (nrow(ent)) {
    j <- if (any(ent$entrance == "main")) which(ent$entrance == "main")[1] else 1L
    return(point(c(ent$lon[j], ent$lat[j]), "entrance"))
  }
  co <- el$coords
  if (nrow(co) > 2 && all(co[1, ] == co[nrow(co), ])) co <- co[-nrow(co), , drop = FALSE]
  hit <- nearest_on(colMeans(co), routable)
  if (is.null(hit)) NULL else point(hit$pt, "nearest_road")
}

# Resolve the anchors (cfg) from a pbf (osmium) into `out`. Returns list(ok,
# message, summary = data.frame(name, how, element: the element's kind of
# tags, e.g. "building=yes")); no ids or coordinates.
resolve_route_anchors <- function(cfg = route_anchors_config(), pbf = route_anchors_pbf(), out = ROUTE_ANCHORS_FILE) {
  if (is.null(cfg)) return(list(ok = FALSE, message = "ROUTE_ANCHORS is not set"))
  if (!file.exists(pbf)) return(list(ok = FALSE, message = paste("OSM file not found:", basename(pbf))))
  run <- function(args) {
    o <- suppressWarnings(system2("osmium", shQuote(args), stdout = TRUE, stderr = TRUE))
    if (!is.null(attr(o, "status")) && attr(o, "status") != 0) stop(paste(o, collapse = "\n"))
    o
  }
  tmp <- tempfile("anchors_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  ids <- vapply(cfg, function(a) {
    p <- strsplit(a$osm %||% "", "/", fixed = TRUE)[[1]]
    if (length(p) == 2 && p[1] %in% c("node", "way")) paste0(substr(p[1], 1, 1), p[2]) else NA_character_
  }, character(1))
  if (anyNA(ids)) return(list(ok = FALSE, message = "ROUTE_ANCHORS: osm must be \"node/<id>\" or \"way/<id>\""))
  els <- tryCatch({
    run(c("getid", "-r", normalizePath(pbf), unique(ids), "-o", file.path(tmp, "el.osm.pbf"), "--overwrite"))
    osm_opl_parse(run(c("add-locations-to-ways", file.path(tmp, "el.osm.pbf"), "-f", "opl", "-o", "-")))
  }, error = function(e) NULL)
  if (is.null(els)) return(list(ok = FALSE, message = "osmium getid failed"))
  element <- function(id) {
    if (startsWith(id, "n")) {
      n <- els$nodes[els$nodes$id == id, ]
      if (!nrow(n) || is.na(n$lon[1])) return(NULL)
      return(list(type = "node", lon = n$lon[1], lat = n$lat[1]))
    }
    w <- Filter(function(w) identical(w$id, id), els$ways)
    if (!length(w)) return(NULL)
    w <- w[[1]]
    nodes <- els$nodes[match(w$nodes, els$nodes$id), c("lon", "lat", "entrance")]
    list(type = "way", tags = w$tags, coords = w$coords, nodes = nodes[!is.na(nodes$lon), , drop = FALSE])
  }
  found <- lapply(ids, element)
  if (any(vapply(found, is.null, logical(1)))) {
    return(list(ok = FALSE, message = sprintf("%d of the anchors' OSM elements not in %s", sum(vapply(found, is.null, logical(1))), basename(pbf))))
  }
  # The roads around them (within ~800 m)
  xy <- do.call(rbind, lapply(found, function(e) if (e$type == "node") c(e$lon, e$lat) else e$coords))
  pad <- c(800 / (111320 * cos(mean(xy[, 2]) * pi / 180)), 800 / 110540)
  bbox <- sprintf("%.6f,%.6f,%.6f,%.6f", min(xy[, 1]) - pad[1], min(xy[, 2]) - pad[2], max(xy[, 1]) + pad[1], max(xy[, 2]) + pad[2])
  roads <- tryCatch({
    run(c("extract", "-b", bbox, "--strategy=complete_ways", normalizePath(pbf), "-o", file.path(tmp, "box.osm.pbf"), "--overwrite"))
    run(c("tags-filter", file.path(tmp, "box.osm.pbf"), "w/highway", "-o", file.path(tmp, "roads.osm.pbf"), "--overwrite"))
    w <- osm_opl_parse(run(c("add-locations-to-ways", file.path(tmp, "roads.osm.pbf"), "-f", "opl", "-o", "-")))$ways
    lapply(w, function(x) list(name = x$tags$name %||% "", highway = x$tags$highway %||% "", coords = x$coords[stats::complete.cases(x$coords), , drop = FALSE]))
  }, error = function(e) NULL)
  if (is.null(roads)) return(list(ok = FALSE, message = "osmium extract of the roads around the anchors failed"))
  recs <- lapply(seq_along(cfg), function(k) {
    pt <- anchor_point_from_osm(cfg[[k]], found[[k]], roads)
    if (is.null(pt)) return(NULL)
    c(list(name = cfg[[k]]$name, radius_m = as.numeric(cfg[[k]]$radius_m %||% 300),
           valid_from = as.character(cfg[[k]]$valid_from %||% NA), valid_to = as.character(cfg[[k]]$valid_to %||% NA)), pt)
  })
  kind_of <- function(e) {
    if (e$type == "node") return("node")
    main <- intersect(c("highway", "building", "amenity", "office", "landuse", "leisure", "shop"), names(e$tags))
    if (length(main)) paste0(main[1], "=", e$tags[[main[1]]]) else "way"
  }
  summary <- data.frame(name = vapply(cfg, `[[`, "", "name"),
                        how = vapply(recs, function(r) r$how %||% "unresolved", character(1)),
                        element = vapply(found, kind_of, character(1)))
  if (any(vapply(recs, is.null, logical(1)))) return(list(ok = FALSE, message = "an anchor has no usable point", summary = summary))
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(key = route_anchors_key(cfg), pbf = basename(pbf), pbf_mtime = as.numeric(file.mtime(pbf)),
               built = Sys.time(), anchors = recs), out)
  list(ok = TRUE, message = sprintf("%d anchors resolved from %s", length(recs), basename(pbf)), summary = summary)
}

.route_anchors <- new.env(parent = emptyenv())

# The resolved anchors (NULL: the feature is off, or not resolved for the
# current ROUTE_ANCHORS yet); cached per file version
route_anchors <- function(file = ROUTE_ANCHORS_FILE, cfg = route_anchors_config()) {
  if (is.null(cfg) || !file.exists(file)) return(NULL)
  key <- paste(as.numeric(file.mtime(file)), route_anchors_key(cfg))
  if (!identical(.route_anchors$key, key)) {
    d <- tryCatch(readRDS(file), error = function(e) NULL)
    .route_anchors$data <- if (identical(d$key, route_anchors_key(cfg))) d$anchors
    .route_anchors$key <- key
  }
  .route_anchors$data
}

# Whether the anchors need resolving: configured, and the file missing, made
# for other anchors, or older than the pbf
route_anchors_stale <- function(file = ROUTE_ANCHORS_FILE, cfg = route_anchors_config(), pbf = route_anchors_pbf()) {
  if (is.null(cfg)) return(FALSE)
  if (!file.exists(file)) return(TRUE)
  d <- tryCatch(readRDS(file), error = function(e) NULL)
  is.null(d) || !identical(d$key, route_anchors_key(cfg)) || (file.exists(pbf) && as.numeric(file.mtime(pbf)) > (d$pbf_mtime %||% 0))
}

# The anchors at a trip's ends: its first (last) sample within an anchor's
# radius_m, on a date the anchor is valid (valid_from .. valid_to, local
# dates; NA open); the nearest wins. A road anchor's point is the one on it
# nearest that sample. list(start, end): each NULL or list(name, lon, lat,
# dist_m, wait: where a wait there goes, the original node of an anchor
# projected onto a street, else NULL).
trip_anchor_ends <- function(s, day, anchors = route_anchors()) {
  out <- list(start = NULL, end = NULL)
  if (!length(anchors) || is.null(s) || !nrow(s)) return(out)
  day <- as.Date(day)
  ok_day <- function(a) {
    from <- suppressWarnings(as.Date(a$valid_from %||% NA))
    to <- suppressWarnings(as.Date(a$valid_to %||% NA))
    (is.na(from) || day >= from) && (is.na(to) || day <= to)
  }
  valid <- Filter(ok_day, anchors)
  at <- function(p) {
    best <- NULL
    for (a in valid) {
      pt <- if (identical(a$kind, "road")) {
        cum <- path_cumulative_m(a$coords)
        sp <- polyline_segment_projection(a$coords, cum, p)
        path_points_at(a$coords, cum, sp$d[which.min(sp$off)])[1, ]
      } else {
        c(a$lon, a$lat)
      }
      d <- geosphere::distHaversine(p, pt)
      if (d <= a$radius_m && (is.null(best) || d < best$dist_m)) {
        best <- list(name = a$name, lon = unname(pt[1]), lat = unname(pt[2]), dist_m = d,
                     wait = if (!is.null(a$node_lon)) c(a$node_lon, a$node_lat))
      }
    }
    best
  }
  list(start = at(c(s$lon[1], s$lat[1])), end = at(c(s$lon[nrow(s)], s$lat[nrow(s)])))
}

# A trip's ends for the splice: a dragged one (user_ends: start / end, c(lon,
# lat)) wins over its anchor (trip_anchor_ends()). list(ends (start / end:
# c(lon, lat)), info (per end: source "drag" | "anchor", anchor: the anchor's
# name there, if any; wait: an anchored end's wait point), reach (per
# anchored end: how far along the base its splice may look,
# splice_route_fix()'s reach))
review_trip_ends <- function(user_ends, anchored, th = REVIEW_THRESHOLDS) {
  ends <- list()
  info <- list()
  reach <- list()
  for (w in c("start", "end")) {
    if (!is.null(user_ends[[w]])) {
      ends[[w]] <- user_ends[[w]]
      info[[w]] <- list(source = "drag", anchor = anchored[[w]]$name)
    } else if (!is.null(anchored[[w]])) {
      ends[[w]] <- c(anchored[[w]]$lon, anchored[[w]]$lat)
      info[[w]] <- list(source = "anchor", anchor = anchored[[w]]$name, wait = anchored[[w]]$wait)
      reach[[w]] <- th$pins$splice$anchor_reach_m
    }
  }
  list(ends = ends, info = info, reach = reach)
}

# Whether the splices at a trip's ends (splice_route_fix()'s with only fixed
# points moved) are short enough to keep the proposal eligible for "Accept
# all": each at most anchor_bulk_m long
anchor_splices_short <- function(pf, sp = REVIEW_THRESHOLDS$pins$splice) {
  at_ends <- Filter(function(g) any(c("start", "end") %in% g$keys), pf$splices %||% list())
  all(vapply(at_ends, function(g) sum(vapply(g$legs, `[[`, numeric(1), "length_m")) <= sp$anchor_bulk_m, logical(1)))
}
