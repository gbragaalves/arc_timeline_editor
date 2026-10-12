# ---- Map overlays: bus infrastructure, router network ----
# "Bus infrastructure": reference/bus_infra.geojson (tools/refresh_bus_infra.py,
# rebuilt by the nightly OSM refresh), sent to the map by bounds.
# "Router network": OSRM's vector tiles (/tile/v1) of a road profile, drawn
# client-side (ROUTE_EDIT_JS).

BUS_INFRA_FILE      <- if (exists("BUS_INFRA_FILE")) BUS_INFRA_FILE else file.path("reference", "bus_infra.geojson")
BUS_INFRA_MIN_ZOOM  <- 12     # below this the layer stays empty (too many ways)
BUS_INFRA_MAX_WAYS  <- 4000   # per request

.layers_state <- new.env(parent = emptyenv())

# Features of the bus infrastructure file with their bounding boxes (cached by
# file mtime); NULL when the file is missing
bus_infra_data <- function(file = BUS_INFRA_FILE) {
  if (!file.exists(file)) return(NULL)
  key <- paste(normalizePath(file), as.numeric(file.mtime(file)))
  if (identical(.layers_state$bus_key, key)) return(.layers_state$bus)
  gj <- tryCatch(jsonlite::fromJSON(file, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(gj)) return(NULL)
  feats <- gj$features
  bbox <- t(vapply(feats, function(f) {
    xy <- matrix(unlist(f$geometry$coordinates), ncol = 2, byrow = TRUE)
    c(min(xy[, 1]), min(xy[, 2]), max(xy[, 1]), max(xy[, 2]))
  }, numeric(4)))
  .layers_state$bus <- list(features = feats, bbox = bbox, built = gj$built %||% NA_character_)
  .layers_state$bus_key <- key
  .layers_state$bus
}

# Features intersecting the box (w, s, e, n), padded by pad (fraction); at most
# max_ways (the main kinds first). Empty below BUS_INFRA_MIN_ZOOM.
bus_infra_in_bounds <- function(w, s, e, n, zoom, pad = 0.2, max_ways = BUS_INFRA_MAX_WAYS, data = bus_infra_data()) {
  if (is.null(data) || is.null(zoom) || zoom < BUS_INFRA_MIN_ZOOM) return(list())
  dx <- (e - w) * pad
  dy <- (n - s) * pad
  b <- data$bbox
  k <- which(b[, 3] >= w - dx & b[, 1] <= e + dx & b[, 4] >= s - dy & b[, 2] <= n + dy)
  if (length(k) > max_ways) {
    blocked <- vapply(data$features[k], function(f) identical(f$properties$kind, "blocked"), logical(1))
    k <- c(k[!blocked], k[blocked])[seq_len(max_ways)]
  }
  data$features[k]
}

# OSRM vector-tile URL template of a profile (MapLibre placeholders)
router_tile_url <- function(profile) {
  base <- OSRM_SERVERS[[profile]] %||% OSRM_SERVERS[["car"]]
  paste0(base, "/tile/v1/", OSRM_PROFILES[[profile]] %||% "driving", "/tile({x},{y},{z}).mvt")
}
