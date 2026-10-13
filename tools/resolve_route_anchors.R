# Resolves ROUTE_ANCHORS (config_local.R) into reference/route_anchors.rds from an
# OSM extract (see resolve_route_anchors() in R/utils_anchors.R); run by
# ~/osrm-data/nightly_refresh.sh after my changesets are applied, so my OSM
# edits carry over. Prints the anchors' names and how each was resolved only.
# Usage (from the project root): Rscript tools/resolve_route_anchors.R [path/to/extract.osm.pbf]
args <- commandArgs(TRUE)
for (e in parse("app.R")) {
  if (is.call(e) && identical(e[[1]], as.name("shinyApp"))) next
  suppressPackageStartupMessages(eval(e, globalenv()))
}
if (is.null(route_anchors_config())) {
  cat("ROUTE_ANCHORS is not set: nothing to resolve\n")
  quit(status = 0)
}
res <- resolve_route_anchors(pbf = if (length(args)) args[1] else route_anchors_pbf())
if (!is.null(res$summary)) for (k in seq_len(nrow(res$summary))) {
  cat(sprintf("  %s: %s (%s)\n", res$summary$name[k], res$summary$how[k], res$summary$element[k]))
}
if (!isTRUE(res$ok)) {
  cat("route_anchors.rds not rebuilt:", res$message, "\n")
  quit(status = 1)
}
cat(res$message, "\n")
