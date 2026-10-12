# Rebuilds reference/rails.gpkg (rails and stations for the rail fixes, see
# build_rail_reference() in R/utils_review.R) from an OSM extract; run by
# ~/osrm-data/nightly_refresh.sh after my changesets touch rails or stations.
# The new file replaces the old one only when the build succeeds.
# Usage (from the project root): Rscript tools/refresh_rails.R [path/to/extract.osm.pbf]
args <- commandArgs(TRUE)
for (e in parse("app.R")) {
  if (is.call(e) && identical(e[[1]], as.name("shinyApp"))) next
  suppressPackageStartupMessages(eval(e, globalenv()))
}
pbf <- if (length(args)) args[1] else OSM_PBF_FILE
tmp <- tempfile(fileext = ".gpkg")
res <- build_rail_reference(pbf = pbf, out = tmp)
if (!isTRUE(res$ok)) {
  cat("rails.gpkg not rebuilt:", res$message, "\n")
  quit(status = 1)
}
dir.create(dirname(RAIL_REFERENCE_FILE), recursive = TRUE, showWarnings = FALSE)
if (!file.copy(tmp, RAIL_REFERENCE_FILE, overwrite = TRUE)) quit(status = 1)
unlink(tmp)
cat(sprintf("%s rebuilt from %s: %s\n", RAIL_REFERENCE_FILE, basename(pbf), res$message))
