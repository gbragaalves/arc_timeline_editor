# Builds assets/rio_city.geojson: the city of Rio de Janeiro's limit (OSM
# relation 2697338, admin_level 8), simplified to 25 m and without parts under
# 2 ha. The "Rio Mosaico 2024" base map shows the IPP orthophoto only inside it
# (the IPP server answers white, not transparent, outside the city). Needs
# osmium. Run again only if the limit changes.
# Usage (from the project root): Rscript tools/build_rio_city.R [path/to/extract.osm.pbf]
args <- commandArgs(TRUE)
pbf <- if (length(args)) args[1] else path.expand("~/osrm-data/clipped.osm.pbf")
out <- file.path("assets", "rio_city.geojson")
tmp <- tempfile("rio_city_")
dir.create(tmp)
rel <- file.path(tmp, "rel.osm.pbf")
gj <- file.path(tmp, "rel.geojson")
if (system2("osmium", c("getid", "-r", "-f", "pbf", "-o", rel, "--overwrite", pbf, "r2697338"), stdout = FALSE) != 0 ||
    system2("osmium", c("export", "-f", "geojson", "--geometry-types=polygon", "-o", gj, "--overwrite", rel), stdout = FALSE) != 0) {
  cat("osmium failed\n")
  quit(status = 1)
}
suppressPackageStartupMessages(library(sf))
g <- st_read(gj, quiet = TRUE)
city <- g[g$name %in% "Rio de Janeiro" & g$admin_level %in% "8", ]
if (nrow(city) != 1) {
  cat("the city's relation was not found in", pbf, "\n")
  quit(status = 1)
}
parts <- st_cast(st_geometry(st_simplify(st_transform(city, 31983), dTolerance = 25, preserveTopology = TRUE)), "POLYGON")
parts <- parts[as.numeric(st_area(parts)) > 2e4]
geom <- st_transform(st_combine(parts), 4326)
dir.create(dirname(out), showWarnings = FALSE)
unlink(out)
st_write(st_sf(name = "Rio de Janeiro", geometry = geom), out, driver = "GeoJSON", quiet = TRUE,
         layer_options = c("COORDINATE_PRECISION=5", "RFC7946=YES"))
cat(sprintf("%s: %d parts, %d vertices, %.0f km2\n", out, length(parts),
            nrow(st_coordinates(geom)), as.numeric(st_area(geom)) / 1e6))
