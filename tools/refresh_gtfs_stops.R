# Refreshes reference/gtfs_stops_rio.csv (stop names for split waits, see
# R/utils_stops.R) from the latest SMTR GTFS feed in BigQuery
# (rj-smtr.gtfs.stops, feed_start_date = MAX), billed to GTFS_BQ_PROJECT with
# the Google account GTFS_BQ_EMAIL (config_local.R). The first run, or an
# expired login, needs a browser sign-in: bigrquery::bq_auth() in an
# interactive R session.
# Usage (from the project root): Rscript tools/refresh_gtfs_stops.R
if (file.exists("config_local.R")) source("config_local.R")
source("R/utils_stops.R")
res <- refresh_gtfs_stops()
cat(sprintf("%s: %d stops, feed %s\n", res$file, res$rows, paste(res$feed_start_date, collapse = ", ")))
