#############################
# Arc Timeline Builder 2.0
#############################

# ---- Packages ----
library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(sf)
library(dplyr)
library(lubridate)
library(uuid)
library(lutz)
library(xml2)
library(stringr)
library(purrr)
library(geosphere)
library(zip)

# ---- Local config (private data, not tracked) ----
source("config_local.R", local = FALSE)

# ---- Load modules ----
# Order matters: base modules first, then dependents

source("R/utils_time.R", local = FALSE)
source("R/utils_geo.R", local = FALSE)
source("R/utils_osrm.R", local = FALSE)
source("R/utils_google.R", local = FALSE)
source("R/utils_arc.R", local = FALSE)
source("R/utils_jae.R", local = FALSE)
source("R/utils_weeks.R", local = FALSE)
source("R/utils_i18n.R", local = FALSE)
source("R/utils_icloud.R", local = FALSE)
source("R/utils_icloud_cleanup.R", local = FALSE)
source("R/utils_review.R", local = FALSE)
source("R/utils_stops.R", local = FALSE)
source("R/utils_smtr.R", local = FALSE)
source("R/export_arc.R", local = FALSE)
source("R/utils_layers.R", local = FALSE)
source("R/ui_route_edit_js.R", local = FALSE)
source("R/ui.R", local = FALSE)
source("R/server_review.R", local = FALSE)
source("R/server.R", local = FALSE)

# ---- Launch app ----
shinyApp(ui, server)
