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

# People config for Location History overlay (optional)
if (file.exists("R/config_people.R")) {
  source("R/config_people.R", local = FALSE)
} else {
  PEOPLE_CONFIG <- list()
}

# ---- Load modules ----
# Order matters: base modules first, then dependents

source("R/utils_time.R", local = FALSE)
source("R/utils_geo.R", local = FALSE)
source("R/utils_osrm.R", local = FALSE)
source("R/utils_google.R", local = FALSE)
source("R/utils_arc.R", local = FALSE)
source("R/utils_weeks.R", local = FALSE)
source("R/export_arc.R", local = FALSE)
source("R/ui.R", local = FALSE)
source("R/server.R", local = FALSE)

# ---- Launch app ----
shinyApp(ui, server)
