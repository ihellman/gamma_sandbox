# global.R -----------------------------------------------------------------------
# Evaluated ONCE per R process (before ui/server in app.R). Everything here is
# shared by every user session on that process: package attachment, global
# options, static content and (later) static spatial layers.
#
# Load order for a single-file app.R app: Shiny first auto-sources every file
# in R/ into a shared environment (option shiny.autoload.r, default TRUE), then
# evaluates app.R. It does NOT source global.R for app.R apps, which is why
# app.R calls source("global.R") explicitly.

library(shiny)
library(bslib)
library(shinyjs)
library(shinycssloaders)
library(readr)
library(readxl)
library(arrow)      # taxonomy parquet (arrow::open_dataset in module_controls.R)
library(dplyr)
library(tidyr)
library(yaml)
library(markdown)   # includeMarkdown() on shinyapps.io needs it attached
library(leaflet)
library(leaflet.extras)
library(DT)
library(sf)
library(terra)
library(ggplot2)
library(rgbif)
library(htmlwidgets)
library(rmapshaper)

sf::sf_use_s2(FALSE)
options(shiny.autoreload = FALSE)

# Source repository: footer links ("View Source Code", "Report an Issue",
# "Read Documentation") are built from this. Update after the repo migration.
REPO_URL <- "https://github.com/ihellman/gamma_sandbox"

# App version, shown in the footer. Bump VERSION when releasing.
APP_VERSION <- if (file.exists("VERSION")) trimws(readLines("VERSION", n = 1, warn = FALSE)) else "dev"

# Landing page copy lives in YAML so it can be edited without touching R.
landing_text <- yaml::read_yaml("appData/landing_text.yml")

# Static spatial layers for the gap analysis, read ONCE per process instead of
# on every "Run Gap Analysis" click (land 1.2 MB, ecoregions 9 MB). The loader
# functions live in R/gap_analysis_functions.R. Shiny auto-sources R/ into a
# separate environment that this file cannot see, so source it explicitly here.
source("R/gap_analysis_functions.R")
GAP_LAND       <- load_land_layer("appData/land_simple.gpkg")
GAP_ECOREGIONS <- load_ecoregions_layer("appData/ecoregionsSimplified.gpkg")
