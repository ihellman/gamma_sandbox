# global.R -----------------------------------------------------------------------
# Evaluated ONCE per R process (before ui/server in app.R). Everything here is
# shared by every user session on that process: package attachment, global
# options, static content and (later) static spatial layers.
#
# Shiny sources this file automatically when the app is launched from the app
# directory (runApp(), rsconnect/Connect, shinyapps.io). The R/ directory is
# auto-sourced by Shiny as well (option shiny.autoload.r, default TRUE).

library(shiny)
library(bslib)
library(shinyjs)
library(shinycssloaders)
library(readr)
library(readxl)
library(arrow)      # taxonomy parquet (previously only called via arrow::)
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
library(rmarkdown)
library(htmlwidgets)
library(rmapshaper)

sf::sf_use_s2(FALSE)
options(shiny.autoreload = FALSE)

# App version, shown in the footer. Bump VERSION when releasing.
APP_VERSION <- if (file.exists("VERSION")) trimws(readLines("VERSION", n = 1, warn = FALSE)) else "dev"

# Landing page copy lives in YAML so it can be edited without touching R.
landing_text <- yaml::read_yaml("appData/landing_text.yml")
