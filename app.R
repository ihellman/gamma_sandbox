library(shiny)
library(bslib)
library(shinyjs)
library(readr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(DT)
library(yaml)
library(markdown)

# necesary only if app is called from the "play" button in positron
source("R/leaflet_maps.R")

# Load text from YAML file
landing_text <- read_yaml("appData/landing_text.yml")

# MAIN UI ----------------------------------------------------------------------------------
ui <- page_navbar(
  title = "Data Explorer",
  id = "navbar",
  theme = bs_theme(version = 5),
  # LINKING THE CSS FILE HERE
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  nav_spacer(),

  # Initialize shinyjs
  useShinyjs(),

  # Landing page overlay
  landingUI("landing", landing_text = landing_text),

  # Navigation pages
  nav_panel("Home", value = "home", div()),
  nav_panel("Data Analysis", value = "data", dataAnalysisUI("data_analysis")),
  nav_panel("Gap Analysis", value = "gap", gapAnalysisUI("gap_analysis")),
  nav_panel("About", value = "about", aboutUI("about"))
)

# MAIN SERVER -------------------------------------------------------------------------------
server <- function(input, output, session) {
  combined_data <- reactiveVal(data.frame())
  selected_points <- reactiveVal(numeric(0))

  # Landing page module
  launch_actions <- landingServer("landing", landing_text = landing_text)

  # Observe launch button (Get Started)
  observeEvent(launch_actions$launch(), {
    req(launch_actions$launch() > 0)
    shinyjs::addClass(selector = "body", class = "app-launched")
    shinyjs::removeClass(selector = "body", class = "on-home")
    updateNavbarPage(session, "navbar", selected = "data")
  })

  # Observe learn more button
  observeEvent(launch_actions$learn_more(), {
    req(launch_actions$learn_more() > 0)
    shinyjs::addClass(selector = "body", class = "app-launched")
    shinyjs::removeClass(selector = "body", class = "on-home")
    updateNavbarPage(session, "navbar", selected = "about")
  })

  # Observe navbar changes - if user clicks Home, show landing page
  observe({
    if (!is.null(input$navbar) && input$navbar == "home") {
      shinyjs::addClass(selector = "body", class = "on-home")
    } else if (!is.null(input$navbar)) {
      shinyjs::removeClass(selector = "body", class = "on-home")
    }
  })

  # Initialize other modules
  dataAnalysisServer("data_analysis", combined_data, selected_points)
  controlsModuleServer("controls", combined_data, selected_points)
  gapAnalysisServer("gap_analysis", combined_data)
  aboutServer("about")
}

# Run the application
shinyApp(ui, server)
