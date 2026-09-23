# Package attachment, global options and static content live in global.R.
# Shiny only auto-sources global.R for ui.R/server.R apps, so a single-file
# app.R must source it explicitly (into the global env, once per process).
source("global.R")

# MAIN UI ----------------------------------------------------------------------------------
ui <- tagList(
  page_navbar(
    title = "GAMMa",
    id = "navbar",
    theme = bs_theme(version = 5),
    # LINKING THE CSS FILE HERE
    header = tags$head(
      tags$link(rel = "stylesheet", type = "text/css",
                href = paste0("custom.css?v=", as.integer(file.mtime("www/custom.css")))),
    ),
    nav_spacer(),
    # Navigation pages
    nav_panel("Home", value = "home", div()),
    nav_panel("Data Analysis", value = "data", dataAnalysisUI("data_analysis")),
    nav_panel("Gap Analysis", value = "gap", gapAnalysisUI("gap_analysis")),
    nav_panel("About", value = "about", aboutUI("about"))
  ),
  # Need to keep these items out of page_navbar, otherwise they show up as blank nav items
  # Initialize shinyjs
  useShinyjs(),
  # Landing page overlay
  landingUI("landing", landing_text = landing_text),
)

# MAIN SERVER -------------------------------------------------------------------------------
server <- function(input, output, session) {
  # assign some storage reactive values for functions
  analysis_data <- reactiveVal(data.frame())
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
  # NOTE: the controls module is owned by dataAnalysisServer (it lives in that
  # page's sidebar); do not instantiate it here as well.
  dataAnalysisServer("data_analysis", analysis_data, selected_points)
  gapAnalysisServer("gap_analysis", analysis_data,
                    active = reactive(identical(input$navbar, "gap")))
  aboutServer("about")
}

# Run the application
shinyApp(ui, server)
