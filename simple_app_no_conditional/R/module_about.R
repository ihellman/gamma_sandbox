# ABOUT MODULE ------------------------------------------------------------------------
aboutUI <- function(id) {
  ns <- NS(id)
  div(
    h2("About"),
    p("This application demonstrates a modular Shiny app with:"),
    tags$ul(
      tags$li("A landing page with custom styling"),
      tags$li("Modular page structure"),
      tags$li("Custom navbar styling"),
      tags$li("Persistent Leaflet map state"),
      tags$li("No navbar flicker on load")
    )
  )
}

aboutServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server logic needed
  })
}
