# ABOUT MODULE ------------------------------------------------------------------------
aboutUI <- function(id) {
  ns <- NS(id)
  div(
    class = "container",
    style = "max-width: 900px; padding: 40px;",
    # This line does all the work
    includeMarkdown("appData/about.md"),
    footer_ui()
  )
}

aboutServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server logic needed
  })
}
