# ABOUT MODULE ------------------------------------------------------------------------
aboutUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "container",
      style = "max-width: 900px; padding: 40px;",
      # This line does all the work
      includeMarkdown("appData/about.md")
    ),
    # Shared footer sits below the text column, not inside it
    footer_ui()
  )
}

aboutServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server logic needed
  })
}
