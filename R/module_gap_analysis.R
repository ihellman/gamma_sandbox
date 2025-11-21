# GAP ANALYSIS MODULE ----------------------------------------------------------------
gapAnalysisUI <- function(id) {
  ns <- NS(id)
  div(
    h2("GAP Analysis"),
    p("This is the GAP Analysis page."),
    plotOutput(ns("plot"))
  )
}

gapAnalysisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      plot(
        1:10,
        1:10,
        main = "Sample GAP Analysis Plot",
        xlab = "X",
        ylab = "Y",
        col = "purple",
        pch = 19,
        cex = 2
      )
    })
  })
}