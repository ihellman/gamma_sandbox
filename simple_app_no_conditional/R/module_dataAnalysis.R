# DATA ANALYSIS MODULE ------------------------------------------------------------------
dataAnalysisUI <- function(id) {
  ns <- NS(id)
  div(
    controlsModuleUI("controls"),
    mapModuleUI(ns("map")),
    tableModuleUI(ns("table"))
  )
}

dataAnalysisServer <- function(id, combined_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    controlsModuleServer("controls")
    mapModuleServer("map", combined_data, selected_points)
    tableModuleServer("table", combined_data, selected_points)
  })
}
