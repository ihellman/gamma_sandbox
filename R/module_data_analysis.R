# DATA ANALYSIS MODULE ------------------------------------------------------------------
dataAnalysisUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = controlsModuleUI("controls"),
    mapModuleUI(ns("map")),
    navset_tab(
      nav_panel(title = "Reactable", tableModuleUI(ns("table"))),
      nav_panel(title = "DT", DT_tableModuleUI(ns("DT_table")))
    )
  )
}

dataAnalysisServer <- function(id, combined_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    controlsModuleServer("controls")
    mapModuleServer("map", combined_data, selected_points)
    tableModuleServer("table", combined_data, selected_points)
    DT_tableModuleServer("DT_table", combined_data, selected_points)
  })
}