# DATA ANALYSIS MODULE ------------------------------------------------------------------
dataAnalysisUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = controlsModuleUI("controls"),
    fillable = TRUE,
    layout_columns(
      col_widths = c(6, 6),
      row_heights = c(1),
      fillable = TRUE,

      # Map takes full height on the left
      card(
        full_screen = TRUE,
        #card_header("Map"),
        card_body(
          padding = 0,
          mapModuleUI(ns("map"))
        )
      ),

      # Tables stacked vertically on the right
      layout_columns(
        col_widths = 12,
        row_heights = c(1, 1),
        fillable = TRUE,

        # GBIF table (top half)
        card(
          full_screen = TRUE,
          card_header("GBIF",
          style = "background-color: #40a999; color: white;"),
          card_body(
            padding = 0,
            DT_tableModuleUI(ns("DT_table_GBIF"))
          )
        ),

        # Upload table (bottom half)
        card(
          full_screen = TRUE,
          card_header("Upload",
          style = "background-color: #c2914b; color: white;"),
          card_body(
            padding = 0,
            DT_tableModuleUI(ns("DT_table_upload"))
          )
        )
      )
    )
  )
}

dataAnalysisServer <- function(id, combined_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    controlsModuleServer("controls")
    mapModuleServer("map", combined_data, selected_points)
    DT_tableModuleServer("DT_table_GBIF", combined_data, selected_points, data_source = "GBIF")
    DT_tableModuleServer("DT_table_upload", combined_data, selected_points, data_source = "upload")
  })
}
