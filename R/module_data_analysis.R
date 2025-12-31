# DATA ANALYSIS MODULE ------------------------------------------------------------------
dataAnalysisUI <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = controlsModuleUI("controls"),
    fillable = TRUE,
    
    layout_columns(
      row_heights = c(6, 6), # Top row 8/12, Bottom row 4/12
      col_widths = 12,
      
      # --- TOP ROW ---
      layout_columns(
        col_widths = c(9, 3), 
        
        # Map Card
        card(
          full_screen = TRUE,
          card_body(padding = 0, mapModuleUI(ns("map")))
        ),
        
        # Value Boxes: layout_column_wrap stacks them vertically 
        # when width is set to 1 (1 column)
        layout_column_wrap(
          width = 1, 
          heights_equal = "row",
          value_box(
            title = "GBIF",
            value = "192",
            showcase = bsicons::bs_icon("graph-up"),
            theme = value_box_theme(bg = "#d5dff0"),
            p("Records")
          ),
          value_box(
            title = "GBIF",
            value = "192",
            showcase = bsicons::bs_icon("graph-up"),
            theme = value_box_theme(bg = "#d5dff0"),
            p("Detail 2")
          ),
          value_box(
            title = "GBIF",
            value = "192",
            #showcase = bsicons::bs_icon("graph-up"),
            theme = value_box_theme(bg = "#d5dff0"),
            p("Detail 3"),
            p("Detail 4"),
          )
        )
      ),

      # --- BOTTOM ROW ---
      navset_card_underline(
        full_screen = TRUE,
        nav_panel(
          "GBIF",
          card_body(padding = 0, DT_tableModuleUI(ns("DT_table_GBIF")))
        ),
        nav_panel(
          "Uploaded",
          card_body(padding = 0, DT_tableModuleUI(ns("DT_table_upload")))
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
