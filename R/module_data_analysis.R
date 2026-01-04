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
          card_header(
            tags$strong("GBIF"),
            uiOutput(ns("row_count_gbif"), inline = TRUE),
            style = "background-color: #57b16b; color: white;"
          ),
          card_body(
            padding = 0,
            DT_tableModuleUI(ns("DT_table_GBIF"))
          )
        ),

        # Upload table (bottom half)
        card(
          full_screen = TRUE,
          card_header(
            tags$strong("Upload"),
            uiOutput(ns("row_count_upload"), inline = TRUE),
            style = "background-color: #9e6cb1; color: white;"
          ),
          card_body(
            padding = 0,
            DT_tableModuleUI(ns("DT_table_upload"))
          )
        )
      )
    )
  )
}

dataAnalysisServer <- function(id, analysis_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    controlsModuleServer("controls")
    mapModuleServer("map", analysis_data, selected_points)
    DT_tableModuleServer(
      "DT_table_GBIF",
      analysis_data,
      selected_points,
      data_source = "GBIF"
    )
    DT_tableModuleServer(
      "DT_table_upload",
      analysis_data,
      selected_points,
      data_source = "upload"
    )

    # Render GBIF row count
    output$row_count_gbif <- renderUI({
      req(nrow(analysis_data()) > 0)

      selected <- analysis_data() %>%
        filter(source == "GBIF", index %in% selected_points()) %>%
        nrow()

      if (selected > 0) {
        HTML(paste0(
          "&nbsp;&nbsp;&nbsp;(",
          selected,
          " row",
          if (selected != 1) "s",
          " selected)"
        ))
      } else {
        ""
      }
    })

    # Render Upload row count
    output$row_count_upload <- renderUI({
      req(nrow(analysis_data()) > 0)

      selected <- analysis_data() %>%
        filter(source == "upload", index %in% selected_points()) %>%
        nrow()

      if (selected > 0) {
        HTML(paste0(
          "&nbsp;&nbsp;&nbsp;(",
          selected,
          " row",
          if (selected != 1) "s",
          " selected)"
        ))
      } else {
        ""
      }
    })
  })
}
