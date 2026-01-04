# DATA ANALYSIS MODULE ------------------------------------------------------------------
dataAnalysisUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = controlsModuleUI("controls"),
    fillable = TRUE,
    layout_columns(
      col_widths = c(6, 6),
      fillable = TRUE,
      
      # Map card - left side
      card(
        full_screen = TRUE,
        card_body(
          padding = 0,
          div(
            style = "position: relative; height: 100%;",
            mapModuleUI(ns("map")),
            absolutePanel(
              bottom = 30, right = 10,
              style = "background: white; padding: 5px; border-radius: 4px; box-shadow: 0 2px 4px rgba(0,0,0,0.2); z-index: 1000;",
              div(
                actionButton(ns("deleteSelection"), "Delete Selection",
                  icon = icon("trash-can"),
                  style = "font-size: 11px; padding: 3px 10px; width: 100%; background-color: #ba5b5b; color: white; border: none;"
                ),
                style = "margin-bottom: 5px;"
              ),
              div(
                actionButton(ns("undoLastDelete"), "Undo Delete",
                  icon = icon("rotate-left"),
                  style = "font-size: 11px; padding: 3px 10px; width: 100%; background-color: white; color: black; border: 1px solid #ddd;"
                ),
                style = "margin-bottom: 5px;"
              ),
              div(
                actionButton(ns("clearSelection"), "Clear Selection",
                  icon = icon("square-minus"),
                  style = "font-size: 11px; padding: 3px 10px; width: 100%; background-color: white; color: black; border: 1px solid #ddd;"
                )
              )
            )
          )
        )
      ),

      # Tables - right side
      layout_columns(
        col_widths = 12,
        fillable = TRUE,
        
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
    # Empty reactive for backup of analysis_data() for undo functionality
    analysis_data_backup <- reactiveVal(data.frame())

    # Load Internal Modules ------------------------------------------------------
    controlsModuleServer("controls")
    mapModuleServer("map", analysis_data, selected_points)
    DT_tableModuleServer("DT_table_GBIF", analysis_data, selected_points, data_source = "GBIF")
    DT_tableModuleServer("DT_table_upload", analysis_data, selected_points, data_source = "upload")

    # Render row selection counts ------------------------------------------------
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

    # Delete selected points ---------------------------------------------------------
    observeEvent(input$deleteSelection, {
      req(nrow(analysis_data()) > 0)

      # Backup before deletion
      analysis_data_backup(analysis_data())

      current_data <- analysis_data()
      current_selection <- selected_points()

      # Only proceed if there are selected points
      if (length(current_selection) == 0) {
        showNotification("No points selected to delete", type = "warning")
        return()
      }
      # Remove selected rows
      updated_data <- current_data %>%
        filter(!index %in% current_selection) %>%
        mutate(index = row_number()) # Re-index after deletion

      # Update the data
      analysis_data(updated_data)

      # Clear the selection
      selected_points(numeric(0))
      print(paste0(
        "There are",
        nrow(analysis_data_backup()),
        "rows in the BACKUP data."
      ))
      print(paste0(
        "There are",
        nrow(analysis_data()),
        "rows in the CURRENT data."
      ))
    })

    # Enable/disable Undo button based on backup existence ------------------------
    observe({
      shinyjs::toggleState("undoLastDelete", condition = nrow(analysis_data_backup()) > 0)
    })

    # Undo last delete -------------------------------------------------------------
    observeEvent(input$undoLastDelete, {
      req(nrow(analysis_data_backup()) > 0)

      # Restore from backup
      analysis_data(analysis_data_backup())

      # Clear the backup
      analysis_data_backup(data.frame())
    })

    # Clear selection ---------------------------------------------------------------
    observeEvent(input$clearSelection, {
      selected_points(numeric(0))
    })
  })
}
