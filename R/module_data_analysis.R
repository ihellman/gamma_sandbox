# DATA ANALYSIS MODULE ------------------------------------------------------------------
dataAnalysisUI <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = controlsModuleUI(ns("controls")), 
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
              bottom = 30,
              right = 10,
              style = "background: white; padding: 5px; border-radius: 4px; box-shadow: 0 2px 4px rgba(0,0,0,0.2); z-index: 1000;",
              div(
                actionButton(ns("deleteSelection"), "Delete Selection", icon = icon("trash-can"), style = "font-size: 11px; padding: 3px 10px; width: 100%; background-color: #ba5b5b; color: white; border: none;"),
                style = "margin-bottom: 5px;"
              ),
              div(
                actionButton(ns("undoLastDelete"), "Undo Delete", icon = icon("rotate-left"), style = "font-size: 11px; padding: 3px 10px; width: 100%; background-color: white; color: black; border: 1px solid #ddd;"),
                style = "margin-bottom: 5px;"
              ),
              div(
                actionButton(ns("clearSelection"), "Clear Selection", icon = icon("square-minus"), style = "font-size: 11px; padding: 3px 10px; width: 100%; background-color: white; color: black; border: 1px solid #ddd;")
              )
            )
          )
        )
      ),

      # Tables - right side
      div(
        class = "table-flex-container html-fill-item html-fill-container",
        div(
          id = ns("gbif_card_wrapper"),
          class = "table-card-wrapper",
          card(
            full_screen = TRUE,
            card_header(
              tags$strong("GBIF"),
              uiOutput(ns("row_count_gbif"), inline = TRUE),
              style = paste0("background-color: ", gbifColor[1], "; color: #2c3e50;") 
            ),
            card_body(
              padding = 0,
              DT_tableModuleUI(ns("DT_table_GBIF"))
            )
          )
        ),
        div(
          id = ns("upload_card_wrapper"),
          class = "table-card-wrapper",
          card(
            full_screen = TRUE,
            card_header(
              tags$strong("Upload"),
              uiOutput(ns("row_count_upload"), inline = TRUE),
              style = paste0("background-color: ", uploadColor[1], "; color: #2c3e50;")
            ),
            card_body(
              padding = 0,
              DT_tableModuleUI(ns("DT_table_upload"))
            )
          )
        )
      )
    )
  )
}

# --- SERVER ---
dataAnalysisServer <- function(id, analysis_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    analysis_data_backup <- reactiveVal(data.frame())

    # Load Internal Modules ------------------------------------------------------
    controlsModuleServer("controls", analysis_data, selected_points) 
    
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
      data_source = "upload"    )

    # Dynamic table card sizing ------------------------------------------------
    # Reactively toggle CSS classes on the GBIF and Upload card wrappers
    # based on how many rows each source has. This controls the flex layout:
    #   - "hidden-card": collapses the card to zero height when the source has no data
    #   - "small-card":  caps the card at a fixed height (~5 rows) when it has
    #                    very few entries, letting the other card fill remaining space
    #   - (no class):   both cards split 50/50 when each has enough data
    observe({
      data <- analysis_data()
      
      gbif_n <- if (nrow(data) > 0 && "source" %in% names(data)) {
        sum(data$source == "GBIF")
      } else {
        0L
      }
      
      upload_n <- if (nrow(data) > 0 && "source" %in% names(data)) {
        sum(data$source == "upload")
      } else {
        0L
      }
      
      # Hide card entirely when source has 0 rows; show it otherwise
      shinyjs::toggleClass(
        id = "gbif_card_wrapper",
        class = "hidden-card",
        condition = gbif_n == 0
      )
      # Cap card height when source has 1-5 rows
      shinyjs::toggleClass(
        id = "gbif_card_wrapper",
        class = "small-card",
        condition = gbif_n > 0 && gbif_n <= 5
      )
      
      shinyjs::toggleClass(
        id = "upload_card_wrapper",
        class = "hidden-card",
        condition = upload_n == 0
      )
      shinyjs::toggleClass(
        id = "upload_card_wrapper",
        class = "small-card",
        condition = upload_n > 0 && upload_n <= 5
      )
    })

    # Render row selection counts ------------------------------------------------
    output$row_count_gbif <- renderUI({
      req(nrow(analysis_data()) > 0)
      selected <- analysis_data() %>% filter(source == "GBIF", index %in% selected_points()) %>% nrow()
      if (selected > 0) HTML(paste0("&nbsp;&nbsp;&nbsp;(", selected, " row", if (selected != 1) "s", " selected)")) else ""
    })

    output$row_count_upload <- renderUI({
      req(nrow(analysis_data()) > 0)
      selected <- analysis_data() %>% filter(source == "upload", index %in% selected_points()) %>% nrow()
      if (selected > 0) HTML(paste0("&nbsp;&nbsp;&nbsp;(", selected, " row", if (selected != 1) "s", " selected)")) else ""
    })

    # Data management observers --------------------------------------------------
    observeEvent(input$deleteSelection, {
      req(nrow(analysis_data()) > 0)
      analysis_data_backup(analysis_data())
      current_data <- analysis_data()
      current_selection <- selected_points()

      if (length(current_selection) == 0) {
        showNotification("No records selected to delete", type = "warning")
        return()
      }
      updated_data <- current_data %>% filter(!index %in% current_selection) %>% mutate(index = row_number()) 
      analysis_data(updated_data)
      selected_points(numeric(0))
      showNotification(paste0("Successfully deleted ", length(current_selection), " record", if (length(current_selection) > 1) "s"), type = "message")
    })

    observe({
      shinyjs::toggleState("undoLastDelete", condition = nrow(analysis_data_backup()) > 0)
    })

    observeEvent(input$undoLastDelete, {
      req(nrow(analysis_data_backup()) > 0)
      numRestoredRecords <- nrow(analysis_data_backup()) - nrow(analysis_data())
      analysis_data(analysis_data_backup())
      showNotification(paste0("Successfully restored ", numRestoredRecords, " record", if (numRestoredRecords > 1) "s"), type = "message")
      analysis_data_backup(data.frame())
    })

    observeEvent(input$clearSelection, {
      selected_points(numeric(0))
    })
  })
}