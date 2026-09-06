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
            # Button styling lives in www/custom.css (.map-action-btn*)
            absolutePanel(
              bottom = 30,
              right = 10,
              class = "map-action-panel",
              div(
                actionButton(ns("deleteSelection"), "Delete Selection", icon = icon("trash-can"),
                             class = "map-action-btn map-action-btn--danger")
              ),
              div(
                bslib::tooltip(
                  actionButton(ns("undoLastDelete"), "Undo Delete", icon = icon("rotate-left"),
                               class = "map-action-btn"),
                  "Restores the most recent delete only (one level of undo)."
                )
              ),
              div(
                actionButton(ns("clearSelection"), "Clear Selection", icon = icon("square-minus"),
                             class = "map-action-btn")
              )
            )
          )
        )
      ),

      # Tables - right side
      div(
        class = "table-flex-container html-fill-item html-fill-container",

        # Empty-state instructions. Visible on page load; the server observer
        # below hides it as soon as any data is loaded. Edit the text in
        # appData/data_analysis_instructions.md — no R changes needed.
        div(
          id = ns("instructions_panel"),
          class = "instructions-panel",
          includeMarkdown("appData/data_analysis_instructions.md")
        ),

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

      # Instructions replace the tables whenever the working dataset is empty.
      # Unlike the cards below, this panel has no DataTable inside it, so a
      # plain show/hide is safe here — the cards need the class-toggle trick.
      shinyjs::toggle(id = "instructions_panel", condition = nrow(data) == 0)

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
      data <- analysis_data()
      req(nrow(data) > 0)
      total <- data %>% filter(source == "GBIF") %>% nrow()
      selected <- data %>% filter(source == "GBIF", index %in% selected_points()) %>% nrow()
      
      tagList(
        span(paste0("(", total, " records)"), style = "font-weight: normal; font-size: 0.85em; margin-left: 10px;"),
        if (selected > 0) {
          span(
            paste0("— ", selected, " row", if (selected != 1) "s", " selected"),
            style = "font-weight: normal; font-size: 0.85em; margin-left: 5px;"
          )
        }
      )
    })

    output$row_count_upload <- renderUI({
      data <- analysis_data()
      req(nrow(data) > 0)
      total <- data %>% filter(source == "upload") %>% nrow()
      selected <- data %>% filter(source == "upload", index %in% selected_points()) %>% nrow()
      
      tagList(
        span(paste0("(", total, " records)"), style = "font-weight: normal; font-size: 0.85em; margin-left: 10px;"),
        if (selected > 0) {
          span(
            paste0("— ", selected, " row", if (selected != 1) "s", " selected"),
            style = "font-weight: normal; font-size: 0.85em; margin-left: 5px;"
          )
        }
      )
    })

    # Data management observers --------------------------------------------------
    # NOTE: any change to analysis_data() (delete / undo / new upload / new GBIF
    # pull) also clears the gap-analysis results: gapAnalysisServer observes
    # analysis_data() and resets its map layers, scores and report state.
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