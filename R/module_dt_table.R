# DT_TABLE MODULE ---------------------------------------------------------------
DT_tableModuleUI <- function(id) {
  ns <- NS(id)
  DTOutput(outputId = ns("DT_pointsTable"))
}

DT_tableModuleServer <- function(id, combined_data, selected_points, data_source = "NULL") {
  moduleServer(id, function(input, output, session) {

    # 1. Helper Reactive
    table_data <- reactive({
      # Standardize the data retrieval
      # We check for existence, but we let the filter handle the 0-row case
      # so that column names are preserved (crucial for replaceData).
      data <- combined_data()
      
      # If data is completely missing/null (app start), return empty frame
      if (is.null(data) || nrow(data) == 0 && ncol(data) == 0) {
        return(data.frame())
      }
      
      st_drop_geometry(data) %>%
        filter(source == data_source)
    })

    # 2. Initialization Latch
    # We only want renderDT to run ONE time when data first arrives.
    init_trigger <- reactiveVal(FALSE)

    observe({
      data <- table_data()
      # If we have valid columns (even if 0 rows), and haven't initialized, go.
      if (ncol(data) > 0 && !init_trigger()) {
        init_trigger(TRUE)
      }
    })

    # 3. Render the Table (RUNS ONCE)
    output$DT_pointsTable <- renderDT({
      req(init_trigger())
      
      # ISOLATE everything to prevent re-rendering loops
      data <- isolate(table_data())
      current_selection <- isolate(selected_points())
      initial_rows <- which(data$index %in% current_selection)

      datatable(
        data,
        rownames = FALSE,
        selection = list(mode = 'multiple', selected = initial_rows),
        filter = 'none',
        options = list(
          dom = 't',
          paging = FALSE,
          autoWidth = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          columnDefs = list(
            list(
              targets = '_all',
              className = 'dt-left',
              createdCell = JS(
                "function(td, cellData, rowData, row, col) {",
                "  if(cellData != null) $(td).attr('title', cellData);",
                "}"
              )
            )
          )
        ),
        class = 'cell-border stripe hover compact'
      ) %>%
        formatStyle(
          columns = 1:ncol(data),
          `max-width` = '300px',
          `white-space` = 'nowrap',
          `overflow` = 'hidden',
          `text-overflow` = 'ellipsis',
          fontSize = '13px'
        ) %>%
        formatStyle(
          'Current Germplasm Type',
          backgroundColor = styleEqual(c('G', 'H'), c('#e3f2fd', '#f1f8e9')),
          fontWeight = 'bold'
        ) %>%
        formatRound(c('Latitude', 'Longitude'), 4)
    })

    # 4. Proxy
    proxy <- dataTableProxy("DT_pointsTable")

    # 5. Handle Data Updates (Deletions)
    observeEvent(table_data(), {
      req(init_trigger())
      new_data <- table_data()
      
      replaceData(
        proxy, 
        new_data, 
        rownames = FALSE, # match rownames=FALSE setting from renderDT
        resetPaging = FALSE, 
        clearSelection = "none"
      )
    })

    # 6. Map -> Table Selection
    observeEvent(selected_points(), {
      req(init_trigger())
      data <- table_data()
      
      if (nrow(data) > 0) {
        rows_to_select <- which(data$index %in% selected_points())
        selectRows(proxy, rows_to_select)
      } else {
        selectRows(proxy, NULL)
      }
    }, ignoreNULL = FALSE)

    # 7. Table -> Map Selection
    observeEvent(input$DT_pointsTable_rows_selected, {
      req(init_trigger())
      data <- table_data()
      
      local_indices <- input$DT_pointsTable_rows_selected
      local_ids <- if (is.null(local_indices)) numeric(0) else data$index[local_indices]

      current_global <- selected_points()
      foreign_ids <- setdiff(current_global, data$index)
      new_global <- sort(unique(c(foreign_ids, local_ids)))

      if (!setequal(new_global, current_global)) {
        selected_points(new_global)
      }
    }, ignoreNULL = FALSE)
  })
}