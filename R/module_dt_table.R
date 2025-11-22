# DT_TABLE MODULE ---------------------------------------------------------------
DT_tableModuleUI <- function(id) {
  ns <- NS(id)
  DTOutput(outputId = ns("DT_pointsTable"))
}

DT_tableModuleServer <- function(id, combined_data, selected_points, data_source = "NULL") {
  moduleServer(id, function(input, output, session) {

    output$DT_pointsTable <- renderDT({
      req(nrow(combined_data()) > 0)
      data <- st_drop_geometry(combined_data()) %>%
        filter(source == data_source)

      datatable(
        data,
        rownames = FALSE,
        filter = 'none',
        options = list(
          dom = 't',
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
          ),
          paging = FALSE,
          autoWidth = FALSE,
          scrollX = TRUE
          #scrollY = "600px"
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
    
    observeEvent(
      input$DT_pointsTable_rows_selected,
      {
        table_selected <- input$DT_pointsTable_rows_selected
        print(input$DT_pointsTable_rows_selected)
        if (length(table_selected) > 0) {
          selected_points(combined_data()$index[table_selected])
        } else {
          selected_points(numeric(0))
        }
      },
      ignoreNULL = FALSE
    )
  })
}