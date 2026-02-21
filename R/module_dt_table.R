# DT_TABLE MODULE ---------------------------------------------------------------
DT_tableModuleUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Force Font Awesome dependency load (Fixes missing icons)
    tags$div(style = "display: none;", icon("search")),

    # Custom CSS for the Link and Hover Effect
    tags$head(
      tags$style(HTML(
        "
        /* Base Link Style */
        a.dt-link {
          color: #333333;       /* Dark Grey Text */
          text-decoration: none; /* No underline */
          font-weight: bold;
          transition: all 0.2s ease-in-out; /* Smooth transition */
          display: inline-flex;  
          align-items: center;
        }

        /* Icon Style */
        a.dt-link i {
          color: #007bff;       /* Blue Icon */
          margin-right: 8px;
          transition: all 0.2s ease-in-out;
        }

        /* --- HOVER STATES --- */
        
        /* When hovering over the link, change text color */
        a.dt-link:hover {
          color: #007bff; 
        }

        /* When hovering over the link, make the icon 'pop' (grow larger) */
        a.dt-link:hover i {
          transform: scale(1.3); 
          color: #0056b3; /* Darker blue */
        }
        
        /* Prevent column header wrapping */
        table.dataTable thead th {
          white-space: nowrap !important;
        }
      "
      ))
    ),
    
    # fill = TRUE allows the widget to expand to the bslib card size (helps frozen headers)
    DTOutput(outputId = ns("DT_pointsTable"), fill = TRUE)
  )
}

DT_tableModuleServer <- function(
  id,
  analysis_data,
  selected_points,
  data_source = "NULL") {
  moduleServer(id, function(input, output, session) {
    # 1. Helper Reactive
    table_data <- reactive({
      data <- analysis_data()
      if (is.null(data) || nrow(data) == 0 && ncol(data) == 0) {
        return(data.frame())
      }

      data <- data %>%
        mutate(
          germplasm_color = case_when(
            source == 'GBIF' & `Current Germplasm Type` == 'H' ~ '#a6dba0',
            source == 'GBIF' & `Current Germplasm Type` == 'G' ~ '#008837',
            source == 'upload' & `Current Germplasm Type` == 'H' ~ '#c2a5cf',
            source == 'upload' & `Current Germplasm Type` == 'G' ~ '#7b3294',
            TRUE ~ 'transparent' 
          )
        )
      data %>% filter(source == data_source)
    })

    # 2. Initialization Latch
    init_trigger <- reactiveVal(FALSE)

    observe({
      data <- table_data()
      if (ncol(data) > 0 && !init_trigger()) {
        init_trigger(TRUE)
      }
    })

    # 3. Render the Table (RUNS ONCE)
    output$DT_pointsTable <- renderDT({
      req(init_trigger())

      data <- isolate(table_data())
      current_selection <- isolate(selected_points())
      initial_rows <- which(data$index %in% current_selection)

      col_names <- names(data)
      accession_col_index <- which(col_names == "Accession Number") - 1
      source_col_index <- which(col_names == "source") - 1

      js_renderer <- JS(
        sprintf(
          "function(data, type, rowData, meta) {
             var accessionNumber = data;
             var source = rowData[%d]; 
             var base_url = 'https://www.gbif.org/occurrence/';

             if (source === 'GBIF') {
               var link = base_url + accessionNumber;
               return '<a href=\"' + link + '\" target=\"_blank\" class=\"dt-link\">' +
                      '<i class=\"fa fa-search\"></i>' +
                      accessionNumber + '</a>';
             } else {
               return accessionNumber;
             }
           }",
          source_col_index
        )
      )

      datatable(
        data,
        rownames = FALSE,
        selection = list(mode = 'multiple', selected = initial_rows),
        filter = 'none',
        fillContainer = TRUE, 
        options = list(
          dom = 't',       
          paging = FALSE,  
          scrollX = TRUE,  
          deferRender = TRUE,
          autoWidth = FALSE,
          columnDefs = list(
            list(targets = c('source', 'index', 'germplasm_color'), visible = FALSE),
            list(targets = '_all', className = 'dt-left', createdCell = JS("function(td, cellData, rowData, row, col) { if(cellData != null) $(td).attr('title', cellData); }")),
            list(targets = accession_col_index, render = js_renderer)
          )
        ),
        class = 'cell-border stripe hover compact'
      ) %>%
        formatStyle(
          columns = 1:ncol(data),
          `min-width` = 'fit-content',
          `max-width` = '300px',
          `white-space` = 'nowrap',
          `overflow` = 'hidden',
          `text-overflow` = 'ellipsis',
          fontSize = '13px'
        ) %>%
        formatStyle(
          'Current Germplasm Type',
          valueColumns = 'germplasm_color', 
          backgroundColor = styleEqual(
            c('#a6dba0', '#008837', '#c2a5cf', '#7b3294'), 
            c('#a6dba066', '#00883766', '#c2a5cf66', '#7b329466') 
          ),
          fontWeight = 'bold'
        ) %>%
        formatRound(c('Latitude', 'Longitude'), 4)
    })

    # 4. Proxy
    proxy <- dataTableProxy("DT_pointsTable")

    # 5. Handle Data Updates 
    observeEvent(table_data(), {
      req(init_trigger())
      new_data <- table_data()

      replaceData(proxy, new_data, rownames = FALSE, resetPaging = FALSE, clearSelection = "none")
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