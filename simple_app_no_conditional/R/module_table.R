# TABLE MODULE -------------------------------------------------------------------------
tableModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    reactableOutput(outputId = ns("pointsTable"))
  )
}

tableModuleServer <- function(id, combined_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    output$pointsTable <- renderReactable({
      req(nrow(combined_data()) > 0)
      data <- st_drop_geometry(combined_data())
      selected <- selected_points()

      calc_width <- function(text) {
        max(100, nchar(text) * 12) # Roughly 8px per character + padding
      }

      reactable(
        data = data,
        selection = "multiple",
        defaultSelected = which(data$index %in% selected),
        onClick = "select",
        highlight = TRUE,
        pagination = FALSE,
        groupBy = "source",
        columns = lapply(names(data), function(col) {
          colDef(
            minWidth = calc_width(col)
          )
        }) %>%
          setNames(names(data))
      )
    })

    # Handle table row selection
    observeEvent(
      getReactableState("pointsTable", "selected"),
      {
        table_selected <- getReactableState("pointsTable", "selected")
        print(table_selected)
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
