# TABLE MODULE -------------------------------------------------------------------------
tableModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    reactableOutput(outputId = ns("pointsTable"))
  )
}

tableModuleServer <- function(id, analysis_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    output$pointsTable <- renderReactable({
      req(nrow(analysis_data()) > 0)
      data <- st_drop_geometry(analysis_data())
      selected <- selected_points()

      # Workaround to dynamically change width of columns.  Reactable does not have this functionality
      # for some reason.  Currently assumes fixed px/charicter so is pretty rough.
      dynamicColWidths <- function(reachCondLong, otherColDefs = NULL) {
        getColWidths <- function(colName) {
          # Width of column name
          ncharColName <- nchar(colName)

          # Width of data.  Code below handles if NA count in this colName is some, all, or none.
          # max(NA, rm.na =TRUE) throws a warning without this.
          nonNAdata <- reachCondLong %>% pull(colName) %>% nchar() %>% na.omit()

          if (length(nonNAdata) > 0) {
            ncharVal <- nonNAdata %>% max()
          } else if (length(nonNAdata) == 0) {
            ncharVal <- 0
          }

          # When grouping rows, add a few characters for the EvaluationID appended row count (e.g. "XE-SS-5141_2013-08-23 (10)")
          if (colName == "EvaluationID") {
            ncharVal <- ncharVal + 1
          }

          maxLength <- max(ncharColName, ncharVal, na.rm = TRUE) * 12 # 12 = pixels per character (font size?)

          reactable::colDef(minWidth = maxLength)
        }

        # Make list of column widths.  set_names names the list elements properly in purrr:map().
        singleLineColWidths <- purrr::set_names(names(reachCondLong)) %>%
          purrr::map(getColWidths)

        # Merge column width settings and other colDefs.
        allColDefsMerged <- purrr::list_merge(
          singleLineColWidths,
          !!!otherColDefs
        ) # '!!!' splices 2nd list into first

        # Fix attributes.  the S3 class attribute isn't allowed through with list_merge.
        # Each element is assigned the class here manually.  There must be a better way.

        # Function to fix attributes
        setColDefAttr <- function(x, elemName) {
          y <- x[elemName]
          attr(y[[elemName]], "class") <- "colDef"
          return(y)
        }

        # Apply function to all elements.
        allColDefs <- purrr::map(
          names(allColDefsMerged),
          ~ setColDefAttr(allColDefsMerged, .x)
        ) %>%
          purrr::flatten()
      }

      reactable(
        data = data,
        selection = "multiple",
        defaultSelected = which(data$index %in% selected),
        onClick = "select",
        highlight = TRUE,
        pagination = FALSE,
        columns = dynamicColWidths(data)
      )
    })

    # Handle table row selection
    observeEvent(
      getReactableState("pointsTable", "selected"),
      {
        table_selected <- getReactableState("pointsTable", "selected")
        print(table_selected)
        if (length(table_selected) > 0) {
          selected_points(analysis_data()$index[table_selected])
        } else {
          selected_points(numeric(0))
        }
      },
      ignoreNULL = FALSE
    )
  })
}
