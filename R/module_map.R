# MAP MODULE -----------------------------------------------------------------------------
mapModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("dataEvalMap"), height = "50vh") # vh = viewport height. 50% doesnt work.
  )
}

mapModuleServer <- function(id, combined_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    # Initial map render
    output$dataEvalMap <- renderLeaflet({
      data_eval_base_map()
    })

    # Update map when data or selection changes
    observe({
      data_eval_map_gbif_proxy(
        mapID = "dataEvalMap",
        allPoints = combined_data(),
        selected = selected_points()
      )
    })

    # Handle marker clicks
    observeEvent(input$dataEvalMap_marker_click, {
      click <- input$dataEvalMap_marker_click
      current_selected <- selected_points()

      if (click$id %in% current_selected) {
        selected_points(setdiff(current_selected, click$id))
      } else {
        selected_points(c(current_selected, click$id))
      }
    })

    # Handle polygon selection
    observeEvent(input$dataEvalMap_draw_new_feature, {
      req(nrow(combined_data()) > 0)
      currentData <- combined_data()

      # Extract polygon feature
      feature <- input$dataEvalMap_draw_new_feature
      coords <- feature$geometry$coordinates[[1]]
      selectionPoly <- st_polygon(list(matrix(
        unlist(coords),
        ncol = 2,
        byrow = TRUE
      )))

      # Find points within the polygon
      pointsInPoly <- st_filter(currentData, selectionPoly)

      # Get index values for new points
      newPolySelection <- pointsInPoly %>% pull(index)

      # Combine with previously selected and save
      newSelection <- c(selected_points(), newPolySelection) %>% unique()
      selected_points(newSelection)
    })
  })
}