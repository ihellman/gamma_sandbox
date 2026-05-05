# MAP MODULE -----------------------------------------------------------------------------
mapModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("dataEvalMap"), height = "100%", width = "100%")
  )
}

mapModuleServer <- function(id, analysis_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    map_count_control <- function(data) {
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }

      count_type <- function(df, type) {
        sum(df$`Current Germplasm Type` == type, na.rm = TRUE)
      }

      gbif_data <- data %>% filter(source == "GBIF")
      upload_data <- data %>% filter(source == "upload")

      gbif_g <- count_type(gbif_data, "G")
      gbif_h <- count_type(gbif_data, "H")
      upload_g <- count_type(upload_data, "G")
      upload_h <- count_type(upload_data, "H")

      htmltools::HTML(
        sprintf(
          "<div style='background: white; padding: 6px 8px; border-radius: 4px; box-shadow: 0 2px 4px rgba(0,0,0,0.2); font-size: 12px;'>GBIF: %s G, %s H&nbsp;&nbsp;|&nbsp;&nbsp;Upload: %s G, %s H</div>",
          formatC(gbif_g, big.mark = ",", format = "d"),
          formatC(gbif_h, big.mark = ",", format = "d"),
          formatC(upload_g, big.mark = ",", format = "d"),
          formatC(upload_h, big.mark = ",", format = "d")
        )
      )
    }

    # Initial map render
    output$dataEvalMap <- renderLeaflet({
      data_eval_base_map()
    })

    # Handle data handling of GBIF/uploaded data
    observeEvent(analysis_data(), {
      req(analysis_data())

      # Redraw the base points if data is updated
      render_base_points("dataEvalMap", analysis_data())

      count_html <- map_count_control(analysis_data())
      if (!is.null(count_html)) {
        leafletProxy("dataEvalMap") %>%
          leaflet::removeControl("gbif_upload_counts") %>%
          leaflet::addControl(
            html = count_html,
            position = "bottomleft",
            layerId = "gbif_upload_counts"
          )
      }

      # Re-apply highlights (in case we filtered data but kept selection)
      update_selection_highlights(
        "dataEvalMap",
        analysis_data(),
        selected_points()
      )
    })

    # Handle selection changes
    observeEvent(
      selected_points(),
      {
        update_selection_highlights(
          "dataEvalMap",
          analysis_data(),
          selected_points()
        )
      },
      ignoreNULL = FALSE
    ) # ignoreNULL=FALSE ensures highlights clear if selection is empty

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
      req(nrow(analysis_data()) > 0)
      currentData <- analysis_data()

      # Filter only rows with valid coordinates for spatial intersection
      spatialData <- currentData %>%
        mutate(
          Latitude = as.numeric(Latitude),
          Longitude = as.numeric(Longitude)
        ) %>%
        filter(!is.na(Latitude) & !is.na(Longitude)) %>%
        sf::st_as_sf(
          coords = c("Longitude", "Latitude"),
          crs = 4326,
          remove = FALSE
        )

      # Extract polygon feature
      feature <- input$dataEvalMap_draw_new_feature
      coords <- feature$geometry$coordinates[[1]]
      selectionPoly <- st_polygon(list(matrix(
        unlist(coords),
        ncol = 2,
        byrow = TRUE
      )))

      # Find points within the polygon using the filtered spatialData
      pointsInPoly <- st_filter(spatialData, selectionPoly)

      # Get index values for new points
      newPolySelection <- pointsInPoly %>% pull(index)

      # Combine with previously selected and save
      newSelection <- c(selected_points(), newPolySelection) %>% unique()
      selected_points(newSelection)
    })
  })
}
