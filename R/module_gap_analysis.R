gapAnalysisUI <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      title = "Controls",

      selectInput(
        inputId = ns("buffer_dist"),
        label = "Buffer Distance (km)",
        choices = c(5, 10, 25, 50, 100),
        selected = 10
      ),

      actionButton(
        inputId = ns("generate_buffers"),
        label = "Generate Buffers",
        icon = icon("layer-group"),
        class = "btn-primary w-100 mb-3"
      ),

      actionButton(
        inputId = ns("refresh"),
        label = "Refresh Data",
        icon = icon("sync"),
        class = "w-100"
      )
    ),

    card(
      card_header("Spatial Distribution"),
      leaflet::leafletOutput(ns("gap_map"), height = "400px"),
      full_screen = TRUE
    ),

    card(
      card_header("Gap Analysis Data"),
      DT::DTOutput(ns("gap_table")),
      full_screen = TRUE
    )
  )
}

# server
gapAnalysisServer <- function(id, combined_data) {
  moduleServer(id, function(input, output, session) {
    # --- 0. Reactive Buffer Distance ---
    buffer_dist_km <- reactive({
      req(input$buffer_dist)
      val <- as.numeric(input$buffer_dist)
      if (is.na(val) || val <= 0) {
        return(NULL)
      }
      return(val)
    })

    # --- 1. Render Base Map with Z-Index Panes ---
    output$gap_map <- leaflet::renderLeaflet({
      # Load the base map and add custom panes for ordering
      gap_base_map() %>%
        # Buffers go on the bottom (above tiles, below points)
        leaflet::addMapPane("buffers", zIndex = 410) %>%
        # Points go on top
        leaflet::addMapPane("points", zIndex = 420)
    })

    # --- 2. Update Map Points (Runs when data changes) ---
    observe({
      data <- combined_data()
      if (is.data.frame(data) && nrow(data) > 0) {
        col_name <- if ("Current Germplasm Type" %in% names(data)) {
          "Current Germplasm Type"
        } else if ("type" %in% names(data)) {
          "type"
        } else {
          return()
        }

        ref_points <- data %>% dplyr::filter(.data[[col_name]] == "H")
        germ_points <- data %>% dplyr::filter(.data[[col_name]] == "G")

        leaflet::leafletProxy("gap_map", session) %>%
          leaflet::clearGroup("Reference Records") %>%
          leaflet::addCircleMarkers(
            data = ref_points,
            group = "Reference Records",
            radius = 5,
            color = "white",
            fillColor = combinedColor[1],
            fillOpacity = 0.8,
            weight = 1,
            stroke = TRUE,
            label = point_labels(ref_points),
            # EXPLICITLY assign to the top pane
            options = leaflet::pathOptions(pane = "points")
          ) %>%
          leaflet::clearGroup("Germplasm Records") %>%
          leaflet::addCircleMarkers(
            data = germ_points,
            group = "Germplasm Records",
            radius = 5,
            color = "white",
            fillColor = combinedColor[2],
            fillOpacity = 0.8,
            weight = 1,
            stroke = TRUE,
            label = point_labels(germ_points),
            # EXPLICITLY assign to the top pane
            options = leaflet::pathOptions(pane = "points")
          )
      } else {
        leaflet::leafletProxy("gap_map", session) %>%
          leaflet::clearGroup("Reference Records") %>%
          leaflet::clearGroup("Germplasm Records")
      }
    })

    # --- Render Table ---
    output$gap_table <- DT::renderDT({
      req(is.data.frame(combined_data()) && nrow(combined_data()) > 0)
      DT::datatable(
        combined_data(),
        options = list(pageLength = 10, scrollX = TRUE, dom = 'Bfrtip'),
        rownames = FALSE,
        class = "display nowrap"
      )
    })

    # --- Generate Buffers Action ---
    observeEvent(input$generate_buffers, {
      data <- combined_data()
      req(is.data.frame(data) && nrow(data) > 0)

      dist_km <- buffer_dist_km()
      req(dist_km)

      shiny::withProgress(
        message = "Generating Buffers",
        detail = "Initializing...",
        value = 0,
        {
          incProgress(0.2, detail = "Calculating geometry...")

          df_base <- as.data.frame(data) |>
            dplyr::filter(!is.na(Longitude)) |>
            dplyr::mutate(
              Longitude = as.numeric(Longitude),
              Latitude = as.numeric(Latitude),
            )

          v <- terra::vect(
            df_base,
            geom = c("Longitude", "Latitude"),
            crs = "EPSG:4326"
          )

          incProgress(0.3, detail = paste("Buffering", dist_km, "km..."))
          v_buffer <- terra::buffer(v, width = dist_km * 1000)
          sf_buffers <- sf::st_as_sf(v_buffer)

          # Column Detection
          col_name <- if ("Current Germplasm Type" %in% names(data)) {
            "Current Germplasm Type"
          } else if ("type" %in% names(data)) {
            "type"
          } else {
            NULL
          }

          incProgress(0.4, detail = "Rendering layers...")
          proxy <- leaflet::leafletProxy("gap_map", session) %>%
            leaflet::clearGroup("Buffers")

          if (!is.null(col_name) && nrow(sf_buffers) > 0) {
            # 1. FLIPPED COLORS in Palette
            # H = Purple (Color[2]), G = Orange (Color[1])
            pal_type <- leaflet::colorFactor(
              palette = c("H" = combinedColor[2], "G" = combinedColor[1]),
              domain = sf_buffers[[col_name]]
            )

            fill_cols <- pal_type(sf_buffers[[col_name]])

            proxy %>%
              leaflet::addPolygons(
                data = sf_buffers,
                group = "Buffers",
                stroke = TRUE,
                weight = 1,
                color = fill_cols,
                fillColor = fill_cols,
                fillOpacity = 0.2,
                popup = paste("Buffer Distance:", dist_km, "km"),
                # EXPLICITLY assign to the lower pane
                options = leaflet::pathOptions(pane = "buffers")
              )
          } else if (nrow(sf_buffers) > 0) {
            proxy %>%
              leaflet::addPolygons(
                data = sf_buffers,
                group = "Buffers",
                stroke = TRUE,
                color = "gray",
                weight = 1,
                fillOpacity = 0.2,
                options = leaflet::pathOptions(pane = "buffers")
              )
          }
        }
      )
      showNotification(
        paste("Generated", dist_km, "km buffers."),
        type = "message"
      )
    })
  })
}
