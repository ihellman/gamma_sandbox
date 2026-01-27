# --- Module UI ---
gapAnalysisUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Main Content Layout
    layout_sidebar(
      sidebar = sidebar(
        title = "Controls",

        selectInput(
          inputId = ns("buffer_dist"),
          label = "Buffer Distance (km)",
          choices = c(5, 10, 25, 50, 100),
          selected = 10
        ),

        # UPDATED: Button Label
        actionButton(
          inputId = ns("generate_buffers"), # Keeping ID same for logic, changing label
          label = "Run Gap Analysis",
          icon = icon("play-circle"),
          class = "btn-primary w-100 mb-3"
        )
      ),

      # Map Card
      card(
        card_header("Spatial Distribution"),
        leaflet::leafletOutput(ns("gap_map"), height = "400px"),
        full_screen = TRUE
      ),

      # Tabbed Data & Figures
      bslib::navset_card_underline(
        id = ns("results_tabs"), # Added ID for potential programmatic switching
        title = "Gap Analysis Results",

        # Tab 1: Records (Default view)
        bslib::nav_panel(
          title = "Occurrence Records",
          DT::DTOutput(ns("records_table"))
        ),

        # Tab 2: Metrics Summary
        bslib::nav_panel(
          title = "Metrics Summary",
          DT::DTOutput(ns("metrics_table"))
        ),

        # Tab 3: Visual Figure
        bslib::nav_panel(
          title = "Scores Plot",
          plotOutput(ns("metrics_plot"))
        )
      )
    ),

    # Footer Element (Appended to the bottom of the module UI)
    footer_ui()
  )
}

# --- Module Server ---
gapAnalysisServer <- function(id, analysis_data) {
  moduleServer(id, function(input, output, session) {
    # reactive values
    srs_ex <- shiny::reactiveVal(NULL)
    grs_ex <- shiny::reactiveVal(NULL)
    ers_ex <- shiny::reactiveVal(NULL)

    # 0. Reactive Buffer
    buffer_dist_km <- reactive({
      req(input$buffer_dist)
      val <- as.numeric(input$buffer_dist)
      if (is.na(val) || val <= 0) {
        return(NULL)
      }
      return(val)
    })

    # Helper: Prep Data
    prepLatLon <- function(data) {
      vals <- as.data.frame(data) |>
        dplyr::filter(!is.na(Longitude)) |>
        dplyr::mutate(
          Longitude = as.numeric(Longitude),
          Latitude = as.numeric(Latitude),
        )
      return(vals)
    }

    # 1. Base Map
    output$gap_map <- leaflet::renderLeaflet({
      gap_base_map() %>%
        leaflet::addMapPane("buffers", zIndex = 410) %>%
        leaflet::addMapPane("points", zIndex = 420)
    })

    # 2. Initial Data Load (Map Points & SRS Pre-calculation)
    observe({
      req(analysis_data(), input$gap_map_bounds)
      data <- analysis_data() |> prepLatLon()

      # Calculate SRS immediately
      srsMetrics <- SRSex(taxon = data$`Taxon Name`[1], occurrence_Data = data)
      srs_ex(srsMetrics)

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
            lng = ~Longitude,
            lat = ~Latitude,
            group = "Reference Records",
            radius = 5,
            color = "white",
            fillColor = combinedColor[1],
            fillOpacity = 0.8,
            weight = 1,
            stroke = TRUE,
            label = point_labels(ref_points),
            options = leaflet::pathOptions(pane = "points")
          ) %>%
          leaflet::clearGroup("Germplasm Records") %>%
          leaflet::addCircleMarkers(
            data = germ_points,
            lng = ~Longitude,
            lat = ~Latitude,
            group = "Germplasm Records",
            radius = 5,
            color = "white",
            fillColor = combinedColor[2],
            fillOpacity = 0.8,
            weight = 1,
            stroke = TRUE,
            label = point_labels(germ_points),
            options = leaflet::pathOptions(pane = "points")
          )
      } else {
        leaflet::leafletProxy("gap_map", session) %>%
          leaflet::clearGroup("Reference Records") %>%
          leaflet::clearGroup("Germplasm Records")
      }
    })

    # 3. Reactive Dataframe for Summary Table/Plot
    gap_scores_df <- reactive({
      req(srs_ex(), grs_ex(), ers_ex())

      # Extract Values using correct column names from your functions
      srs_data <- srs_ex()
      srs_val <- if ("SRS exsitu" %in% names(srs_data)) {
        srs_data[["SRS exsitu"]]
      } else {
        NA
      }

      grs_data <- grs_ex()
      grs_val <- if ("GRS exsitu" %in% names(grs_data)) {
        grs_data[["GRS exsitu"]]
      } else {
        NA
      }

      ers_list <- ers_ex()
      ers_val <- if (
        !is.null(ers_list$summary) && "ERS exsitu" %in% names(ers_list$summary)
      ) {
        ers_list$summary[["ERS exsitu"]]
      } else {
        NA
      }

      # Create Summary DF with UPDATED NAMES
      dplyr::tibble(
        Metric = c(
          "SRS sampling score",
          "GRS geographic score",
          "ERS ecological score"
        ),
        Score = as.numeric(c(srs_val, grs_val, ers_val)),
        # Short codes for color mapping if needed, or just use the full string
        Type = c("SRS", "GRS", "ERS")
      ) |>
        dplyr::mutate(
          Score = round(Score, 1)
        )
    })

    # 4. Render Tables & Plot

    # Tab 1: Records
    output$records_table <- DT::renderDT({
      req(analysis_data())
      DT::datatable(
        analysis_data(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })

    # Tab 2: Metrics Summary
    output$metrics_table <- DT::renderDT({
      req(gap_scores_df())

      df <- gap_scores_df()
      fcs <- mean(df$Score, na.rm = TRUE)

      # Bind FCS row
      df_display <- df |>
        dplyr::select(Metric, Score) |>
        dplyr::bind_rows(dplyr::tibble(
          Metric = "FCS (Final Conservation Score)",
          Score = round(fcs, 1)
        ))

      DT::datatable(
        df_display,
        options = list(dom = 't', ordering = FALSE),
        rownames = FALSE,
        class = "cell-border stripe"
      )
    })

    # Tab 3: Plot
    output$metrics_plot <- renderPlot({
      req(gap_scores_df())

      df <- gap_scores_df()

      # Use the "Type" column for color mapping to match your theme
      # SRS = combinedColor[1], GRS = grsexColor, ERS = ersexColors[2]

      ggplot2::ggplot(df, ggplot2::aes(x = Metric, y = Score, fill = Type)) +
        ggplot2::geom_col(width = 0.5) +
        ggplot2::geom_text(
          ggplot2::aes(label = Score),
          vjust = -0.5,
          size = 6,
          fontface = "bold"
        ) +
        ggplot2::scale_y_continuous(limits = c(0, 110), expand = c(0, 0)) +
        ggplot2::scale_fill_manual(
          values = c(
            "SRS" = combinedColor[1],
            "GRS" = grsexColor,
            "ERS" = ersexColors[2]
          )
        ) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::labs(
          y = "Score (0-100)",
          x = "",
          title = "Conservation Gap Analysis"
        ) +
        ggplot2::theme(
          legend.position = "none",
          panel.grid.major.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(size = 12, face = "bold"),
          plot.title = ggplot2::element_text(hjust = 0.5, size = 18)
        )
    })

    # 5. Run Gap Analysis Action
    observeEvent(input$generate_buffers, {
      data <- analysis_data()
      req(is.data.frame(data) && nrow(data) > 0)
      dist_km <- buffer_dist_km()
      req(dist_km)

      target_col <- if ("Current Germplasm Type" %in% names(data)) {
        "Current Germplasm Type"
      } else if ("type" %in% names(data)) {
        "type"
      } else {
        NULL
      }

      shiny::withProgress(
        message = "Running Gap Analysis",
        detail = "Initializing...",
        value = 0,
        {
          incProgress(0.1, detail = "Loading Land Data...")
          land <- terra::vect("appData/land_simple.gpkg")

          incProgress(0.2, detail = "Buffering points...")
          df_base <- prepLatLon(data)
          if (!is.null(target_col)) {
            df_base$processing_type <- df_base[[target_col]]
          } else {
            df_base$processing_type <- "All"
          }

          v <- terra::vect(
            df_base,
            geom = c("Longitude", "Latitude"),
            crs = "EPSG:4326"
          )
          v_buffer <- terra::buffer(v, width = dist_km * 1000)

          incProgress(0.3, detail = "Clipping to Land...")
          land_proj <- terra::project(land, terra::crs(v_buffer))
          v_clipped <- terra::intersect(v_buffer, land_proj)

          # --- GRSex Calculation ---
          incProgress(0.4, detail = "Calculating GRSex...")
          gBuff <- v_clipped[v_clipped$processing_type == "G", ]
          hBuff <- v_clipped[v_clipped$processing_type == "H", ]

          if (length(gBuff) > 0) {
            grsMap_element <- terra::erase(x = hBuff, y = gBuff)
          } else {
            grsMap_element <- hBuff
          }

          # Call GRSex
          grsMetrics <- GRSex(
            allBuffers = v_clipped,
            outsideGBuffers = grsMap_element
          )
          grs_ex(grsMetrics)

          # --- ERSex Calculation ---
          incProgress(0.5, detail = "Calculating ERSex...")
          ersMetrics <- ERSex(gapPoints = v, g_buffer = gBuff)
          ers_ex(ersMetrics)

          # --- Prep Visualization Layers ---
          incProgress(0.7, detail = "Preparing visualization...")

          # 1. Dissolve Standard Buffers
          sf_buffers_raw <- sf::st_as_sf(v_clipped)
          sf_buffers <- sf_buffers_raw |>
            sf::st_make_valid() |>
            dplyr::group_by(processing_type) |>
            dplyr::summarize(
              geometry = sf::st_union(geometry),
              .groups = "drop"
            )

          # 2. Convert GRS Gap Area (Aggregated inside Terra to fix topology errors)
          if (length(grsMap_element) > 0) {
            grs_clean <- terra::makeValid(grsMap_element)
            grs_agg <- terra::aggregate(grs_clean)
            sf_grs_gap <- sf::st_as_sf(grs_agg)
          } else {
            sf_grs_gap <- NULL
          }

          # 3. ERS Regions
          if (!is.null(ersMetrics$spatial)) {
            sf_ers_regions <- sf::st_as_sf(ersMetrics$spatial)
          } else {
            sf_ers_regions <- NULL
          }

          # --- Rendering ---
          incProgress(0.9, detail = "Updating Map...")
          proxy <- leaflet::leafletProxy("gap_map", session) %>%
            leaflet::clearGroup("Buffers") %>%
            leaflet::clearGroup("GRS Gap") %>%
            leaflet::clearGroup("ERS Regions")

          if (!is.null(target_col) && nrow(sf_buffers) > 0) {
            pal_type <- leaflet::colorFactor(
              palette = c("H" = combinedColor[2], "G" = combinedColor[1]),
              domain = sf_buffers$processing_type
            )
            fill_cols <- pal_type(sf_buffers$processing_type)
            proxy %>%
              leaflet::addPolygons(
                data = sf_buffers,
                group = "Buffers",
                stroke = TRUE,
                weight = 1,
                color = fill_cols,
                fillColor = fill_cols,
                fillOpacity = 0.4,
                options = leaflet::pathOptions(pane = "buffers"),
                popup = paste("Buffer:", dist_km, "km")
              )
          }

          if (!is.null(sf_grs_gap)) {
            proxy %>%
              leaflet::addPolygons(
                data = sf_grs_gap,
                group = "GRS Gap",
                stroke = TRUE,
                weight = 1,
                color = grsexColor,
                fillColor = grsexColor,
                fillOpacity = 0.4,
                options = leaflet::pathOptions(pane = "buffers"),
                popup = "Geographic Gap"
              )
          }

          if (!is.null(sf_ers_regions)) {
            pal_ers <- leaflet::colorFactor(
              c(ersexColors[1], ersexColors[2]),
              domain = sf_ers_regions$gap_status
            )
            proxy %>%
              leaflet::addPolygons(
                data = sf_ers_regions,
                group = "ERS Regions",
                stroke = TRUE,
                weight = 2,
                color = "black",
                fillColor = ~ pal_ers(gap_status),
                fillOpacity = 0.3,
                options = leaflet::pathOptions(pane = "buffers"),
                popup = ~ paste(
                  "<b>Region:</b>",
                  ECO_NAME,
                  "<br><b>Status:</b>",
                  gap_status
                )
              )
          }

          # Add Layer Control
          proxy %>%
            leaflet::addLayersControl(
              baseGroups = c("Positron", "Satellite"),
              overlayGroups = c(
                "Buffers",
                "GRS Gap",
                "ERS Regions",
                "Reference Records",
                "Germplasm Records"
              ),
              options = leaflet::layersControlOptions(collapsed = FALSE)
            ) %>%
            leaflet::hideGroup("GRS Gap") %>%
            leaflet::hideGroup("ERS Regions")

          # OPTIONAL: Switch focus to Summary tab automatically
          bslib::nav_select(id = "results_tabs", selected = "Metrics Summary")
        }
      )
      showNotification(paste("Gap Analysis Complete"), type = "message")
    })
  })
}
