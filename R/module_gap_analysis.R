# --- Module UI ---
gapAnalysisUI <- function(id) {
  ns <- NS(id)

  tagList(
    layout_sidebar(
      fillable = TRUE,
      class = "p-0", # Removes padding so the map is flush with the edges
      sidebar = sidebar(
        title = "Controls",
        open = "always",
        selectInput(
          inputId = ns("buffer_dist"),
          label = "Buffer Distance (km)",
          choices = c(5, 10, 25, 50, 100),
          selected = 10
        ),
        actionButton(
          inputId = ns("generate_buffers"),
          label = "Run Gap Analysis",
          icon = icon("play-circle"),
          class = "btn-primary w-100 mb-3"
        ),
        # Inactive Download Button
        actionButton(
          inputId = ns("download_report"),
          label = "Download Report",
          icon = icon("download"),
          style = "background-color: white; color: #a9a9a9; border: 1px solid #ddd; width: 100%; cursor: not-allowed;",
          disabled = "disabled" # HTML attribute to fully disable click events
        )
      ),
      
      # Main area: Full screen map container with inset plot
      div(
        style = "position: relative; width: 100%; height: 100%; min-height: 80vh;",
        
        # Base Map
        leaflet::leafletOutput(ns("gap_map"), width = "100%", height = "100%"),
        
        # Floating Inset Plot (Hidden by default until analysis runs)
        shinyjs::hidden(
          div(
            id = ns("plot_inset"),
            style = "position: absolute; bottom: 30px; right: 60px; z-index: 1000; 
                     background: white; padding: 15px; border-radius: 8px; 
                     box-shadow: 0 4px 12px rgba(0,0,0,0.2); width: 380px; height: 180px;",
            plotOutput(ns("metrics_plot"), width = "100%", height = "100%")
          )
        )
      )
    )
  )
}

# --- Module Server ---
gapAnalysisServer <- function(id, analysis_data) {
  moduleServer(id, function(input, output, session) {
    srs_ex <- shiny::reactiveVal(NULL)
    grs_ex <- shiny::reactiveVal(NULL)
    ers_ex <- shiny::reactiveVal(NULL)
    
    # State tracking variables
    analysis_active <- shiny::reactiveVal(FALSE)
    pending_warning <- shiny::reactiveVal(FALSE)
    
    # Watch for changes in the underlying data
    observeEvent(analysis_data(), {
      req(analysis_active()) 
      
      analysis_active(FALSE)
      
      leaflet::leafletProxy("gap_map", session) %>%
        leaflet::clearGroup("Buffers") %>%
        leaflet::clearGroup("GRS Gap") %>%
        leaflet::clearGroup("ERS Regions") %>%
        leaflet::removeControl("gap_legend")
      
      shinyjs::hide("plot_inset")
      
      srs_ex(NULL)
      grs_ex(NULL)
      ers_ex(NULL)
      
      shinyjs::removeClass(id = "generate_buffers", class = "btn-primary")
      shinyjs::addClass(id = "generate_buffers", class = "btn-warning")
      
      showModal(modalDialog(
        title = "Dataset Modified",
        "The underlying dataset has been changed. Previous gap analysis results have been cleared from the map to prevent inaccuracies.",
        br(), br(),
        "Please click ", strong("Run Gap Analysis"), " again to calculate metrics for the new data.",
        easyClose = TRUE,
        footer = modalButton("Understood"),
        size = "m"
      ))
    }, ignoreInit = TRUE)
    

    buffer_dist_km <- reactive({
      req(input$buffer_dist)
      val <- as.numeric(input$buffer_dist)
      if (is.na(val) || val <= 0) {
        return(NULL)
      }
      return(val)
    })

    # --- Robust Helper Function ---
    prepLatLon <- function(data) {
      if (is.null(data) || !is.data.frame(data)) return(data.frame())
      if (!all(c("Longitude", "Latitude") %in% names(data))) return(data.frame())
      if (nrow(data) == 0) return(data.frame())

      vals <- as.data.frame(data) |>
        dplyr::filter(!is.na(Longitude)) |>
        dplyr::mutate(
          Longitude = as.numeric(Longitude),
          Latitude = as.numeric(Latitude),
        )
      return(vals)
    }

    output$gap_map <- leaflet::renderLeaflet({
      gap_base_map()
    })

    # --- Safe Observer for Base Points ---
    observe({
      req(analysis_data(), input$gap_map_bounds)

      data <- prepLatLon(analysis_data())
      req(nrow(data) > 0)

      if (!is.null(data$`Taxon Name`)) {
        srsMetrics <- SRSex(
          taxon = data$`Taxon Name`[1],
          occurrence_Data = data
        )
        srs_ex(srsMetrics)
      }

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
    })

    # Reactive Dataframe for Inset Plot
    gap_scores_df <- reactive({
      req(srs_ex(), grs_ex(), ers_ex())

      srs_data <- srs_ex()
      srs_val <- if ("SRS exsitu" %in% names(srs_data)) srs_data[["SRS exsitu"]] else NA

      grs_data <- grs_ex()
      grs_val <- if ("GRS exsitu" %in% names(grs_data)) grs_data[["GRS exsitu"]] else NA

      ers_list <- ers_ex()
      ers_val <- if (!is.null(ers_list$summary) && "ERS exsitu" %in% names(ers_list$summary)) {
        ers_list$summary[["ERS exsitu"]]
      } else NA
      
      # Calculate Final Conservation Score
      fcs_val <- mean(c(srs_val, grs_val, ers_val), na.rm = TRUE)

      dplyr::tibble(
        # Convert to factor so ggplot maintains the exact order on the X-axis
        Metric = factor(c("SRS", "GRS", "ERS", "FCS"), levels = c("SRS", "GRS", "ERS", "FCS")),
        Score = as.numeric(c(srs_val, grs_val, ers_val, fcs_val)),
        Type = c("SRS", "GRS", "ERS", "FCS")
      ) |>
        dplyr::mutate(Score = round(Score, 1))
    })

    # Render Inset Plot
    output$metrics_plot <- renderPlot({
      req(gap_scores_df())
      df <- gap_scores_df()
      ggplot2::ggplot(df, ggplot2::aes(x = Metric, y = Score, fill = Type)) +
        ggplot2::geom_col(width = 0.6) +
        ggplot2::geom_text(
          ggplot2::aes(label = Score),
          vjust = -0.5,
          size = 5,
          fontface = "bold",
          color = "#2c3e50"
        ) +
        ggplot2::scale_y_continuous(limits = c(0, 115), expand = c(0, 0)) +
        ggplot2::scale_fill_manual(
          values = c(
            "SRS" = combinedColor[1],
            "GRS" = grsexColor,
            "ERS" = ersexColors[2],
            "FCS" = "#2c3e50" # A distinct, bold navy blue for the final score
          )
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(
          y = "Score (0-100)",
          x = "",
          title = "Conservation Gap Scores"
        ) +
        ggplot2::theme(
          legend.position = "none",
          panel.grid.major.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(size = 11, face = "bold"),
          plot.title = ggplot2::element_text(hjust = 0.5, size = 15, face = "bold", color = "#2c3e50")
        )
    })

    # 5. Run Gap Analysis
    observeEvent(input$generate_buffers, {
      data <- prepLatLon(analysis_data())
      req(nrow(data) > 0)

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
          df_base <- data
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

          incProgress(0.4, detail = "Calculating GRSex...")
          gBuff <- v_clipped[v_clipped$processing_type == "G", ]
          hBuff <- v_clipped[v_clipped$processing_type == "H", ]

          if (length(gBuff) > 0) {
            grsMap_element <- terra::erase(x = hBuff, y = gBuff)
          } else {
            grsMap_element <- hBuff
          }
          grsMetrics <- GRSex(
            allBuffers = v_clipped,
            outsideGBuffers = grsMap_element
          )
          grs_ex(grsMetrics)

          incProgress(0.5, detail = "Calculating ERSex...")
          ersMetrics <- ERSex(gapPoints = v, g_buffer = gBuff)
          ers_ex(ersMetrics)

          incProgress(0.7, detail = "Preparing visualization...")
          sf_buffers_raw <- sf::st_as_sf(v_clipped)
          sf_buffers <- sf_buffers_raw |>
            sf::st_make_valid() |>
            dplyr::group_by(processing_type) |>
            dplyr::summarize(
              geometry = sf::st_union(geometry),
              .groups = "drop"
            )

          if (length(grsMap_element) > 0) {
            grs_clean <- terra::makeValid(grsMap_element)
            grs_agg <- terra::aggregate(grs_clean)
            sf_grs_gap <- sf::st_as_sf(grs_agg)
          } else {
            sf_grs_gap <- NULL
          }

          if (!is.null(ersMetrics$spatial)) {
            sf_ers_regions <- sf::st_as_sf(ersMetrics$spatial)
          } else {
            sf_ers_regions <- NULL
          }

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

          proxy %>%
            leaflet::showGroup(c("Buffers", "GRS Gap", "ERS Regions"))
          
          # Dynamic Map Legend
          legend_colors <- c()
          legend_labels <- c()
          
          if (!is.null(sf_grs_gap)) {
            legend_colors <- c(legend_colors, grsexColor)
            legend_labels <- c(legend_labels, "Geographic Gap (GRS)")
          }
          
          if (!is.null(sf_ers_regions)) {
            ers_statuses <- sort(unique(sf_ers_regions$gap_status))
            legend_colors <- c(legend_colors, pal_ers(ers_statuses))
            legend_labels <- c(legend_labels, paste("ERS:", ers_statuses))
          }
          
          if (length(legend_labels) > 0) {
            proxy %>%
              leaflet::addLegend(
                layerId = "gap_legend", 
                position = "bottomleft",
                colors = legend_colors,
                labels = legend_labels,
                title = "Gap Elements",
                opacity = 0.7
              )
          } else {
            proxy %>% leaflet::removeControl("gap_legend")
          }
        }
      )
      
      # Reveal the inset plot and reset UI elements
      shinyjs::show("plot_inset")
      analysis_active(TRUE)
      shinyjs::removeClass(id = "generate_buffers", class = "btn-warning")
      shinyjs::addClass(id = "generate_buffers", class = "btn-primary")
      
      showNotification(paste("Gap Analysis Complete"), type = "message")
    })
  })
}