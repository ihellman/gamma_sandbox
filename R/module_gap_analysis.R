# --- Module UI ---
gapAnalysisUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      fillable = TRUE,
      class = "p-0",
      sidebar = sidebar(
        title = "Controls",
        open = "always",
        selectInput(
          inputId = ns("buffer_dist"),
          label = "Buffer Distance (km)",
          choices = c(1, 5, 10, 25, 50, 100, 250),
          selected = 50
        ),
        actionButton(
          inputId = ns("generate_buffers"),
          label = "Run Gap Analysis",
          icon = icon("play-circle"),
          class = "btn-primary w-100 mb-3"
        ),
        # Dynamic Download Button
        shinycssloaders::withSpinner(
          uiOutput(ns("download_report_ui")),
          type = 8,
          color = "#007bff",
          size = 0.5,
          proxy.height = "40px"
        )
      ),
      
      div(
        style = "position: relative; width: 100%; height: 100%; min-height: 80vh;",
        leaflet::leafletOutput(ns("gap_map"), width = "100%", height = "100%"),
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
    
    # Store the spatial layers for the report to prevent recalculating
    spatial_buffers <- shiny::reactiveVal(NULL)
    spatial_grs_gap <- shiny::reactiveVal(NULL)
    spatial_ers_regions <- shiny::reactiveVal(NULL)
    
    analysis_active <- shiny::reactiveVal(FALSE)
    
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
      spatial_buffers(NULL)
      spatial_grs_gap(NULL)
      spatial_ers_regions(NULL)
      
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
      if (is.na(val) || val <= 0) return(NULL)
      return(val)
    })
    
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
    outputOptions(output, "gap_map", suspendWhenHidden = FALSE)
    
    observe({
      req(analysis_data())
      # Wait until the map is actually visible and rendered (fixes the hidden tab bug)
      req(input$gap_map_zoom)
      
      all_data <- analysis_data()
      req(nrow(all_data) > 0)
      
      # Skip the taxon filter and map all points
      data <- prepLatLon(all_data)
      req(nrow(data) > 0)
      
      col_name <- if ("Current Germplasm Type" %in% names(data)) "Current Germplasm Type" else if ("type" %in% names(data)) "type" else return()
      
      ref_points <- data %>% dplyr::filter(.data[[col_name]] == "H")
      germ_points <- data %>% dplyr::filter(.data[[col_name]] == "G")
      
      proxy <- leaflet::leafletProxy("gap_map", session) %>%
        leaflet::clearGroup("Reference Records") %>%
        leaflet::clearGroup("Germplasm Records")
      
      if (nrow(ref_points) > 0) {
        proxy %>% leaflet::addCircleMarkers(
          data = ref_points, lng = ~Longitude, lat = ~Latitude,
          group = "Reference Records", radius = 5, color = "#4d4d4d",
          fillColor = combinedColor[1], fillOpacity = 0.8, weight = 1, stroke = TRUE,
          label = point_labels(ref_points), options = leaflet::pathOptions(pane = "points")
        )
      }
      
      if (nrow(germ_points) > 0) {
        proxy %>% leaflet::addCircleMarkers(
          data = germ_points, lng = ~Longitude, lat = ~Latitude,
          group = "Germplasm Records", radius = 5, color = "#4d4d4d",
          fillColor = combinedColor[2], fillOpacity = 0.8, weight = 1, stroke = TRUE,
          label = point_labels(germ_points), options = leaflet::pathOptions(pane = "points")
        )
      }
    })
    
    gap_scores_df <- reactive({
      req(srs_ex(), grs_ex(), ers_ex())
      
      srs_val <- if ("SRS exsitu" %in% names(srs_ex())) srs_ex()[["SRS exsitu"]] else NA
      grs_val <- if ("GRS exsitu" %in% names(grs_ex())) grs_ex()[["GRS exsitu"]] else NA
      ers_val <- if (!is.null(ers_ex()$summary) && "ERS exsitu" %in% names(ers_ex()$summary)) ers_ex()$summary[["ERS exsitu"]] else NA
      fcs_val <- mean(c(srs_val, grs_val, ers_val), na.rm = TRUE)
      
      dplyr::tibble(
        Metric = factor(c("SRS", "GRS", "ERS", "FCS"), levels = c("SRS", "GRS", "ERS", "FCS")),
        Score = as.numeric(c(srs_val, grs_val, ers_val, fcs_val)),
        Type = c("SRS", "GRS", "ERS", "FCS")
      ) |> dplyr::mutate(Score = round(Score, 1))
    })
    
    output$metrics_plot <- renderPlot({
      req(gap_scores_df())
      df <- gap_scores_df()
      df$x_label <- factor(paste0(df$Metric, "\n", df$Score), levels = paste0(df$Metric, "\n", df$Score))
      
      ggplot2::ggplot(df, ggplot2::aes(x = x_label, y = Score, fill = Type)) +
        ggplot2::geom_col(width = 0.6) +
        ggplot2::scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100), expand = c(0, 0)) +
        ggplot2::scale_fill_manual(values = c("SRS" = combinedColor[1], "GRS" = grsexColor, "ERS" = ersexColors[2], "FCS" = "#2c3e50")) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(y = "Score (0-100)", x = "", title = "Conservation Gap Scores") +
        ggplot2::theme(
          legend.position = "none",
          panel.grid.major.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
          plot.title = ggplot2::element_text(hjust = 0.5, size = 15, face = "bold", color = "#2c3e50")
        )
    })
    
    observeEvent(input$generate_buffers, {
      all_data <- analysis_data()
      req(nrow(all_data) > 0)
      
      # 1. Grab the first taxon name to use as a universal label
      display_taxon <- all_data$`Taxon Name`[1]
      
      # 2. OVERWRITE the Taxon Name for the entire dataset.
      # This prevents SRSex() from filtering out the uploaded data.
      all_data$`Taxon Name` <- display_taxon
      
      # 3. Prepare coordinates WITHOUT the dplyr::filter
      data <- prepLatLon(all_data)
      req(nrow(data) > 0)
      
      dist_km <- buffer_dist_km()
      
      target_col <- if ("Current Germplasm Type" %in% names(data)) "Current Germplasm Type" else if ("type" %in% names(data)) "type" else NULL
      
      shiny::withProgress(message = "Running Gap Analysis", value = 0, {
        incProgress(0.1, detail = "Calculating SRSex...")
        
        # 4. Pass the homogenized all_data and display_taxon to SRSex
        srsMetrics <- SRSex(
          taxon = display_taxon,
          occurrence_Data = all_data
        )
        srs_ex(srsMetrics)
        
        incProgress(0.2, detail = "Loading Land Data...")
        land <- terra::vect("appData/land_simple.gpkg")
        
        incProgress(0.3, detail = "Buffering points...")
        df_base <- data
        df_base$processing_type <- if (!is.null(target_col)) df_base[[target_col]] else "All"
        
        v <- terra::vect(df_base, geom = c("Longitude", "Latitude"), crs = "EPSG:4326")
        v_buffer <- terra::buffer(v, width = dist_km * 1000)
        
        incProgress(0.3, detail = "Clipping to Land...")
        land_proj <- terra::project(land, terra::crs(v_buffer))
        v_clipped <- terra::intersect(v_buffer, land_proj)
        
        incProgress(0.4, detail = "Calculating GRSex...")
        gBuff <- v_clipped[v_clipped$processing_type == "G", ]
        hBuff <- v_clipped[v_clipped$processing_type == "H", ]
        
        if (length(hBuff) == 0) {
          grsMap_element <- hBuff
        } else if (length(gBuff) == 0) {
          grsMap_element <- hBuff
        } else {
          grsMap_element <- terra::erase(x = hBuff, y = gBuff)
        }
        
        grsMetrics <- GRSex(allBuffers = v_clipped, outsideGBuffers = grsMap_element)
        grs_ex(grsMetrics)
        
        incProgress(0.5, detail = "Calculating ERSex...")
        ersMetrics <- ERSex(gapPoints = v, g_buffer = gBuff)
        ers_ex(ersMetrics)
        
        incProgress(0.7, detail = "Preparing visualization...")
        sf_buffers_raw <- sf::st_as_sf(v_clipped)
        sf_buffers <- sf_buffers_raw |> sf::st_make_valid() |> dplyr::group_by(processing_type) |> dplyr::summarize(geometry = sf::st_union(geometry), .groups = "drop")
        spatial_buffers(sf_buffers) # Save for report
        
        if (length(grsMap_element) > 0) {
          sf_grs_gap <- sf::st_as_sf(terra::aggregate(terra::makeValid(grsMap_element)))
        } else {
          sf_grs_gap <- NULL
        }
        spatial_grs_gap(sf_grs_gap) # Save for report
        
        sf_ers_regions <- if (!is.null(ersMetrics$spatial)) sf::st_as_sf(ersMetrics$spatial) else NULL
        spatial_ers_regions(sf_ers_regions) # Save for report
        
        incProgress(0.9, detail = "Updating Map...")
        proxy <- leaflet::leafletProxy("gap_map", session) %>%
          leaflet::clearGroup("Buffers") %>% leaflet::clearGroup("GRS Gap") %>% leaflet::clearGroup("ERS Regions")
        
        if (!is.null(target_col) && nrow(sf_buffers) > 0) {
          pal_type <- leaflet::colorFactor(
            palette = c(combinedColor[1], combinedColor[2]),
            levels = c("H", "G")
          )
          
          proxy %>% leaflet::addPolygons(
            data = sf_buffers,
            group = "Buffers",
            color = ~pal_type(processing_type),
            fillColor = ~pal_type(processing_type),
            fillOpacity = 0.4,
            weight = 1,
            options = leaflet::pathOptions(pane = "buffers"),
            popup = paste("Buffer:", dist_km, "km")
          )
        }
        
        if (!is.null(sf_grs_gap)) {
          proxy %>% leaflet::addPolygons(
            data = sf_grs_gap, group = "GRS Gap", color = grsexColor, fillColor = grsexColor,
            fillOpacity = 0.4, weight = 1, options = leaflet::pathOptions(pane = "buffers"), popup = "Geographic Gap"
          )
        }
        
        if (!is.null(sf_ers_regions)) {
          pal_ers <- leaflet::colorFactor(c(ersexColors[1], ersexColors[2]), domain = sf_ers_regions$gap_status)
          proxy %>% leaflet::addPolygons(
            data = sf_ers_regions, group = "ERS Regions", color = "black", fillColor = ~ pal_ers(gap_status),
            fillOpacity = 0.3, weight = 2, options = leaflet::pathOptions(pane = "buffers"),
            popup = ~ paste("<b>Region:</b>", ECO_NAME, "<br><b>Status:</b>", gap_status)
          )
        }
        
        proxy %>% leaflet::showGroup(c("Buffers", "GRS Gap", "ERS Regions"))
      })
      
      shinyjs::show("plot_inset")
      analysis_active(TRUE)
      shinyjs::removeClass(id = "generate_buffers", class = "btn-warning")
      shinyjs::addClass(id = "generate_buffers", class = "btn-primary")
      
      showNotification(paste("Gap Analysis Complete"), type = "message")
    })
    
    # --------------------------------------------------------------------------
    # Report Generation Logic
    # --------------------------------------------------------------------------
    output$download_report_ui <- renderUI({
      if (analysis_active()) {
        downloadButton(
          outputId = session$ns("download_report"),
          label = "Download Report",
          icon = icon("file-pdf"),
          class = "btn-outline-primary w-100",
          style = "font-weight: 500;"
        )
      } else {
        # Render a disabled button visually when inactive
        tags$button(
          id = session$ns("download_report_disabled"),
          class = "btn btn-outline-primary w-100 disabled",
          style = "font-weight: 500;",
          icon("file-pdf"), " Download Report"
        )
      }
    })
    
    output$download_report <- downloadHandler(
      filename = function() {
        data <- prepLatLon(analysis_data())
        taxon <- if(nrow(data) > 0 && !is.null(data$`Taxon Name`)) data$`Taxon Name`[1] else "Taxon"
        paste0(gsub(" ", "_", taxon), "_gap_analysis_", Sys.Date(), ".html")
      },
      content = function(file) {
        shiny::withProgress(message = "Generating Report", value = 0, {
          
          shiny::incProgress(0.2, detail = "Preparing environment...")
          
          # Copy the Rmd file to a temporary directory
          tempReport <- file.path(tempdir(), "reportTemplate.Rmd")
          file.copy("reportTemplate.Rmd", tempReport, overwrite = TRUE)
          
          # Set up parameters to pass to Rmd
          params <- list(
            points = prepLatLon(analysis_data()),
            bufferDist = buffer_dist_km(),
            srsMetrics = srs_ex(),
            grsMetrics = grs_ex(),
            ersMetrics = ers_ex(),
            sf_buffers = spatial_buffers(),
            sf_grs_gap = spatial_grs_gap(),
            sf_ers_regions = spatial_ers_regions(),
            combinedColor = combinedColor,
            grsexColor = grsexColor,
            ersexColors = ersexColors
          )
          
          shiny::incProgress(0.5, detail = "Rendering document (this may take a moment)...")
          
          # Render the report
          rmarkdown::render(
            tempReport,
            output_file = file,
            params = params,
            envir = new.env(parent = globalenv())
          )
          
          shiny::incProgress(1.0, detail = "Download ready!")
        })
      }
    )
  })
}