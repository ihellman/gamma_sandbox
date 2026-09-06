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
    # Result of the last run of run_gap_analysis() (NULL until run / after the
    # dataset changes). Holds metrics, sf layers, the analysed points and FCS.
    gap_result <- shiny::reactiveVal(NULL)
    analysis_active <- shiny::reactiveVal(FALSE)

    # Any change to the working dataset invalidates the results (this observer is
    # what the delete / undo / upload / GBIF flows rely on).
    observeEvent(analysis_data(), {
      req(analysis_active())

      analysis_active(FALSE)
      gap_result(NULL)

      leaflet::leafletProxy("gap_map", session) %>%
        leaflet::clearGroup("Buffers") %>%
        leaflet::clearGroup("GRS Gap") %>%
        leaflet::clearGroup("ERS Regions") %>%
        leaflet::removeControl("gap_legend")

      shinyjs::hide("plot_inset")

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
      data <- prep_lat_lon(all_data)
      req(nrow(data) > 0)

      col_name <- "Current Germplasm Type"
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
      req(gap_result())
      gap_result()$scores
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
      dist_km <- buffer_dist_km()
      req(dist_km)

      # The whole computation lives in run_gap_analysis() (R/gap_analysis_functions.R)
      res <- shiny::withProgress(message = "Running Gap Analysis", value = 0, {
        tryCatch(
          run_gap_analysis(all_data, dist_km,
                           progress = function(value, detail) shiny::setProgress(value, detail = detail)),
          error = function(e) e
        )
      })
      if (inherits(res, "error")) {
        showNotification(paste("Gap analysis failed:", conditionMessage(res)), type = "error")
        return()
      }
      gap_result(res)

      proxy <- leaflet::leafletProxy("gap_map", session) %>%
        leaflet::clearGroup("Buffers") %>% leaflet::clearGroup("GRS Gap") %>% leaflet::clearGroup("ERS Regions")

      if (nrow(res$sf_buffers) > 0) {
        pal_type <- leaflet::colorFactor(
          palette = c(combinedColor[1], combinedColor[2]),
          levels = c("H", "G")
        )

        proxy %>% leaflet::addPolygons(
          data = res$sf_buffers,
          group = "Buffers",
          color = ~pal_type(processing_type),
          fillColor = ~pal_type(processing_type),
          fillOpacity = 0.4,
          weight = 1,
          options = leaflet::pathOptions(pane = "buffers"),
          popup = paste("Buffer:", dist_km, "km")
        )
      }

      if (!is.null(res$sf_grs_gap)) {
        proxy %>% leaflet::addPolygons(
          data = res$sf_grs_gap, group = "GRS Gap", color = grsexColor, fillColor = grsexColor,
          fillOpacity = 0.4, weight = 1, options = leaflet::pathOptions(pane = "buffers"), popup = "Geographic Gap"
        )
      }

      if (!is.null(res$sf_ers_regions)) {
        pal_ers <- leaflet::colorFactor(c(ersexColors[1], ersexColors[2]), domain = res$sf_ers_regions$gap_status)
        proxy %>% leaflet::addPolygons(
          data = res$sf_ers_regions, group = "ERS Regions", color = "black", fillColor = ~ pal_ers(gap_status),
          fillOpacity = 0.3, weight = 2, options = leaflet::pathOptions(pane = "buffers"),
          popup = ~ paste("<b>Region:</b>", ECO_NAME, "<br><b>Status:</b>", gap_status)
        )
      }

      proxy %>% leaflet::showGroup(c("Buffers", "GRS Gap", "ERS Regions"))

      shinyjs::show("plot_inset")
      analysis_active(TRUE)
      shinyjs::removeClass(id = "generate_buffers", class = "btn-warning")
      shinyjs::addClass(id = "generate_buffers", class = "btn-primary")

      showNotification(
        sprintf("Gap Analysis Complete: FCS %.1f, %s", res$fcs, res$priority$label),
        type = "message"
      )
    })

    # --------------------------------------------------------------------------
    # Report Generation Logic
    # --------------------------------------------------------------------------
    output$download_report_ui <- renderUI({
      if (analysis_active()) {
        downloadButton(
          outputId = session$ns("download_report"),
          label = "Download HTML Report",
          icon = icon("file-lines"),
          class = "btn-outline-primary w-100",
          style = "font-weight: 500;"
        )
      } else {
        # Render a disabled button visually when inactive
        tags$button(
          id = session$ns("download_report_disabled"),
          class = "btn btn-outline-primary w-100 disabled",
          style = "font-weight: 500;",
          icon("file-lines"), " Download HTML Report"
        )
      }
    })

    output$download_report <- downloadHandler(
      filename = function() {
        res <- gap_result()
        taxon <- if (!is.null(res)) res$taxon else "Taxon"
        paste0(gsub("[^A-Za-z0-9_]+", "_", taxon), "_gap_analysis_", Sys.Date(), ".html")
      },
      content = function(file) {
        res <- gap_result()
        req(res)
        shiny::withProgress(message = "Generating Report", value = 0, {
          shiny::incProgress(0.2, detail = "Preparing environment...")
          shiny::incProgress(0.5, detail = "Rendering document (this may take a moment)...")
          render_gap_report(res, file)
          shiny::incProgress(1.0, detail = "Download ready!")
        })
      }
    )
  })
}

# Render reportTemplate.Rmd for a run_gap_analysis() result. Kept outside the
# module so the report can be produced (and tested) without Shiny. Renders in a
# temp copy so the working directory / intermediate files never touch the app.
render_gap_report <- function(res, output_file, template = "reportTemplate.Rmd") {
  work_dir <- tempfile("gap_report_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  tmp_rmd <- file.path(work_dir, basename(template))
  file.copy(template, tmp_rmd, overwrite = TRUE)

  params <- list(
    taxon = res$taxon,
    points = res$points,          # the rows that were analysed
    bufferDist = res$dist_km,
    srsMetrics = res$srs,
    grsMetrics = res$grs,
    ersMetrics = res$ers,
    fcs = res$fcs,
    priority = res$priority$label,
    priorityColor = res$priority$color,
    sf_buffers = res$sf_buffers,
    sf_grs_gap = res$sf_grs_gap,
    sf_ers_regions = res$sf_ers_regions,
    combinedColor = combinedColor,
    grsexColor = grsexColor,
    ersexColors = ersexColors
  )

  rmarkdown::render(
    tmp_rmd,
    output_file = normalizePath(output_file, mustWork = FALSE),
    params = params,
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
  invisible(output_file)
}
