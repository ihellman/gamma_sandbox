# CONTROLS UI ----------------------------------------------------------------------
controlsModuleUI <- function(id) {
  ns <- NS(id)

  panels <- accordion(
      multiple = FALSE,
      open = FALSE,

      # --- GBIF PANEL ---
      accordion_panel(
        title = "GBIF Data",
        value = "panel_gbif",
        icon = icon("database"),
        tagList(
          p(
            class = "text-muted small mb-3",
            "Select taxonomic criteria to query and download occurrence records directly from GBIF."
          ),
          selectizeInput(
            ns("taxon_genus"),
            "Genus",
            choices = NULL,
            width = "100%"
          ),
          selectizeInput(
            ns("taxon_species"),
            "Specific Epithet",
            choices = NULL,
            width = "100%"
          ),
          selectizeInput(
            ns("taxon_rank"),
            "Taxon Rank",
            choices = NULL,
            width = "100%"
          ),
          selectizeInput(
            ns("taxon_infra"),
            "Infraspecific Epithet",
            choices = NULL,
            width = "100%"
          ),
          hr(style = "margin: 1.5rem 0;"),
          uiOutput(
            outputId = ns("gbif_summary_display"),
            class = "mb-2 text-center"
          ),
          sliderInput(
            ns("gbif_limit"),
            "Max Occurrences",
            min = 0,
            max = 1000,
            value = 200,
            step = 50,
            width = "100%"
          ),
          # Advanced options live in their OWN accordion. A bare accordion_panel()
          # here would register as a sibling of the "GBIF Data" panel and, with
          # multiple = FALSE on the parent, collapse it when opened (issue #62).
          div(
            class = "gbif-advanced",
            accordion(
              id = ns("gbif_advanced_acc"),
              open = FALSE,
              multiple = TRUE,
              accordion_panel(
                # Add tooltip and info icon to the accordion header
                title = bslib::tooltip(
                  tags$span("Advanced options ", icon("circle-info", class = "ms-1 text-muted", style = "font-size: 0.85em;")),
                  "You may notice slower download times with these options selected."
                ),
                value = "panel_gbif_advanced",

                bslib::tooltip(
                  checkboxInput(
                    ns("apply_date_filter"),
                    "Apply date filter",
                    value = FALSE
                  ),
                  "Select or enter a specific date range of interest."
                ),

                conditionalPanel(
                  condition = sprintf("input['%s']", ns("apply_date_filter")),
                  dateRangeInput(
                    ns("gbif_date_range"),
                    "Event Date Range",
                    start = NULL,
                    end = NULL,
                    width = "100%"
                  )
                ),

                bslib::tooltip(
                  checkboxInput(
                    ns("exclude_inat"),
                    "Exclude iNaturalist records",
                    value = FALSE
                  ),
                  "Removes all records originally collected through iNaturalist."
                ),

                bslib::tooltip(
                  checkboxInput(
                    ns("include_synonyms"),
                    "Include taxonomic synonyms",
                    value = FALSE # FALSE means synonyms are excluded by default
                  ),
                  "Toggle to bring \"accepted synonyms\" back into the pull. When unchecked (default), synonyms are removed and you may need to adjust the download size to get the correct number of features."
                ),

                bslib::tooltip(
                  checkboxInput(
                    ns("random_selection"),
                    "Random selection",
                    value = FALSE
                  ),
                  "Selected randomly rather than the default of recent records first."
                )
              )
            )
          ),
          uiOutput(
            outputId = ns("taxon_id_display"),
            class = "mb-3 text-center fw-bold"
          ),
          shinyjs::disabled(
            actionButton(
              inputId = ns("loadGBIF"),
              label = "Gather GBIF Occurrences",
              icon = icon("cloud-arrow-down"),
              class = "btn-primary w-100",
              style = "font-weight: 500;"
            )
          )
        )
      ),

      # --- UPLOAD PANEL ---
      accordion_panel(
        title = "Custom Data",
        value = "panel_upload",
        icon = icon("file-import"),
        tagList(
          p(
            class = "text-muted small mb-2",
            "Upload your own specimen data as a CSV or Excel (.xlsx) file."
          ),
          p(
            class = "mb-3",
            actionLink(
              inputId = ns("viewDataFormat"),
              label = "View format requirements",
              icon = icon("circle-info"),
              style = "font-size: 0.875rem;"
            )
          ),
          fileInput(
            inputId = ns("uploadData"),
            label = NULL,
            accept = c(
              ".csv",
              "text/csv",
              ".xlsx",
              ".xls",
              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
              "application/vnd.ms-excel"
            ),
            buttonLabel = tagList(icon("folder-open"), "Browse"),
            placeholder = "No file selected",
            width = "100%"
          )
        )
      )
    )

  # bslib::accordion(multiple = FALSE) stamps data-bs-parent="#<outer id>" on EVERY
  # descendant .accordion-collapse, including the nested Advanced-options panel,
  # which makes Bootstrap close "GBIF Data" whenever Advanced is opened (#62).
  # The nested accordion is multiple = TRUE, so it needs no parent at all.
  panels <- htmltools::tagQuery(panels)$
    find(".gbif-advanced .accordion-collapse")$
    removeAttrs("data-bs-parent")$
    allTags()

  tagList(
    panels,

    # --- EXPORT SECTION (Moved outside accordion) ---
    div(
      class = "d-grid px-2",
      uiOutput(ns("export_button_ui")) # <-- Dynamically render the button here
    ),
    p(
      class = "text-muted small mt-2 mb-0 text-center",
      "Download the current working dataset including all loaded records."
    )
  )
}

# CONTROLS SERVER ----------------------------------------------------------------------
controlsModuleServer <- function(id, analysis_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    # 1. Initialize Parquet Dataset ----------------------------------------------------
    taxonomy_ds <- arrow::open_dataset("appData/plant_taxonomy_lean.parquet")

    # Date range as c(start, end) Dates, or NULL when the filter is off/incomplete
    active_date_range <- reactive({
      if (!isTRUE(input$apply_date_filter)) return(NULL)
      date_range <- input$gbif_date_range
      if (is.null(date_range) || length(date_range) != 2 || any(is.na(date_range))) return(NULL)
      as.Date(date_range)
    })

    # 2. Populate Genus ----------------------------------------------------------------
    observe({
      genera <- taxonomy_ds %>%
        select(genericName) %>%
        distinct() %>%
        collect() %>%
        filter(!is.na(genericName)) %>%
        arrange(genericName) %>%
        pull(genericName)

      updateSelectizeInput(
        session,
        "taxon_genus",
        choices = c("Select Genus" = "", genera),
        server = TRUE
      )
    })

    # 3. Populate Specific Epithet -----------------------------------------------------
    observeEvent(input$taxon_genus, {
      req(input$taxon_genus)
      species_epithets <- taxonomy_ds %>%
        filter(genericName == input$taxon_genus) %>%
        select(specificEpithet) %>%
        distinct() %>%
        collect() %>%
        filter(!is.na(specificEpithet)) %>%
        arrange(specificEpithet) %>%
        pull(specificEpithet)

      updateSelectizeInput(
        session,
        "taxon_species",
        choices = c("Select Epithet" = "", species_epithets),
        server = TRUE
      )
      updateSelectizeInput(session, "taxon_rank", choices = character(0))
      updateSelectizeInput(session, "taxon_infra", choices = character(0))
    })

    # 4. Populate Taxon Rank -----------------------------------------------------------
    observeEvent(input$taxon_species, {
      req(input$taxon_genus, input$taxon_species)
      ranks <- taxonomy_ds %>%
        filter(
          genericName == input$taxon_genus,
          specificEpithet == input$taxon_species
        ) %>%
        select(taxonRank) %>%
        distinct() %>%
        collect() %>%
        filter(!is.na(taxonRank)) %>%
        arrange(taxonRank) %>%
        pull(taxonRank)

      updateSelectizeInput(
        session,
        "taxon_rank",
        choices = c("Select Rank" = "", ranks),
        server = TRUE
      )
      updateSelectizeInput(session, "taxon_infra", choices = character(0))
    })

    # 5. Populate Infraspecific Epithet ------------------------------------------------
    observeEvent(input$taxon_rank, {
      req(input$taxon_genus, input$taxon_species, input$taxon_rank)
      infra_opts <- taxonomy_ds %>%
        filter(
          genericName == input$taxon_genus,
          specificEpithet == input$taxon_species,
          taxonRank == input$taxon_rank
        ) %>%
        select(infraspecificEpithet) %>%
        distinct() %>%
        collect() %>%
        mutate(
          infraspecificEpithet = tidyr::replace_na(infraspecificEpithet, "")
        ) %>%
        arrange(infraspecificEpithet) %>%
        pull(infraspecificEpithet)

      if (length(infra_opts) == 1 && infra_opts == "") {
        updateSelectizeInput(
          session,
          "taxon_infra",
          choices = c("N/A" = ""),
          selected = ""
        )
      } else {
        updateSelectizeInput(
          session,
          "taxon_infra",
          choices = c("Select Infra" = "", infra_opts),
          server = TRUE
        )
      }
    })

    # Enable/Disable the Gather button based on Taxon ID resolution
    observe({
      shinyjs::toggleState(
        id = "loadGBIF",
        condition = !is.null(selected_taxon_id())
      )
    })

    # 6. Resolve Target Taxon ID -------------------------------------------------------
    selected_taxon_id <- reactive({
      req(input$taxon_genus, input$taxon_species, input$taxon_rank)
      query <- taxonomy_ds %>%
        filter(
          genericName == input$taxon_genus,
          specificEpithet == input$taxon_species,
          taxonRank == input$taxon_rank
        )

      if (isTruthy(input$taxon_infra)) {
        query <- query %>% filter(infraspecificEpithet == input$taxon_infra)
      } else {
        query <- query %>%
          filter(is.na(infraspecificEpithet) | infraspecificEpithet == "")
      }

      res <- query %>% select(taxonID, taxonomicStatus) %>% collect()
      if (nrow(res) == 0) {
        return(NULL)
      }

      accepted_res <- res %>% filter(toupper(taxonomicStatus) == "ACCEPTED")
      if (nrow(accepted_res) > 0) {
        return(accepted_res$taxonID[1])
      }
      return(res$taxonID[1])
    })

    output$taxon_id_display <- renderUI({
      tid <- selected_taxon_id()
      if (!is.null(tid)) {
        tags$span(class = "text-success", paste("Target Taxon ID:", tid))
      } else if (isTruthy(input$taxon_species)) {
        tags$span(class = "text-danger", "No matching Taxon ID found.")
      }
    })

    # GBIF index counts. Debounced so that clicking through the rank / infra
    # selectors does not fire three HTTP requests per keystroke.
    taxon_for_counts <- debounce(selected_taxon_id, 500)
    gbif_observation_counts <- reactive({
      tid <- taxon_for_counts()
      req(tid)
      dr <- active_date_range()
      event_date <- if (!is.null(dr)) paste(format(dr, "%Y-%m-%d"), collapse = ",") else NULL
      gbif_counts(tid, event_date = event_date)
    })

    # Per-step record counts of the most recent successful gather, so the
    # sidebar can show what was actually loaded (issue #61).
    last_gather <- reactiveVal(NULL)
    observeEvent(selected_taxon_id(), last_gather(NULL), ignoreNULL = FALSE)

    output$gbif_summary_display <- renderUI({
      counts <- gbif_observation_counts()
      if (is.na(counts$total)) {
        return(NULL)
      }

      format_count <- function(value) {
        if (is.na(value)) "\u2014" else formatC(value, big.mark = ",", format = "d")
      }

      loaded <- last_gather()
      tags$div(
        class = "text-muted small",
        tags$div(paste("Records with coordinates on GBIF:", format_count(counts$total))),
        tags$div(paste("Living specimens (G) on GBIF, before filters:", format_count(counts$living))),
        tags$div(paste("iNaturalist records:", format_count(counts$inat))),
        if (!is.null(loaded)) {
          tags$div(
            class = "text-success mt-1",
            sprintf("Loaded: %s G / %s H (from %s records after filters)",
                    format_count(loaded$n_g), format_count(loaded$n_h),
                    format_count(loaded$steps[[length(loaded$steps) - 1]]))
          )
        }
      )
    })

    # 7. Gather and Format Live GBIF Data -----------------------------------------------
    # All download / filter / selection logic lives in R/gbif_functions.R.
    gbifData_temp <- reactiveVal(NULL)
    gbif_cache <- reactiveVal(NULL)   # raw pool keyed by taxon + pool size + date range

    observeEvent(input$loadGBIF, {
      tid <- selected_taxon_id()
      req(tid)

      selection_limit <- if (is.numeric(input$gbif_limit)) input$gbif_limit else 200
      date_range <- active_date_range()
      limits <- gbif_pool_limits(selection_limit)
      cache_key <- paste(tid, limits$living, limits$other,
                         paste(format(date_range), collapse = ","), sep = "|")

      cached <- gbif_cache()
      pool <- if (!is.null(cached) && identical(cached$key, cache_key)) cached$pool else NULL

      result <- shiny::withProgress(
        message = "GBIF API Search",
        detail = "Initializing...",
        value = 0,
        {
          tryCatch(
            gbif_gather(
              taxon_key = tid,
              limit = selection_limit,
              include_synonyms = isTRUE(input$include_synonyms),
              exclude_inat = isTRUE(input$exclude_inat),
              date_range = date_range,
              random = isTRUE(input$random_selection),
              pool = pool,
              progress = function(value, detail) shiny::setProgress(value, detail = detail)
            ),
            error = function(e) e
          )
        }
      )

      if (inherits(result, "error")) {
        showNotification(paste("GBIF download failed:", conditionMessage(result)), type = "error")
        return()
      }
      gbif_cache(list(key = cache_key, pool = result$pool))

      if (nrow(result$pool) == 0) {
        showNotification("No records with coordinates found on GBIF for this taxon.", type = "warning")
        return()
      }
      if (nrow(result$data) == 0) {
        showNotification(
          "All retrieved records were removed by the current filters (synonyms, iNaturalist, dates). Try relaxing them or increasing Max Occurrences.",
          type = "warning"
        )
        return()
      }
      last_gather(result)
      formatted_gbif <- result$data

      current <- analysis_data()
      has_gbif <- nrow(current) > 0 && any(current$source == "GBIF")

      if (has_gbif) {
        gbifData_temp(formatted_gbif)
        showModal(modalDialog(
          title = "Overwrite GBIF Data?",
          "GBIF data is already loaded. Do you want to overwrite it with this new taxon?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              session$ns("confirmLoadGBIF"),
              "Overwrite",
              class = "btn-primary"
            )
          )
        ))
      } else {
        updated_df <- merge_and_index(current, formatted_gbif)
        analysis_data(updated_df)
        showNotification(
          sprintf("Successfully loaded %d records (%d G, %d H).", nrow(formatted_gbif), result$n_g, result$n_h),
          type = "message"
        )
      }
    })

    # 8. Confirm GBIF Overwrite Logic --------------------------------------------------
    observeEvent(
      input$confirmLoadGBIF,
      {
        req(gbifData_temp())
        current <- analysis_data()

        filtered <- current %>% filter(source != "GBIF")
        updated_df <- merge_and_index(filtered, gbifData_temp())

        analysis_data(updated_df)
        gbifData_temp(NULL)
        selected_points(numeric(0))

        removeModal()
        showNotification("GBIF data successfully updated.", type = "message")
      },
      ignoreInit = TRUE
    )

    # 9. Upload file logic -------------------------------------------------------------
    uploadData_temp <- reactiveVal(NULL)

    observeEvent(input$uploadData, {
      req(input$uploadData)
      durations <- list(
        validation = 8,
        success = 5,
        warning = 10,
        system = NULL
      )
      res <- read_upload_file(input$uploadData)

      if (res$status == "validation_error") {
        showNotification(
          res$message,
          type = "warning",
          duration = durations$validation
        )
        return()
      }
      if (res$status == "system_error") {
        showNotification(
          res$message,
          type = "error",
          duration = durations$system
        )
        return()
      }

      new_points <- res$data
      current <- analysis_data()

      if (nrow(current) > 0 && "upload" %in% current$source) {
        uploadData_temp(res)
        showModal(modalDialog(
          title = "Overwrite Upload Data?",
          "Upload data is already loaded. Do you want to overwrite it?",
          footer = tagList(
            actionButton(session$ns("cancelUpload"), "Cancel"),
            actionButton(
              session$ns("confirmloadUpload"),
              "Overwrite",
              class = "btn-primary"
            )
          )
        ))
      } else {
        updated_df <- merge_and_index(current, new_points)
        analysis_data(updated_df)
        dur <- if (res$message_type == "warning") {
          durations$warning
        } else {
          durations$success
        }
        showNotification(res$message, type = res$message_type, duration = dur)
      }
    })

    observeEvent(input$confirmloadUpload, {
      req(uploadData_temp())
      res <- uploadData_temp()
      current <- analysis_data()
      new_points <- res$data

      current_clean <- current %>% filter(source != "upload")
      updated_df <- merge_and_index(current_clean, new_points)
      analysis_data(updated_df)

      selected_points(numeric(0))
      uploadData_temp(NULL)
      removeModal()

      dur <- if (res$message_type == "warning") 10 else 5
      showNotification(res$message, type = res$message_type, duration = dur)
    })

    observeEvent(input$cancelUpload, {
      uploadData_temp(NULL)
      removeModal()
    })

    # 10. Dynamic Export Button UI -----------------------------------------------------
    output$export_button_ui <- renderUI({
      current_data <- analysis_data()

      # Check if we have valid data
      has_data <- isTruthy(current_data) &&
        is.data.frame(current_data) &&
        nrow(current_data) > 0

      if (has_data) {
        # If data exists, render the actual download button
        downloadButton(
          outputId = session$ns("exportData"),
          label = "Export Analysis Data",
          icon = icon("file-arrow-down"),
          class = "btn-outline-secondary",
          style = "font-weight: 500;"
        )
      } else {
        # If no data, render a completely disabled action button that looks identical
        shinyjs::disabled(
          actionButton(
            inputId = session$ns("dummy_export"),
            label = "Export Analysis Data",
            icon = icon("file-arrow-down"),
            class = "btn-outline-secondary",
            style = "font-weight: 500;"
          )
        )
      }
    })

    # 11. Export analysis data Handler -------------------------------------------------
    output$exportData <- downloadHandler(
      filename = function() {
        paste0("analysis_data_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
      },
      content = function(file) {
        write.csv(analysis_data(), file, row.names = FALSE)
      }
    )

    # 12. Data Format Requirements Modal -----------------------------------------------
    observeEvent(input$viewDataFormat, {
      showModal(modalDialog(
        title = "Data Format Requirements",
        tagList(
          tags$h5("Required Columns:"),
          tags$ul(
            tags$li(tags$strong("Accession Number")),
            tags$li(tags$strong("Taxon Name")),
            tags$li(tags$strong("Current Germplasm Type")),
            tags$li(tags$strong("Collection Date")),
            tags$li(
              tags$strong("Latitude"),
              " - Decimal degrees (required for map display)"
            ),
            tags$li(
              tags$strong("Longitude"),
              " - Decimal degrees (required for map display)"
            ),
            tags$li(tags$strong("Locality")),
            tags$li(tags$strong("Collector"))
          ),
          tags$h5("Optional Columns:"),
          tags$ul(
            tags$li(
              tags$strong("issues"),
              " - Optional notes or data quality flags"
            )
          ),
          tags$p(
            tags$strong("Important:"),
            tags$ul(
              tags$li(
                "Do not include additional columns beyond those listed above."
              ),
              tags$li(
                "If you do not provide coordinates (Latitude/Longitude), your data will still be included in the analysis but will not appear on the map."
              )
            )
          ),
          tags$p(
            tags$strong("Example CSV:"),
            tags$a(
              href = "upload_example.csv",
              download = "upload_example.csv",
              "Download example file"
            )
          )
        ),
        footer = modalButton("Close"),
        size = "m"
      ))
    })
  })
}
