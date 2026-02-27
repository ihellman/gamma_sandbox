# CONTROLS UI ----------------------------------------------------------------------
controlsModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    accordion(
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
            inputId = ns("taxon_genus"),
            label = "Genus",
            choices = NULL,
            width = "100%"
          ),
          selectizeInput(
            inputId = ns("taxon_species"),
            label = "Specific Epithet",
            choices = NULL,
            width = "100%"
          ),
          selectizeInput(
            inputId = ns("taxon_rank"),
            label = "Taxon Rank",
            choices = NULL,
            width = "100%"
          ),
          selectizeInput(
            inputId = ns("taxon_infra"),
            label = "Infraspecific Epithet",
            choices = NULL,
            width = "100%"
          ),
          hr(style = "margin: 1.5rem 0;"),
          sliderInput(
            inputId = ns("gbif_limit"),
            label = "Max Occurrences",
            min = 0,
            max = 500,
            value = 200,
            step = 50,
            width = "100%"
          ),
          radioButtons(
            inputId = ns("backfill_strategy"),
            label = "Wild Record Prioritization",
            choices = c("Most Recent" = "recent", "Random Selection" = "random"),
            selected = "recent",
            inline = TRUE
          ),
          hr(style = "margin: 1rem 0;"),
          uiOutput(outputId = ns("taxon_id_display"), class = "mb-3 text-center fw-bold"),
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
            "Upload your own specimen data in CSV format."
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
            accept = c(".csv", "text/csv"),
            buttonLabel = tagList(icon("folder-open"), "Browse"),
            placeholder = "No file selected",
            width = "100%"
          ),
          hr(style = "margin: 1.5rem 0;"),
          div(
            class = "d-grid",
            downloadButton(
              outputId = ns("exportData"),
              label = "Export Analysis Data",
              icon = icon("file-arrow-down"),
              class = "btn-outline-secondary",
              style = "font-weight: 500;"
            )
          ),
          p(
            class = "text-muted small mt-2 mb-0",
            "Download the current working dataset including all loaded records."
          )
        )
      )
    )
  )
}

# CONTROLS SERVER ----------------------------------------------------------------------
controlsModuleServer <- function(id, analysis_data, selected_points) {
  moduleServer(id, function(input, output, session) {

    # 1. Initialize Parquet Dataset ----------------------------------------------------
    taxonomy_ds <- arrow::open_dataset("appData/plant_taxonomy_lean.parquet")

    # 2. Populate Genus ----------------------------------------------------------------
    observe({
      genera <- taxonomy_ds %>%
        select(genericName) %>%
        distinct() %>%
        collect() %>%
        filter(!is.na(genericName)) %>%
        arrange(genericName) %>%
        pull(genericName)
      
      updateSelectizeInput(session, "taxon_genus", choices = c("Select Genus" = "", genera), server = TRUE)
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
      
      updateSelectizeInput(session, "taxon_species", choices = c("Select Epithet" = "", species_epithets), server = TRUE)
      updateSelectizeInput(session, "taxon_rank", choices = character(0))
      updateSelectizeInput(session, "taxon_infra", choices = character(0))
    })

    # 4. Populate Taxon Rank -----------------------------------------------------------
    observeEvent(input$taxon_species, {
      req(input$taxon_genus, input$taxon_species)
      ranks <- taxonomy_ds %>%
        filter(genericName == input$taxon_genus, specificEpithet == input$taxon_species) %>%
        select(taxonRank) %>%
        distinct() %>%
        collect() %>%
        filter(!is.na(taxonRank)) %>%
        arrange(taxonRank) %>%
        pull(taxonRank)
      
      updateSelectizeInput(session, "taxon_rank", choices = c("Select Rank" = "", ranks), server = TRUE)
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
        mutate(infraspecificEpithet = tidyr::replace_na(infraspecificEpithet, "")) %>%
        arrange(infraspecificEpithet) %>%
        pull(infraspecificEpithet)
      
      if (length(infra_opts) == 1 && infra_opts == "") {
        updateSelectizeInput(session, "taxon_infra", choices = c("N/A" = ""), selected = "")
      } else {
        updateSelectizeInput(session, "taxon_infra", choices = c("Select Infra" = "", infra_opts), server = TRUE)
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
        query <- query %>% filter(is.na(infraspecificEpithet) | infraspecificEpithet == "")
      }
      
      res <- query %>% select(taxonID, taxonomicStatus) %>% collect()
      if (nrow(res) == 0) return(NULL)
      
      accepted_res <- res %>% filter(toupper(taxonomicStatus) == "ACCEPTED")
      if (nrow(accepted_res) > 0) return(accepted_res$taxonID[1])
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

    # 7. Gather and Format Live GBIF Data -----------------------------------------------
    gbifData_temp <- reactiveVal(NULL) 
    
    observeEvent(input$loadGBIF, {
      tid <- selected_taxon_id()
      req(tid)
      
      Gather_limit <- if (is.numeric(input$gbif_limit)) input$gbif_limit else 200
      
      shiny::withProgress(
        message = "GBIF API Search",
        detail = "Initializing...",
        value = 0,
        {
          shiny::incProgress(0.2, detail = "Prioritizing living specimens...")
          raw_living <- rgbif::occ_search(
            taxonKey = as.numeric(tid), 
            hasCoordinate = TRUE, 
            basisOfRecord = "LIVING_SPECIMEN",
            limit = Gather_limit
          )
          
          living_data <- if (is.null(raw_living$data)) data.frame() else raw_living$data
          
          remaining_limit <- Gather_limit - nrow(living_data)
          other_data <- data.frame()
          
          if (remaining_limit > 0) {
            pool_size <- min(remaining_limit * 5, 2000)
            
            shiny::incProgress(0.4, detail = paste("Gathering reference records", remaining_limit, "remaining..."))
            raw_all <- rgbif::occ_search(
              taxonKey = as.numeric(tid), 
              hasCoordinate = TRUE, 
              limit = pool_size
            )
            
            if (!is.null(raw_all$data)) {
              other_data <- raw_all$data
              
              if (nrow(living_data) > 0) {
                other_data <- other_data %>% dplyr::filter(!gbifID %in% living_data$gbifID)
              }
              
              if (nrow(other_data) > remaining_limit) {
                if (input$backfill_strategy == "recent") {
                  other_data <- other_data %>% dplyr::arrange(desc(eventDate)) %>% head(remaining_limit)
                } else if (input$backfill_strategy == "random") {
                  other_data <- other_data %>% dplyr::slice_sample(n = remaining_limit)
                }
              }
            }
          }
          
          shiny::incProgress(0.6, detail = "Formatting downloaded records...")
          combined_df <- dplyr::bind_rows(living_data, other_data)
          
          if (nrow(combined_df) == 0) {
            showNotification("No records with coordinates found on GBIF for this taxon.", type = "warning")
            return()
          }
          
          safe_extract <- function(col_name) { 
            if (col_name %in% names(combined_df)) combined_df[[col_name]] else NA_character_ 
          }
          
          formatted_gbif <- data.frame(
            `Accession Number` = as.character(combined_df$gbifID),
            `Taxon Name` = combined_df$scientificName,
            `Current Germplasm Type` = ifelse(safe_extract("basisOfRecord") == "LIVING_SPECIMEN", "G", "H"),
            `Collection Date` = as.character(safe_extract("eventDate")),
            Latitude = as.numeric(combined_df$decimalLatitude),
            Longitude = as.numeric(combined_df$decimalLongitude),
            Locality = as.character(safe_extract("stateProvince")),
            Collector = as.character(safe_extract("recordedBy")),
            source = "GBIF",
            check.names = FALSE
          ) %>% mutate(index = row_number())
          
          shiny::incProgress(0.9, detail = "Checking current dataset...")
          
          current <- analysis_data()
          has_gbif <- nrow(current) > 0 && any(current$source == "GBIF")
          
          if (has_gbif) {
            gbifData_temp(formatted_gbif)
            showModal(modalDialog(
              title = "Overwrite GBIF Data?",
              "GBIF data is already loaded. Do you want to overwrite it with this new taxon?",
              footer = tagList(
                modalButton("Cancel"),
                actionButton(session$ns("confirmLoadGBIF"), "Overwrite", class = "btn-primary")
              )
            ))
          } else {
            updated_df <- merge_and_index(current, formatted_gbif)
            analysis_data(updated_df)
            showNotification(paste("Successfully loaded", nrow(formatted_gbif), "records."), type = "message")
          }
        }
      )
    })

    # 8. Confirm GBIF Overwrite Logic --------------------------------------------------
    observeEvent(input$confirmLoadGBIF, {
      req(gbifData_temp()) 
      current <- analysis_data()
      
      filtered <- current %>% filter(source != "GBIF")
      updated_df <- merge_and_index(filtered, gbifData_temp()) 
      
      analysis_data(updated_df)
      gbifData_temp(NULL) 
      selected_points(numeric(0)) 
      
      removeModal()
      showNotification("GBIF data successfully updated.", type = "message")
    }, ignoreInit = TRUE)

    # 9. Upload file logic -------------------------------------------------------------
    uploadData_temp <- reactiveVal(NULL)

    observeEvent(input$uploadData, {
      req(input$uploadData)
      durations <- list(validation = 8, success = 5, warning = 10, system = NULL)
      res <- read_upload_file(input$uploadData)

      if (res$status == "validation_error") {
        showNotification(res$message, type = "warning", duration = durations$validation)
        return()
      }
      if (res$status == "system_error") {
        showNotification(res$message, type = "error", duration = durations$system)
        return()
      }

      new_points <- res$data
      current    <- analysis_data()
      
      if (nrow(current) > 0 && "upload" %in% current$source) {
        uploadData_temp(res) 
        showModal(modalDialog(
          title = "Overwrite Upload Data?",
          "Upload data is already loaded. Do you want to overwrite it?",
          footer = tagList(
            actionButton(session$ns("cancelUpload"), "Cancel"),
            actionButton(session$ns("confirmloadUpload"), "Overwrite", class = "btn-primary")
          )
        ))
      } else {
        updated_df <- merge_and_index(current, new_points)
        analysis_data(updated_df)
        dur <- if (res$message_type == "warning") durations$warning else durations$success
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

      dur <- if(res$message_type == "warning") 10 else 5
      showNotification(res$message, type = res$message_type, duration = dur)
    })

    observeEvent(input$cancelUpload, {
      uploadData_temp(NULL) 
      removeModal()
    })
    
    # 10. Export analysis data ---------------------------------------------------------
    output$exportData <- downloadHandler(
      filename = function() {
        paste0("analysis_data_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
      },
      content = function(file) {
        write.csv(analysis_data(), file, row.names = FALSE)
      }
    )
    
    # 11. Data Format Requirements Modal -----------------------------------------------
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
            tags$li(tags$strong("Latitude"), " - Decimal degrees (required for map display)"),
            tags$li(tags$strong("Longitude"), " - Decimal degrees (required for map display)"),
            tags$li(tags$strong("Locality")),
            tags$li(tags$strong("Collector"))
          ),
          tags$h5("Optional Columns:"),
          tags$ul(
            tags$li(tags$strong("issues"), " - Optional notes or data quality flags")
          ),
          tags$p(
            tags$strong("Important:"),
            tags$ul(
              tags$li("Do not include additional columns beyond those listed above."),
              tags$li("If you do not provide coordinates (Latitude/Longitude), your data will still be included in the analysis but will not appear on the map.")
            )
          ),
          tags$p(
            tags$strong("Example CSV:"),
            tags$a(href = "upload_example.csv", download = "upload_example.csv", "Download example file")
          )
        ),
        footer = modalButton("Close"),
        size = "m"
      ))
    })
  })
}