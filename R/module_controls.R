# CONTROLS UI ----------------------------------------------------------------------
controlsModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    accordion(
      multiple = TRUE,
      open = TRUE,
      accordion_panel(
        title = "Download GBIF Data",
        icon = icon("download"),
        actionButton(inputId = ns("loadGBIF"), label = "Load GBIF")
      ),
      accordion_panel(
        title = "Upload Data",
        icon = icon("upload"),
        fileInput(
          inputId = ns("uploadData"),
          label = tagList(
            "Upload a file",
            actionLink(
              inputId = ns("viewDataFormat"),
              label = icon("circle-question"),
              title = "Click to view data format requirements",
              style = "margin-left: 8px; cursor: help;"
            )
          ),
          accept = c(".csv"),
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        ),
        actionButton(inputId = ns("loadUpload"), label = "Load Upload (debug)")
      )
    )
  )
}

# CONTROLS SERVER ----------------------------------------------------------------------
controlsModuleServer <- function(id, analysis_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    # Empty reactive for backup of analysis_data() for undo functionality
    #analysis_data_backup <- reactiveVal(data.frame())

    # Load sample GBIF data (for testing) ----------------------------------------------
    load_gbif_sample <- function() {
      read_csv(
        "appData/Magnolia_acuminata_data.csv",
        col_types = cols(.default = "c")
      ) %>%
        mutate(index = dplyr::row_number(), source = "GBIF")
    }

    # Load sample upload data (for testing) ----------------------------------------------
    load_upload_sample <- function() {
      read_csv(
        "appData/upload_sample.csv",
        col_types = cols(.default = "c")
      ) %>%
        mutate(index = dplyr::row_number(), source = "upload", issues = "") # !!!! Placeholder to allow tables to join
    }

    # Load GBIF data Button --------------------------------------------------------------
    observeEvent(input$loadGBIF, {
      current <- analysis_data()
      gbifPoints <- load_gbif_sample()

      # Check if GBIF data already exists
      has_gbif <- nrow(current) > 0 && any(current$source == "GBIF")

      if (has_gbif) {
        showModal(modalDialog(
          title = "Overwrite GBIF Data?",
          "GBIF data is already loaded. Do you want to overwrite it with new data?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              session$ns("confirmLoadGBIF"), # No automatic namespacing in modals withtin module.
              "Overwrite",
              class = "btn-primary"
            )
          )
        ))
      } else {
        # If no GBIF data exists, load directly, else merge with current
        updated_df <- merge_and_index(current, gbifPoints)
        analysis_data(updated_df)
      }
    })

    # Confirm GBIF overwrite
    observeEvent(
      input$confirmLoadGBIF,
      {
        current <- analysis_data()
        gbifPoints <- load_gbif_sample()

        # Remove existing GBIF data and add new
        filtered <- current %>% filter(source != "GBIF")

        # If no GBIF data exists, load directly, else merge with current
        updated_df <- merge_and_index(filtered, gbifPoints)
        analysis_data(updated_df)

        removeModal()
      },
      ignoreInit = TRUE
    )

    # Load sample upload data ---------------------------------------------------------------------
    observeEvent(input$loadUpload, {
      current <- analysis_data()
      uploadPoints <- load_upload_sample()

      # Check if upload data already exists
      has_upload <- nrow(current) > 0 && any(current$source == "upload")

      if (has_upload) {
        uploadData_temp(uploadPoints)
        showModal(modalDialog(
          title = "Overwrite Upload Data?",
          "Upload data is already loaded. Do you want to overwrite it with new data?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              session$ns("confirmloadUpload"), # No automatic namespacing in modals withtin module.
              "Overwrite",
              class = "btn-primary"
            )
          )
        ))
      } else {
        # If no Upload data exists, load directly, else merge with current
        updated_df <- merge_and_index(current, uploadPoints)
        analysis_data(updated_df)
      }
    })

    # Upload file logic ---------------------------------------------------------------------
    uploadData_temp <- reactiveVal(NULL)

    observeEvent(input$uploadData, {
      req(input$uploadData)

      # --- Config: Notification Durations (seconds) ---
      durations <- list(
        validation = 8,    # User error (e.g wrong columns)
        success    = 5,    # Clean load
        warning    = 10,   # Load with missing coords
        system     = NULL  # Code crash
      )

      # 1. Run logic to upload file and check for issues
      res <- read_upload_file(input$uploadData)

      # 2. Handle Errors (Guard Clauses)
      if (res$status == "validation_error") {
        showNotification(res$message, type = "warning", duration = durations$validation)
        return()
      }
      
      if (res$status == "system_error") {
        showNotification(res$message, type = "error", duration = durations$system)
        return()
      }

      # 3. Handle Success
      # Logic: Check for overwrite conflict -> Merge -> Notify
      new_points <- res$data
      current    <- analysis_data()
      
      # Check if "upload" data already exists
      if (nrow(current) > 0 && "upload" %in% current$source) {
        
        # Conflict: Hold data in temp reactive and ask user
        # Store the ENTIRE 'res' object, not just 'new_points'
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
        
        # No Conflict: Load immediately
        updated_df <- merge_and_index(current, new_points)
        analysis_data(updated_df)
        
        # Determine duration based on whether we have a warning (missing coords)
        dur <- if (res$message_type == "warning") {
          durations$warning
        } else {
          durations$success
        }
        showNotification(res$message, type = res$message_type, duration = dur)
        }
    })

    # Handle confirmation in modal to overwrite data
    observeEvent(input$confirmloadUpload, {
      req(uploadData_temp()) # Ensure we actually have data waiting

      # 1. Retrieve the FULL result object (Data + Messages)
      res <- uploadData_temp() 
      
      current <- analysis_data()
      new_points <- res$data # Extract just the dataframe

      # 2. Filter out OLD upload data
      current_clean <- current %>% filter(source != "upload")

      # 3. Merge NEW upload data
      updated_df <- merge_and_index(current_clean, new_points)
      analysis_data(updated_df)

      # 4. Cleanup
      selected_points(numeric(0))
      uploadData_temp(NULL) 
      removeModal()

      # 5. Show the SPECIFIC message passed from the read function
      # Use the specific message (e.g., "Missing Lat/Lon") and specific type (Warning vs Message)
      
      # Define duration based on type
      dur <- if(res$message_type == "warning") 10 else 5
      
      showNotification(
        res$message,        # "Loaded 50 records. Note: 3 missing coords..."
        type = res$message_type, # "warning" or "message"
        duration = dur
      )
    })

    # Handle Cancel
    observeEvent(input$cancelUpload, {
      uploadData_temp(NULL) # Clear the pending data
      removeModal()
    })
    
    # Data Format Requirements Modal --------------------------------------------------------
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
