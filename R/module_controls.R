# CONTROLS UI ----------------------------------------------------------------------
controlsModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(inputId = ns("loadGBIF"), label = "Load GBIF"),
    actionButton(inputId = ns("loadUpload"), label = "Load Upload"),
    fileInput(
      inputId = ns("uploadData"),
      label = "Upload Data",
      accept = c(".csv", ".xlsx", ".xls"),
      buttonLabel = "Browse...",
      placeholder = "No file selected"
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
        sf::st_as_sf(
          coords = c("Longitude", "Latitude"),
          crs = 4326,
          remove = FALSE
        ) %>%
        mutate(index = dplyr::row_number(), source = "GBIF")
    }

    # Load sample upload data (for testing) ----------------------------------------------
    load_upload_sample <- function() {
      read_csv(
        "appData/upload_sample.csv",
        col_types = cols(.default = "c")
      ) %>%
        sf::st_as_sf(
          coords = c("Longitude", "Latitude"),
          crs = 4326,
          remove = FALSE
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
        # No GBIF data exists, load directly
        if (nrow(current) == 0) {
          analysis_data(gbifPoints)
        } else {
          new_dat <- bind_rows(current, gbifPoints) %>%
            mutate(index = row_number())
          analysis_data(new_dat)
        }
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

        if (nrow(filtered) == 0) {
          analysis_data(gbifPoints)
        } else {
          new_dat <- bind_rows(filtered, gbifPoints) %>%
            mutate(index = row_number())
          analysis_data(new_dat)
        }

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
        # No upload data exists, load directly
        if (nrow(current) == 0) {
          analysis_data(uploadPoints)
        } else {
          new_dat <- bind_rows(current, uploadPoints) %>%
            mutate(index = row_number())
          analysis_data(new_dat)
        }
      }
    })

    # Upload file logic ---------------------------------------------------------------------
    uploadData_temp <- reactiveVal(NULL)

    observeEvent(input$uploadData, {
      req(input$uploadData)

      # 1. Read and Validate
      upload_result <- read_upload_file(file_info = input$uploadData)

      if (!upload_result$success) {
        showNotification(upload_result$message, type = "error", duration = 10)
        return()
      }

      new_points <- upload_result$data
      current <- analysis_data()

      # 2. Check for conflict
      # Check if we have existing data specifically from "upload" source
      has_existing_upload <- nrow(current) > 0 &&
        any(current$source == "upload")

      if (has_existing_upload) {
        # 3a. CONFLICT: Store temp data and ask user
        uploadData_temp(new_points)

        showModal(modalDialog(
          title = "Overwrite Upload Data?",
          "Upload data is already loaded. Do you want to overwrite it?",
          footer = tagList(
            # Use actionButton for Cancel so we can clear temp data explicitly
            actionButton(session$ns("cancelUpload"), "Cancel"),
            actionButton(
              session$ns("confirmloadUpload"),
              "Overwrite",
              class = "btn-primary"
            )
          )
        ))
      } else {
        # 3b. NO CONFLICT: Update immediately
        updated_df <- merge_and_index(current, new_points)
        analysis_data(updated_df)

        showNotification(upload_result$message, type = "message", duration = 3)
      }
    })

    # Handle Confirmation
    observeEvent(input$confirmloadUpload, {
      req(uploadData_temp()) # Ensure we actually have data waiting

      current <- analysis_data()
      new_points <- uploadData_temp()

      # Filter out OLD upload data
      current_clean <- current %>% filter(source != "upload")

      # Merge NEW upload data using the helper function
      updated_df <- merge_and_index(current_clean, new_points)
      analysis_data(updated_df)

      # Cleanup
      selected_points(numeric(0))
      uploadData_temp(NULL) # Clear memory
      removeModal()

      showNotification(
        paste("Successfully uploaded", nrow(new_points), "records"),
        type = "message",
        duration = 3
      )
    })

    # Handle Cancel
    observeEvent(input$cancelUpload, {
      uploadData_temp(NULL) # Clear the pending data
      removeModal()
    })
  })
}
