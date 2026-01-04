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
    ),
    actionButton(inputId = ns("clearSelection"), label = "Clear Selection"),
    # Delete and Undo Buttons (div allows closer vertical stacking)
    div(
      style = "display: inline-block;",
      actionButton(
        inputId = ns("deleteSelection"),
        label = "Delete Selection",
        class = "btn-danger",
        style = "width: 100%; margin-bottom: 2px;"
      ),
      actionButton(
        inputId = ns("undoLastDelete"),
        label = "Undo Last Delete",
        class = "btn-secondary",
        style = "font-size: 11px; padding: 3px 10px; width: 100%;"
      )
    )
  )
}

# CONTROLS SERVER ----------------------------------------------------------------------
controlsModuleServer <- function(id, analysis_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    # Empty reactive for backup of analysis_data() for undo functionality
    analysis_data_backup <- reactiveVal(data.frame())

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

    # Handle file upload
    observeEvent(input$uploadData, {
      req(input$uploadData)

      # Read and validate the file
      upload_result <- read_upload_file(file_info = input$uploadData) # helper function

      # Check if upload was successful
      if (!upload_result$success) {
        showNotification(
          upload_result$message,
          type = "error",
          duration = 10
        )
        return()
      }

      # Upload was successful, get the data
      uploadPoints <- upload_result$data

      # Check if upload data already exists
      current <- analysis_data()
      has_upload <- nrow(current) > 0 && any(current$source == "upload")

      if (has_upload) {
        showModal(modalDialog(
          title = "Overwrite Upload Data?",
          "Upload data is already loaded. Do you want to overwrite it with new data?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              session$ns("confirmloadUpload"),
              "Overwrite",
              class = "btn-primary"
            )
          )
        ))

        # Store the upload data temporarily for the confirm action
        uploadData_temp(uploadPoints)
      } else {
        # No upload data exists, load directly
        if (nrow(current) == 0) {
          uploadPoints <- uploadPoints %>%
            mutate(index = row_number())
          analysis_data(uploadPoints)
        } else {
          new_dat <- bind_rows(current, uploadPoints) %>%
            mutate(index = row_number())
          analysis_data(new_dat)
        }

        showNotification(
          upload_result$message,
          type = "message",
          duration = 3
        )
      }
    })

    # Handle the confirmation for overwriting
    observeEvent(input$confirmloadUpload, {
      current <- analysis_data()
      uploadPoints <- uploadData_temp()

      # Remove existing upload data
      current <- current %>%
        filter(source != "upload")

      # Add new upload data
      if (nrow(current) == 0) {
        uploadPoints <- uploadPoints %>%
          mutate(index = row_number())
        analysis_data(uploadPoints)
      } else {
        new_dat <- bind_rows(current, uploadPoints) %>%
          mutate(index = row_number())
        analysis_data(new_dat)
      }

      # Clear selections that might reference old upload indices
      selected_points(numeric(0))

      removeModal()
      showNotification(
        paste("Successfully uploaded", nrow(uploadPoints), "records"),
        type = "message",
        duration = 3
      )

      # Clear temporary storage
      uploadData_temp(NULL)
    })

    # Clear selection ---------------------------------------------------------------
    observeEvent(input$clearSelection, {
      selected_points(numeric(0))
    })

    # Delete selected points ---------------------------------------------------------
    observeEvent(input$deleteSelection, {
      req(nrow(analysis_data()) > 0)

      # Backup before deletion
      analysis_data_backup(analysis_data())

      current_data <- analysis_data()
      current_selection <- selected_points()

      # Only proceed if there are selected points
      #if (length(current_selection) > 0 && nrow(current_data) > 0) {
      # Remove selected rows
      updated_data <- current_data %>%
        filter(!index %in% current_selection) %>%
        mutate(index = row_number()) # Re-index after deletion

      # Update the data
      analysis_data(updated_data)

      # Clear the selection
      selected_points(numeric(0))
      print(paste0(
        "There are",
        nrow(analysis_data_backup()),
        "rows in the BACKUP data."
      ))
      print(paste0(
        "There are",
        nrow(analysis_data()),
        "rows in the CURRENT data."
      ))
    })

    # Undo last delete -------------------------------------------------------------
    observeEvent(input$undoLastDelete, {
      req(nrow(analysis_data_backup()) > 0)

      # Restore from backup
      analysis_data(analysis_data_backup())

      # Clear the backup
      analysis_data_backup(data.frame())
    })
  })
}
