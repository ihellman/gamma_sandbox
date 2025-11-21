# CONTROLS MODULE ----------------------------------------------------------------------
controlsModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(inputId = ns("loadGBIF"), label = "Load GBIF"),
    actionButton(inputId = ns("loadUpload"), label = "Load Upload"),
    actionButton(inputId = ns("clearSelection"), label = "Clear Selection"),
    actionButton(
      inputId = ns("deleteSelection"),
      label = "Delete Selection",
      class = "btn-danger"
    )
  )
}

controlsModuleServer <- function(id, combined_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    # Load sample GBIF data (for testing)
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

    # Load sample upload data (for testing)
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
        mutate(index = dplyr::row_number(), source = "upload")
    }

    # Load GBIF data Button
    observeEvent(input$loadGBIF, {
      current <- combined_data()
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
          combined_data(gbifPoints)
        } else {
          new_dat <- bind_rows(current, gbifPoints) %>%
            mutate(index = row_number())
          combined_data(new_dat)
        }
      }
    })

    # Confirm GBIF overwrite
    observeEvent(
      input$confirmLoadGBIF,
      {
        current <- combined_data()
        gbifPoints <- load_gbif_sample()

        # Remove existing GBIF data and add new
        filtered <- current %>% filter(source != "GBIF")

        if (nrow(filtered) == 0) {
          combined_data(gbifPoints)
        } else {
          new_dat <- bind_rows(filtered, gbifPoints) %>%
            mutate(index = row_number())
          combined_data(new_dat)
        }

        removeModal()
      },
      ignoreInit = TRUE
    )

    # Load upload data
    observeEvent(input$loadUpload, {
      current <- combined_data()
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
          combined_data(uploadPoints)
        } else {
          new_dat <- bind_rows(current, uploadPoints) %>%
            mutate(index = row_number())
          combined_data(new_dat)
        }
      }
    })

    # Confirm upload overwrite
    observeEvent(
      input$confirmloadUpload,
      {
        current <- combined_data()
        uploadPoints <- load_upload_sample()

        # Remove existing upload data and add new
        filtered <- current %>% filter(source != "upload")

        if (nrow(filtered) == 0) {
          combined_data(uploadPoints)
        } else {
          new_dat <- bind_rows(filtered, uploadPoints) %>%
            mutate(index = row_number())
          combined_data(new_dat)
        }

        removeModal()
      },
      ignoreInit = TRUE
    )

    # Clear selection
    observeEvent(input$clearSelection, {
      selected_points(numeric(0))
    })

    # Delete selected points
    observeEvent(input$deleteSelection, {
      req(nrow(combined_data()) > 0)
      current_data <- combined_data()
      current_selection <- selected_points()

      # Only proceed if there are selected points
      #if (length(current_selection) > 0 && nrow(current_data) > 0) {
      # Remove selected rows
      updated_data <- current_data %>%
        filter(!index %in% current_selection) %>%
        mutate(index = row_number()) # Re-index after deletion

      # Update the data
      combined_data(updated_data)

      # Clear the selection
      selected_points(numeric(0))
      #}
    })
  })
}