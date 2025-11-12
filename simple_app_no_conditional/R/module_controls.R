# Controls Module
controlsModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(inputId = ns("loadGBIF"), label = "Load GBIF"),
    br(),
    br(),
    actionButton(inputId = ns("loadupload"), label = "Load Upload"),
    br(),
    br(),
    actionButton(inputId = ns("clearSelection"), label = "Clear Selection"),
    br(),
    br(),
    actionButton(
      inputId = ns("deleteSelection"),
      label = "Delete Selection",
      class = "btn-danger"
    ),
    br()
  )
}

controlsModuleServer <- function(id, combined_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    # Load sample data (for testing)
    gbifPoints <- read_csv(
      "appData/Magnolia_acuminata_data.csv",
      col_types = cols(.default = "c")
    ) %>%
      sf::st_as_sf(
        coords = c("Longitude", "Latitude"),
        crs = 4326,
        remove = FALSE
      ) %>%
      mutate(index = dplyr::row_number())

    uploadPoints <- read_csv(
      "appData/upload_sample.csv",
      col_types = cols(.default = "c")
    ) %>%
      sf::st_as_sf(
        coords = c("Longitude", "Latitude"),
        crs = 4326,
        remove = FALSE
      ) %>%
      mutate(index = dplyr::row_number())

    # Load GBIF data
    observeEvent(input$loadGBIF, {
      current <- combined_data()

      if (nrow(current) == 0) {
        combined_data(gbifPoints)
      } else {
        new_dat <- bind_rows(current, gbifPoints) %>%
          mutate(index = row_number())
        combined_data(new_dat)
      }
    })

    # Load upload data
    observeEvent(input$loadupload, {
      current <- combined_data()

      if (nrow(current) == 0) {
        combined_data(uploadPoints)
      } else {
        new_dat <- bind_rows(current, uploadPoints) %>%
          mutate(index = row_number())
        combined_data(new_dat)
      }
    })

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
