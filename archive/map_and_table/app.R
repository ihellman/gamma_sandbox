library(readr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(shiny)
library(reactable)

source("leaflet_maps.R")
ui <- fluidPage(
  actionButton(inputId = "loadGBIF", label = "Load GBIF"),
  actionButton(inputId = "loadupload", label = "Load Upload"),
  leafletOutput("dataEvalMap"),
  reactableOutput(outputId = "pointsTable"),
  actionButton(inputId = "clearSelection", label = "Clear Selection")
)

server <- function(input, output, session) {

# Reactive value to store the combined data
  combined_data <- reactiveVal(data.frame())

# Load app data with buttons - for testing only --------------------
  
# load sample data
gbifPoints <- read_csv("Magnolia_acuminata_data.csv", col_types = cols(.default = "c")) %>%
  sf::st_as_sf(coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE) %>%
  mutate(index = dplyr::row_number())

uploadPoints <- read_csv("upload_sample.csv", col_types = cols(.default = "c")) %>%
  sf::st_as_sf(coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE) %>%
  mutate(index = dplyr::row_number())
  
  # Load base data
  observeEvent(input$loadGBIF, {
    current <- combined_data()
    
    if (nrow(current) == 0) {
      # No data yet, just load base
      combined_data(gbifPoints)
    } else {
      # Merge with existing data
      new_dat <- bind_rows(current, gbifPoints) %>%
        mutate(index = row_number())
      combined_data(new_dat)
    }
  })
  
  # Load secondary data
  observeEvent(input$loadupload, {
    current <- combined_data()
    
    if (nrow(current) == 0) {
      # No data yet, just load secondary
      combined_data(uploadPoints)
    } else {
      # Merge with existing data
      new_dat <- bind_rows(current, uploadPoints) %>%
        mutate(index = row_number())
      combined_data(new_dat)
    }
  })

# MAP ----------------------------------------------------------------
# Initial data evaluation map with just basemaps and general settings.
output$dataEvalMap <- leaflet::renderLeaflet(data_eval_base_map())

# Update map on data load/update and/or if point is selected
observeEvent(c(combined_data(), selected_points()), {
  data_eval_map_gbif_proxy(mapID = "dataEvalMap", 
                           allPoints = combined_data(), 
                           selected = selected_points())
}, ignoreInit = TRUE)

# TABLE ---------------------------------------------------------------
output$pointsTable <- renderReactable({

  req(nrow(combined_data()) > 0)
  data <- st_drop_geometry(combined_data())
  selected <- selected_points()

  reactable(data = data,
            selection = "multiple",
            defaultSelected = which(data$index %in% selected),
            onClick = "select",
            highlight = TRUE,
            pagination = FALSE,  
            groupBy = "source")
})

# SELECTION LOGIC -----------------------------------------------------
# Reactive value to track selected IDs
  selected_points <- reactiveVal(numeric(0))

# Handle marker clicks on map
  observeEvent(input$dataEvalMap_marker_click, {
    click <- input$dataEvalMap_marker_click
    current_selected <- selected_points()
    
    if (click$id %in% current_selected) {
      # Deselect if already selected
      selected_points(setdiff(current_selected, click$id))
    } else {
      # Add to selection
      selected_points(c(current_selected, click$id))
    }
  })
  
    # Handle table row selection
  observeEvent(getReactableState("pointsTable", "selected"), {
    table_selected <- getReactableState("pointsTable", "selected")
    print(table_selected)
    if (length(table_selected) > 0) {
      selected_points(combined_data()$index[table_selected])
    } else {
      # Clear all selections when nothing is selected in table
      selected_points(numeric(0))
    }
  }, ignoreNULL = FALSE)

    # POLYON SELECTION - Select all points within a drawn polygon
    observeEvent(input$dataEvalMap_draw_new_feature, {
      polySelected <- selected_points()
      currentData <- combined_data()

      # Extract polygon feature
      feature <- input$dataEvalMap_draw_new_feature
      coords <- feature$geometry$coordinates[[1]]
      selectionPoly <- st_polygon(list(matrix(unlist(coords), ncol = 2, byrow = TRUE)))

      # Find points within the polygon
      pointsInPoly <- st_filter(currentData, selectionPoly)

      # Get index values for new points  
      newPolySelection <- pointsInPoly %>% pull(index)
      
      # Combine with previously selected and save
      newSelection <- c(selected_points(), newPolySelection) %>% unique()
      selected_points(newSelection)

    })
    
  observeEvent(input$clearSelection, {
    selected_points(numeric(0))
  })

}

shinyApp(ui, server)
