library(readr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(shiny)
library(reactable)

source("leaflet_maps.R")

# MAP MODULE -----------------------------------------------------------------

mapModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("dataEvalMap"))
  )
}

mapModuleServer <- function(id, combined_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    
    # Initial map render
    output$dataEvalMap <- renderLeaflet(data_eval_base_map())
    
    # Update map when data or selection changes
    observeEvent(c(combined_data(), selected_points()), {
      data_eval_map_gbif_proxy(mapID = "dataEvalMap", 
                               allPoints = combined_data(), 
                               selected = selected_points())
    }, ignoreInit = TRUE)
    
    # Handle marker clicks
    observeEvent(input$dataEvalMap_marker_click, {
      click <- input$dataEvalMap_marker_click
      current_selected <- selected_points()
      
      if (click$id %in% current_selected) {
        selected_points(setdiff(current_selected, click$id))
      } else {
        selected_points(c(current_selected, click$id))
      }
    })
    
    # Handle polygon selection
    observeEvent(input$dataEvalMap_draw_new_feature, {
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
  })
}


# TABLE MODULE --------------------------------------------------------------

tableModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    reactableOutput(outputId = ns("pointsTable"))
  )
}

tableModuleServer <- function(id, combined_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    
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
    
    # Handle table row selection
    observeEvent(getReactableState("pointsTable", "selected"), {
      table_selected <- getReactableState("pointsTable", "selected")
      print(table_selected)
      if (length(table_selected) > 0) {
        selected_points(combined_data()$index[table_selected])
      } else {
        selected_points(numeric(0))
      }
    }, ignoreNULL = FALSE)
  })
}


# PAGE 1 MODULE -----------------------------------------------------------
page1ModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Data Evaluation Page"),
    actionButton(inputId = ns("loadGBIF"), label = "Load GBIF"),
    actionButton(inputId = ns("loadupload"), label = "Load Upload"),
    actionButton(inputId = ns("clearSelection"), label = "Clear Selection"),
    actionButton(inputId = ns("deleteSelection"), label = "Delete Selection", class = "btn-danger"),
    hr(),
    mapModuleUI(ns("map")),
    hr(),
    tableModuleUI(ns("table"))
  )
}

page1ModuleServer <- function(id, combined_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    
    # Load sample data (for testing)
    gbifPoints <- read_csv("Magnolia_acuminata_data.csv", col_types = cols(.default = "c")) %>%
      sf::st_as_sf(coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE) %>%
      mutate(index = dplyr::row_number())
    
    uploadPoints <- read_csv("upload_sample.csv", col_types = cols(.default = "c")) %>%
      sf::st_as_sf(coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE) %>%
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
      current_data <- combined_data()
      current_selection <- selected_points()
      
      # Only proceed if there are selected points
      if (length(current_selection) > 0 && nrow(current_data) > 0) {
        # Remove selected rows
        updated_data <- current_data %>%
          filter(!index %in% current_selection) %>%
          mutate(index = row_number())  # Re-index after deletion
        
        # Update the data
        combined_data(updated_data)
        
        # Clear the selection
        selected_points(numeric(0))
      }
    })
    
    # Initialize nested modules
    mapModuleServer("map", combined_data, selected_points)
    tableModuleServer("table", combined_data, selected_points)
  })
}


# MAIN APP ---------------------------------------------------------------

ui <- navbarPage(
  title = "Modular Data App",
  tabPanel("Data Evaluation", page1ModuleUI("page1"))
)

server <- function(input, output, session) {
  # Shared reactive values accessible by all pages
  combined_data <- reactiveVal(data.frame())
  selected_points <- reactiveVal(numeric(0))
  
  # Initialize Page 1 module with shared data
  page1ModuleServer("page1", combined_data, selected_points)
}

shinyApp(ui, server)