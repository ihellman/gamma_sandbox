# Icons, colors, shapes, labels -----------------------------------------------------------
# Color Palettes --- H , G 
# Data Eval map - Input data
uploadColor <-  c("#dfc27d", "#a6611a")
gbifColor <- c("#80cdc1", "#018571")
# GAP Map
combinedColor <- c("#f1a340","#542788") 
grsexColor <- c("#ef8a62")
ersexColor <- c("#d1e5f0")

# define shapes for legend elements 
## from https://stackoverflow.com/questions/52812238/custom-legend-with-r-leaflet-circles-and-squares-in-same-plot-legends
make_shapes <- function(colors, sizes, borders, shapes) {
  shapes <- gsub("circle", "50%", shapes)
  shapes <- gsub("square", "0%", shapes)
  paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
}
  
gbif_legend_shape <- make_shapes(gbifColor, sizes = 20, borders = "white", shapes = "circle")
upload_legend_shape <- make_shapes(uploadColor, sizes = 20, borders = "white", shapes = "circle")

# Fx to generate labels on map render rather than stored inn dataframe
point_labels <- function(data){
  popup <- paste0("<strong>", as.character(data$`Taxon Name`),"</strong>", # needs to be text
                "<br/><strong> Type: </strong>", data$`Current Germplasm Type`,
                "<br/><b>Collector Name:</b> ", data$Collector,
                "<br/><b>Locality Description):</b> ", data$Locality) %>%
    lapply(htmltools::HTML)
}


# Maps ---------------------------------------------------------------------------
# Initial data evaluation map with just basemaps and general settings.



data_eval_base_map <- function() {
  
  leaflet::leaflet(options = leafletOptions(minZoom = 3, maxZoom = 16)) |>
    setView(lng = "-97.511993", lat = "40.023401", zoom = 4) |>
    addProviderTiles("OpenStreetMap", group = "OpenStreetMap") |>
    addProviderTiles("Esri.WorldTopoMap", group = "Topography") |>
    addProviderTiles("Esri.WorldImagery", group = "Imagery") |>  # layer control groups should not be set at the map proxy level as they will overwrite the existing element.
    addLayersControl(
      position = "topleft",
      overlayGroups = c("Upload",
                        "Upload Selection",
                        "GBIF",
                        "GBIF Selection"),
      baseGroups = c("OpenStreetMap",
                     "Topography",
                     "Imagery"),
      options = layersControlOptions(collapsed = TRUE)
    )|>
    addLegend(
      position = "topright",
      colors = gbif_legend_shape,
      labels = c("GBIF Reference", "GBIF Germplasm"),
      group = "GBIF"
    )|>
    addLegend(
      position = "topright",
      colors = upload_legend_shape,
      labels = c("Upload Reference", "Upload Germplasm"),
      group = "Upload"
    )|>
    addLegend(
      position = "topright",
      colors = "red",
      labels = c("Selected Point")
    ) |>
    addDrawToolbar(
      singleFeature = TRUE,
      toolbar = toolbarOptions(
        actions = list(title = "Cancel", text = "Cancel"),
        finish = list(title = "Done", text = "Done"),
        undo = list(title = "Delete last vertex", text = "Undo"),
        buttons = list(polygon      = "Select by polygon",
                       rectangle    = "Select by rectangle")
      ),
      handlers =  handlersOptions(
        polygon = list(tooltipStart  = "Start drawing.  Click first point to complete shape")
      ),
      editOptions = editToolbarOptions(edit = F),
      polylineOptions = F, rectangleOptions = T, circleOptions = F,
      polygonOptions = T, markerOptions = F, circleMarkerOptions = F)
}


data_eval_map_gbif_proxy <- function(mapID, allPoints, selected){
  
  # IMPORTANT: Check for empty data FIRST, before any operations
  if (nrow(allPoints) == 0) {
    leafletProxy(mapID) %>%
      clearMarkers()
    return(invisible(NULL))
  }
  
  # Define colors within table
  data <- allPoints %>%
    dplyr::mutate(
        color = case_when(
          index %in% selected ~ "red",
          `Current Germplasm Type` == "H" & source == "GBIF" ~ gbifColor[1],
          `Current Germplasm Type` == "G" & source == "GBIF" ~ gbifColor[2],
          `Current Germplasm Type` == "H" & source == "upload" ~ uploadColor[1],
          `Current Germplasm Type` == "G" & source == "upload" ~ uploadColor[2],
        ))
  
  leafletProxy(mapID) |>
      #setView(lng = mean(as.numeric(data$Longitude)), lat = mean(as.numeric(data$Latitude)), zoom = 6) |>
      #clearGroup("GBIF") |>
      clearMarkers() |>
      addCircleMarkers(
        data = data,
        layerId = ~index,
        group = "GBIF",
        radius = 5,
        color = "white",
        fillColor = ~color,
        stroke = TRUE,
        weight = 1,
        fillOpacity = 1,
        label = point_labels(data)
      )
}

