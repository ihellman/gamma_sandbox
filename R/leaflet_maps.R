# Icons, colors, shapes, labels -----------------------------------------------------------

# --- Color Palettes ---
# Data Eval map - Input data
uploadColor <- c("#c2a5cf", "#7b3294") #(GBIF H, GBIF G)
gbifColor <- c("#a6dba0", "#008837") #(upload H, upload G)

# GAP Map
combinedColor <- c("#f1a340", "#542788")
grsexColor <- c("#ef8a62")
ersexColor <- c("#d1e5f0")

# --- Legend Shapes Helper ---
# Creates CSS strings for custom legend markers
make_shapes <- function(colors, sizes, borders, shapes) {
  shapes <- gsub("circle", "50%", shapes)
  shapes <- gsub("square", "0%", shapes)
  paste0(
    colors,
    "; width:",
    sizes,
    "px; height:",
    sizes,
    "px; border:3px solid ",
    borders,
    "; border-radius:",
    shapes
  )
}

# Define Legend Icons
gbif_legend_shape <- make_shapes(
  gbifColor,
  sizes = 20,
  borders = "white",
  shapes = "circle"
)
upload_legend_shape <- make_shapes(
  uploadColor,
  sizes = 20,
  borders = "white",
  shapes = "circle"
)
# might need to complicate this a bit for the different shapes
gap_legend_shape <- make_shapes(
  uploadColor,
  sizes = 20,
  borders = "white",
  shapes = "circle"
)

# --- Label Generator ---
# Generates HTML labels on render
point_labels <- function(data) {
  paste0(
    "<strong>",
    as.character(data$`Taxon Name`),
    "</strong>",
    "<br/><strong> Type: </strong>",
    data$`Current Germplasm Type`,
    "<br/><b>Collector Name:</b> ",
    data$Collector,
    "<br/><b>Locality Description:</b> ",
    data$Locality
  ) %>%
    lapply(htmltools::HTML)
}

# Maps ---------------------------------------------------------------------------

# 1. Initial Map Setup
# Renders the empty basemap with controls and legends
data_eval_base_map <- function() {
  leaflet::leaflet(options = leafletOptions(minZoom = 3, maxZoom = 16)) |>
    setView(lng = "-97.511993", lat = "40.023401", zoom = 4) |>
    # Base Layers
    addProviderTiles("OpenStreetMap", group = "OpenStreetMap") |>
    addProviderTiles("Esri.WorldTopoMap", group = "Topography") |>
    addProviderTiles("Esri.WorldImagery", group = "Imagery") |>
    # Controls
    addLayersControl(
      position = "topleft",
      overlayGroups = c("Upload", "Upload Selection", "GBIF", "GBIF Selection"),
      baseGroups = c("OpenStreetMap", "Topography", "Imagery"),
      options = layersControlOptions(collapsed = TRUE)
    ) |>
    # Legends
    addLegend(
      position = "topright",
      colors = gbif_legend_shape,
      labels = c("GBIF Reference", "GBIF Germplasm"),
      group = "GBIF"
    ) |>
    addLegend(
      position = "topright",
      colors = upload_legend_shape,
      labels = c("Upload Reference", "Upload Germplasm"),
      group = "Upload"
    ) |>
    # addLegend(
    #   position = "topright",
    #   colors = "red",
    #   labels = c("Selected Point")
    # ) |>
    # Draw Toolbar
    addDrawToolbar(
      singleFeature = TRUE,
      toolbar = toolbarOptions(
        actions = list(title = "Cancel", text = "Cancel"),
        finish = list(title = "Done", text = "Done"),
        undo = list(title = "Delete last vertex", text = "Undo"),
        buttons = list(
          polygon = "Select by polygon",
          rectangle = "Select by rectangle"
        )
      ),
      handlers = handlersOptions(
        polygon = list(
          tooltipStart = "Start drawing. Click first point to complete shape"
        )
      ),
      editOptions = editToolbarOptions(edit = FALSE),
      polylineOptions = FALSE,
      rectangleOptions = TRUE,
      circleOptions = FALSE,
      polygonOptions = TRUE,
      markerOptions = FALSE,
      circleMarkerOptions = FALSE
    )
}

# 2. Render Main Points
# Logic to color and render the base dataset (Runs when data loads)
render_base_points <- function(mapID, allPoints) {
  # Safety check
  if (nrow(allPoints) == 0) {
    leafletProxy(mapID) %>% clearMarkers()
    return(invisible(NULL))
  }

  # Assign Colors
  data <- allPoints %>%
    dplyr::mutate(
      color = case_when(
        `Current Germplasm Type` == "H" & source == "GBIF" ~ gbifColor[1],
        `Current Germplasm Type` == "G" & source == "GBIF" ~ gbifColor[2],
        `Current Germplasm Type` == "H" & source == "upload" ~ uploadColor[1],
        `Current Germplasm Type` == "G" & source == "upload" ~ uploadColor[2],
        TRUE ~ "gray"
      )
    )

  # Draw Points
  leafletProxy(mapID) |>
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
  
  if (nrow(allPoints) > 0) {
    leafletProxy(mapID) |>
      fitBounds(
          lng1 = min(allPoints$Longitude, na.rm = TRUE),
          lat1 = min(allPoints$Latitude, na.rm = TRUE),
          lng2 = max(allPoints$Longitude, na.rm = TRUE),
          lat2 = max(allPoints$Latitude, na.rm = TRUE)
        )
  }
}

# 3. Update Selection
# Efficiently highlights specific points without re-rendering the whole map
update_selection_highlights <- function(mapID, allPoints, selected_ids) {
  proxy <- leafletProxy(mapID)

  # Clear previous highlights
  proxy |> clearGroup("GBIF Selection")

  if (length(selected_ids) == 0) {
    return(invisible(NULL))
  }

  # Filter data
  selected_data <- allPoints %>%
    dplyr::filter(index %in% selected_ids)

  if (nrow(selected_data) == 0) {
    return(invisible(NULL))
  }

  # Add Halo Effect for Selection
  proxy |>
    addCircleMarkers(
      data = selected_data,
      group = "GBIF Selection",
      radius = 12,
      color = "transparent",
      fillColor = "#025c8f",
      fillOpacity = 0.3,
      stroke = FALSE,
      options = pathOptions(interactive = FALSE) # Clicks pass through to base point
    )
}


# Gap Analysis Map Setup
# Renders the empty basemap with controls and legends
gap_base_map <- function() {
  leaflet::leaflet(options = leafletOptions(minZoom = 3, maxZoom = 16)) |>
    setView(lng = "-97.511993", lat = "40.023401", zoom = 4) |>
    # Base Layers
    addProviderTiles("OpenStreetMap", group = "OpenStreetMap") |>
    addProviderTiles("Esri.WorldTopoMap", group = "Topography") |>
    addProviderTiles("Esri.WorldImagery", group = "Imagery") |>
    # Controls
    addLayersControl(
      position = "topleft",
      overlayGroups = c(
        "Reference Records",
        "Germplasm Records",
        "Buffers",
        "GRS gaps",
        "ERS gaps"
      ),
      baseGroups = c("OpenStreetMap", "Topography", "Imagery"),
      options = layersControlOptions(collapsed = TRUE)
    ) |>
    # Legends
    addLegend(
      position = "topright",
      colors = combinedColor,
      labels = c("Reference", "Germplasm"),
      group = "all records"
    )
}
