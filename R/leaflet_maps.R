# Icons, colors, shapes, labels -----------------------------------------------------------

# --- Color Palettes ---
# Data Eval map - Input data
uploadColor <- c("#fdae61", "#f46d43") #(upload H, upload G)
gbifColor <- c("#92c5de", "#0571b0")
# GAP Map
combinedColor <- c("#f1a340", "#542788")
grsexColor <- c("#ef8a62")
ersexColor <- c("#d1e5f0")
# Ers gaps
ersexColors <- c("#8ae6c7", "#728587")


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
      group = "GBIF",
      opacity = 1
    ) |>
    addLegend(
      position = "topright",
      colors = upload_legend_shape,
      labels = c("Upload Reference", "Upload Germplasm"),
      group = "Upload",
      opacity = 1
    ) |>
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
    ) |>
    # Remove drawn layer after creation.  Currently, there is not an R-only way to do this.
    htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        map.on('draw:created', function(e) {
          var layer = e.layer;
          // Remove the drawn layer immediately after it's created
          setTimeout(function() {
            map.removeLayer(layer);
          }, 0);
        });
      }
    ")
}

# 2. Render Main Points
render_base_points <- function(mapID, allPoints) {
  # Safety check
  if (nrow(allPoints) == 0) {
    leafletProxy(mapID) %>% clearMarkers()
    return(invisible(NULL))
  }

  # Filter for valid coordinates
  mappable_data <- allPoints %>%
    mutate(
      Latitude = as.numeric(Latitude),
      Longitude = as.numeric(Longitude)
    ) %>%
    filter(!is.na(Latitude) & !is.na(Longitude))

  if (nrow(mappable_data) == 0) {
    leafletProxy(mapID) %>% clearMarkers()
    return(invisible(NULL))
  }

  # Convert to sf object
  mappable_data <- mappable_data %>%
    sf::st_as_sf(
      coords = c("Longitude", "Latitude"),
      crs = 4326,
      remove = FALSE
    )

  # Assign Colors (using mappable_data)
  data <- mappable_data %>%
    dplyr::mutate(
      color = case_when(
        `Current Germplasm Type` == "H" & source == "GBIF" ~ gbifColor[1],
        `Current Germplasm Type` == "G" & source == "GBIF" ~ gbifColor[2],
        `Current Germplasm Type` == "H" & source == "upload" ~ uploadColor[1],
        `Current Germplasm Type` == "G" & source == "upload" ~ uploadColor[2],
        TRUE ~ "gray"
      )
    )

  proxy <- leafletProxy(mapID) |> clearMarkers()

  # Draw GBIF Points
  data_gbif <- data %>% filter(source == "GBIF")
  if(nrow(data_gbif) > 0) {
    proxy <- proxy |> addCircleMarkers(
      data = data_gbif,
      layerId = ~index,
      group = "GBIF", # ASSIGNED TO TOGGLE
      radius = 5,
      color = "white",
      fillColor = ~color,
      stroke = TRUE,
      weight = 1,
      fillOpacity = 1,
      label = point_labels(data_gbif)
    )
  }

  # Draw Upload Points
  data_upload <- data %>% filter(source == "upload")
  if(nrow(data_upload) > 0) {
    proxy <- proxy |> addCircleMarkers(
      data = data_upload,
      layerId = ~index,
      group = "Upload", # ASSIGNED TO TOGGLE
      radius = 5,
      color = "white",
      fillColor = ~color,
      stroke = TRUE,
      weight = 1,
      fillOpacity = 1,
      label = point_labels(data_upload)
    )
  }

  # Fit Bounds
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
update_selection_highlights <- function(mapID, allPoints, selected_ids) {
  if (nrow(allPoints) == 0 || length(selected_ids) == 0) {
    leafletProxy(mapID) |> clearGroup("GBIF Selection") |> clearGroup("Upload Selection")
    return(invisible(NULL))
  }

  # Filter for valid coordinates
  mappable_data <- allPoints %>%
    filter(index %in% selected_ids) %>% 
    mutate(
      Latitude = as.numeric(Latitude),
      Longitude = as.numeric(Longitude)
    ) %>%
    filter(!is.na(Latitude) & !is.na(Longitude))

  if (nrow(mappable_data) == 0) {
    leafletProxy(mapID) |> clearGroup("GBIF Selection") |> clearGroup("Upload Selection")
    return(invisible(NULL))
  }

  mappable_data <- mappable_data %>%
    sf::st_as_sf(
      coords = c("Longitude", "Latitude"),
      crs = 4326,
      remove = FALSE
    )

  proxy <- leafletProxy(mapID)

  # Clear previous highlights for BOTH groups
  proxy |> clearGroup("GBIF Selection") |> clearGroup("Upload Selection")

  # Add Halo Effect for GBIF Selection
  data_gbif_sel <- mappable_data %>% filter(source == "GBIF")
  if(nrow(data_gbif_sel) > 0) {
    proxy <- proxy |> addCircleMarkers(
      data = data_gbif_sel,
      group = "GBIF Selection",
      radius = 12,
      color = "transparent",
      fillColor = "#025c8f",
      fillOpacity = 0.3,
      stroke = FALSE,
      options = pathOptions(interactive = FALSE)
    )
  }

  # Add Halo Effect for Upload Selection
  data_upload_sel <- mappable_data %>% filter(source == "upload")
  if(nrow(data_upload_sel) > 0) {
    proxy <- proxy |> addCircleMarkers(
      data = data_upload_sel,
      group = "Upload Selection",
      radius = 12,
      color = "transparent",
      fillColor = "#025c8f",
      fillOpacity = 0.3,
      stroke = FALSE,
      options = pathOptions(interactive = FALSE)
    )
  }
}

# Gap Analysis Map Setup
# Renders the empty basemap with controls and legends
gap_base_map <- function() {
  leaflet::leaflet() %>%
    leaflet::addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
    leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Topography") %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
    # map pane elements 
    leaflet::addMapPane("buffers", zIndex = 410) %>%
    leaflet::addMapPane("points", zIndex = 420) %>%
    
    leaflet::addLayersControl(
      baseGroups = c("OpenStreetMap", "Topography", "Imagery"),
      overlayGroups = c(
        "Reference Records",
        "Germplasm Records",
        "Buffers",
        "GRS Gap",       # <- MUST BE LISTED HERE
        "ERS Regions"    # <- MUST BE LISTED HERE
      ),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) %>%
    # Optional: Hide them on initial load so the map isn't cluttered
    leaflet::hideGroup(c("GRS Gap", "ERS Regions", "Buffers"))|> 
    addLegend(
      title = "Species Observations",
      position = "bottomleft",
      colors = combinedColor,
      labels = c("Reference", "Germplasm"),
      group = "all records"
    )
}
