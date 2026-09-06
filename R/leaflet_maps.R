# Icons, colors, shapes, labels -----------------------------------------------------------
# NOTE: the palette objects below are APP-WIDE GLOBALS. They are referenced by
# module_dt_table.R (cell colours via styleEqual), module_data_analysis.R (card
# headers), module_gap_analysis.R and reportTemplate.Rmd (passed as params).
# Change a colour here and every consumer follows; never hard-code the hex codes.

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
  borders = "#4d4d4d",
  shapes = "circle"
)
upload_legend_shape <- make_shapes(
  uploadColor,
  sizes = 20,
  borders = "#4d4d4d",
  shapes = "circle"
)
# --- Label Generator ---
# Truncate free-text fields so a 300-character locality cannot blow up the
# hover tooltip (#40); custom.css wraps whatever is left at 280 px.
truncate_label <- function(x, n = 140) {
  x <- as.character(x)
  too_long <- !is.na(x) & nchar(x) > n
  x[too_long] <- paste0(substr(x[too_long], 1, n - 1), "\u2026")
  x
}

# Generates HTML labels on render
point_labels <- function(data) {
  paste0(
    "<strong>",
    htmltools::htmlEscape(as.character(data$`Taxon Name`)),
    "</strong>",
    "<br/><strong> Type: </strong>",
    data$`Current Germplasm Type`,
    "<br/><b>Collector Name:</b> ",
    htmltools::htmlEscape(truncate_label(data$Collector)),
    "<br/><b>Locality Description:</b> ",
    htmltools::htmlEscape(truncate_label(data$Locality))
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
    # Controls
    addLayersControl(
      position = "topleft",
      baseGroups = c("OpenStreetMap", "Topography", "Imagery"),
      overlayGroups = c(
        "Upload", 
        "Upload Selection", 
        "GBIF", 
        "GBIF Selection"),
      options = layersControlOptions(collapsed = TRUE)
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
        // In Shiny, onRender hooks do not receive the map as `this`; resolve it
        // from the widget instance attached to the element.
        var inst = HTMLWidgets.getInstance(el);
        var map = (inst && typeof inst.getMap === 'function') ? inst.getMap() : this;
        if (!map || typeof map.on !== 'function') { return; }
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
      color = "#4d4d4d",
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
      color = "#4d4d4d",
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

# --- Protected / public land overlays ---------------------------------------
# Where seed collecting might be possible. Both are toggleable overlays, hidden
# by default, streamed from the providers (nothing is shipped with the app).
#
# 1. World Database on Protected Areas (UNEP-WCMC / IUCN): global, cached
#    Web-Mercator tiles - fast at any zoom.
WDPA_TILE_URL <- "https://data-gis.unep-wcmc.org/server/rest/services/ProtectedSites/The_World_Database_of_Protected_Areas/MapServer/tile/{z}/{y}/{x}"
WDPA_ATTRIBUTION <- "Protected areas: UNEP-WCMC and IUCN, <a href='https://www.protectedplanet.net' target='_blank'>Protected Planet (WDPA)</a>"
# 2. USGS PAD-US "Public Access" (United States only): open / restricted /
#    closed access polygons from an ArcGIS feature service, drawn client-side
#    (green / amber / red). Only requested at zoom >= 8 so the whole country
#    is never downloaded at once.
PADUS_ACCESS_URL <- "https://services.arcgis.com/v01gqwM5QqNysAAi/arcgis/rest/services/PADUS_Public_Access/FeatureServer/0"
PADUS_ATTRIBUTION <- "Public access: <a href='https://www.usgs.gov/programs/gap-analysis-project/science/pad-us-data-overview' target='_blank'>USGS PAD-US</a>"
PROTECTED_LAND_GROUPS <- c("Protected areas (WDPA, global)", "Public access (PAD-US, USA)")

# esri-leaflet (Apache-2.0) is vendored in www/lib/esri-leaflet because the R
# wrapper package (leaflet.esri) is no longer on CRAN.
esri_leaflet_dependency <- function() {
  htmltools::htmlDependency(
    name = "esri-leaflet", version = "3.0.12",
    src = c(file = normalizePath("www/lib/esri-leaflet")),
    script = "esri-leaflet.js"
  )
}

add_protected_land_layers <- function(map) {
  map <- map %>%
    leaflet::addTiles(
      urlTemplate = WDPA_TILE_URL, group = PROTECTED_LAND_GROUPS[1],
      attribution = WDPA_ATTRIBUTION,
      options = leaflet::tileOptions(opacity = 0.6, maxNativeZoom = 15, maxZoom = 19)
    )
  map$dependencies <- c(map$dependencies, list(esri_leaflet_dependency()))
  # Create the feature layer client-side and hand it to leaflet's layerManager so
  # the existing layers control (and hideGroup / showGroup) can toggle it.
  js <- sprintf("
    function(el, x) {
      // In Shiny, onRender hooks do not receive the map as `this`, and the
      // plugin script may still be loading, so resolve both with a short retry.
      var attempts = 0;
      var addPadus = function() {
        var inst = HTMLWidgets.getInstance(el);
        var map = (inst && typeof inst.getMap === 'function') ? inst.getMap() : null;
        if (!map || !map.layerManager || !L.esri) {
          if (attempts++ < 50) { setTimeout(addPadus, 200); }
          return;
        }
      var layer = L.esri.featureLayer({
        url: %s,
        minZoom: 8,
        fields: ['OBJECTID', 'Unit_Nm', 'Pub_Access', 'MngNm_Desc'],
        style: function(feature) {
          var c = {OA: '#2e8b57', RA: '#e6a700', XA: '#b22222'}[feature.properties.Pub_Access] || '#777777';
          return {color: c, weight: 1, fillColor: c, fillOpacity: 0.25};
        }
      });
      layer.bindPopup(function(l) {
        var p = l.feature.properties;
        var acc = {OA: 'Open access', RA: 'Restricted access', XA: 'Closed access'}[p.Pub_Access] || p.Pub_Access;
        return '<b>' + (p.Unit_Nm || '') + '</b><br/>' + acc + '<br/>' + (p.MngNm_Desc || '');
      });
      var group = %s;
      map.layerManager.addLayer(layer, 'shape', 'padus_public_access', group);
      // start hidden; the layers control toggles the group like any other
      var container = map.layerManager.getLayerGroup(group);
      if (container && map.hasLayer(container)) { map.removeLayer(container); }
      map.attributionControl.addAttribution(%s);
      };
      addPadus();
    }",
    jsonlite::toJSON(PADUS_ACCESS_URL, auto_unbox = TRUE),
    jsonlite::toJSON(PROTECTED_LAND_GROUPS[2], auto_unbox = TRUE),
    jsonlite::toJSON(PADUS_ATTRIBUTION, auto_unbox = TRUE)
  )
  htmlwidgets::onRender(map, js)
}

# Gap Analysis Map Setup
# Renders the empty basemap with controls and legends
gap_base_map <- function() {
  leaflet::leaflet() %>%
    leaflet::addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
    leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Topography") %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
    add_protected_land_layers() %>%
    # map pane elements 
    leaflet::addMapPane("buffers", zIndex = 410) %>%
    leaflet::addMapPane("points", zIndex = 420) %>%
    # Legend
    addLegend(
      title = "Species Observations",
      position = "topright",
      colors = combinedColor,
      labels = c("Reference", "Germplasm"),
      group = "all records"
    ) %>%
    leaflet::addLayersControl(
      position = "topleft",
      baseGroups = c("OpenStreetMap", "Topography", "Imagery"),
      overlayGroups = c(
        "Reference Records",
        "Germplasm Records",
        "Range (convex hull)",
        "Buffers",
        "GRS Gap",       # <- MUST BE LISTED HERE
        "ERS Regions",   # <- MUST BE LISTED HERE
        PROTECTED_LAND_GROUPS
      ),
      options = leaflet::layersControlOptions(collapsed = TRUE)
    ) %>%
    # Legend for the PAD-US colours; shown/hidden together with that layer
    addLegend(
      title = "Public land access (PAD-US)",
      position = "bottomleft",
      colors = c("#2e8b57", "#e6a700", "#b22222"),
      labels = c("Open access", "Restricted access", "Closed access"),
      opacity = 0.6,
      group = PROTECTED_LAND_GROUPS[2]
    ) %>%
    # Optional: Hide them on initial load so the map isn't cluttered
    leaflet::hideGroup(c("Range (convex hull)", "GRS Gap", "ERS Regions", "Buffers", PROTECTED_LAND_GROUPS))
}
