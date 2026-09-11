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

# Fit a map (by output id) to the extent of the rows that have coordinates,
# with a little padding. No-op when nothing has coordinates.
fit_map_to_points <- function(map_id, data, session = shiny::getDefaultReactiveDomain()) {
  lat <- suppressWarnings(as.numeric(data$Latitude))
  lon <- suppressWarnings(as.numeric(data$Longitude))
  ok <- !is.na(lat) & !is.na(lon)
  if (!any(ok)) return(invisible(NULL))
  leaflet::leafletProxy(map_id, session) |>
    leaflet::fitBounds(lng1 = min(lon[ok]), lat1 = min(lat[ok]), lng2 = max(lon[ok]), lat2 = max(lat[ok]),
                       options = list(padding = c(30, 30), maxZoom = 10))
  invisible(NULL)
}

# Maps ---------------------------------------------------------------------------

# 1. Initial Map Setup
# Renders the empty basemap with controls and legends
data_eval_base_map <- function() {
  leaflet::leaflet(options = leafletOptions(minZoom = 2, maxZoom = 16)) |>
    # Open on a global view (whole world at the most zoomed-out level) rather
    # than a US-centred one; the map zooms to the records once data is loaded.
    setView(lng = 0, lat = 0, zoom = 2) |>
    # Base Layers
    addProviderTiles("Esri.WorldGrayCanvas", group = "Light Gray") |>
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
      baseGroups = c("Light Gray", "Topography", "Imagery"),
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

# --- Protected land overlay ---------------------------------------------------
# Where seed collecting might be possible. A toggleable overlay, hidden by
# default, streamed from the provider (nothing is shipped with the app).
#
# World Database on Protected Areas (UNEP-WCMC / IUCN): global, cached
# Web-Mercator tiles - fast at any zoom. Tiles carry no attributes, so a click
# on the map while the layer is shown asks the same MapServer's identify
# endpoint what is at that point and shows the answer in a popup.
WDPA_SERVICE_URL <- "https://data-gis.unep-wcmc.org/server/rest/services/ProtectedSites/The_World_Database_of_Protected_Areas/MapServer"
WDPA_TILE_URL    <- paste0(WDPA_SERVICE_URL, "/tile/{z}/{y}/{x}")
WDPA_ATTRIBUTION <- "Protected areas: UNEP-WCMC and IUCN, <a href='https://www.protectedplanet.net' target='_blank'>Protected Planet (WDPA)</a>"
PROTECTED_LAND_GROUP <- "Protected areas (WDPA)"

add_protected_land_layers <- function(map) {
  map <- map %>%
    leaflet::addTiles(
      urlTemplate = WDPA_TILE_URL, group = PROTECTED_LAND_GROUP,
      attribution = WDPA_ATTRIBUTION,
      options = leaflet::tileOptions(opacity = 0.6, maxNativeZoom = 15, maxZoom = 19)
    )
  # Click-to-identify popup. Only runs while the WDPA group is switched on, and
  # ignores clicks that landed on a marker / polygon (those have their own
  # popups and their click bubbles up to the map).
  js <- sprintf("
    function(el, x) {
      // In Shiny, onRender hooks do not receive the map as `this`; resolve it.
      var inst = HTMLWidgets.getInstance(el);
      var map = (inst && typeof inst.getMap === 'function') ? inst.getMap() : null;
      if (!map) { return; }
      var group = %s, service = %s;
      var esc = function(v) {
        return String(v == null ? '' : v).replace(/[&<>]/g, function(c) {
          return {'&': '&amp;', '<': '&lt;', '>': '&gt;'}[c];
        });
      };
      map.on('click', function(e) {
        var lg = map.layerManager && map.layerManager.getLayerGroup(group);
        if (!lg || !map.hasLayer(lg)) { return; }
        var t = e.originalEvent && e.originalEvent.target;
        if (t && (t.tagName === 'path' || (t.classList && t.classList.contains('leaflet-marker-icon')))) { return; }
        var b = map.getBounds(), size = map.getSize();
        var url = service + '/identify?f=json&geometryType=esriGeometryPoint&sr=4326&layers=all' +
          '&tolerance=3&returnGeometry=false' +
          '&geometry=' + e.latlng.lng + ',' + e.latlng.lat +
          '&mapExtent=' + [b.getWest(), b.getSouth(), b.getEast(), b.getNorth()].join(',') +
          '&imageDisplay=' + size.x + ',' + size.y + ',96';
        var popup = L.popup({maxWidth: 320}).setLatLng(e.latlng)
          .setContent('<span class=\"text-muted\">Looking up protected areas…</span>').openOn(map);
        fetch(url).then(function(r) { return r.json(); }).then(function(d) {
          var seen = {}, items = [];
          (d.results || []).forEach(function(r) {
            var a = r.attributes || {}, key = a.WDPA_PID || a.WDPAID || a.NAME;
            if (!key || seen[key]) { return; }
            seen[key] = true;
            var rows = [];
            if (a.DESIG_ENG) { rows.push(esc(a.DESIG_ENG) + (a.DESIG_TYPE ? ' (' + esc(a.DESIG_TYPE) + ')' : '')); }
            if (a.IUCN_CAT) { rows.push('IUCN category: ' + esc(a.IUCN_CAT)); }
            if (a.STATUS) { rows.push(esc(a.STATUS) + (a.STATUS_YR && a.STATUS_YR !== '0' ? ' ' + esc(a.STATUS_YR) : '')); }
            if (a.MANG_AUTH && a.MANG_AUTH !== 'Not Reported') { rows.push('Managed by: ' + esc(a.MANG_AUTH)); }
            if (a.REP_AREA && Number(a.REP_AREA) > 0) { rows.push('Reported area: ' + Number(a.REP_AREA).toLocaleString(undefined, {maximumFractionDigits: 0}) + ' km²'); }
            var link = a.WDPAID ? ' <a href=\"https://www.protectedplanet.net/' + encodeURIComponent(a.WDPAID) + '\" target=\"_blank\">Protected Planet</a>' : '';
            items.push('<div style=\"margin-bottom:6px\"><b>' + esc(a.NAME || 'Unnamed protected area') + '</b>' + link + '<br/>' + rows.join('<br/>') + '</div>');
          });
          popup.setContent(items.length ? items.join('') : 'No WDPA protected area recorded at this point.');
        }).catch(function() {
          popup.setContent('Protected-area lookup failed (WDPA service unavailable).');
        });
      });
    }",
    jsonlite::toJSON(PROTECTED_LAND_GROUP, auto_unbox = TRUE),
    jsonlite::toJSON(WDPA_SERVICE_URL, auto_unbox = TRUE)
  )
  htmlwidgets::onRender(map, js)
}

# Gap Analysis Map Setup
# Renders the empty basemap with controls and legends
gap_base_map <- function() {
  leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 2)) %>%
    leaflet::setView(lng = 0, lat = 0, zoom = 2) %>%   # global view until results are drawn
    leaflet::addProviderTiles("Esri.WorldGrayCanvas", group = "Light Gray") %>%
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
      baseGroups = c("Light Gray", "Topography", "Imagery"),
      overlayGroups = c(
        "Reference Records",
        "Germplasm Records",
        "Range (convex hull)",
        "Buffers",
        "GRS Gap",       # <- MUST BE LISTED HERE
        "ERS Regions",   # <- MUST BE LISTED HERE
        PROTECTED_LAND_GROUP
      ),
      options = leaflet::layersControlOptions(collapsed = TRUE)
    ) %>%
    # Optional: Hide them on initial load so the map isn't cluttered
    leaflet::hideGroup(c("Range (convex hull)", "GRS Gap", "ERS Regions", "Buffers", PROTECTED_LAND_GROUP))
}
