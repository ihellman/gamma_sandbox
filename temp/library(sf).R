library(sf)
library(dplyr)
library(ggplot2)

# 1. Create dummy points (Longitude, Latitude)
# Point A is near a 'coast', Point B is further inland
pts_df <- data.frame(
  id = c(1, 2),
  type = c("G", "H"),
  lon = c(-81.5, -80.5),
  lat = c(41.5, 41.0)
)

# Convert to sf object (WGS84)
pts_df <- st_as_sf(pts_df, coords = c("lon", "lat"), crs = 4326)

# 2. Create a dummy 'Land' polygon (a simple rectangle)
# This represents our coastline boundary
land_poly <- st_polygon(list(matrix(
  c(
    -82.0,
    39.5,
    -81.0,
    39.5,
    -81.0,
    42.0,
    -82.0,
    42.0,
    -82.0,
    40.5
  ),
  ncol = 2,
  byrow = TRUE
))) %>%
  st_sfc(crs = 4326) %>%
  st_as_sf() %>%
  mutate(land = "North Shore")

# 3. Buffer the points (Transform to meters first for accuracy)
# We use EPSG:3857 (Web Mercator) for a quick 50km buffer
pts_buffered <- pts_sf %>%
  st_transform(3857) %>%
  st_buffer(dist = 50000) %>%
  st_transform(4326)

# 4. Clip to Land using st_intersection
# This keeps the 'type' attribute from pts and 'land' from land_poly
clipped_buffers <- st_intersection(pts_buffered, land_poly)

# 5. Dissolve by type to fix the "Black Blob" overlap issue
dissolved_buffers <- clipped_buffers %>%
  group_by(type) %>%
  summarize(geometry = st_union(geometry), .groups = "drop")

# --- Visualization ---
ggplot() +
  geom_sf(data = land_poly, fill = "antiquewhite", color = "darkgrey") +
  geom_sf(data = dissolved_buffers, aes(fill = type), alpha = 0.4) +
  geom_sf(data = pts_sf, color = "black", size = 2) +
  theme_minimal() +
  labs(
    title = "Clipped and Dissolved Buffers",
    subtitle = "Note how buffers are cut off by the land boundary"
  )
