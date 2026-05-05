source("R/gap_analysis_functions.R")
pacman::p_load(sf, dplyr, rmarkdown)

# Load test data
test_data <- read.csv("appData/Magnolia_acuminata_data.csv")

# Create mock points object with proper types
points <- test_data %>%
  dplyr::filter(!is.na(Longitude)) %>%
  dplyr::mutate(
    Longitude = as.numeric(Longitude),
    Latitude = as.numeric(Latitude),
    `Current Germplasm Type` = sample(c("G", "H"), n(), replace = TRUE),
    `Taxon Name` = "Magnolia acuminata",
    Collector = "Test Collector",
    Locality = "Test Locality"
  )

# Convert to sf for calculations
sf_points <- sf::st_as_sf(points, coords = c("Longitude", "Latitude"), crs = 4326)
sf_points_proj <- sf::st_transform(sf_points, 3857)

# Load land
land <- terra::vect("appData/land_simple.gpkg")
v <- terra::vect(points, geom = c("Longitude", "Latitude"), crs = "EPSG:4326")
v_buffer <- terra::buffer(v, width = 50 * 1000)

land_proj <- terra::project(land, terra::crs(v_buffer))
v_clipped <- terra::intersect(v_buffer, land_proj)

gBuff <- v_clipped[v_clipped$`Current Germplasm Type` == "G", ]
hBuff <- v_clipped[v_clipped$`Current Germplasm Type` == "H", ]

grsMap_element <- terra::erase(x = hBuff, y = gBuff)

# Calculate metrics
srs_metrics <- SRSex("Magnolia acuminata", points)
grs_metrics <- GRSex(v_clipped, grsMap_element)
ers_metrics <- ERSex(v, gBuff)

# Prepare sf objects
sf_buffers_raw <- sf::st_as_sf(v_clipped)
sf_buffers <- sf_buffers_raw %>% 
  sf::st_make_valid() %>% 
  dplyr::group_by(processing_type = `Current Germplasm Type`) %>% 
  dplyr::summarize(geometry = sf::st_union(geometry), .groups = "drop")

sf_grs_gap <- sf::st_as_sf(terra::aggregate(terra::makeValid(grsMap_element)))
sf_ers_regions <- if (!is.null(ers_metrics$spatial)) sf::st_as_sf(ers_metrics$spatial) else NULL

params_list <- list(
  points = points,
  bufferDist = 50,
  srsMetrics = srs_metrics,
  grsMetrics = grs_metrics,
  ersMetrics = ers_metrics,
  sf_buffers = sf_buffers,
  sf_grs_gap = sf_grs_gap,
  sf_ers_regions = sf_ers_regions,
  combinedColor = c("#007bc2", "#f45100"),
  grsexColor = "#74149c",
  ersexColors = c("#00bf7f", "#f9b928")
)

rmarkdown::render(
  "reportTemplate.Rmd",
  output_file = "test_report.html",
  params = params_list
)
