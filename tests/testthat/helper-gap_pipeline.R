# Reference implementation of the gap-analysis pipeline as executed by the
# "Run Gap Analysis" button, used to regression-test the metric functions.
# (Phase 3 of the work plan extracts this from the module into a pure function;
# until then the test mirrors the module body.)
run_gap_pipeline_reference <- function(all_data, dist_km) {
  display_taxon <- all_data$`Taxon Name`[1]
  all_data$`Taxon Name` <- display_taxon
  data <- as.data.frame(all_data) |>
    dplyr::filter(!is.na(Longitude)) |>
    dplyr::mutate(Longitude = as.numeric(Longitude), Latitude = as.numeric(Latitude))
  srs <- SRSex(taxon = display_taxon, occurrence_Data = all_data)
  land <- terra::vect("appData/land_simple.gpkg")
  data$processing_type <- data[["Current Germplasm Type"]]
  v <- terra::vect(data, geom = c("Longitude", "Latitude"), crs = "EPSG:4326")
  v_clipped <- terra::intersect(terra::buffer(v, width = dist_km * 1000), terra::project(land, "EPSG:4326"))
  gBuff <- v_clipped[v_clipped$processing_type == "G", ]
  hBuff <- v_clipped[v_clipped$processing_type == "H", ]
  gap <- if (length(hBuff) == 0 || length(gBuff) == 0) hBuff else terra::erase(hBuff, gBuff)
  grs <- GRSex(allBuffers = v_clipped, outsideGBuffers = gap)
  ers <- ERSex(gapPoints = v, g_buffer = gBuff)
  list(srs = srs$`SRS exsitu`, grs = grs$`GRS exsitu`, ers = ers$summary$`ERS exsitu`,
       n_eco = nrow(ers$spatial), n_buffers = nrow(v_clipped))
}
