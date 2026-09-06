# The gap-analysis pipeline is run_gap_analysis() in R/gap_analysis_functions.R;
# the tests call it directly. Layers are loaded once per test session here.
gap_layers <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) cache <<- list(land = gap_land(), eco = gap_ecoregions())
    cache
  }
})
run_gap_pipeline_reference <- function(all_data, dist_km) {
  L <- gap_layers()
  r <- run_gap_analysis(all_data, dist_km, land = L$land, ecoRegions = L$eco)
  list(srs = r$srs$`SRS exsitu`, grs = r$grs$`GRS exsitu`, ers = r$ers$summary$`ERS exsitu`,
       n_eco = nrow(r$ers$spatial), n_buffers = nrow(r$sf_buffers) + 0L, result = r)
}
