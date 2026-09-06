# Regenerates tests/testthat/fixtures/gbif_magnolia_fraseri.rds - a raw GBIF pool
# for Magnolia fraseri (taxonKey 3153619) as returned by gbif_fetch(), plus the
# index counts at download time (stored as attributes). Requires network access.
# Run from the app root:  Rscript tests/testthat/fixtures/make_gbif_fixture.R
suppressPackageStartupMessages(source("global.R"))
source("R/gbif_functions.R")
key <- 3153619
pool <- gbif_fetch(key, living_limit = 200, other_limit = 1000)
attr(pool, "taxon_key") <- key
attr(pool, "api_counts") <- gbif_counts(key)
attr(pool, "canonical_name") <- "Magnolia fraseri"
attr(pool, "downloaded") <- Sys.Date()
saveRDS(pool, "tests/testthat/fixtures/gbif_magnolia_fraseri.rds", compress = "xz")
cat("rows:", nrow(pool), " living:", sum(pool$basisOfRecord == "LIVING_SPECIMEN"),
    " api living:", attr(pool, "api_counts")$living, "\n")
