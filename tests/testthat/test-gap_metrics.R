# Golden gap-analysis scores captured from the app at the start of the September
# 2026 refactor (buffer clipped to appData/land_simple.gpkg, ecoregions from
# appData/ecoregionsSimplified.gpkg). Any change to these numbers is a behaviour
# change in SRSex/GRSex/ERSex or the buffering pipeline and must be deliberate.

use_app_root()

gap_cases <- list(
  list(file = "Magnolia_acuminata_data_small.csv", km = 10, srs = 100.000000, grs = 58.844268, ers = 50.000000, n_eco = 8, n_buffers = 8),
  list(file = "Magnolia_acuminata_data_small.csv", km = 50, srs = 100.000000, grs = 61.924207, ers = 50.000000, n_eco = 8, n_buffers = 11),
  list(file = "Magnolia_acuminata_data.csv", km = 10, srs = 12.359551, grs = 9.826068, ers = 33.330000, n_eco = 159, n_buffers = 200),
  list(file = "Magnolia_acuminata_data.csv", km = 50, srs = 12.359551, grs = 14.550262, ers = 50.000000, n_eco = 159, n_buffers = 207),
  list(file = "upload_sample.csv", km = 10, srs = 2.564103, grs = 6.163115, ers = 30.000000, n_eco = 19, n_buffers = 200),
  list(file = "upload_sample.csv", km = 50, srs = 2.564103, grs = 11.467496, ers = 30.000000, n_eco = 19, n_buffers = 200)
)

for (case in gap_cases) {
  test_that(sprintf("gap scores for %s at %d km match golden values", case$file, case$km), {
    d <- load_fixture_dataset(case$file)
    r <- run_gap_pipeline_reference(d, case$km)
    expect_equal(r$srs, case$srs, tolerance = 1e-6)
    expect_equal(r$grs, case$grs, tolerance = 1e-6)
    expect_equal(r$ers, case$ers, tolerance = 1e-6)
    expect_equal(r$n_eco, case$n_eco)
    expect_equal(r$n_buffers, case$n_buffers)
  })
}

test_that("SRSex handles the zero-data edge cases", {
  d <- load_fixture_dataset("upload_sample_small.csv")
  only_g <- d |> dplyr::mutate(`Current Germplasm Type` = "G")
  only_h <- d |> dplyr::mutate(`Current Germplasm Type` = "H")
  expect_equal(SRSex(only_g$`Taxon Name`[1], only_g)$`SRS exsitu`, 100)
  expect_equal(SRSex(only_h$`Taxon Name`[1], only_h)$`SRS exsitu`, 0)
  expect_equal(SRSex("not a taxon", d)$`SRS exsitu`, 0)
  expect_equal(SRSex("not a taxon", d)$`Total records`, 0)
})

test_that("SRSex caps the G:H ratio at 100 and reports counts", {
  d <- load_fixture_dataset("Magnolia_acuminata_data_small.csv")
  s <- SRSex(d$`Taxon Name`[1], d)
  expect_equal(s$`Total records`, 8)
  expect_true(s$`SRS exsitu` <= 100)
  expect_equal(s$`Total G records` + s$`Total H records`, 8)
})

test_that("GRSex and ERSex return zero scores for empty inputs", {
  expect_equal(GRSex(NULL, NULL)$`GRS exsitu`, 0)
  e <- ERSex(NULL, NULL)
  expect_equal(e$summary$`ERS exsitu`, 0)
  expect_null(e$spatial)
})
