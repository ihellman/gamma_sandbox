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
  })
}

test_that("run_gap_analysis returns the analysed rows, layers and derived scores", {
  d <- load_fixture_dataset("Magnolia_acuminata_data_small.csv")
  L <- gap_layers()
  r <- run_gap_analysis(d, 50, land = L$land, ecoRegions = L$eco)
  expect_equal(nrow(r$points), sum(!is.na(d$Latitude) & !is.na(d$Longitude)))
  expect_equal(r$taxon, "Magnolia acuminata (L.) L.")
  expect_equal(r$fcs, mean(c(100, 61.924207, 50)), tolerance = 1e-6)
  expect_equal(r$priority$code, "MP")
  expect_equal(r$scores$Score, round(c(100, 61.924207, 50, r$fcs), 1))
  expect_s3_class(r$sf_buffers, "sf")
  expect_true(all(c("H", "G") %in% r$sf_buffers$processing_type))
  expect_s3_class(r$sf_ers_regions, "sf")
  expect_error(run_gap_analysis(d[0, ], 50), "nrow")
})

test_that("SRSex with taxon = NULL counts every row, mixed names included", {
  d <- load_fixture_dataset("Magnolia_acuminata_data_small.csv")
  d$`Taxon Name`[1:3] <- "Magnolia acuminata var. subcordata"
  all_rows <- SRSex(NULL, d)
  expect_equal(all_rows$`Total records`, 8)
  expect_equal(all_rows$Taxon, "Magnolia acuminata var. subcordata, Magnolia acuminata (L.) L.")
  one_name <- SRSex("Magnolia acuminata (L.) L.", d)
  expect_equal(one_name$`Total records`, 5)
})

test_that("FCS and priority helpers follow the GapAnalysis thresholds", {
  expect_equal(compute_fcs(10, 20, 30), 20)
  expect_equal(compute_fcs(10, NA, 30), 20)
  expect_true(is.na(compute_fcs(NA, NA, NA)))
  expect_equal(fcs_priority(0)$code, "UP");  expect_equal(fcs_priority(25)$code, "UP")
  expect_equal(fcs_priority(25.1)$code, "HP"); expect_equal(fcs_priority(50)$code, "HP")
  expect_equal(fcs_priority(75)$code, "MP");  expect_equal(fcs_priority(75.1)$code, "LP")
  expect_equal(fcs_priority(NA)$label, "Not assessed")
  expect_equal(taxon_label(data.frame(`Taxon Name` = c("a", "a", NA, "b"), check.names = FALSE)), "a, b")
  expect_equal(taxon_label(data.frame(`Taxon Name` = letters[1:5], check.names = FALSE)), "a, b, c (+2 more)")
  expect_equal(taxon_label(data.frame(`Taxon Name` = NA_character_, check.names = FALSE)), "Taxon")
})

test_that("prep_lat_lon drops rows missing either coordinate", {
  d <- data.frame(Latitude = c("1", NA, "3", "4"), Longitude = c("1", "2", NA, "x"))
  expect_equal(nrow(prep_lat_lon(d)), 1)
  expect_equal(nrow(prep_lat_lon(NULL)), 0)
  expect_equal(nrow(prep_lat_lon(data.frame(a = 1))), 0)
})

test_that("the HTML report renders from a run_gap_analysis() result without pacman", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  d <- load_fixture_dataset("Magnolia_acuminata_data_small.csv")
  L <- gap_layers()
  r <- run_gap_analysis(d, 50, land = L$land, ecoRegions = L$eco)
  out <- tempfile(fileext = ".html")
  render_gap_report(r, out)
  expect_true(file.exists(out))
  html <- gsub("\\s+", " ", paste(readLines(out, warn = FALSE), collapse = " "))   # pandoc wraps lines
  expect_true(grepl("Magnolia acuminata", html, fixed = TRUE))
  expect_true(grepl(r$priority$label, html, fixed = TRUE))
  expect_true(grepl(sprintf("%.1f", r$fcs), html, fixed = TRUE))   # FCS in report == FCS in app
  expect_false(grepl("pacman", html, fixed = TRUE))
})


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
