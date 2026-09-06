# Server-side behaviour of the controls module, driven with shiny::testServer.
# The GBIF network layer is replaced by the saved fixture pool.
use_app_root()
pool <- readRDS(fixture_path("gbif_magnolia_fraseri.rds"))

# Replace the network layer (sourced into the global env by helper-setup.R)
# for the duration of the calling test.
mock_gbif <- function(env = parent.frame()) {
  originals <- list(gbif_fetch = gbif_fetch, gbif_counts = gbif_counts)
  assign("gbif_fetch",  function(...) pool, envir = globalenv())
  assign("gbif_counts", function(...) list(total = nrow(pool), living = 9, inat = 100), envir = globalenv())
  withr::defer(for (nm in names(originals)) assign(nm, originals[[nm]], envir = globalenv()), envir = env)
}

select_magnolia_fraseri <- function(session) {
  session$setInputs(taxon_genus = "Magnolia")
  session$setInputs(taxon_species = "fraseri")
  session$setInputs(taxon_rank = "species")
  session$setInputs(taxon_infra = "")
}

test_that("taxon selectors resolve the accepted GBIF taxon key", {
  analysis_data <- reactiveVal(data.frame()); selected_points <- reactiveVal(numeric(0))
  testServer(controlsModuleServer, args = list(analysis_data = analysis_data, selected_points = selected_points), {
    select_magnolia_fraseri(session)
    expect_equal(selected_taxon_id(), 3153619)
    session$setInputs(taxon_rank = "variety")
    session$setInputs(taxon_infra = "pyramidata")
    expect_equal(selected_taxon_id(), 8091117)
  })
})

test_that("gather loads a G-first dataset into analysis_data and records honest counts", {
  mock_gbif()
    analysis_data <- reactiveVal(data.frame()); selected_points <- reactiveVal(numeric(0))
    testServer(controlsModuleServer, args = list(analysis_data = analysis_data, selected_points = selected_points), {
      select_magnolia_fraseri(session)
      session$setInputs(gbif_limit = 150, apply_date_filter = FALSE, exclude_inat = FALSE,
                        include_synonyms = FALSE, reference_selection = "recent")
      session$setInputs(loadGBIF = 1)
      d <- analysis_data()
      expect_equal(nrow(d), 150)
      expect_equal(sum(d$`Current Germplasm Type` == "G"), 9)
      expect_true(all(d$source == "GBIF"))
      expect_equal(d$index, 1:150)
      expect_equal(last_gather()$n_g, 9)
      expect_equal(gbif_cache()$pool, pool)

      # A second gather with GBIF data present asks before overwriting
      session$setInputs(gbif_limit = 50)
      session$setInputs(loadGBIF = 2)
      expect_equal(nrow(analysis_data()), 150)     # unchanged until confirmed
      expect_equal(nrow(gbifData_temp()), 50)
      session$setInputs(confirmLoadGBIF = 1)
      expect_equal(nrow(analysis_data()), 50)
      expect_null(gbifData_temp())
    })
})

test_that("gather merges with uploaded data and keeps upload rows on overwrite", {
  mock_gbif()
    upload <- load_fixture_dataset("upload_sample_small.csv")
    analysis_data <- reactiveVal(upload); selected_points <- reactiveVal(c(1, 2))
    testServer(controlsModuleServer, args = list(analysis_data = analysis_data, selected_points = selected_points), {
      select_magnolia_fraseri(session)
      session$setInputs(gbif_limit = 20, apply_date_filter = FALSE, exclude_inat = FALSE,
                        include_synonyms = FALSE, reference_selection = "spatial")
      session$setInputs(loadGBIF = 1)
      d <- analysis_data()
      expect_equal(nrow(d), 25)
      expect_equal(sum(d$source == "upload"), 5)
      expect_equal(d$index, 1:25)
    })
})

test_that("uploading a file loads it and a missing-column file loads nothing", {
  analysis_data <- reactiveVal(data.frame()); selected_points <- reactiveVal(numeric(0))
  testServer(controlsModuleServer, args = list(analysis_data = analysis_data, selected_points = selected_points), {
    session$setInputs(uploadData = fixture_file("upload_sample_missing_2_cols.csv"))
    expect_equal(nrow(analysis_data()), 0)
    expect_equal(upload_status()$type, "error")            # #32: the failure is shown
    expect_match(upload_status()$text, "Missing columns")
    session$setInputs(uploadData = fixture_file("upload_sample_small.csv"))
    expect_equal(nrow(analysis_data()), 5)
    expect_true(all(analysis_data()$source == "upload"))
    expect_equal(upload_status()$type, "success")
    session$setInputs(uploadData = fixture_file("upload_sample_manual_15miss_coords.csv"))
    session$setInputs(confirmloadUpload = 1)
    expect_equal(upload_status()$type, "warning")
    expect_equal(nrow(analysis_data()), 200)
    # another upload triggers the overwrite modal; confirming replaces the rows
    session$setInputs(uploadData = fixture_file("Magnolia_acuminata_data_small.csv"))
    expect_equal(nrow(analysis_data()), 200)
    session$setInputs(confirmloadUpload = 2)
    expect_equal(nrow(analysis_data()), 8)
  })
})
