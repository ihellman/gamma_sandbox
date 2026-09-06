# Server-side behaviour of the gap-analysis module via shiny::testServer.
use_app_root()

test_that("Run Gap Analysis computes results and a dataset change clears them", {
  analysis_data <- reactiveVal(load_fixture_dataset("Magnolia_acuminata_data_small.csv"))
  testServer(gapAnalysisServer, args = list(analysis_data = analysis_data), {
    session$setInputs(gap_map_zoom = 4, buffer_dist = "50")
    expect_null(gap_result())
    session$setInputs(generate_buffers = 1)
    r <- gap_result()
    expect_false(is.null(r))
    expect_equal(r$srs$`SRS exsitu`, 100)
    expect_equal(r$grs$`GRS exsitu`, 61.924207, tolerance = 1e-6)
    expect_equal(r$ers$summary$`ERS exsitu`, 50)
    expect_equal(gap_scores_df()$Metric, factor(c("SRS", "GRS", "ERS", "FCS"), levels = c("SRS", "GRS", "ERS", "FCS")))
    expect_true(analysis_active())

    # editing the working dataset invalidates the analysis
    analysis_data(analysis_data()[-1, ] |> dplyr::mutate(index = dplyr::row_number()))
    session$flushReact()
    expect_null(gap_result())
    expect_false(analysis_active())

    # and it can be re-run with a different buffer
    session$setInputs(buffer_dist = "10")
    session$setInputs(generate_buffers = 2)
    expect_equal(gap_result()$dist_km, 10)
  })
})

test_that("a dataset without usable coordinates reports an error instead of crashing", {
  d <- load_fixture_dataset("upload_sample_small.csv")
  d$Latitude <- NA_real_
  analysis_data <- reactiveVal(d)
  testServer(gapAnalysisServer, args = list(analysis_data = analysis_data), {
    session$setInputs(gap_map_zoom = 4, buffer_dist = "50")
    session$setInputs(generate_buffers = 1)
    expect_null(gap_result())
    expect_false(analysis_active())
  })
})
