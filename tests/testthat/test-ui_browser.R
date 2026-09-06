# Browser-level checks with shinytest2 (headless Chrome/Chromium). Skipped when
# shinytest2 or a browser is not available, so the rest of the suite stays fast.
skip_if_not_installed("shinytest2")
skip_if(is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)), "no Chrome/Chromium found")

start_app <- function(name) {
  app <- shinytest2::AppDriver$new(app_root, name = name, load_timeout = 90000,
                                   seed = 1, height = 900, width = 1400)
  app$click("landing-launch")                                     # "Get Started"
  app$wait_for_js("document.body.classList.contains('app-launched')", timeout = 10000)
  app
}
is_open <- function(app, item_selector) {
  isTRUE(app$get_js(sprintf(
    "document.querySelector('%s .accordion-collapse').classList.contains('show')", item_selector)))
}
click_header <- function(app, item_selector) {
  app$run_js(sprintf("document.querySelector('%s > .accordion-header > .accordion-button').click()", item_selector))
  Sys.sleep(0.8)   # bootstrap collapse animation
}
GBIF_ITEM <- ".accordion-item[data-value=\"panel_gbif\"]"
ADV_ITEM  <- ".gbif-advanced .accordion-item[data-value=\"panel_gbif_advanced\"]"
UPL_ITEM  <- ".accordion-item[data-value=\"panel_upload\"]"

test_that("opening Advanced options keeps the GBIF Data panel open (#62)", {
  app <- start_app("accordion")
  on.exit(app$stop(), add = TRUE)

  click_header(app, GBIF_ITEM)
  expect_true(is_open(app, GBIF_ITEM))
  expect_false(is_open(app, ADV_ITEM))

  click_header(app, ADV_ITEM)
  expect_true(is_open(app, ADV_ITEM))
  expect_true(is_open(app, GBIF_ITEM))      # the bug collapsed this one

  click_header(app, ADV_ITEM)
  expect_false(is_open(app, ADV_ITEM))
  expect_true(is_open(app, GBIF_ITEM))

  # Sibling panels still behave as a single-open accordion
  click_header(app, UPL_ITEM)
  expect_true(is_open(app, UPL_ITEM))
  expect_false(is_open(app, GBIF_ITEM))
})

test_that("upload -> run gap analysis -> download report works end to end", {
  app <- start_app("gap_flow")
  on.exit(app$stop(), add = TRUE)

  click_header(app, UPL_ITEM)
  app$upload_file(`data_analysis-controls-uploadData` = file.path(app_root, "appData", "Magnolia_acuminata_data_small.csv"))
  app$wait_for_idle(timeout = 20000)
  expect_match(app$get_text(".upload-status"), "Successfully loaded 8 records")

  app$run_js("document.querySelector('a.nav-link[data-value=\"gap\"]').click()")
  app$wait_for_js("document.querySelector('#gap_analysis-gap_map.leaflet-container') !== null", timeout = 20000)
  app$set_inputs(`gap_analysis-buffer_dist` = "50")
  app$click("gap_analysis-generate_buffers")
  app$wait_for_js("getComputedStyle(document.getElementById('gap_analysis-plot_inset')).display !== 'none'", timeout = 60000)
  # the download button is rendered by renderUI once results exist; wait for
  # Shiny to populate its href before fetching
  app$wait_for_js("($('#gap_analysis-download_report').attr('href') || '') !== ''", timeout = 20000)

  report <- app$get_download("gap_analysis-download_report")
  expect_true(file.exists(report))
  html <- gsub("\\s+", " ", paste(readLines(report, warn = FALSE), collapse = " "))
  expect_true(grepl("Magnolia acuminata", html, fixed = TRUE))
  expect_true(grepl("Medium Priority (MP)", html, fixed = TRUE))
})
