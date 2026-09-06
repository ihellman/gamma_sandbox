# Golden behaviour of read_upload_file() captured from the sample files in appData/.

test_that("rejects unsupported file extensions before reading", {
  res <- read_upload_file(list(name = "data.txt", datapath = tempfile()))
  expect_equal(res$status, "validation_error")
  expect_match(res$message, "Invalid file type")
})

test_that("well-formed CSV loads all rows with the expected columns", {
  res <- read_upload_file(fixture_file("upload_sample.csv"))
  expect_equal(res$status, "success")
  expect_equal(nrow(res$data), 200)
  expect_equal(res$message_type, "message")
  expect_match(res$message, "Successfully loaded 200 records")
  expect_true(all(c("Latitude", "Longitude", "issues", "index", "source") %in% names(res$data)))
  expect_true(is.numeric(res$data$Latitude))
  expect_true(all(res$data$source == "upload"))
  expect_equal(res$data$index, seq_len(200))
})

test_that("small fixtures load", {
  expect_equal(nrow(read_upload_file(fixture_file("upload_sample_small.csv"))$data), 5)
  expect_equal(nrow(read_upload_file(fixture_file("Magnolia_acuminata_data_small.csv"))$data), 8)
  expect_equal(nrow(read_upload_file(fixture_file("Magnolia_acuminata_data.csv"))$data), 200)
})

test_that("missing coordinates produce a warning-type message with the count", {
  res <- read_upload_file(fixture_file("upload_sample_manual_15miss_coords.csv"))
  expect_equal(res$status, "success")
  expect_equal(res$message_type, "warning")
  expect_match(res$message, "15 records have invalid coordinates")
  expect_equal(sum(is.na(res$data$Latitude) | is.na(res$data$Longitude)), 15)
})

test_that("missing optional 'issues' column is injected as NA", {
  res <- read_upload_file(fixture_file("upload_sample_manual_no_issues_col.csv"))
  expect_equal(res$status, "success")
  expect_true("issues" %in% names(res$data))
  expect_true(all(is.na(res$data$issues)))
})

test_that("Excel files read the first sheet with all-text coercion", {
  res <- read_upload_file(fixture_file("upload_sample_manual_properly_formatted_excel.xlsx"))
  expect_equal(res$status, "success")
  expect_equal(nrow(res$data), 200)
  expect_match(res$message, "first sheet only")

  res2 <- read_upload_file(fixture_file("upload_sample_manual_properly_formatted_excel_missing_lat.xlsx"))
  expect_equal(res2$status, "success")
  expect_equal(res2$message_type, "warning")
  expect_match(res2$message, "200 records have invalid coordinates")
})

test_that("missing required columns are reported by name and nothing is loaded", {
  res <- read_upload_file(fixture_file("upload_sample_missing_2_cols.csv"))
  expect_true(res$status %in% c("validation_error", "system_error"))
  expect_null(res$data)
  expect_match(res$message, "Missing columns: Locality, Collector")
})

test_that("an unreadable file is caught and returned as a system error", {
  bad <- tempfile(fileext = ".xlsx")
  writeLines("this is not an xlsx", bad)
  res <- read_upload_file(list(name = "bad.xlsx", datapath = bad))
  expect_equal(res$status, "system_error")
  expect_match(res$message, "Critical error reading file")
})
