test_that("truncate_label shortens long strings and leaves short/NA alone", {
  long <- strrep("x", 300)
  out <- truncate_label(c(long, "short", NA))
  expect_equal(nchar(out[1]), 140)
  expect_true(grepl("\u2026$", out[1]))
  expect_equal(out[2], "short")
  expect_true(is.na(out[3]))
})

test_that("point_labels escapes HTML and truncates free text (#40)", {
  d <- data.frame(`Taxon Name` = "<b>Q</b>", `Current Germplasm Type` = "G",
                  Collector = "A & B", Locality = strrep("L", 500), check.names = FALSE)
  lab <- as.character(point_labels(d)[[1]])
  expect_false(grepl("<b>Q</b>", lab, fixed = TRUE))
  expect_true(grepl("&amp; B", lab, fixed = TRUE))
  expect_lt(nchar(lab), 400)
})

test_that("palette globals used by DT, maps and the report are defined", {
  expect_length(gbifColor, 2); expect_length(uploadColor, 2); expect_length(combinedColor, 2)
  expect_length(ersexColors, 2); expect_length(grsexColor, 1)
})
