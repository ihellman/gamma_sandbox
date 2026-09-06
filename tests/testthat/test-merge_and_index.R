canonical_cols <- c(
  "Accession Number", "Taxon Name", "Current Germplasm Type", "Collection Date",
  "Latitude", "Longitude", "Locality", "Collector", "issues", "source", "index"
)

test_that("empty inputs return an empty tibble", {
  expect_equal(nrow(merge_and_index(data.frame(), data.frame())), 0)
  expect_equal(nrow(merge_and_index(NULL, NULL)), 0)
})

test_that("output always has exactly the canonical schema, in order", {
  new <- data.frame(
    `Accession Number` = c("a", "b"), `Taxon Name` = c(" Quercus alba", "Quercus alba "),
    `Current Germplasm Type` = c(" g", "H "), Latitude = c("33.1", "34.2"), Longitude = c("-84", "-85"),
    extra_col = c(1, 2), check.names = FALSE
  )
  out <- merge_and_index(data.frame(), new)
  expect_identical(names(out), canonical_cols)
  expect_type(out$Latitude, "double")
  expect_type(out$`Accession Number`, "character")
  expect_equal(out$`Current Germplasm Type`, c("G", "H"))   # upper-cased and trimmed
  expect_equal(out$`Taxon Name`, c("Quercus alba", "Quercus alba"))
  expect_true(all(is.na(out$issues)))                        # injected
  expect_false("extra_col" %in% names(out))                  # rogue column dropped
  expect_equal(out$index, 1:2)
})

test_that("merging appends rows and re-indexes from 1..n", {
  a <- load_fixture_dataset("upload_sample_small.csv")
  b <- load_fixture_dataset("Magnolia_acuminata_data_small.csv") |> dplyr::mutate(source = "GBIF")
  out <- merge_and_index(a, b)
  expect_equal(nrow(out), nrow(a) + nrow(b))
  expect_equal(out$index, seq_len(nrow(out)))
  expect_equal(sum(out$source == "upload"), nrow(a))
  expect_equal(sum(out$source == "GBIF"), nrow(b))
})
