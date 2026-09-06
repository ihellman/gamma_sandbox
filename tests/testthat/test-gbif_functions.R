# Offline tests of the GBIF pipeline on a saved response pool
# (tests/testthat/fixtures/gbif_magnolia_fraseri.rds, see make_gbif_fixture.R).
pool <- readRDS(fixture_path("gbif_magnolia_fraseri.rds"))
api_counts <- attr(pool, "api_counts")
TAXON <- attr(pool, "canonical_name")

# A pool with the issue-#61 shape: every living specimen present twice.
duplicated_pool <- dplyr::bind_rows(pool, pool[pool$basisOfRecord == "LIVING_SPECIMEN", ])

test_that("fixture has the expected shape", {
  expect_true(all(GBIF_RAW_FIELDS %in% names(pool)))
  expect_gt(nrow(pool), 500)
  expect_equal(sum(pool$basisOfRecord == "LIVING_SPECIMEN"), api_counts$living)
})

test_that("gbif_pool_limits scales with the slider and is capped", {
  expect_equal(gbif_pool_limits(200), list(living = 400, other = 1000))
  expect_equal(gbif_pool_limits(1000), list(living = 2000, other = 2000))
  expect_equal(gbif_pool_limits(0), list(living = 200, other = 500))
})

test_that("filters de-duplicate by gbifID so G is never double counted (#61)", {
  f <- gbif_apply_filters(duplicated_pool)
  expect_equal(f$steps$raw, nrow(duplicated_pool))
  expect_equal(f$steps$deduplicated, nrow(pool))
  expect_false(any(duplicated(f$data$gbifID)))
  expect_lte(sum(f$data$basisOfRecord == "LIVING_SPECIMEN"), api_counts$living)
})

test_that("record canonical names are built from GBIF's interpreted name fields", {
  d <- data.frame(genus = c("Magnolia", "Magnolia", NA), specificEpithet = c("fraseri", "fraseri", NA),
                  infraspecificEpithet = c(NA, "pyramidata", NA))
  expect_equal(gbif_record_canonical_name(d), c("Magnolia fraseri", "Magnolia fraseri pyramidata", NA))
  expect_equal(gbif_record_canonical_name(data.frame(genus = "Quercus")), "Quercus")
})

test_that("scientific-name matching keeps the selected taxon and its infraspecifics only", {
  canon <- gbif_record_canonical_name(pool)
  m <- gbif_name_matches(pool, TAXON)
  expect_true(all(startsWith(canon[m], TAXON)))
  expect_true(any(canon[m] == "Magnolia fraseri pyramidata"))     # variety of the species is kept
  expect_false(any(canon[m] == "Magnolia pyramidata"))            # a different (synonym) name is not
  expect_equal(sum(!m), sum(canon == "Magnolia pyramidata"))
  v <- gbif_name_matches(pool, "Magnolia fraseri pyramidata")     # infraspecific selection is exact
  expect_true(all(canon[v] == "Magnolia fraseri pyramidata"))
  expect_false(any(gbif_name_matches(pool, "Magnolia grandiflora")))
})

test_that("the name-match filter is on by default and reported as a step", {
  f <- gbif_apply_filters(pool, taxon_name = TAXON, include_synonyms = TRUE)
  expect_equal(f$steps$scientific_name_matches, sum(gbif_name_matches(pool, TAXON)))
  off <- gbif_apply_filters(pool, taxon_name = TAXON, include_synonyms = TRUE, require_name_match = FALSE)
  expect_null(off$steps$scientific_name_matches)
  expect_gt(nrow(off$data), nrow(f$data))
  none <- gbif_apply_filters(pool, taxon_name = NULL)              # no name known -> no filtering
  expect_null(none$steps$scientific_name_matches)
})

test_that("synonym toggle changes the record count and default excludes them", {
  with_syn <- gbif_apply_filters(pool, include_synonyms = TRUE)
  without  <- gbif_apply_filters(pool, include_synonyms = FALSE)
  n_syn <- sum(pool$taxonomicStatus != "ACCEPTED")
  expect_gt(n_syn, 0)
  expect_equal(nrow(without$data), nrow(with_syn$data) - n_syn)
  expect_true(all(without$data$taxonomicStatus == "ACCEPTED"))
  expect_null(with_syn$steps$accepted_names_only)
})

test_that("iNaturalist, fossil and date filters work", {
  inat <- gbif_apply_filters(pool, exclude_inat = TRUE)
  expect_false(any(inat$data$datasetKey %in% INAT_DATASET_KEY))
  expect_equal(inat$steps$without_inaturalist, nrow(inat$data))

  fossil_pool <- pool
  fossil_pool$basisOfRecord[1:3] <- "FOSSIL_SPECIMEN"
  expect_false(any(gbif_apply_filters(fossil_pool)$data$basisOfRecord == "FOSSIL_SPECIMEN"))

  dated <- gbif_apply_filters(pool, date_range = as.Date(c("2015-01-01", "2018-12-31")))
  yrs <- as.integer(substr(dated$data$eventDate, 1, 4))
  expect_true(all(yrs >= 2015 & yrs <= 2018))
  expect_equal(dated$steps$in_date_range, nrow(dated$data))
})

test_that("selection takes every living specimen first, then most recent references, up to the limit", {
  f <- gbif_apply_filters(pool)$data
  sel <- gbif_select_records(f, limit = 50)
  expect_equal(nrow(sel), 50)
  expect_equal(sum(sel$basisOfRecord == "LIVING_SPECIMEN"), sum(f$basisOfRecord == "LIVING_SPECIMEN"))
  others <- sel[sel$basisOfRecord != "LIVING_SPECIMEN", ]
  expect_equal(others$eventDate, sort(others$eventDate, decreasing = TRUE, na.last = TRUE))
  expect_equal(nrow(gbif_select_records(f, limit = 0)), 0)
  expect_equal(nrow(gbif_select_records(f, limit = 1e6)), nrow(f))
  # a limit smaller than the number of living specimens still returns exactly `limit`
  n_liv <- sum(f$basisOfRecord == "LIVING_SPECIMEN")
  expect_equal(nrow(gbif_select_records(f, limit = n_liv - 1)), n_liv - 1)
})

test_that("random and yearly-spread selection return the right number of rows", {
  f <- gbif_apply_filters(pool)$data
  r <- gbif_select_records(f, limit = 40, method = "random")
  expect_equal(nrow(r), 40)
  expect_equal(nrow(gbif_select_records(f, limit = 40, random = TRUE)), 40)   # legacy flag
  y <- gbif_select_records(f, limit = 40, yearly_spread = TRUE)
  expect_equal(nrow(y), 40)
  expect_false("year_val" %in% names(y))
  # yearly spread: reference records cover every year available in the pool
  # (or one per selected row if there are more years than slots)
  h_pool <- f[f$basisOfRecord != "LIVING_SPECIMEN", ]
  years_available <- length(unique(substr(h_pool$eventDate, 1, 4)))
  years_selected  <- length(unique(substr(y$eventDate[y$basisOfRecord != "LIVING_SPECIMEN"], 1, 4)))
  expect_equal(years_selected, min(years_available, sum(y$basisOfRecord != "LIVING_SPECIMEN")))
})

test_that("gbif_to_schema maps onto the canonical working-dataset columns", {
  d <- gbif_to_schema(pool[1:20, ])
  expect_equal(names(d), c("Accession Number", "Taxon Name", "Current Germplasm Type", "Collection Date",
                           "Latitude", "Longitude", "Locality", "Collector", "source", "index"))
  expect_true(all(d$`Current Germplasm Type` %in% c("G", "H")))
  expect_true(all(d$source == "GBIF"))
  expect_equal(d$index, 1:20)
  expect_equal(nrow(gbif_to_schema(pool[0, ])), 0)
  merged <- merge_and_index(data.frame(), d)
  expect_equal(nrow(merged), 20)
})

test_that("gbif_gather runs the whole chain from a pool and reports honest counts", {
  fake_fetch <- function(...) stop("network must not be used when a pool is supplied")
  g <- gbif_gather(3153619, limit = 100, pool = duplicated_pool, fetch = fake_fetch)
  expect_equal(nrow(g$data), 100)
  expect_equal(g$n_g, api_counts$living)          # the #61 acceptance criterion
  expect_equal(g$n_g + g$n_h, 100)
  expect_equal(g$steps$selected, 100)
  expect_identical(g$pool, duplicated_pool)
})

test_that("gbif_gather fetches when no pool is supplied and passes the pool limits through", {
  seen <- NULL
  fake_fetch <- function(taxon_key, living_limit, other_limit, event_date = NULL, progress = NULL) {
    seen <<- list(taxon_key = taxon_key, living_limit = living_limit, other_limit = other_limit, event_date = event_date)
    pool
  }
  g <- gbif_gather(3153619, limit = 200, date_range = as.Date(c("1900-01-01", "2030-12-31")), fetch = fake_fetch)
  expect_equal(seen$taxon_key, 3153619)
  expect_equal(seen$living_limit, 400)
  expect_equal(seen$other_limit, 1000)
  expect_equal(seen$event_date, "1900-01-01,2030-12-31")
  expect_equal(nrow(g$data), 200)

  empty <- gbif_gather(1, limit = 200, fetch = function(...) data.frame())
  expect_equal(nrow(empty$data), 0)
  expect_equal(empty$steps$raw, 0)
})

# Synthetic clustered pool for the spatial-spread selection (#58)
make_clustered_pool <- function(n_clusters = 6, per_cluster = 25, seed = 42) {
  set.seed(seed)
  centres <- data.frame(lat = runif(n_clusters, 25, 48), lon = runif(n_clusters, -120, -70))
  rows <- do.call(rbind, lapply(seq_len(n_clusters), function(i) data.frame(
    gbifID = paste0(i, "_", seq_len(per_cluster)),
    decimalLatitude = centres$lat[i] + rnorm(per_cluster, sd = 0.05),
    decimalLongitude = centres$lon[i] + rnorm(per_cluster, sd = 0.05),
    eventDate = sprintf("%d-06-01", sample(1990:2024, per_cluster, replace = TRUE)),
    basisOfRecord = "PRESERVED_SPECIMEN", cluster = i, stringsAsFactors = FALSE
  )))
  rows
}
mean_nn_distance <- function(df) {
  m <- as.matrix(dist(cbind(df$decimalLongitude, df$decimalLatitude)))
  diag(m) <- Inf
  mean(apply(m, 1, min))
}

test_that("select_spatially_spread covers every spatial cluster, starting from the newest record (#58)", {
  p <- make_clustered_pool()
  s <- select_spatially_spread(p, n = 6)
  expect_equal(nrow(s), 6)
  expect_equal(sort(s$cluster), 1:6)                       # every cluster represented once
  expect_equal(s$eventDate[1], max(p$eventDate))           # seeded with the most recent record
  expect_identical(select_spatially_spread(p, n = 1000), p)  # nothing to thin
  expect_equal(nrow(select_spatially_spread(p, n = 40)), 40) # more slots than clusters still fills up
  expect_false(any(duplicated(select_spatially_spread(p, n = 40)$gbifID)))

  # duplicate locations only fill leftover slots
  dup <- p[rep(1, 10), ]; dup$gbifID <- paste0("d", 1:10)
  s2 <- select_spatially_spread(rbind(p, dup), n = 6)
  expect_equal(sort(s2$cluster), 1:6)
})

test_that("spatially spread selection is more dispersed than random or most-recent", {
  p <- make_clustered_pool()
  spatial <- select_spatially_spread(p, n = 6)
  recent  <- p |> dplyr::arrange(dplyr::desc(eventDate)) |> dplyr::slice_head(n = 6)
  set.seed(7); random <- p[sample(nrow(p), 6), ]
  expect_gt(mean_nn_distance(spatial), mean_nn_distance(random))
  expect_gte(mean_nn_distance(spatial), mean_nn_distance(recent))
})

test_that("gbif_select_records(method = 'spatial') keeps G first and honours the limit", {
  p <- make_clustered_pool()
  p$basisOfRecord[1:3] <- "LIVING_SPECIMEN"
  s <- gbif_select_records(p, limit = 9, method = "spatial")
  expect_equal(nrow(s), 9)
  expect_equal(sum(s$basisOfRecord == "LIVING_SPECIMEN"), 3)
  expect_equal(length(unique(s$cluster[s$basisOfRecord != "LIVING_SPECIMEN"])), 6)
  expect_error(gbif_select_records(p, limit = 5, method = "nope"))
})
