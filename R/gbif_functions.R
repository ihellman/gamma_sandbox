# GBIF DOWNLOAD PIPELINE --------------------------------------------------------------
# Pure, non-reactive functions behind the "Gather GBIF Occurrences" button.
# The Shiny controls module only wires inputs to these; everything here can be
# unit-tested on a saved response fixture without a network connection.
#
# Pipeline:  gbif_fetch()  ->  gbif_apply_filters()  ->  gbif_select_records()  ->  gbif_to_schema()
# `gbif_gather()` runs the whole chain and reports per-step record counts so the
# UI can show honest "loaded n G / m H" numbers (issue #61).

INAT_DATASET_KEY <- "50c9509d-22c7-4a22-a47d-8c48425ef4a7"
GBIF_POOL_CAP    <- 2000   # hard cap on records requested per query (rgbif pages at 300)

# Columns of the raw occurrence table the pipeline relies on. Anything else is
# dropped after download to keep the cached pool small.
GBIF_RAW_FIELDS <- c(
  "gbifID", "scientificName", "taxonomicStatus", "basisOfRecord", "datasetKey",
  "eventDate", "decimalLatitude", "decimalLongitude", "stateProvince", "recordedBy"
)

# Total / living / iNaturalist record counts with coordinates, as reported by the
# GBIF index (these are BEFORE any of the app-side filters).
gbif_counts <- function(taxon_key, event_date = NULL) {
  count <- function(...) {
    tryCatch(
      rgbif::occ_search(taxonKey = as.numeric(taxon_key), hasCoordinate = TRUE,
                        eventDate = event_date, limit = 0, ...)$meta$count,
      error = function(e) NA_integer_
    )
  }
  list(
    total  = count(),
    living = count(basisOfRecord = "LIVING_SPECIMEN"),
    inat   = count(datasetKey = INAT_DATASET_KEY)
  )
}

# How many records to request for a given slider value. Living specimens are
# fetched separately so that ALL of them (up to the cap) are available for the
# G-first selection; the reference pool is a multiple of the slider so that the
# app-side filters (synonyms, iNat, fossils, dates) still leave enough records.
gbif_pool_limits <- function(limit) {
  limit <- max(0, as.integer(limit))
  list(
    living = min(max(2L * limit, 200L), GBIF_POOL_CAP),
    other  = min(max(5L * limit, 500L), GBIF_POOL_CAP)
  )
}

# Download the raw pool: living specimens first, then an unfiltered pool.
# NOTE the unfiltered query also returns living specimens, so the pool contains
# duplicates by design; gbif_apply_filters() removes them by gbifID.
gbif_fetch <- function(taxon_key, living_limit, other_limit, event_date = NULL, progress = NULL) {
  report <- function(v, d) if (is.function(progress)) progress(v, d)
  get <- function(limit, ...) {
    if (limit <= 0) return(data.frame())
    res <- rgbif::occ_search(
      taxonKey = as.numeric(taxon_key), hasCoordinate = TRUE,
      eventDate = event_date, limit = limit, ...
    )
    if (is.null(res$data) || nrow(res$data) == 0) return(data.frame())
    as.data.frame(res$data)
  }

  report(0.1, "Downloading living specimens...")
  living <- get(living_limit, basisOfRecord = "LIVING_SPECIMEN")
  report(0.3, "Downloading reference records...")
  other <- get(other_limit)

  pool <- dplyr::bind_rows(living, other)
  if (nrow(pool) == 0) return(pool)
  missing <- setdiff(GBIF_RAW_FIELDS, names(pool))
  for (m in missing) pool[[m]] <- NA_character_
  pool <- pool[, GBIF_RAW_FIELDS]
  pool$gbifID <- as.character(pool$gbifID)
  pool
}

# Apply the user's filters and record how many rows survive each step.
gbif_apply_filters <- function(df, include_synonyms = FALSE, exclude_inat = FALSE, date_range = NULL) {
  steps <- list(raw = nrow(df))
  if (nrow(df) == 0) return(list(data = df, steps = steps))

  # Issue #61: the living query and the unfiltered query overlap, so every living
  # specimen appeared twice and was counted twice as "G". Dedupe first.
  df <- dplyr::distinct(df, gbifID, .keep_all = TRUE)
  steps$deduplicated <- nrow(df)

  df <- dplyr::filter(df, !is.na(decimalLatitude), !is.na(decimalLongitude))
  steps$with_coordinates <- nrow(df)

  # GBIF's taxonomicStatus says how the record's *name* matched the backbone.
  # Keeping only ACCEPTED drops records filed under synonyms (and the rare
  # DOUBTFUL / NA matches).
  if (!isTRUE(include_synonyms) && "taxonomicStatus" %in% names(df)) {
    df <- dplyr::filter(df, taxonomicStatus == "ACCEPTED")
    steps$accepted_names_only <- nrow(df)
  }

  if (isTRUE(exclude_inat) && "datasetKey" %in% names(df)) {
    df <- dplyr::filter(df, is.na(datasetKey) | datasetKey != INAT_DATASET_KEY)
    steps$without_inaturalist <- nrow(df)
  }

  if ("basisOfRecord" %in% names(df)) {
    df <- dplyr::filter(df, !basisOfRecord %in% "FOSSIL_SPECIMEN")
    steps$without_fossils <- nrow(df)
  }

  if (!is.null(date_range) && length(date_range) == 2 && !any(is.na(date_range))) {
    start <- as.Date(date_range[1]); end <- as.Date(date_range[2])
    parsed <- suppressWarnings(as.Date(df$eventDate))
    df <- df[!is.na(parsed) & parsed >= start & parsed <= end, , drop = FALSE]
    steps$in_date_range <- nrow(df)
  }

  list(data = df, steps = steps)
}

# Pick at most `limit` records: every living specimen first (G), then reference
# records (H) ordered most-recent-first, randomly, or - when a date range was
# applied - one record per year first so the range is spread evenly.
gbif_select_records <- function(df, limit, random = FALSE, yearly_spread = FALSE) {
  if (nrow(df) == 0 || limit <= 0) return(df[0, , drop = FALSE])
  is_living <- df$basisOfRecord %in% "LIVING_SPECIMEN"
  living <- dplyr::slice_head(df[is_living, , drop = FALSE], n = min(limit, sum(is_living)))
  other  <- df[!is_living, , drop = FALSE]

  remaining <- limit - nrow(living)
  if (remaining <= 0 || nrow(other) == 0) return(living)

  if (isTRUE(yearly_spread)) {
    other$year_val <- suppressWarnings(as.integer(substr(other$eventDate, 1, 4)))
    yearly_unique <- other |> dplyr::group_by(year_val) |> dplyr::slice_head(n = 1) |> dplyr::ungroup()
    remainder <- other |> dplyr::filter(!gbifID %in% yearly_unique$gbifID)
    picked <- dplyr::bind_rows(yearly_unique, remainder) |>
      dplyr::slice_head(n = min(remaining, nrow(other))) |>
      dplyr::select(-year_val)
  } else if (isTRUE(random)) {
    picked <- dplyr::slice_sample(other, n = min(remaining, nrow(other)))
  } else {
    picked <- other |> dplyr::arrange(dplyr::desc(eventDate)) |> dplyr::slice_head(n = min(remaining, nrow(other)))
  }
  dplyr::bind_rows(living, picked)
}

# Map GBIF occurrence columns onto the app's canonical working-dataset schema.
gbif_to_schema <- function(df) {
  if (nrow(df) == 0) return(data.frame())
  get <- function(col) if (col %in% names(df)) as.character(df[[col]]) else rep(NA_character_, nrow(df))
  data.frame(
    `Accession Number`       = get("gbifID"),
    `Taxon Name`             = get("scientificName"),
    `Current Germplasm Type` = ifelse(get("basisOfRecord") %in% "LIVING_SPECIMEN", "G", "H"),
    `Collection Date`        = get("eventDate"),
    Latitude                 = as.numeric(df$decimalLatitude),
    Longitude                = as.numeric(df$decimalLongitude),
    Locality                 = get("stateProvince"),
    Collector                = get("recordedBy"),
    source                   = "GBIF",
    check.names = FALSE
  ) |> dplyr::mutate(index = dplyr::row_number())
}

# Whole pipeline. `pool` lets the caller reuse a previously downloaded raw pool
# (the controls module caches it per taxon so re-gathering with different
# filters or slider values does not hit GBIF again).
gbif_gather <- function(taxon_key, limit, include_synonyms = FALSE, exclude_inat = FALSE,
                        date_range = NULL, random = FALSE, pool = NULL,
                        fetch = gbif_fetch, progress = NULL) {
  report <- function(v, d) if (is.function(progress)) progress(v, d)
  limits <- gbif_pool_limits(limit)
  event_date <- if (!is.null(date_range) && !any(is.na(date_range))) {
    paste(format(as.Date(date_range), "%Y-%m-%d"), collapse = ",")
  } else NULL

  if (is.null(pool)) {
    pool <- fetch(taxon_key, living_limit = limits$living, other_limit = limits$other,
                  event_date = event_date, progress = progress)
  }

  report(0.6, "Applying filters...")
  filtered <- gbif_apply_filters(pool, include_synonyms = include_synonyms,
                                 exclude_inat = exclude_inat, date_range = date_range)
  selected <- gbif_select_records(filtered$data, limit = limit, random = random,
                                  yearly_spread = !is.null(date_range))
  report(0.8, "Formatting downloaded records...")
  data <- gbif_to_schema(selected)

  steps <- filtered$steps
  steps$selected <- nrow(selected)
  list(
    data  = data,
    pool  = pool,
    steps = steps,
    n_g   = sum(data$`Current Germplasm Type` == "G"),
    n_h   = sum(data$`Current Germplasm Type` == "H")
  )
}
