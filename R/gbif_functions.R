# GBIF DOWNLOAD PIPELINE --------------------------------------------------------------
# Pure, non-reactive functions behind the "Gather GBIF Occurrences" button.
# The Shiny controls module only wires inputs to these; everything here can be
# unit-tested on a saved response fixture without a network connection.
#
# Two download modes, chosen by gbif_gather():
#
#   standard  (no advanced option set)  gbif_fetch_standard()
#       Ask GBIF for exactly what the slider needs: every living specimen (up to
#       the slider), then the shortfall in non-living, non-fossil records, in
#       GBIF's own order. Fossils and living specimens are excluded by the query
#       itself, so no oversampling and no app-side de-duplication is needed.
#       Bare minimum of records and requests -> fastest.
#
#   advanced  (date filter / iNaturalist exclusion / a reference-selection
#              method other than GBIF order)  gbif_fetch()
#       Download an oversampled pool (gbif_pool_limits), filter and sample it on
#       the app side. Slower, but the app-side filters and selection methods
#       need candidates to choose from.
#
# Either way:  fetch  ->  gbif_apply_filters()  ->  gbif_select_records()  ->  gbif_to_schema()
# `gbif_gather()` runs the whole chain and reports per-step record counts so the
# UI can show honest "loaded n G / m H" numbers (issue #61).

INAT_DATASET_KEY <- "50c9509d-22c7-4a22-a47d-8c48425ef4a7"
GBIF_POOL_CAP    <- 2000   # hard cap on records requested per advanced-mode query (rgbif pages at 300)

# Every GBIF basisOfRecord value that yields a reference (H) record: everything
# except LIVING_SPECIMEN (those are G) and FOSSIL_SPECIMEN (never wanted).
# GBIF accepts several values in one request when they are ";"-separated.
GBIF_REFERENCE_BASES <- c(
  "PRESERVED_SPECIMEN", "HUMAN_OBSERVATION", "MATERIAL_SAMPLE", "OCCURRENCE",
  "MATERIAL_CITATION", "MACHINE_OBSERVATION", "OBSERVATION"
)

# Columns of the raw occurrence table the pipeline relies on. Anything else is
# dropped after download to keep the cached pool small.
GBIF_RAW_FIELDS <- c(
  "gbifID", "scientificName", "acceptedScientificName", "verbatimScientificName",
  "genus", "specificEpithet", "infraspecificEpithet", "taxonRank", "taxonomicStatus",
  "basisOfRecord", "datasetKey", "eventDate", "decimalLatitude", "decimalLongitude",
  "stateProvince", "recordedBy"
)

# GBIF's interpreted name for each record, as a canonical string
# ("Genus epithet [infraepithet]", no authorship, no rank marker) built from the
# interpreted genus / specificEpithet / infraspecificEpithet fields. This is the
# same form as `canonicalName` in appData/plant_taxonomy_lean.parquet.
gbif_record_canonical_name <- function(df) {
  get <- function(col) if (col %in% names(df)) as.character(df[[col]]) else rep(NA_character_, nrow(df))
  parts <- cbind(get("genus"), get("specificEpithet"), get("infraspecificEpithet"))
  parts[is.na(parts) | parts == ""] <- NA
  apply(parts, 1, function(p) {
    p <- p[!is.na(p)]
    if (length(p) == 0) NA_character_ else paste(p, collapse = " ")
  })
}

# Records that GBIF assigned to the requested taxon. `taxon_name` is the
# canonical name selected in the sidebar. A species selection accepts its
# infraspecific taxa (a record of "Magnolia acuminata subcordata" still belongs
# to Magnolia acuminata); an infraspecific selection requires the full name.
gbif_name_matches <- function(df, taxon_name) {
  canon <- gbif_record_canonical_name(df)
  target <- trimws(taxon_name)
  n_words <- length(strsplit(target, "\\s+")[[1]])
  if (n_words <= 2) {
    !is.na(canon) & (canon == target | startsWith(canon, paste0(target, " ")))
  } else {
    !is.na(canon) & canon == target
  }
}

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

# Reduce a raw occ_search() table to GBIF_RAW_FIELDS (adding any that are
# missing) with a character gbifID.
gbif_standardise <- function(pool) {
  if (nrow(pool) == 0) return(pool)
  missing <- setdiff(GBIF_RAW_FIELDS, names(pool))
  for (m in missing) pool[[m]] <- NA_character_
  pool <- pool[, GBIF_RAW_FIELDS]
  pool$gbifID <- as.character(pool$gbifID)
  pool
}

# Is this request one the standard (exact) download can serve? Anything that
# needs candidates to filter or sample on the app side forces the advanced pool.
gbif_is_standard_request <- function(exclude_inat = FALSE, date_range = NULL, method = "gbif") {
  !isTRUE(exclude_inat) && is.null(date_range) && identical(method, "gbif")
}

# STANDARD download: exactly `limit` records in the fewest requests.
#   1. living specimens, up to `limit` (the query itself excludes fossils)
#   2. the shortfall (limit - living) in reference records, requested with
#      basisOfRecord restricted to GBIF_REFERENCE_BASES so neither living
#      specimens nor fossils come back
# `keep(df)` is an optional row predicate (name match / synonym filter). Rows it
# rejects are replaced by paging further with `start`, at most `max_rounds`
# requests per query, so the result still hits `limit` when GBIF has enough.
# `occ` is the rgbif search function (injectable for offline tests).
gbif_fetch_standard <- function(taxon_key, limit, keep = NULL, occ = rgbif::occ_search,
                                progress = NULL, max_rounds = 4L) {
  report <- function(v, d) if (is.function(progress)) progress(v, d)
  limit <- max(0L, as.integer(limit))
  requests <- 0L

  pull <- function(target, basis) {
    got <- data.frame(); start <- 0L; rounds <- 0L
    while (nrow(got) < target && rounds < max_rounds) {
      rounds <- rounds + 1L; requests <<- requests + 1L
      res <- occ(taxonKey = as.numeric(taxon_key), hasCoordinate = TRUE,
                 basisOfRecord = basis, limit = target - nrow(got), start = start)
      page <- if (is.null(res$data) || nrow(res$data) == 0) data.frame() else as.data.frame(res$data)
      if (nrow(page) == 0) break
      start <- start + nrow(page)
      page <- gbif_standardise(page)
      if (is.function(keep)) page <- page[keep(page), , drop = FALSE]
      got <- dplyr::bind_rows(got, page)
      total <- res$meta$count
      if (!is.null(total) && !is.na(total) && start >= total) break   # GBIF has no more
    }
    got
  }

  living <- data.frame(); other <- data.frame()
  if (limit > 0) {
    report(0.1, "Downloading living specimens...")
    living <- pull(limit, "LIVING_SPECIMEN")
    shortfall <- limit - nrow(living)
    if (shortfall > 0) {
      report(0.3, sprintf("Downloading %d reference records...", shortfall))
      other <- pull(shortfall, paste(GBIF_REFERENCE_BASES, collapse = ";"))
    }
  }
  pool <- dplyr::bind_rows(living, other)
  attr(pool, "requests") <- requests
  pool
}

# ADVANCED download: an oversampled raw pool - living specimens first, then an
# unfiltered pool. NOTE the unfiltered query also returns living specimens, so
# the pool contains duplicates by design; gbif_apply_filters() removes them by
# gbifID.
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

  gbif_standardise(dplyr::bind_rows(living, other))
}

# Apply the user's filters and record how many rows survive each step.
gbif_apply_filters <- function(df, include_synonyms = FALSE, exclude_inat = FALSE, date_range = NULL,
                               taxon_name = NULL, require_name_match = TRUE) {
  steps <- list(raw = nrow(df))
  if (nrow(df) == 0) return(list(data = df, steps = steps))

  # Issue #61: the living query and the unfiltered query overlap, so every living
  # specimen appeared twice and was counted twice as "G". Dedupe first.
  df <- dplyr::distinct(df, gbifID, .keep_all = TRUE)
  steps$deduplicated <- nrow(df)

  df <- dplyr::filter(df, !is.na(decimalLatitude), !is.na(decimalLongitude))
  steps$with_coordinates <- nrow(df)

  # The taxonKey query returns whatever GBIF's backbone matching assigned to the
  # key. Check the record's own interpreted scientific name against the name
  # that was selected, so records GBIF re-assigned (fuzzy or higher-rank matches)
  # are dropped rather than silently attributed to this taxon.
  if (isTRUE(require_name_match) && !is.null(taxon_name) && nzchar(taxon_name)) {
    df <- df[gbif_name_matches(df, taxon_name), , drop = FALSE]
    steps$scientific_name_matches <- nrow(df)
  }

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

# Spatially spread subset (issue #58): deterministic farthest-point sampling.
# Start from the most recent record, then repeatedly add the record farthest
# from everything already chosen. Longitude is scaled by cos(latitude) so
# distances are roughly equal-area at any latitude. Records at locations that
# are already represented (distance 0) are only used to fill leftover slots, in
# most-recent order.
select_spatially_spread <- function(df, n) {
  if (nrow(df) <= n) return(df)
  df <- dplyr::arrange(df, dplyr::desc(eventDate))
  lat <- as.numeric(df$decimalLatitude)
  lon <- as.numeric(df$decimalLongitude)
  x <- lon * cos(mean(lat, na.rm = TRUE) * pi / 180)
  y <- lat
  ok <- !is.na(x) & !is.na(y)

  chosen <- 1L
  min_d <- (x - x[1])^2 + (y - y[1])^2
  min_d[!ok] <- -Inf
  min_d[1] <- -Inf
  while (length(chosen) < n) {
    j <- which.max(min_d)
    if (!is.finite(min_d[j]) || min_d[j] <= 0) break     # every remaining location is already covered
    chosen <- c(chosen, j)
    min_d <- pmin(min_d, (x - x[j])^2 + (y - y[j])^2)
    min_d[j] <- -Inf
  }
  if (length(chosen) < n) {
    chosen <- c(chosen, setdiff(seq_len(nrow(df)), chosen)[seq_len(n - length(chosen))])
  }
  df[chosen, , drop = FALSE]
}

# Pick at most `limit` records: every living specimen first (G), then reference
# records (H) chosen by `method` - "gbif" (the order GBIF returned them, which
# is what the standard download gives), "recent" (most recent eventDate first),
# "random", or "spatial" (see select_spatially_spread). When a date range was
# applied, one record per year is taken first so the range is spread evenly.
gbif_select_records <- function(df, limit, method = c("gbif", "recent", "random", "spatial"),
                                yearly_spread = FALSE, random = NULL) {
  if (isTRUE(random)) method <- "random"          # backwards compatibility
  method <- match.arg(method)
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
  } else if (method == "random") {
    picked <- dplyr::slice_sample(other, n = min(remaining, nrow(other)))
  } else if (method == "spatial") {
    picked <- select_spatially_spread(other, n = min(remaining, nrow(other)))
  } else if (method == "recent") {
    picked <- other |> dplyr::arrange(dplyr::desc(eventDate)) |> dplyr::slice_head(n = min(remaining, nrow(other)))
  } else {
    picked <- dplyr::slice_head(other, n = min(remaining, nrow(other)))
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

# Whole pipeline. `mode` "auto" picks the standard (exact) download unless an
# advanced option forces the oversampled pool (see gbif_is_standard_request).
# `pool` lets the caller reuse a previously downloaded raw pool (the controls
# module caches it so re-gathering with the same request does not hit GBIF
# again).
gbif_gather <- function(taxon_key, limit, include_synonyms = FALSE, exclude_inat = FALSE,
                        date_range = NULL, method = "gbif", pool = NULL,
                        taxon_name = NULL, require_name_match = TRUE,
                        mode = c("auto", "standard", "advanced"),
                        fetch = gbif_fetch, fetch_standard = gbif_fetch_standard,
                        progress = NULL, random = NULL) {
  if (isTRUE(random)) method <- "random"          # backwards compatibility
  mode <- match.arg(mode)
  if (mode == "auto") {
    mode <- if (gbif_is_standard_request(exclude_inat, date_range, method)) "standard" else "advanced"
  }
  report <- function(v, d) if (is.function(progress)) progress(v, d)
  event_date <- if (!is.null(date_range) && !any(is.na(date_range))) {
    paste(format(as.Date(date_range), "%Y-%m-%d"), collapse = ",")
  } else NULL

  # The app-side filters that apply in BOTH modes. In standard mode they run on
  # each downloaded page so rejected rows are replaced before the download ends.
  filter_pool <- function(df) {
    gbif_apply_filters(df, include_synonyms = include_synonyms,
                       exclude_inat = exclude_inat, date_range = date_range,
                       taxon_name = taxon_name, require_name_match = require_name_match)
  }

  if (is.null(pool)) {
    if (mode == "standard") {
      pool <- fetch_standard(taxon_key, limit = limit, progress = progress,
                             keep = function(df) df$gbifID %in% filter_pool(df)$data$gbifID)
    } else {
      limits <- gbif_pool_limits(limit)
      pool <- fetch(taxon_key, living_limit = limits$living, other_limit = limits$other,
                    event_date = event_date, progress = progress)
    }
  }

  report(0.6, "Applying filters...")
  filtered <- filter_pool(pool)
  selected <- gbif_select_records(filtered$data, limit = limit, method = method,
                                  yearly_spread = !is.null(date_range))
  report(0.8, "Formatting downloaded records...")
  data <- gbif_to_schema(selected)

  steps <- filtered$steps
  steps$selected <- nrow(selected)
  list(
    data  = data,
    pool  = pool,
    steps = steps,
    mode  = mode,
    n_g   = sum(data$`Current Germplasm Type` == "G"),
    n_h   = sum(data$`Current Germplasm Type` == "H")
  )
}
