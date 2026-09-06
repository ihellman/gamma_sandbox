# Local implementations of the GapAnalysis package metrics (SRSex / GRSex / ERSex)
# plus the shared score helpers and the pipeline that the "Run Gap Analysis"
# button and the unit tests both call. The package is off CRAN; when it returns
# these can be swapped for the package functions and verified against
# tests/testthat/test-gap_metrics.R.

# --- Static spatial layers --------------------------------------------------------
# Read once per R process (global.R calls these at start-up) instead of on every
# button click. Both are terra SpatVectors in EPSG:4326.
load_land_layer <- function(path = "appData/land_simple.gpkg") {
  terra::vect(path)
}

load_ecoregions_layer <- function(path = "appData/ecoregionsSimplified.gpkg") {
  eco <- terra::vect(path)
  terra::subset(eco, eco$ECO_NAME != "Lake")
}

# Accessors with a lazy fallback so the functions also work outside the app
# (tests, scripts) when global.R has not populated the globals.
gap_land <- function() {
  if (exists("GAP_LAND", envir = globalenv())) get("GAP_LAND", envir = globalenv()) else load_land_layer()
}
gap_ecoregions <- function() {
  if (exists("GAP_ECOREGIONS", envir = globalenv())) get("GAP_ECOREGIONS", envir = globalenv()) else load_ecoregions_layer()
}

# --- Score helpers (used by the app inset AND the report) --------------------------
compute_fcs <- function(srs, grs, ers) {
  vals <- c(srs, grs, ers)
  if (all(is.na(vals))) return(NA_real_)
  mean(vals, na.rm = TRUE)
}

# Priority category thresholds follow the GapAnalysis methodology.
fcs_priority <- function(fcs) {
  if (is.na(fcs)) return(list(label = "Not assessed", code = NA_character_, color = "#dddddd"))
  if (fcs <= 25) {
    list(label = "Urgent Priority (UP)", code = "UP", color = "#ffb4b3")
  } else if (fcs <= 50) {
    list(label = "High Priority (HP)", code = "HP", color = "#ffd380")
  } else if (fcs <= 75) {
    list(label = "Medium Priority (MP)", code = "MP", color = "#ffff80")
  } else {
    list(label = "Low Priority (LP)", code = "LP", color = "#a8d2a8")
  }
}

# Scores table for the bar chart (app inset and report share this shape)
gap_scores_table <- function(srs, grs, ers) {
  fcs <- compute_fcs(srs, grs, ers)
  dplyr::tibble(
    Metric = factor(c("SRS", "GRS", "ERS", "FCS"), levels = c("SRS", "GRS", "ERS", "FCS")),
    Score = as.numeric(c(srs, grs, ers, fcs)),
    Type = c("SRS", "GRS", "ERS", "FCS")
  ) |> dplyr::mutate(Score = round(Score, 1))
}

# Label for a (possibly mixed-taxon) dataset: the distinct names, comma-separated
taxon_label <- function(data, max_names = 3) {
  nms <- unique(trimws(as.character(data$`Taxon Name`)))
  nms <- nms[!is.na(nms) & nms != ""]
  if (length(nms) == 0) return("Taxon")
  if (length(nms) > max_names) {
    return(paste0(paste(nms[seq_len(max_names)], collapse = ", "), " (+", length(nms) - max_names, " more)"))
  }
  paste(nms, collapse = ", ")
}

# Rows usable for spatial work: numeric coordinates, both present.
prep_lat_lon <- function(data) {
  if (is.null(data) || !is.data.frame(data)) return(data.frame())
  if (!all(c("Longitude", "Latitude") %in% names(data))) return(data.frame())
  if (nrow(data) == 0) return(data.frame())
  as.data.frame(data) |>
    dplyr::mutate(Longitude = as.numeric(Longitude), Latitude = as.numeric(Latitude)) |>
    dplyr::filter(!is.na(Longitude), !is.na(Latitude))
}

# --- Metrics --------------------------------------------------------------------
# `taxon = NULL` uses every row (the app analyses one working dataset, which may
# hold GBIF and uploaded records under slightly different name spellings).
generateCounts <- function(taxon = NULL, occurrence_Data) {
  dataThin <- occurrence_Data
  if (!is.null(taxon)) {
    dataThin <- dataThin |> dplyr::filter(`Taxon Name` == taxon)
  }
  label <- if (is.null(taxon)) taxon_label(occurrence_Data) else taxon

  # CHECK: Early return if no data exists for the taxon
  if (nrow(dataThin) == 0) {
    return(data.frame(
      species = label, totalRecords = 0, hasLat = 0, hasLong = 0,
      totalUseful = 0, totalGRecords = 0, totalGUseful = 0,
      totalHRecords = 0, totalHUseful = 0
    ))
  }

  dataThin <- dataThin |>
    dplyr::select(c(
      "Taxon Name",
      "Latitude",
      "Longitude",
      "Current Germplasm Type"
    )) |>
    dplyr::mutate(
      hasLat = !is.na(Latitude) &
        Latitude != "\\N" &
        Latitude != "" &
        !is.null(Latitude) &
        Latitude != "NULL"
    ) |>
    dplyr::mutate(
      hasLong = !is.na(Longitude) &
        Longitude != "\\N" &
        Longitude != "" &
        !is.null(Longitude) &
        Longitude != "NULL"
    ) |>
    dplyr::mutate(hasLatLong = hasLat & hasLong)

  # set column names for counts df
  colNames <- c(
    "species",
    "totalRecords",
    "hasLat",
    "hasLong",
    "totalUseful",
    "totalGRecords",
    "totalGUseful",
    "totalHRecords",
    "totalHUseful"
  )

  # summarize data
  tbl <- dataThin |>
    dplyr::group_by(`Current Germplasm Type`, hasLatLong) |>
    dplyr::summarize(total = dplyr::n(), .groups = "drop")

  # generate counts df
  countsData <- data.frame(matrix(NA, nrow = 1, ncol = 9))
  colnames(countsData) <- colNames

  # assign values to counts df safely
  countsData$species <- label
  countsData$totalRecords <- nrow(dataThin)
  countsData$totalUseful <- sum((subset(tbl, hasLatLong == TRUE))$total)
  countsData$totalGRecords <- sum((subset(tbl, `Current Germplasm Type` == "G"))$total)
  countsData$totalGUseful <- sum((subset(tbl, `Current Germplasm Type` == "G" & hasLatLong == TRUE))$total)
  countsData$totalHRecords <- sum((subset(tbl, `Current Germplasm Type` == "H"))$total)
  countsData$totalHUseful <- sum((subset(tbl, `Current Germplasm Type` == "H" & hasLatLong == TRUE))$total)
  countsData$hasLat <- sum(dataThin$hasLat)
  countsData$hasLong <- sum(dataThin$hasLong)

  return(countsData)
}


SRSex <- function(taxon = NULL, occurrence_Data) {
  # generate the counts data for species
  sp_counts <- generateCounts(taxon = taxon, occurrence_Data = occurrence_Data)

  # CHECKS: Explicit conditional logic for all zero-data scenarios
  if (sp_counts$totalGRecords == 0 && sp_counts$totalHRecords == 0) {
    # No data at all
    srs <- 0
  } else if (sp_counts$totalGRecords >= 1 && sp_counts$totalHRecords == 0) {
    # Only G records exist
    srs <- 100
  } else if (sp_counts$totalGRecords == 0 && sp_counts$totalHRecords >= 1) {
    # Only H records exist
    srs <- 0
  } else {
    # Both H and G exist
    srs <- min(c(100, (sp_counts$totalGRecords / sp_counts$totalHRecords) * 100))
  }

  # create data.frame with output
  out_df <- dplyr::tibble(
    Taxon = sp_counts$species,
    "Total records" = sp_counts$totalRecords,
    "Total with coordinates" = sp_counts$totalUseful,
    "Total G records" = sp_counts$totalGRecords,
    "G records with coordinates" = sp_counts$totalGUseful,
    "Total H records" = sp_counts$totalHRecords,
    "H records with coordinates" = sp_counts$totalHUseful,
    "SRS exsitu" = srs
  )
  return(out_df)
}


GRSex <- function(allBuffers, outsideGBuffers) {
  # CHECK: Handle empty input vectors (0 total records)
  if (is.null(allBuffers) || nrow(allBuffers) == 0) {
    return(dplyr::tibble(
      'Area of model km2' = 0,
      'G buffer areas in model km2' = 0,
      "GRS exsitu" = 0
    ))
  }

  # total area
  totalArea <- allBuffers |>
    terra::aggregate() |>
    terra::expanse(unit = "km")

  # CHECK: Handle cases where there are no gaps (e.g., 0 H records)
  if (is.null(outsideGBuffers) || nrow(outsideGBuffers) == 0) {
    gapArea <- 0
  } else {
    gapArea <- outsideGBuffers |>
      terra::aggregate() |>
      terra::expanse(unit = "km")
  }

  # calculate GRSex score
  difference <- totalArea - gapArea

  if (difference <= 0) {
    grsex_score <- 0
    gArea <- 0
  } else {
    gArea <- totalArea - gapArea
    grsex_score <- (gArea / totalArea) * 100
  }

  out_df <- dplyr::tibble(
    'Area of model km2' = totalArea,
    'G buffer areas in model km2' = gArea,
    "GRS exsitu" = grsex_score
  )
  return(out_df)
}


# `ecoRegions` is the (lake-free) ecoregion SpatVector; defaults to the layer
# loaded once at start-up rather than re-reading the 9 MB file per call.
# The "universe" of ecoregions is every ecoregion containing a record (buffer
# method) or, when `model_area` is given, every ecoregion the range polygon
# overlaps (convex hull method).
ERSex <- function(gapPoints, g_buffer, ecoRegions = gap_ecoregions(), model_area = NULL) {
  # CHECK: Handle case with absolutely no points (no H and no G)
  if (is.null(gapPoints) || nrow(gapPoints) == 0) {
    out_df <- dplyr::tibble(
      `Ecoregions with records` = 0,
      `Ecoregions within G buffer` = 0,
      `ERS exsitu` = 0
    )
    return(list(summary = out_df, spatial = NULL))
  }

  # 1. Define the "Universe"
  if (!is.null(model_area) && nrow(model_area) > 0) {
    inter_points <- terra::intersect(x = terra::project(model_area, terra::crs(ecoRegions)), y = ecoRegions)
  } else {
    inter_points <- terra::intersect(x = gapPoints, y = ecoRegions)
  }

  # CHECK: Handle case where points exist but fall outside known ecoregions
  if (nrow(inter_points) == 0) {
     out_df <- dplyr::tibble(
       `Ecoregions with records` = 0,
       `Ecoregions within G buffer` = 0,
       `ERS exsitu` = 0
     )
     return(list(summary = out_df, spatial = NULL))
  }

  ecoCodes <- unique(inter_points$ECO_NAME)
  currentEcos <- ecoRegions[ecoRegions$ECO_NAME %in% ecoCodes, ]

  # 2. Determine Conserved Ecoregions (Overlap with Germplasm Buffer)
  # CHECK: Handle case with no G buffer (0 G points)
  if (is.null(g_buffer) || nrow(g_buffer) == 0) {
    conserved_codes <- character(0)
    conserved_count <- 0
  } else {
    g_buffer_proj <- terra::project(g_buffer, terra::crs(currentEcos))
    gEco <- terra::intersect(x = g_buffer_proj, y = currentEcos)
    conserved_codes <- unique(gEco$ECO_NAME)
    conserved_count <- length(conserved_codes)
  }

  # 3. Calculate Metrics
  total_count <- length(ecoCodes)

  # ERS = Percentage of ecoregions covered
  ers_score <- (conserved_count / total_count) * 100

  # 4. Create Summary DataFrame
  out_df <- dplyr::tibble(
    `Ecoregions with records` = total_count,
    `Ecoregions within G buffer` = conserved_count,
    `ERS exsitu` = round(ers_score, 2)
  )

  # 5. Create Spatial Object with Status
  currentEcos$gap_status <- ifelse(
    currentEcos$ECO_NAME %in% conserved_codes,
    "Covered",
    "Gap Ecoregion"
  )

  out_spatial <- currentEcos[, c("ECO_NAME", "gap_status")]

  # 6. Return List
  return(list(
    summary = out_df,
    spatial = out_spatial
  ))
}


# --- Range model: convex hull ----------------------------------------------------
# Convex hull around every record with coordinates, clipped to land. Needs at
# least three distinct, non-collinear locations to enclose an area.
convex_hull_range <- function(v, land_proj) {
  coords <- unique(terra::crds(v))
  if (nrow(coords) < 3) {
    stop("The convex hull method needs at least 3 records at distinct locations; use the buffer method instead.")
  }
  hull <- terra::convHull(v)
  if (terra::geomtype(hull) != "polygons" || terra::expanse(hull, unit = "km") <= 0) {
    stop("The records are collinear, so a convex hull has no area; use the buffer method instead.")
  }
  hull$processing_type <- "range"
  clipped <- terra::intersect(hull, land_proj)
  if (length(clipped) == 0) stop("The convex hull does not overlap any land area.")
  terra::aggregate(clipped)
}

# --- Whole pipeline ------------------------------------------------------------
# Everything the "Run Gap Analysis" button does, without Shiny. Returns the three
# metric tables, the sf layers drawn on the map and passed to the report, the
# rows that were actually analysed, and the derived FCS / priority.
#
# method = "buffer": the range is the union of `dist_km` buffers around every
#   record (H and G); G buffers are the conserved area (original behaviour).
# method = "hull":   the range is the convex hull of all records, clipped to
#   land; G buffers (still `dist_km`) clipped to the hull are the conserved area,
#   and ERS counts the ecoregions the hull overlaps.
# `progress(value, detail)` is an optional callback for withProgress.
run_gap_analysis <- function(all_data, dist_km, method = c("buffer", "hull"),
                             land = gap_land(), ecoRegions = gap_ecoregions(),
                             progress = NULL) {
  method <- match.arg(method)
  report <- function(v, d) if (is.function(progress)) progress(v, d)
  stopifnot(is.data.frame(all_data), nrow(all_data) > 0, is.numeric(dist_km), dist_km > 0)

  data <- prep_lat_lon(all_data)
  if (nrow(data) == 0) stop("No records with valid coordinates to analyse.")
  target_col <- "Current Germplasm Type"

  report(0.1, "Calculating SRSex...")
  srsMetrics <- SRSex(taxon = NULL, occurrence_Data = all_data)

  report(0.3, "Buffering points...")
  df_base <- data
  df_base$processing_type <- df_base[[target_col]]
  v <- terra::vect(df_base, geom = c("Longitude", "Latitude"), crs = "EPSG:4326")
  v_buffer <- terra::buffer(v, width = dist_km * 1000)

  report(0.4, "Clipping to land...")
  land_proj <- terra::project(land, terra::crs(v_buffer))
  v_clipped <- terra::intersect(v_buffer, land_proj)
  gBuff <- v_clipped[v_clipped$processing_type == "G", ]
  hBuff <- v_clipped[v_clipped$processing_type == "H", ]

  report(0.5, "Calculating GRSex...")
  sf_model <- NULL
  if (method == "hull") {
    model <- convex_hull_range(v, land_proj)
    if (length(gBuff) > 0) gBuff <- terra::intersect(gBuff, model)     # conserved area within the range
    grsMap_element <- if (length(gBuff) == 0) model else terra::erase(x = model, y = gBuff)
    grsMetrics <- GRSex(allBuffers = model, outsideGBuffers = grsMap_element)
    sf_model <- sf::st_as_sf(model) |> sf::st_make_valid()
    drawn <- gBuff
  } else {
    model <- NULL
    grsMap_element <- if (length(hBuff) == 0 || length(gBuff) == 0) hBuff else terra::erase(x = hBuff, y = gBuff)
    grsMetrics <- GRSex(allBuffers = v_clipped, outsideGBuffers = grsMap_element)
    drawn <- v_clipped
  }

  report(0.6, "Calculating ERSex...")
  ersMetrics <- ERSex(gapPoints = v, g_buffer = gBuff, ecoRegions = ecoRegions, model_area = model)

  report(0.75, "Preparing visualization...")
  sf_buffers <- if (length(drawn) > 0) {
    sf::st_as_sf(drawn) |>
      sf::st_make_valid() |>
      dplyr::group_by(processing_type) |>
      dplyr::summarize(geometry = sf::st_union(geometry), .groups = "drop")
  } else {
    sf::st_sf(processing_type = character(0), geometry = sf::st_sfc(crs = 4326))
  }
  sf_grs_gap <- if (length(grsMap_element) > 0) sf::st_as_sf(terra::aggregate(terra::makeValid(grsMap_element))) else NULL
  sf_ers_regions <- if (!is.null(ersMetrics$spatial)) sf::st_as_sf(ersMetrics$spatial) else NULL

  srs <- srsMetrics[["SRS exsitu"]]
  grs <- grsMetrics[["GRS exsitu"]]
  ers <- ersMetrics$summary[["ERS exsitu"]]
  fcs <- compute_fcs(srs, grs, ers)

  list(
    taxon = taxon_label(all_data),
    dist_km = dist_km,
    method = method,
    sf_model = sf_model,           # convex hull range (NULL for the buffer method)
    points = data,                 # exactly the rows used for buffering / ERS
    srs = srsMetrics, grs = grsMetrics, ers = ersMetrics,
    fcs = fcs, priority = fcs_priority(fcs),
    scores = gap_scores_table(srs, grs, ers),
    sf_buffers = sf_buffers, sf_grs_gap = sf_grs_gap, sf_ers_regions = sf_ers_regions
  )
}
