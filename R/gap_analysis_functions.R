# ideally we would just be sourcing in the gap r library for these function but going to wait till it's back on cran to do so

generateCounts <- function(taxon, occurrence_Data) {
  # define presence of usable lat long values
  dataThin <- occurrence_Data |>
    dplyr::filter(`Taxon Name` == taxon) 
    
  # CHECK: Early return if no data exists for the taxon
  if (nrow(dataThin) == 0) {
    return(data.frame(
      species = taxon, totalRecords = 0, hasLat = 0, hasLong = 0,
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
  countsData$species <- taxon
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


SRSex <- function(taxon, occurrence_Data) {
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


ERSex <- function(gapPoints, g_buffer) {
  # CHECK: Handle case with absolutely no points (no H and no G)
  if (is.null(gapPoints) || nrow(gapPoints) == 0) {
    out_df <- dplyr::tibble(
      `Ecoregions with records` = 0,
      `Ecoregions within G buffer` = 0,
      `ERS exsitu` = 0
    )
    return(list(summary = out_df, spatial = NULL))
  }

  # 1. Load Data
  ecoRegions <- terra::vect("appData/ecoregionsSimplified.gpkg")
  ecoRegions <- terra::subset(ecoRegions, ecoRegions$ECO_NAME != "Lake")

  # 2. Define the "Universe" (Ecoregions containing any record)
  inter_points <- terra::intersect(x = gapPoints, y = ecoRegions)
  
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

  # 3. Determine Conserved Ecoregions (Overlap with Germplasm Buffer)
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

  # 4. Calculate Metrics
  total_count <- length(ecoCodes)

  # ERS = Percentage of ecoregions covered
  ers_score <- (conserved_count / total_count) * 100

  # 5. Create Summary DataFrame
  out_df <- dplyr::tibble(
    `Ecoregions with records` = total_count,
    `Ecoregions within G buffer` = conserved_count,
    `ERS exsitu` = round(ers_score, 2)
  )

  # 6. Create Spatial Object with Status
  currentEcos$gap_status <- ifelse(
    currentEcos$ECO_NAME %in% conserved_codes,
    "Covered",
    "Gap Ecoregion"
  )

  out_spatial <- currentEcos[, c("ECO_NAME", "gap_status")]

  # 7. Return List
  return(list(
    summary = out_df,
    spatial = out_spatial
  ))
}