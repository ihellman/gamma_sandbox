# ideally we would just be sourcing in the gap r library for these function but going to wait till it's back on cran to do so

generateCounts <- function(taxon, occurrence_Data) {
  # define presence of usable lat long values
  dataThin <- occurrence_Data |>
    dplyr::filter(`Taxon Name` == taxon) |>
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
    dplyr::summarize(total = dplyr::n())

  # generate counts df
  countsData <- data.frame(matrix(NA, nrow = 1, ncol = 9))
  colnames(countsData) <- colNames
  #assign values to counts df
  countsData$species <- taxon
  countsData$totalRecords <- nrow(dataThin)
  countsData$totalUseful <- sum((subset(tbl, hasLatLong == TRUE))$total)
  countsData$totalGRecords <- sum(
    (subset(tbl, `Current Germplasm Type` == "G"))$total
  )
  countsData$totalGUseful <- sum(
    (subset(tbl, `Current Germplasm Type` == "G" & hasLatLong == TRUE))$total
  )
  countsData$totalHRecords <- sum(
    (subset(tbl, `Current Germplasm Type` == "H"))$total
  )
  countsData$totalHUseful <- sum(
    (subset(tbl, `Current Germplasm Type` == "H" & hasLatLong == TRUE))$total
  )
  countsData$hasLat <- sum(dataThin$hasLat)
  countsData$hasLong <- sum(dataThin$hasLong)
  return(countsData)
}


SRSex <- function(taxon, occurrence_Data) {
  # generarte the counts data for species
  sp_counts <- generateCounts(taxon = taxon, occurrence_Data = occurrence_Data)
  # caluse for no h points
  if (sp_counts$totalGRecords >= 1 & sp_counts$totalHRecords == 0) {
    srs <- 100
  }

  #clause for no data
  if (sp_counts$totalGRecords == 0 & sp_counts$totalHRecords == 0) {
    srs <- 0
  } else {
    # clause for species with data
    srs <- min(c(100, sp_counts$totalGRecords / sp_counts$totalHRecords * 100))
  }

  #create data.frame with output
  out_df <- dplyr::tibble(
    Taxon = sp_counts$species,
    "Total records" = sp_counts$totalRecords,
    "Total with cooordinates" = sp_counts$totalUseful,
    "Total G records" = sp_counts$totalGRecords,
    "G records with coordinates" = sp_counts$totalGUseful,
    "Total H records" = sp_counts$totalHRecords,
    "H records with coordinates" = sp_counts$totalHUseful,
    "SRS exsitu" = srs
  )
  return(out_df)
}


GRSex <- function(allBuffers, outsideGBuffers) {
  # total area
  totalArea <- allBuffers |>
    terra::aggregate() |>
    terra::expanse(unit = "km")

  # calculate areas outside of gbuffer
  gapArea <- outsideGBuffers |>
    terra::aggregate() |>
    terra::expanse(unit = "km")

  # calculate GRSex score
  difference <- totalArea - gapArea

  # FIX: Ensure gArea is defined in both scenarios
  if (difference <= 0) {
    # Use <= to handle potential floating point tiny negatives
    grsex_score <- 0
    gArea <- 0
  } else {
    gArea <- totalArea - gapArea
    grsex_score <- (gArea / totalArea) * 100
  }

  out_df <- dplyr::tibble(
    'Area of model km2' = totalArea,
    'G buffer areas in model km2' = gArea,
    "GRS exsitu" = grsex_score # Note the column name here
  )
  return(out_df)
}


ERSex <- function(gapPoints, g_buffer) {
  # 1. Load Data
  ecoRegions <- terra::vect("appData/ecoregionsSimplified.gpkg")
  # remove lakes
  ecoRegions <- terra::subset(ecoRegions, ecoRegions$ECO_NAME != "Lake")

  # 2. Define the "Universe" (Ecoregions containing any record)
  inter_points <- terra::intersect(x = gapPoints, y = ecoRegions)
  ecoCodes <- unique(inter_points$ECO_NAME)

  # Subset ecoregions to this universe
  currentEcos <- ecoRegions[ecoRegions$ECO_NAME %in% ecoCodes, ]

  # 3. Determine Conserved Ecoregions (Overlap with Germplasm Buffer)
  # Project buffer to match ecoregions CRS to avoid errors
  g_buffer_proj <- terra::project(g_buffer, terra::crs(currentEcos))

  # Intersect to find overlaps
  gEco <- terra::intersect(x = g_buffer_proj, y = currentEcos)
  conserved_codes <- unique(gEco$ECO_NAME)

  # 4. Calculate Metrics
  total_count <- length(ecoCodes)
  conserved_count <- length(conserved_codes)

  # ERS = Percentage of ecoregions covered
  ers_score <- (conserved_count / total_count) * 100

  # 5. Create Summary DataFrame
  out_df <- dplyr::tibble(
    `Ecoregions with records` = total_count,
    `Ecoregions within G buffer` = conserved_count,
    `ERS exsitu` = round(ers_score, 2)
  )

  # 6. Create Spatial Object with Status
  # Add a status column for mapping
  currentEcos$gap_status <- ifelse(
    currentEcos$ECO_NAME %in% conserved_codes,
    "Covered",
    "Gap Ecoregion"
  )

  # Select useful columns for popup/display
  # Assuming 'ECO_ID' exists; if not, remove it or check your file
  out_spatial <- currentEcos[, c("ECO_NAME", "gap_status")]

  # 7. Return List
  return(list(
    summary = out_df,
    spatial = out_spatial
  ))
}
