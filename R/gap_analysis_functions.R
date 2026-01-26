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
