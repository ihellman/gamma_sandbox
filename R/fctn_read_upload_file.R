# fctn_read_upload_file.R

read_upload_file <- function(file_info) {
  # 1. Remove Lat/Lon from strict requirements
  required_cols <- c(
    "Accession Number",
    "Taxon Name",
    "Current Germplasm Type",
    "Collection Date",
    "Locality",
    "Collector",
    "issues"
  )

  # Validate file was provided
  if (is.null(file_info)) {
    return(list(
      success = FALSE,
      data = NULL,
      message = "⚠️ No file provided"
    ))
  }

  file_ext <- tools::file_ext(file_info$name)

  # Validate file type
  if (!file_ext %in% c("csv")) {
    return(list(
      success = FALSE,
      data = NULL,
      message = "❌ Unsupported file type. Please upload a CSV file."
    ))
  }

  # Try to read the file
  result <- tryCatch(
    {
      if (file_ext == "csv") {
        data <- read_csv(
          file_info$datapath,
          col_types = cols(.default = "c")
        )
      } else {
        stop("Unsupported file type")
      }

      # 2. Ensure Lat/Lon columns exist (fill with NA if missing)
      if (!"Latitude" %in% names(data)) data$Latitude <- NA
      if (!"Longitude" %in% names(data)) data$Longitude <- NA

      # Validate required columns
      missing_cols <- setdiff(required_cols, names(data))

      if (length(missing_cols) > 0) {
        return(list(
          success = FALSE,
          data = NULL,
          message = paste(
            "❌ Missing required columns:",
            paste(missing_cols, collapse = ", ")
          )
        ))
      }

      # 3. Check for missing or invalid coordinates
      # We check if they are NA or cannot be converted to numbers
      n_missing_coords <- data %>%
        dplyr::filter(
          is.na(suppressWarnings(as.numeric(Latitude))) | 
          is.na(suppressWarnings(as.numeric(Longitude)))
        ) %>%
        nrow()

      # Add index and source
      data <- data %>%
        dplyr::mutate(index = dplyr::row_number(), source = "upload")

      # 4. Construct Message
      base_msg <- paste("✅ Successfully loaded", nrow(data), "records")
      
      if (n_missing_coords > 0) {
        final_message <- paste0(
          "⚠️ NOTE: ", n_missing_coords, " records are missing valid Latitude/Longitude data.\n",
          "These records will be included in the GAP analysis, but cannot be displayed on the map."
        )
      } else {
        final_message <- base_msg
      }

      # Success!
      list(
        success = TRUE,
        data = data,
        message = final_message
      )
    },
    # What to return if there's an error
    error = function(e) {
      list(
        success = FALSE,
        data = NULL,
        message = paste("🚫 Error reading file:", e$message)
      )
    }
  )

  return(result)
}