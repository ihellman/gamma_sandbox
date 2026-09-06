# Canonical column names the app understands (see merge_and_index() in utils.R).
UPLOAD_KNOWN_COLS <- c(
  "Accession Number", "Taxon Name", "Current Germplasm Type", "Collection Date",
  "Latitude", "Longitude", "Locality", "Collector", "issues"
)

# Trim whitespace from header names and map case-insensitive matches of the
# known columns onto their canonical spelling. Unknown columns are left as-is
# (merge_and_index() drops them later).
normalise_upload_headers <- function(data) {
  nms <- trimws(names(data))
  key <- tolower(gsub("\\s+", " ", nms))
  canon_key <- tolower(UPLOAD_KNOWN_COLS)
  hit <- match(key, canon_key)
  nms[!is.na(hit)] <- UPLOAD_KNOWN_COLS[hit[!is.na(hit)]]
  names(data) <- nms
  data
}

read_upload_file <- function(file_info) {
  
  # --- 1. Validation: File Extension ---
  file_ext <- tolower(tools::file_ext(file_info$name))
  if (!file_ext %in% c("csv", "xlsx", "xls")) {
    return(list(
      status  = "validation_error", 
      message = "Invalid file type. Please upload a .csv, .xlsx, or .xls file."
    ))
  }

  # --- 2. Safe Execution Block ---
  tryCatch({
    
    # A. Read File (All columns as text initially)
    if (file_ext == "csv") {
      data <- readr::read_csv(
        file_info$datapath, 
        col_types = readr::cols(.default = "c")
      )
    } else {
      # Excel: read first sheet only, coerce all columns to character
      data <- readxl::read_excel(
        file_info$datapath,
        sheet = 1,
        col_types = "text"
      )
    }

    # B. Validation: Required Columns
    # Headers are matched ignoring case and surrounding whitespace (a very common
    # spreadsheet export problem) and renamed to the canonical spelling.
    data <- normalise_upload_headers(data)
    required_cols <- c(
      "Accession Number", "Taxon Name", "Current Germplasm Type",
      "Collection Date", "Locality", "Collector"
    )

    missing_cols <- setdiff(required_cols, names(data))

    if (length(missing_cols) > 0) {
      return(list(
        status  = "validation_error",
        message = paste("Data not loaded.  Missing columns:", paste(missing_cols, collapse = ", "))
      ))
    }

    # C. Processing: Coordinates & Metadata
    data <- data %>%
      dplyr::mutate(
        # Create Lat/Lon as NA if they don't exist
        Latitude  = if ("Latitude" %in% names(.)) Latitude else NA_character_,
        Longitude = if ("Longitude" %in% names(.)) Longitude else NA_character_,
        
        # Coerce to numeric
        Latitude  = suppressWarnings(as.numeric(Latitude)),
        Longitude = suppressWarnings(as.numeric(Longitude)),

        # Issues not required in upload, set to NA if missing
        issues = if ("issues" %in% names(.)) issues else NA_character_,
        
        # Add Metadata
        index  = dplyr::row_number(), 
        source = "upload"
      )

    # D. Quality Check: Missing Coordinates
    n_missing_coords <- sum(is.na(data$Latitude) | is.na(data$Longitude))
    
    excel_note <- if (file_ext %in% c("xlsx", "xls")) " (first sheet only)" else ""
    
    if (n_missing_coords > 0) {
      msg_type <- "warning"
      msg_text <- paste0("Loaded ", nrow(data), " records", excel_note, ". Note: ", n_missing_coords, 
                         " records have invalid coordinates and won't appear on the map.")
    } else {
      msg_type <- "message"
      msg_text <- paste0("Successfully loaded ", nrow(data), " records", excel_note, ".")
    }

    # E. Return Success
    list(
      status = "success",
      data = data,
      message = msg_text,
      message_type = msg_type
    )

  }, error = function(e) {
    # System Crash Handler
    list(
      status  = "system_error", 
      message = paste("Critical error reading file:", e$message)
    )
  })
}