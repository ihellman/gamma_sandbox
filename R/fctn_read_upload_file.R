read_upload_file <- function(file_info) {
  
  # --- 1. Validation: File Extension ---
  if (tools::file_ext(file_info$name) != "csv") {
    return(list(
      status  = "validation_error", 
      message = "Invalid file type. Please upload a .csv file."
    ))
  }

  # --- 2. Safe Execution Block ---
  tryCatch({
    
    # A. Read File (All columns as text initially)
    data <- readr::read_csv(
      file_info$datapath, 
      col_types = readr::cols(.default = "c")
    )

    # B. Validation: Required Columns
    # Note: Ensure these match your CSV headers exactly (case-sensitive)
    required_cols <- c(
      "Accession Number", "Taxon Name", "Current Germplasm Type", 
      "Collection Date", "Locality", "Collector"
    )
    
    missing_cols <- setdiff(required_cols, names(data))
    
    if (length(missing_cols) > 0) {
      return(list(
        status  = "validation_error",
        message = paste("Missing columns:", paste(missing_cols, collapse = ", "))
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
    
    if (n_missing_coords > 0) {
      msg_type <- "warning"
      msg_text <- paste0("Loaded ", nrow(data), " records. Note: ", n_missing_coords, 
                         " records have invalid coordinates and won't appear on the map.")
    } else {
      msg_type <- "message"
      msg_text <- paste("Successfully loaded", nrow(data), "records.")
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