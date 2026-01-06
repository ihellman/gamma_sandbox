# FUNCTION to read and validate uploaded file
#
# file_info = The file input object from Shiny (input$fileInput)
# required_cols = Character vector of required column names

read_upload_file <- function(file_info) {
  # Define required columns for your data
  required_cols <- c(
    "Accession Number",
    "Taxon Name",
    "Current Germplasm Type",
    "Collection Date",
    "Latitude",
    "Longitude",
    "Locality",
    "Collector",
    "issues"
  )

  # Validate file was provided
  if (is.null(file_info)) {
    return(list(
      success = FALSE,
      data = NULL,
      message = "No file provided"
    ))
  }

  file_ext <- tools::file_ext(file_info$name)

  # Validate file type
  if (!file_ext %in% c("csv")) {
    return(list(
      success = FALSE,
      data = NULL,
      message = "Unsupported file type. Please upload a CSV file."
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

      # Validate required columns
      missing_cols <- setdiff(required_cols, names(data))

      if (length(missing_cols) > 0) {
        return(list(
          success = FALSE,
          data = NULL,
          message = paste(
            "Missing required columns:",
            paste(missing_cols, collapse = ", ")
          )
        ))
      }

      # Convert to spatial dataframe (sf object) add index and source.
      data <- data %>%
        sf::st_as_sf(
          coords = c("Longitude", "Latitude"),
          crs = 4326,
          remove = FALSE
        ) %>%
        mutate(index = dplyr::row_number(), source = "upload")

      # Success!
      list(
        success = TRUE,
        data = data,
        message = paste("Successfully loaded", nrow(data), "records")
      )
    },
    error = function(e) {
      list(
        success = FALSE,
        data = NULL,
        message = paste("Error reading file:", e$message)
      )
    }
  )

  return(result)
}
