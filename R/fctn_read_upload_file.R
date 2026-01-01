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
  if (!file_ext %in% c("csv", "xlsx", "xls")) {
    return(list(
      success = FALSE,
      data = NULL,
      message = "Please upload a CSV or Excel file (.csv, .xlsx, .xls)"
    ))
  }

  # Try to read the file
  result <- tryCatch(
    {
      if (file_ext == "csv") {
        data <- read.csv(
          file_info$datapath,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      } else {
        # Requires readxl package
        data <- readxl::read_excel(file_info$datapath)
      }

      # Add source column
      data <- data %>%
        mutate(source = "upload")
      # Validate required columns if specified
      if (!is.null(required_cols)) {
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
      }

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
