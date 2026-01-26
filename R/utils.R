# Merge new data into current data and re-index
merge_and_index <- function(current_data, new_data) {
  # Defensive: ensure inputs exist
  if (is.null(current_data)) current_data <- dplyr::tibble()
  if (is.null(new_data))    new_data    <- dplyr::tibble()

  # Helper: coerce all columns to character (no-op for 0-col df)
  coerce_to_char <- function(df) {
    if (ncol(df) == 0) return(df)
    df |> dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.character(.x)))
  }

  current_char <- coerce_to_char(current_data)
  new_char     <- coerce_to_char(new_data)

  # Combine and re-index (if current empty, return new with index)
  if (nrow(current_char) == 0) {
    new_char |> dplyr::mutate(index = dplyr::row_number())
  } else {
    dplyr::bind_rows(current_char, new_char) |> dplyr::mutate(index = dplyr::row_number())
  }
}