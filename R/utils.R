
# Merge new data into current data and re-index
merge_and_index <- function(current_data, new_data) {
  # If current data is empty, just return new data with index
  if (nrow(current_data) == 0) {
    new_data %>% 
      mutate(index = row_number())
  } else {
    # Combine and re-index
    bind_rows(current_data, new_data) %>%
      mutate(index = row_number())
  }
}