library(DT)
library(readr)

# # Create sample data with long text
# data <- data.frame(
#   ID = 1:5,
#   Name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
#   Description = c(
#     "This is a very long description that should be truncated with ellipsis to make the table more readable and prevent it from taking up too much horizontal space",
#     "Another lengthy text field that contains a lot of information about something important that needs to be displayed in a compact format",
#     "Yet another long string of text that demonstrates how the ellipsis plugin works to truncate content nicely",
#     "Short text",
#     "This description goes on and on with many words to demonstrate the truncation feature working properly in action"
#   ),
#   Score = c(85, 92, 78, 95, 88)
# )

data <- read_csv("sample_data/Magnolia_acuminata_data.csv")

# # Create DT table with CSS-based dynamic ellipsis
# datatable(
#   data,
#   extensions = 'Select',
#   rownames = FALSE,
#   options = list(
#     columnDefs = list(
#       list(
#         targets = '_all',  # Description column
#         className = 'dt-ellipsis',
#         createdCell = JS(
#           "function(td, cellData, rowData, row, col) {",
#           "  $(td).attr('title', cellData);",  # Add tooltip with full text
#           "}"
#         )
#       )
#     ),
#     select = list(style = 'multi', items = 'row'),
#     pageLength = 10,
#     autoWidth = FALSE,
#     scrollX = TRUE
#   ),
#   class = 'display'
# ) %>%
#   formatStyle(
#     columns = 1:ncol(data),
#     `max-width` = '300px',  # Set max width for the column
#     `white-space` = 'nowrap',
#     `overflow` = 'hidden',
#     `text-overflow` = 'ellipsis'
#   )


# Fancy tables

# Reorder columns to put most important info first
data_display <- data[, c("Taxon Name", "Collection Date", "Locality", 
                         "Collector", "Latitude", "Longitude", 
                         "Current Germplasm Type", "Accession Number", 
                         "issues", "source", "index")]

datatable(
  
  data_display,
  extensions = c('Select', 'Buttons'),
  selection = 'none', #turn of DT native select in order to use Select Extension
  rownames = FALSE,
  filter = 'top',
  options = list(
    dom = 'Bfrtip',
    buttons = list(
      'copy',
      list(extend = 'csv', filename = 'data'),
      list(extend = 'excel', filename = 'data')
    ),
    select = list(
      style = 'multi',
      items = 'row'
    ),
    columnDefs = list(
      list(
        targets = '_all',
        className = 'dt-center',
        createdCell = JS(
          "function(td, cellData, rowData, row, col) {",
          "  if(cellData != null) $(td).attr('title', cellData);",
          "}"
        )
      )
    ),
    pageLength = 25,
    lengthMenu = c(10, 25, 50, 100),
    autoWidth = FALSE,
    scrollX = TRUE
  ),
  class = 'cell-border stripe hover compact'
) %>%
  formatStyle(
    columns = 1:ncol(data_display),
    `max-width` = '300px',
    `white-space` = 'nowrap',
    `overflow` = 'hidden',
    `text-overflow` = 'ellipsis',
    fontSize = '13px'
  ) %>%
  formatStyle(
    'Current Germplasm Type',
    backgroundColor = styleEqual(c('G', 'H'), c('#e3f2fd', '#f1f8e9')),
    fontWeight = 'bold'
  ) %>%
  formatRound(c('Latitude', 'Longitude'), 4)


