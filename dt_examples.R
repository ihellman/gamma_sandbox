library(DT)

# Create sample data with long text
data <- data.frame(
  ID = 1:5,
  Name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
  Description = c(
    "This is a very long description that should be truncated with ellipsis to make the table more readable and prevent it from taking up too much horizontal space",
    "Another lengthy text field that contains a lot of information about something important that needs to be displayed in a compact format",
    "Yet another long string of text that demonstrates how the ellipsis plugin works to truncate content nicely",
    "Short text",
    "This description goes on and on with many words to demonstrate the truncation feature working properly in action"
  ),
  Score = c(85, 92, 78, 95, 88)
)

# Create DT table with CSS-based dynamic ellipsis
datatable(
  data,
  options = list(
    columnDefs = list(
      list(
        targets = 3,  # Description column
        className = 'dt-ellipsis',
        createdCell = JS(
          "function(td, cellData, rowData, row, col) {",
          "  $(td).attr('title', cellData);",  # Add tooltip with full text
          "}"
        )
      )
    ),
    pageLength = 10,
    autoWidth = FALSE,
    scrollX = FALSE
  ),
  class = 'display'
) %>%
  formatStyle(
    'Description',
    `max-width` = '300px',  # Set max width for the column
    `white-space` = 'nowrap',
    `overflow` = 'hidden',
    `text-overflow` = 'ellipsis'
  )




