# Merge new data into current data and re-index
merge_and_index <- function(current_data, new_data) {
  # Defensive: ensure inputs exist
  if (is.null(current_data)) {
    current_data <- dplyr::tibble()
  }
  if (is.null(new_data)) {
    new_data <- dplyr::tibble()
  }

  # Helper: coerce all columns to character (no-op for 0-col df)
  coerce_to_char <- function(df) {
    if (ncol(df) == 0) {
      return(df)
    }
    df |> dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.character(.x)))
  }

  current_char <- coerce_to_char(current_data)
  new_char <- coerce_to_char(new_data)

  # Combine and re-index (if current empty, return new with index)
  if (nrow(current_char) == 0) {
    new_char |> dplyr::mutate(index = dplyr::row_number())
  } else {
    dplyr::bind_rows(current_char, new_char) |>
      dplyr::mutate(index = dplyr::row_number())
  }
}


# --- Helper: Modern Footer UI Component ---
# should be able to call this on all the pages to keep that visual consistency
# --- Helper: Modern Footer UI Component ---
footer_ui <- function() {
  tags$footer(
    class = "footer-modern",
    div(
      class = "container",
      div(
        class = "row",

        # COLUMN 1: Partners & Logos
        div(
          class = "col-md-5 footer-col",
          h5("Supported By"),
          div(
            class = "footer-logos",
            tags$a(
              href = "https://www.bgci.org/",
              target = "_blank",
              tags$img(src = "bgci.png", alt = "BGCI Logo")
            ),
            tags$a(
              href = "https://www.usbg.gov/",
              target = "_blank",
              tags$img(src = "usbg.jpg", alt = "USBG Logo")
            ),
            tags$a(
              href = "https://www.imls.gov/",
              target = "_blank",
              tags$img(src = "imls_logo_2c.jpg", alt = "IMLS Logo")
            ),
            tags$a(
              href = "https://atlantabg.org/",
              target = "_blank",
              tags$img(src = "abg.jpg", alt = "ABG Logo")
            )
          )
        ),

        # COLUMN 2: About the Tool
        div(
          class = "col-md-4 footer-col",
          h5("About This Tool"),
          p(
            "This dashboard supports global conservation efforts by providing gap analysis and data exploration tools for prioritized taxa."
          ),
          p(tags$a(href = "#", "Read Documentation", class = "footer-link"))
        ),

        # COLUMN 3: Contact / Links
        div(
          class = "col-md-3 footer-col",
          h5("Contact"),
          tags$ul(
            class = "list-unstyled",
            tags$li(tags$i(class = "bi bi-envelope"), " contact@example.org"),
            tags$li(
              tags$i(class = "bi bi-github"),
              tags$a(href = "#", " View Source Code", class = "footer-link")
            ),
            tags$li(
              tags$i(class = "bi bi-bug"),
              tags$a(href = "#", " Report an Issue", class = "footer-link")
            )
          )
        )
      ),

      # Bottom Copyright Row
      div(
        class = "row mt-4 pt-3 border-top border-secondary",
        div(
          class = "col-md-6 text-muted small",
          paste(
            "©",
            format(Sys.Date(), "%Y"),
            "Global Conservation Consortia. All rights reserved."
          )
        ),
        div(
          class = "col-md-6 text-end text-muted small",
          "Version 1.0.0"
        )
      )
    )
  )
}
