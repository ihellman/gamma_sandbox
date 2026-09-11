# REPORT RENDERING (no pandoc) ------------------------------------------------------
# The HTML report is produced by litedown::fuse(): code chunks and inline code
# in reportTemplate.Rmd are evaluated in R and the Markdown is converted with
# the commonmark C library, all inside this process. rmarkdown::render() was
# replaced because it shells out to pandoc, which needs 230-280 MB on its own
# and pushed the 1 GB shinyapps.io instance out of memory.
#
# litedown does not know about htmlwidgets, so widgets (the leaflet map, the
# DT tables) are written into the document by report_widget(), which records
# their JS / CSS dependencies; embed_html_dependencies() then inlines those
# into the finished page so the report stays a single self-contained file.

.report_deps <- new.env(parent = emptyenv())
.report_deps$list <- list()

# Write an htmlwidget into the document (use in a chunk with results = 'asis')
# and remember its dependencies for embed_html_dependencies().
report_widget <- function(widget) {
  rendered <- htmltools::renderTags(widget)
  .report_deps$list <- c(.report_deps$list, rendered$dependencies)
  cat(as.character(rendered$html), "\n")
  invisible(NULL)
}

# A plain HTML table for the report. `bold_rows` are rendered bold; `cell_bg`
# (list(col = , color = )) shades one column, e.g. the priority category.
report_table <- function(df, bold_rows = integer(0), cell_bg = NULL, big_mark = ",") {
  df <- as.data.frame(df, check.names = FALSE)
  for (j in seq_along(df)) {
    v <- df[[j]]
    if (is.numeric(v)) v <- formatC(v, format = if (all(v == round(v), na.rm = TRUE)) "d" else "f", digits = 1, big.mark = big_mark)
    v <- htmltools::htmlEscape(as.character(v))
    if (!is.null(cell_bg) && j == cell_bg$col) {
      v <- sprintf('<span style="display:inline-block;padding:2px 8px;border-radius:4px;background:%s">%s</span>', cell_bg$color, v)
    }
    if (length(bold_rows)) v[bold_rows] <- paste0("<b>", v[bold_rows], "</b>")
    df[[j]] <- v
  }
  knitr::kable(df, format = "html", escape = FALSE, table.attr = 'class="table"')
}

mime_for <- function(path) {
  switch(tolower(tools::file_ext(path)),
         png = "image/png", gif = "image/gif", jpg = "image/jpeg", jpeg = "image/jpeg",
         svg = "image/svg+xml", woff = "font/woff", woff2 = "font/woff2", ttf = "font/ttf",
         "application/octet-stream")
}

# Replace url(...) references in a stylesheet with data URIs so the CSS keeps
# working once it is inlined (leaflet's layer-control and marker images, etc.).
inline_css_urls <- function(css, dir) {
  m <- gregexpr("url\\(\\s*['\"]?([^'\")]+)['\"]?\\s*\\)", css, perl = TRUE)
  refs <- unique(unlist(regmatches(css, m)))
  for (r in refs) {
    target <- sub("url\\(\\s*['\"]?([^'\")]+)['\"]?\\s*\\)", "\\1", r, perl = TRUE)
    if (grepl("^(data:|https?:|//)", target)) next
    f <- file.path(dir, sub("[?#].*$", "", target))
    if (!file.exists(f) || dir.exists(f)) next
    uri <- tryCatch(sprintf("data:%s;base64,%s", mime_for(f), base64enc::base64encode(f)),
                    error = function(e) NULL)
    if (is.null(uri)) next
    css <- gsub(r, sprintf("url(%s)", uri), css, fixed = TRUE)
  }
  css
}

# Inline the collected widget dependencies into a rendered HTML page:
# stylesheets before </head>, scripts (in dependency order) before </body>.
# htmlwidgets' static renderer runs on DOMContentLoaded, after those scripts.
embed_html_dependencies <- function(html, deps) {
  deps <- htmltools::resolveDependencies(deps)
  styles <- character(0); scripts <- character(0)
  for (d in deps) {
    dir <- if (!is.null(d$src$file)) {
      if (!is.null(d$package)) system.file(d$src$file, package = d$package) else d$src$file
    } else NULL
    if (is.null(dir) || !nzchar(dir)) next
    for (s in d$stylesheet %||% character(0)) {
      f <- file.path(dir, s)
      if (file.exists(f)) styles <- c(styles, sprintf("<style>\n%s\n</style>", inline_css_urls(paste(readLines(f, warn = FALSE), collapse = "\n"), dirname(f))))
    }
    for (s in d$script %||% character(0)) {
      src <- if (is.list(s)) s$src else s
      f <- file.path(dir, src)
      if (file.exists(f)) scripts <- c(scripts, sprintf("<script>\n%s\n</script>", paste(readLines(f, warn = FALSE), collapse = "\n")))
    }
    if (!is.null(d$head)) styles <- c(styles, d$head)
  }
  page <- paste(html, collapse = "\n")
  page <- sub("</head>", paste(c(styles, "</head>"), collapse = "\n"), page, fixed = TRUE)
  page <- sub("</body>", paste(c(scripts, "</body>"), collapse = "\n"), page, fixed = TRUE)
  page
}
`%||%` <- function(a, b) if (is.null(a)) b else a

# Render reportTemplate.Rmd for a run_gap_analysis() result. Kept outside the
# Shiny module so the report can be produced (and tested) without Shiny.
# Renders in a temp copy so intermediate files never touch the app directory.
render_gap_report <- function(res, output_file, template = "reportTemplate.Rmd",
                              css = "reportTemplate.css") {
  work_dir <- tempfile("gap_report_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  tmp_rmd <- file.path(work_dir, "report.Rmd")
  file.copy(template, tmp_rmd, overwrite = TRUE)
  file.copy(css, file.path(work_dir, basename(css)), overwrite = TRUE)

  params <- list(
    taxon = res$taxon,
    app_version = if (exists("APP_VERSION")) APP_VERSION else "dev",
    points = res$points,          # the rows that were analysed spatially
    records = res$records,        # every row of the working dataset
    bufferDist = res$dist_km,
    method = res$method,
    sf_model = res$sf_model,
    srsMetrics = res$srs,
    grsMetrics = res$grs,
    ersMetrics = res$ers,
    fcs = res$fcs,
    priority = res$priority$label,
    priorityColor = res$priority$color,
    sf_buffers = res$sf_buffers,
    sf_grs_gap = res$sf_grs_gap,
    sf_ers_regions = res$sf_ers_regions,
    combinedColor = combinedColor,
    grsexColor = grsexColor,
    ersexColors = ersexColors
  )

  # Parent on the environment this function lives in (Shiny sources R/ into
  # its own env, scripts and tests into the global env) so the template can see
  # report_widget(), report_table() and the palette globals in both cases.
  env <- new.env(parent = parent.env(environment()))
  env$params <- params
  .report_deps$list <- list()
  on.exit(.report_deps$list <- list(), add = TRUE)

  tmp_html <- file.path(work_dir, "report.html")
  litedown::fuse(tmp_rmd, output = tmp_html, envir = env, quiet = TRUE)
  page <- embed_html_dependencies(readLines(tmp_html, warn = FALSE), .report_deps$list)
  writeLines(page, output_file, useBytes = TRUE)
  invisible(output_file)
}
