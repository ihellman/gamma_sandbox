# Shared setup for all tests: run from the app root and source the app's
# function files exactly as Shiny would (global.R, then R/*.R).
app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
suppressPackageStartupMessages(source(file.path(app_root, "global.R"), chdir = TRUE))
for (f in sort(list.files(file.path(app_root, "R"), pattern = "\\.[Rr]$", full.names = TRUE))) source(f)

# testthat runs every file with the working directory set to tests/testthat/.
# The app reads its data with paths relative to the app root ("appData/..."),
# so call this at the top of any test file that exercises such code.
use_app_root <- function(env = parent.frame()) withr::local_dir(app_root, .local_envir = env)

# Absolute path to a file in tests/testthat/fixtures (works after use_app_root()).
fixture_path <- function(name) file.path(app_root, "tests", "testthat", "fixtures", name)

# Build a fileInput-style list for read_upload_file() from a path in appData/
fixture_file <- function(name) {
  path <- file.path(app_root, "appData", name)
  stopifnot(file.exists(path))
  list(name = name, datapath = path, size = file.size(path))
}

# Load a fixture into the canonical working-dataset schema.
load_fixture_dataset <- function(name) {
  res <- read_upload_file(fixture_file(name))
  stopifnot(res$status == "success")
  merge_and_index(data.frame(), res$data)
}
