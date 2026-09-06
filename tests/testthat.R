# Run with: Rscript tests/testthat.R   (or testthat::test_dir("tests/testthat") from the app root)
library(testthat)
Sys.setenv(NOT_CRAN = "true")   # enable shinytest2 browser tests
testthat::test_dir("tests/testthat", reporter = "progress")
