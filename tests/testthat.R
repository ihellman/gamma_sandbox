# Run with: Rscript tests/testthat.R   (or testthat::test_dir("tests/testthat") from the app root)
library(testthat)
testthat::test_dir("tests/testthat", reporter = "progress")
