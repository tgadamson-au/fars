context("Test reading and summarising files")
library(fars)

test_that("Test file does not exist error", {
  expect_error(fars_read("accident_3000.csv.bz2"), "file 'accident_3000.csv.bz2' does not exist")
  expect_warning(fars_read_years(3000), "invalid year: 3000")
})

