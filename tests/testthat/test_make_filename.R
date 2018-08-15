context("Test make_filename")
library(fars)

test_that("Test making filenames",
          {
            expect_match(make_filename(2013), "accident_2013.csv.bz2")
            expect_identical(make_filename(c(2013, 2014, 2015)),
                                           c("accident_2013.csv.bz2", "accident_2014.csv.bz2", "accident_2015.csv.bz2"))


          }
          )

test_that("Test make_filename warning conditions",
          {
            expect_warning(make_filename("character"), 'NAs introduced by coercion')
          }
          )


