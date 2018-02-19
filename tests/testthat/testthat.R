library(testthat)
library(BuildingAnRPackage)

test_that("make_filename() works", {
  expect_equal(make_filename(2015), "accident_2015.csv.bz2")
          })

