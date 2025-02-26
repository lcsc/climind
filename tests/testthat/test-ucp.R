library(testthat)

test_that("pr parameter is numeric", {
  expect_error(ucp(pr = "pr", tmean = 20), "'pr', 'tmean' must be numeric")
})

test_that("tmean parameter is numeric", {
  expect_error(ucp(pr = 0, tmean = "20"), "'pr', 'tmean' must be numeric")
})

test_that("'na.rm' must be logical", {
  expect_error(ucp(pr = 0, tmean = 10, na.rm = 20), "'na.rm' must be logical")
})

test_that("'data_names' must be NULL or character", {
  expect_error(ucp(pr = 0, tmean = 10, na.rm = T, data_names = 10), "'data_names' must be NULL or character")
})
