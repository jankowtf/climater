library(testthat)
context("test-default.R")

test_that("default_dtype-1", {
  target <- default_dtype()
  expect_equal(default_dtype(), target)
})

test_that("default_ftype-1", {
  target <- character()
  expect_equal(default_ftype(), target)
})

test_that("default_path_norm-1", {
  target <- FALSE
  expect_equal(default_path_norm(), target)
})

test_that("default_settings-1", {
  expect_is(res <- default_settings(), "list")
  expect_true(length(res) > 0)
})

test_that("default_name-1", {
  expect_is(res <- default_name("station"), "quosure")
  expect_identical(dplyr::quo_name(res), "station")
})

test_that("default_name-2", {
  expect_error(default_name("abc"), "Invalid mapping \\(ID = .*, type = .*\\)")
})
