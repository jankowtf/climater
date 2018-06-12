library(testthat)

test_that("get_global_data_repo", {
  expect_error(get_global_data_repo())
})

test_that("set_global_data_repo", {
  expect_is(set_global_data_repo(), "character")
  expect_is(ret <- get_global_data_repo(), "character")
  expect_true(file.exists(ret))
})

# test_that("set_global_data_repo", {
#   expect_is(set_global_data_repo("repo_2"), "character")
#   expect_is(ret <- get_global_data_repo(), "character")
#   expect_true(file.exists(ret))
# })
