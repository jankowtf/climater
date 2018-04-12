context("test-quo_prepend.R")

test_that("quo_prepend-1", {
  expect_equal(
    quo_prepend(default_name("station"), with = "dim"),
    dplyr::quo(dim_station)
  )
})

test_that("quo_prepend-2", {
  expect_equal(
    quo_prepend(default_name("station"), with = "msr"),
    dplyr::quo(msr_station)
  )
})

test_that("quo_append", {
  expect_equal(
    quo_append(default_name("station"), with = "abc"),
    dplyr::quo(station_avg)
  )
})

test_that("quo_prepend + quo_append", {
  expect_equal(
    quo_prepend(default_name("station"), with = "dim") %>%
      quo_append(with = "abc"),
    dplyr::quo(dim_station_abc)
  )
})
