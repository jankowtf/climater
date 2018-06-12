library(testthat)
context("test-data_management.R")
settings <- source(here::here("inst/app/settings.R"))$value

test_that("data_con_get_path-1", {
  id <- "station"
  dtype <- "raw"

  target <- "inst/app/data/raw/stations_list_CLIMAT_data.txt"
  expect_identical(
    data_con_get_path(id = id, dtype = dtype, settings = settings),
    target
  )
})

test_that("data_con_get_path-2", {
  id <- "station"
  dtype <- "tidy"

  target <- "inst/app/data/tidy/station.rds"
  expect_identical(
    data_con_get_path(id = id, dtype = dtype, settings = settings),
    target
  )
})

test_that("data_con_get_path-3", {
  id <- "station"
  dtype <- "abc"

  expect_error(
    data_con_get_path(id = id, dtype = dtype, settings = settings)
  )
})

test_that("data_con_get_path-4", {
  id <- "station"
  dtype <- "tidy"
  ftype = "csv"

  target <- "inst/app/data/tidy/station.rds.csv"
  expect_identical(
    data_con_get_path(id = id, dtype = dtype, ftype = ftype,
      settings = settings),
    target
  )
})

test_that("data_con_get_path-5", {
  id <- "station"
  dtype <- "tidy"

  target <- normalizePath("inst/app/data/tidy/station.rds",
    winslash = "/", mustWork = FALSE)
  expect_identical(
    res <- data_con_get_path(id = id, dtype = dtype, norm = TRUE, settings = settings),
    target
  )
})

test_that("data_con_get_path-6", {
  id <- "station"
  dtype <- "tidy"

  target <- normalizePath("inst/app/data/tidy/station.rds",
    winslash = "/", mustWork = FALSE)
  expect_identical(
    res <- data_con_get_path(id = id, dtype = dtype, norm = TRUE),
    target
  )
})


# Data cons ---------------------------------------------------------------

test_that("data_con_station-1", {
  target <- "inst/app/data/tidy/station.rds"
  expect_identical(
    res <- data_con_station(),
    target
  )
})

test_that("data_con_temparature_min-1", {
  target <- "inst/app/data/tidy/temp_min_1961_1990.rds"
  expect_identical(
    res <- data_con_temperature_min(),
    target
  )
})

test_that("data_con_temparature_max-1", {
  target <- "inst/app/data/tidy/temp_max_1961_1990.rds"
  expect_identical(
    res <- data_con_temperature_max(),
    target
  )
})

test_that("data_con_temparature_comb-1", {
  target <- "inst/app/data/tidy/temp_comb_1961_1990.rds"
  expect_identical(
    res <- data_con_temperature_comb(),
    target
  )
})

test_that("data_con_temparature_comb-1", {
  target <- character()
  expect_identical(
    res <- data_con_temperature_comb(dtype = "raw"),
    target
  )
})

test_that("data_con_sunshine_duration-1", {
  target <- "inst/app/data/tidy/sundur_avg_1961_1990.rds"
  expect_identical(
    res <- data_con_sunshine_duration(),
    target
  )
})

test_that("data_con_precipitation_historical-1", {
  target <- "inst/app/data/tidy/precip_avg_hist.rds"
  expect_identical(
    res <- data_con_precipitation_historical(),
    target
  )
})

test_that("data_con_precipitation_recent-1", {
  target <- "inst/app/data/tidy/precip_avg_recent.rds"
  expect_identical(
    res <- data_con_precipitation_recent(),
    target
  )
})

test_that("data_con_precipitation_comb-1", {
  target <- "inst/app/data/tidy/precip_avg_comb.rds"
  expect_identical(
    res <- data_con_precipitation_comb(),
    target
  )
})

test_that("data_con_join_full-1", {
  target <- "inst/app/data/tidy/join_full.rds"
  expect_identical(
    res <- data_con_join_full(),
    target
  )
})

test_that("data_con_join_inner-1", {
  target <- "inst/app/data/tidy/join_inner.rds"
  expect_identical(
    res <- data_con_join_inner(),
    target
  )
})


# Data load ---------------------------------------------------------------

test_that("data_read_station", {
  expect_is(
    res <- data_read_station(dtype = "raw"),
    "data.frame"
  )
  expect_true(
    ncol(res) == 6
  )
  expect_equal(
    colnames(res),
    c("wmo_station_id", "station_name", "latitude", "logitude",
      "high",  "country")
  )
})

test_that("data_read_temperature_min", {
  expect_is(
    res <- data_read_temperature_min(dtype = "raw"),
    "data.frame"
  )
  expect_true(
    ncol(res) == 13
  )
  expect_equal(
    colnames(res),
    c("station", "jan", "feb", "mrz", "apr", "mai",
      "jun", "jul", "aug", "sep", "okt", "nov", "dez")
  )
})

test_that("data_read_temperature_max", {
  expect_is(
    res <- data_read_temperature_max(dtype = "raw"),
    "data.frame"
  )
  expect_true(
    ncol(res) == 13
  )
  expect_equal(
    colnames(res),
    c("station", "jan", "feb", "mrz", "apr", "mai",
      "jun", "jul", "aug", "sep", "okt", "nov", "dez")
  )
})

test_that("data_read_sunshine_duration", {
  expect_is(
    res <- data_read_sunshine_duration(dtype = "raw"),
    "data.frame"
  )
  expect_true(
    ncol(res) == 13
  )
  expect_equal(
    colnames(res),
    c("station", "jan", "feb", "mrz", "apr", "mai",
      "jun", "jul", "aug", "sep", "okt", "nov", "dez")
  )
})

test_that("data_read_precipitation_historical", {
  expect_is(
    res <- data_read_precipitation_historical(dtype = "raw"),
    "data.frame"
  )
  # expect_true(
  #   length(res) == 4166
  # )
  expect_true(
    nrow(res) == 95420
  )
  # expect_true(
  #   ncol(res[[1]]) == 13
  # )
  expect_true(
    ncol(res) == 14
  )
  # expect_equal(
  #   colnames(res[[1]]),
  #   c("jahr", "jan", "feb", "mrz", "apr", "mai",
  #     "jun", "jul", "aug", "sep", "okt", "nov", "dez")
  # )
  expect_equal(
    colnames(res),
    c(".id", "jahr", "jan", "feb", "mrz", "apr", "mai",
      "jun", "jul", "aug", "sep", "okt", "nov", "dez")
  )
})

test_that("data_read_precipitation_recent", {
  expect_is(
    res <- data_read_precipitation_recent(dtype = "raw"),
    "data.frame"
  )
  # expect_true(
  #   length(res) == 4166
  # )
  expect_true(
    nrow(res) == 9291
  )
  # expect_true(
  #   ncol(res[[1]]) == 14
  # )
  expect_true(
    ncol(res) == 14
  )
  # expect_equal(
  #   colnames(res[[1]]),
  #   c("jahr", "jan", "feb", "mrz", "apr", "mai",
  #     "jun", "jul", "aug", "sep", "okt", "nov", "dez")
  # )
  expect_equal(
    colnames(res),
    c(".id", "jahr", "jan", "feb", "mrz", "apr", "mai",
      "jun", "jul", "aug", "sep", "okt", "nov", "dez")
  )
})


# Save --------------------------------------------------------------------

test_that("data_write_station", {
  dat_station <- data_read_station(dtype = "raw")
  dat_station <- data_tidy_station(dat_station)
  file <- data_con_station(dtype = "tidy")

  expect_equal(
    res <- data_write_station(dat_station),
    file
  )
})
