library(testthat)
settings <- default_settings()

# Tidy --------------------------------------------------------------------

context("Tibble -----")

test_that("data_tidy_station", {
  dat <- data_read_station(dtype = "raw")
  expect_is(
    res <- data_tidy_station(dat),
    c("tbl_df", "tbl", "data.frame")
  )
})

test_that("data_tidy_temperature_min", {
  dat <- data_read_temperature_min(dtype = "raw")
  expect_is(
    res <- data_tidy_temperature_min(dat),
    c("tbl_df", "tbl", "data.frame")
  )
})

test_that("data_tidy_temperature_min", {
  dat <- data_read_temperature_max(dtype = "raw")
  expect_is(
    res <- data_tidy_temperature_min(dat),
    c("tbl_df", "tbl", "data.frame")
  )
})

test_that("data_tidy_sunshine_duration", {
  dat <- data_read_sunshine_duration(dtype = "raw")
  expect_is(
    res <- data_tidy_sunshine_duration(dat),
    c("tbl_df", "tbl", "data.frame")
  )
})

test_that("data_tidy_precipitation: historical", {
  dat <- data_read_precipitation_historical(dtype = "raw")
  expect_is(
    res <- data_tidy_precipitation(dat),
    "tbl_df"
  )
})

test_that("data_tidy_precipitation: recent", {
  dat <- data_read_precipitation_recent(dtype = "raw")
  expect_is(
    res <- data_tidy_precipitation(dat),
    "tbl_df"
  )
})


# Transform ---------------------------------------------------------------

context("Transform -----")

test_that("data_trans_temperature_min", {
  dat <- data_read_temperature_min(dtype = "raw")
  dat <- data_tidy_temperature_min(dat)
  expect_is(
    res <- data_trans_temperature_min(dat),
    c("tbl_df", "tbl", "data.frame")
  )
  expect_equal(
    names(res),
    c("dim_station", "time_month", "msr_temp_min")
  )
})

test_that("data_trans_temperature_max", {
  dat <- data_read_temperature_max(dtype = "raw")
  dat <- data_tidy_temperature_max(dat)
  expect_is(
    res <- data_trans_temperature_max(dat),
    c("tbl_df", "tbl", "data.frame")
  )
  expect_equal(
    names(res),
    c("dim_station", "time_month", "msr_temp_max")
  )
})

test_that("data_trans_sunshine_duration", {
  dat <- data_read_sunshine_duration(dtype = "raw")
  dat <- data_tidy_sunshine_duration(dat)
  expect_is(
    res <- data_trans_sunshine_duration(dat),
    c("tbl_df", "tbl", "data.frame")
  )
  expect_equal(
    names(res),
    c("dim_station", "time_month", "msr_sundur_avg")
  )
})

test_that("data_trans_precipitation/historical", {
  dat <- data_read_precipitation_historical(dtype = "raw")
  dat <- data_tidy_precipitation(dat)
  expect_is(
    res <- data_trans_precipitation(dat),
    c("tbl_df", "tbl", "data.frame")
  )
  expect_equal(
    names(res),
    c("dim_station", "time_year", "time_start", "time_stop", "time_month",
      "msr_precip_avg")
  )
})

test_that("data_trans_precipitation/recent", {
  dat <- data_read_precipitation_recent(dtype = "raw")
  dat <- data_tidy_precipitation(dat)
  expect_is(
    res <- data_trans_precipitation(dat),
    c("tbl_df", "tbl", "data.frame")
  )
  expect_equal(
    names(res),
    c("dim_station", "time_year", "time_start", "time_stop", "time_month",
      "msr_precip_avg")
  )
})

# Transform: datetime -----------------------------------------------------

context("Transform> datetime -----")

test_that("data_trans_temperature_min_datetime", {
  dat <- data_read_temperature_min(dtype = "raw")
  dat <- data_tidy_temperature_min(dat)
  dat <- data_trans_temperature_min(dat)
  expect_is(
    res <- data_trans_temperature_min_datetime(dat),
    c("tbl_df", "tbl", "data.frame")
  )
  expect_equal(
    names(table(res$time_month)),
    as.character(1:12)
  )
})

test_that("data_trans_temperature_max_datetime", {
  dat <- data_read_temperature_max(dtype = "raw")
  dat <- data_tidy_temperature_max(dat)
  dat <- data_trans_temperature_max(dat)
  expect_is(
    res <- data_trans_temperature_max_datetime(dat),
    c("tbl_df", "tbl", "data.frame")
  )
  expect_equal(
    names(table(res$time_month)),
    as.character(1:12)
  )
})

test_that("data_trans_sunshine_duration_datetime", {
  dat <- data_read_sunshine_duration(dtype = "raw")
  dat <- data_tidy_sunshine_duration(dat)
  dat <- data_trans_sunshine_duration(dat)
  expect_is(
    res <- data_trans_sunshine_duration_datetime(dat),
    c("tbl_df", "tbl", "data.frame")
  )
  expect_equal(
    names(table(res$time_month)),
    as.character(1:12)
  )
})

test_that("data_trans_precipitation_datetime/historical", {
  dat <- data_read_precipitation_historical(dtype = "raw")
  dat <- data_tidy_precipitation(dat)
  dat <- data_trans_precipitation(dat)
  expect_is(
    res <- data_trans_precipitation_datetime(dat),
    c("tbl_df", "tbl", "data.frame")
  )
  expect_equal(
    sort(names(table(res$time_year))),
    c("1900", "1937", "1938", "1939", "1940", "1941", "1942", "1943",
      "1944", "1945", "1946", "1947", "1948", "1949", "1950", "1951",
      "1952", "1953", "1954", "1955", "1958", "1959", "1960", "1961",
      "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969",
      "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977",
      "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985",
      "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993",
      "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001",
      "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
      "2010", "2011", "2012", "2013", "2014")
  )
})

test_that("data_trans_precipitation_datetime/recent", {
  dat <- data_read_precipitation_recent(dtype = "raw")
  dat <- data_tidy_precipitation(dat)
  dat <- data_trans_precipitation(dat)
  expect_is(
    res <- data_trans_precipitation_datetime(dat),
    c("tbl_df", "tbl", "data.frame")
  )
  expect_equal(
    sort(names(table(res$time_year))),
    c("1900", "1937", "1938", "1939", "1940", "1941", "1942", "1943",
      "1944", "1945", "1946", "1947", "1948", "1949", "1950", "1951",
      "1952", "1953", "1954", "1955", "1958", "1959", "1960", "1961",
      "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969",
      "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977",
      "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985",
      "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993",
      "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001",
      "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
      "2010", "2011", "2012", "2013", "2014")
  )
})

# Summarize ---------------------------------------------------------------

context("Summarize -----")

test_that("transform/temp_min/datetime", {
  settings <- source(test_path("data/settings.R"))$value
  dat <- load_data_raw_precipitation_historical(settings)
  dat <- transform_data_raw_precipitation_tibble(dat)
  dat <- transform_data_raw_precipitation_reshape(dat)
  dat <- transform_data_raw_precipitation_datetime(dat)

  expect_equal(
    names(res <- transform_data_raw_precipitation_summarize(dat)),
    c("dim_station", "time_month", "msr_precip_min", "msr_precip_max",
      "msr_precip_avg", "msr_precip_med")
  )
})

# Transform: combine ------------------------------------------------------

context("Transform: combine -----")

test_that("transform/temp/combine", {
  settings <- source(test_path("data/settings.R"))$value

  dat_temp_min <- readRDS(get_path_data_temperature_min(settings,
    repository = TRUE))
  dat_temp_max <- readRDS(get_path_data_temperature_max(settings,
    repository = TRUE))

  expect_equal(
    names(res <- transform_data_inter_temperature_combine(
      dat_temp_min = dat_temp_min,
      dat_temp_max = dat_temp_max
    )),
    c("dim_station", "time_month", "msr_temp_min", "msr_temp_max",
      "msr_temp_avg", "msr_temp_med")
  )
})

test_that("transform/precip/combine", {
  settings <- source(test_path("data/settings.R"))$value

  dat_precip_hist <- readRDS(get_path_data_precipitation_historical(settings,
    repository = TRUE))
  dat_precip_recent <- readRDS(get_path_data_precipitation_recent(settings,
    repository = TRUE))

  expect_equal(
    names(res <- transform_data_inter_precipitation_combine(
      dat_precip_hist = dat_precip_hist,
      dat_precid_recent = dat_precid_recent
    )),
    c("dim_station", "dim_time_type", "time_month", "msr_precip_min",
      "msr_precip_max", "msr_precip_avg", "msr_precip_med")
  )
})
