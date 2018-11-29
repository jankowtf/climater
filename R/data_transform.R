
# Tidy --------------------------------------------------------------------

data_tidy_generic <- function(dat) {
  dat <- purrr::modify_at(dat, c(2:ncol(dat)), as.numeric)
  tibble::as.tibble(dat)
}

data_tidy_multipe_generic <- function(dat) {
  dat <- lapply(dat, function(dat) {
    meta <- attributes(dat)$meta
    dat <- purrr::modify_at(dat, c(2:ncol(dat)), as.numeric)
    dat <- tibble::as.tibble(dat)
    attributes(dat)$meta <- meta
    dat
  })
}

data_tidy_station <- function(dat) {
  dat <- dplyr::select(dat,
    # dim_station = `wmo-station_id`,
    dim_station = wmo_station_id,
    dim_station_name = station_name,
    dim_country = country,
    dim_latitude = latitude,
    dim_longitude = logitude,
    dim_high = high
  )
  dat <- purrr::modify_at(dat,
    c("dim_latitude", "dim_longitude", "dim_high"), as.numeric)
  dat <- purrr::modify_at(dat, "dim_station", as.character)
  tibble::as.tibble(dat)
}

data_tidy_station_v2 <- function(dat) {
  # v1 -----
  dat <- data_tidy_station(dat = dat)

  # # Normalize station IDs -----
  # # Mutate 4-digits IDS to 5-digit IDs
  # dat <- data_trans_normalize_station_id(dat = dat)

  dat
}

data_tidy_temperature_min <- function(dat) {
  dat <- data_tidy_generic(dat)
  dat <- data_trans_temperature_min(dat)
  dat <- data_trans_temperature_min_datetime(dat)
  dat
}

data_tidy_temperature_min_v2 <- function(dat) {
  # v1 -----
  dat <- data_tidy_temperature_min(dat = dat)

  # # Normalize station IDs -----
  # # Mutate 4-digits IDS to 5-digit IDs
  # dat <- data_trans_normalize_station_id(dat = dat)

  dat
}

data_tidy_temperature_max <- function(dat) {
  dat <- data_tidy_generic(dat)
  dat <- data_trans_temperature_max(dat)
  dat <- data_trans_temperature_max_datetime(dat)
  dat
}

data_tidy_temperature_max_v2 <- function(dat) {
  # v1 -----
  dat <- data_tidy_temperature_max(dat = dat)

  # # Normalize station IDs -----
  # # Mutate 4-digits IDS to 5-digit IDs
  # dat <- data_trans_normalize_station_id(dat = dat)

  dat
}

data_tidy_sunshine_duration <- function(dat) {
  dat <- data_tidy_generic(dat)
  dat <- data_trans_sunshine_duration(dat)
  dat <- data_trans_sunshine_duration_datetime(dat)
  dat <- data_trans_sunshine_duration_hours_per_day(dat)
  dat <- dat %>% mutate_if(is.double, round, 1)
  dat
}

data_tidy_sunshine_duration_v2 <- function(dat) {
  # v1 -----
  dat <- data_tidy_sunshine_duration(dat = dat)

  # # Normalize station IDs -----
  # # Mutate 4-digits IDS to 5-digit IDs
  # dat <- data_trans_normalize_station_id(dat = dat)

  # Abs to get rid of negative values


  dat
}

data_tidy_precipitation <- function(dat) {
  dat <- data_tidy_generic(dat)
  dat <- data_trans_precipitation(dat)
  dat <- data_trans_precipitation_datetime(dat)
  dat <- data_trans_precipitation_summarize(dat)
  dat
}

data_tidy_precipitation_v2 <- function(dat) {
  # v1 -----
  dat <- data_tidy_precipitation(dat = dat)

  # # Normalize station IDs -----
  # # Mutate 4-digits IDS to 5-digit IDs
  # dat <- data_trans_normalize_station_id(dat = dat)

  dat
}

# Transform: normalize station IDs ----------------------------------------

# Normalize station IDs -----
# Mutate 4-digits IDS to 5-digit IDs
data_trans_normalize_station_id <- function(dat) {
  k_dim_station <- quo_prepend("station", "dim")
  dat <- dat %>% mutate(
    !!dplyr::quo_name(k_dim_station) := case_when(
      nchar(!!k_dim_station) == 4 ~ sprintf("0%s", !!k_dim_station),
      TRUE ~ !!k_dim_station
    )
  )
}

# Transform: impute missing values ----------------------------------------

data_trans_impute_missing_values <- function(dat) {
  # Correct Inf values ----
  idx <- dat %>% sapply(function(x) any(is.infinite(x)))
  for (col in which(idx)) {
    dat[[col]][is.infinite(dat[[col]])] <- NA
  }

  # Modified formula due to problems with 'impute_lm' -----
  dat_2 <- dat %>% simputation::impute_lm(
    msr_temp_min + msr_temp_max + msr_temp_avg ~
      dim_longitude + dim_latitude + dim_high + msr_precip_avg + msr_sundur_avg)
  dat_2 <- dat_2 %>% simputation::impute_lm(
    msr_precip_min + msr_precip_max + msr_precip_avg + msr_precip_med ~
      dim_longitude + dim_latitude + dim_high + msr_temp_avg + msr_sundur_avg)
  dat_2 <- dat_2 %>% simputation::impute_lm(msr_sundur_avg ~ dim_longitude + dim_latitude +
      dim_high + msr_precip_avg + msr_temp_avg )
  dat_2 <- dat_2 %>% simputation::impute_lm(msr_sundur_avg ~ dim_longitude + dim_latitude +
      dim_high)

  # Second wave with simpler model for remaining missing values -----
  dat_2 <- dat_2 %>% simputation::impute_lm(
    msr_temp_min + msr_temp_max + msr_temp_avg +
      msr_precip_min + msr_precip_max + msr_precip_avg +
      msr_sundur_avg ~
      dim_longitude + dim_latitude + dim_high)

  # TODO: quosures

  dat_2
}

# Transform: reshape ------------------------------------------------------

data_trans <- function(
  dat,
  key,
  value
) {
  id_col_lhs <- default_id_col_lhs()
  id_col_rhs <-  default_id_col_rhs()

  key <- dplyr::enquo(key)
  value <- dplyr::enquo(value)
  dat <- tidyr::gather(dat,
    2:ncol(dat),
    key = !!key,
    value = !!value)
  dat <- dplyr::select(dat, !!id_col_lhs := !!id_col_rhs, dplyr::everything())
  purrr::modify_at(dat, rlang::eval_tidy(id_col_lhs), as.character)
}

data_trans_temperature_min <- function(dat) {
  data_trans(dat = dat, key = "time_month", value = "msr_temp_min")
}

data_trans_temperature_max <- function(dat) {
  data_trans(dat = dat, key = "time_month", value = "msr_temp_max")
}

data_trans_sunshine_duration <- function(dat) {
  data_trans(dat = dat, key = "time_month",
    value = "msr_sundur_month_avg")
  # value = quo_xpend(default_name("sundur"), "msr", "month_avg"))
}

data_trans_sunshine_duration_hours_per_day <- function(dat) {
  k_sundur <- quo_xpend(default_name("sundur"), "msr", "month_avg")

  days_per_month <- data.frame(
    time_month = 1:12,
    days = c(31, 28, rep(c(31, 30), 2), 31, rep(c(31, 30), 2), 31)
  )

  divide_by_days <- function(msr_sundur_avg, days_per_month, month) {
    round(msr_sundur_avg /
      (days_per_month %>%
      filter(time_month == month) %>%
          # select(days) %>% pull()) * 24)
          select(days) %>% pull()))
  }

  dat <- dat %>% group_by(time_month) %>%
    mutate(
      msr_sundur_day_avg = case_when(
        time_month == 1 ~ divide_by_days(!!k_sundur, days_per_month, 1),
        time_month == 2 ~ divide_by_days(!!k_sundur, days_per_month, 2),
        time_month == 3 ~ divide_by_days(!!k_sundur, days_per_month, 3),
        time_month == 4 ~ divide_by_days(!!k_sundur, days_per_month, 4),
        time_month == 5 ~ divide_by_days(!!k_sundur, days_per_month, 5),
        time_month == 6 ~ divide_by_days(!!k_sundur, days_per_month, 6),
        time_month == 7 ~ divide_by_days(!!k_sundur, days_per_month, 7),
        time_month == 8 ~ divide_by_days(!!k_sundur, days_per_month, 8),
        time_month == 9 ~ divide_by_days(!!k_sundur, days_per_month, 9),
        time_month == 10 ~ divide_by_days(!!k_sundur, days_per_month, 10),
        time_month == 11 ~ divide_by_days(!!k_sundur, days_per_month, 11),
        time_month == 12 ~ divide_by_days(!!k_sundur, days_per_month, 12)
      )
    ) %>%
    ungroup()

  dat
}

data_trans_precipitation <- function(dat) {
  # Keys -----
  k_station <- default_name("station")
  k_time_start <- default_name("time_start")
  k_time_stop <- default_name("time_stop")
  k_year <- quo("jahr")
  k_key <- default_name("time_month")
  k_value <- default_name("msr_precip_avg")

  # Gather data -----
  dat <- tidyr::gather(dat,
    key = !!k_key,
    value = !!k_value,
    -!!k_year, -!!k_station, -!!k_time_start, -!!k_time_stop)

  # Rename -----
  dat <- dplyr::select(dat,
    dim_station := !!k_station,
    time_year := !!k_year, dplyr::everything())

  dat
}

# Transform: datetime -----------------------------------------------------

data_trans_generic_datetime <- function(dat, k_time) {
  k_time_char <- dplyr::quo_name(k_time)
  dplyr::mutate(dat,
    !!k_time_char := dplyr::case_when(
      (!!k_time) == "jan" ~ 1,
      (!!k_time) == "feb" ~ 2,
      (!!k_time) == "mrz" ~ 3,
      (!!k_time) == "apr" ~ 4,
      (!!k_time) == "mai" ~ 5,
      (!!k_time) == "jun" ~ 6,
      (!!k_time) == "jul" ~ 7,
      (!!k_time) == "aug" ~ 8,
      (!!k_time) == "sep" ~ 9,
      (!!k_time) == "okt" ~ 10,
      (!!k_time) == "nov" ~ 11,
      (!!k_time) == "dez" ~ 12,
      TRUE ~ NA_real_)) %>%
    purrr::modify_at(k_time_char, as.integer)
}

data_trans_temperature_min_datetime <- function(dat) {
  data_trans_generic_datetime(dat = dat, k_time = default_name("time_month"))
}

data_trans_temperature_max_datetime <- function(dat) {
  data_trans_generic_datetime(dat = dat, k_time = default_name("time_month"))
}

data_trans_sunshine_duration_datetime <- function(dat) {
  data_trans_generic_datetime(dat = dat, k_time = default_name("time_month"))
}

data_trans_precipitation_datetime <- function(dat) {
  data_trans_generic_datetime(dat = dat, k_time = default_name("time_month"))
}

# Transform: summarize ----------------------------------------------------

data_trans_precipitation_summarize <- function(dat) {
  k_dim_station <- quo_prepend(default_name("station"), "dim")
  k_time_month <- default_name("time_month")
  k_msr_precip_min <- quo_xpend(default_name("precip"),
    before = "msr", after = "min")
  k_msr_precip_max <- quo_xpend(default_name("precip"),
    before = "msr", after = "max")
  k_msr_precip_avg <- quo_xpend(default_name("precip"),
    before = "msr", after = "avg")
  k_msr_precip_med <- quo_xpend(default_name("precip"),
    before = "msr", after = "med")

  dat %>% dplyr::group_by(!!k_dim_station, !!k_time_month) %>%
    dplyr::summarise(
      msr_precip_min = min(!!k_msr_precip_avg, na.rm = TRUE),
      msr_precip_med = median(!!k_msr_precip_avg, na.rm = TRUE),
      msr_precip_max = max(!!k_msr_precip_avg, na.rm = TRUE),
      msr_precip_avg = mean(!!k_msr_precip_avg, na.rm = TRUE)) %>%
    dplyr::select(
      !!k_dim_station,
      !!k_time_month,
      !!k_msr_precip_min,
      !!k_msr_precip_max,
      !!k_msr_precip_avg,
      !!k_msr_precip_med) %>%
    dplyr::arrange(!!k_dim_station, !!k_time_month) %>%
    dplyr::ungroup()
}

# Transform: combine ------------------------------------------------------

data_trans_temperature_combine <- function(
  dat_min,
  dat_max
) {
  k_dim_station <- quo_prepend(default_name("station"), "dim")
  k_time_month <- default_name("time_month")
  k_msr_temp_min <- quo_prepend(default_name("temp_min"), "msr")
  k_msr_temp_max <- quo_prepend(default_name("temp_max"), "msr")

  by <- c(
    rlang::quo_name(k_dim_station),
    rlang::quo_name(k_time_month)
  )
  dat_temp <- dplyr::full_join(dat_min, dat_max, by = by)
  dat_temp <- dplyr::group_by(dat_temp, !!k_dim_station, !!k_time_month) %>%
    dplyr::mutate(
      msr_temp_avg = mean(c(!!k_msr_temp_min, !!k_msr_temp_max), na.rm = TRUE),
      msr_temp_med = median(c(!!k_msr_temp_min, !!k_msr_temp_max), na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  # TODO-20180307-2207: complete tidyfication of eval structure
}

data_trans_precipitation_combine <- function(
  dat_hist,
  dat_recent
) {
  k_dim_station <- quo_prepend(default_name("station"), "dim")
  k_time_month <- default_name("time_month")
  k_msr_precip_min <- quo_xpend(default_name("precip"),
    before = "msr", after = "min")
  k_msr_precip_max <- quo_xpend(default_name("precip"),
    before = "msr", after = "max")
  k_msr_precip_avg <- quo_xpend(default_name("precip"),
    before = "msr", after = "avg")
  k_msr_precip_med <- quo_xpend(default_name("precip"),
    before = "msr", after = "med")
  k_dim_time_type <- quo_prepend(default_name("time_type"), "dim")

  dat_hist <- mutate(
    dat_precip_hist,
    !!dplyr::quo_name(k_dim_time_type) := "historical"
  )
  dat_recent <- mutate(
    dat_precip_recent,
    !!dplyr::quo_name(k_dim_time_type) := "recent"
  )
  dat_precip <- dplyr::bind_rows(dat_hist, dat_recent) %>%
    dplyr::select(
      !!k_dim_station,
      !!k_dim_time_type,
      !!k_time_month,
      !!k_msr_precip_min,
      !!k_msr_precip_max,
      !!k_msr_precip_avg,
      !!k_msr_precip_med)

  dat_precip
}

# Transform: distances ----------------------------------------------------

data_trans_distance <- function(dat, dist_fun = compute_geo_distance) {
  # dist_fun <- geosphere::distHaversine

  # Keys -----
  k_station <- default_name("station") %>% quo_prepend("dim")
  k_station_ref <- default_name("station_ref") %>% quo_prepend("dim")
  k_lat <- default_name("latitude") %>% quo_prepend("dim")
  k_long <- default_name("longitude") %>% quo_prepend("dim")
  k_distance <- default_name("distance") %>% quo_prepend("msr")

  # Nested -----
  dat_nested <- dat %>%
    dplyr::select(!!k_station, !!k_lat, !!k_long) %>%
    dplyr::group_by(!!k_station) %>%
    tidyr::nest(!!k_lat, !!k_long, .key = dim_coords) %>%
    # TODO: handle via quosure
    dplyr::ungroup()

  # Pairwise -----
  dat_nested_2 <- dat_nested %>%
    # slice(1:100) %>%
    # Create the grid
    dplyr::mutate(!!dplyr::quo_name(k_station_ref) := !!k_station) %>%
    dplyr::select(starts_with(dplyr::quo_name(k_station))) %>%
    tidyr::complete(!!k_station, !!k_station_ref) %>%
    dplyr::filter(!!k_station != !!k_station_ref) %>%
    dplyr::left_join(dat_nested, by = dplyr::quo_name(k_station)) %>%
    dplyr::left_join(dat_nested, by = c("dim_station_ref" = "dim_station"))
    # TODO: handle via quosure
    # left_join(dat_nested, by = c(dplyr::quo_name(k_station_ref) := dplyr::quo_name(k_station)))

  dat_nested_3 <- dat_nested_2 %>%
    # Grid completed. Calcualte the distance by distHaversine
    dplyr::mutate(msr_distance = purrr::map2_dbl(dim_coords.x, dim_coords.y, dist_fun))
    # TODO: handle via quosure

  # Select -----
  dat <- dplyr::select(dat_nested_3,
    !!k_station,
    !!k_station_ref,
    !!k_distance)

  dat
}

# Alternative: {geosphere} package
# dat_distance <- geosphere::distm(dat_station %>%
#     dplyr::select(dim_longitude, dim_latitude), fun = geosphere::distHaversine)
#
# station_ids <- dat_station %>% dplyr::pull(dim_station)
# colnames(dat_distance) <- station_ids
# rownames(dat_distance) <- station_ids
#
# dat_distance <- dat_distance %>% as.data.frame()
# dat_distance <- dat_distance %>% tibble::rownames_to_column()
# # dat_distance[1:5, 1:5]
#
# dat_distance <- dat_distance %>% dplyr::rename(dim_station_ref = rowname)
#
# dat_distance_2 <- dat_distance %>% tidyr::gather(key = "dim_station",
#   value = "msr_distance", -dim_station_ref) %>%
#   dplyr::select(dim_station, dim_station_ref, msr_distance)

# Transform: join full ----------------------------------------------------

data_trans_join_full <- function(
  dat_station,
  dat_temp,
  dat_sundur,
  dat_precip
) {
  k_dim_station <- quo_prepend(default_name("station"), "dim")
  k_dim_time_type <- quo_prepend(default_name("time_type"), "dim")
  k_time_month <- default_name("time_month")

  k_msr_temp_min <- quo_xpend(default_name("temp"),
    before = "msr", after = "min")
  k_msr_temp_max <- quo_xpend(default_name("temp"),
    before = "msr", after = "max")
  k_msr_temp_avg <- quo_xpend(default_name("temp"),
    before = "msr", after = "avg")

  k_msr_precip_min <- quo_xpend(default_name("precip"),
    before = "msr", after = "min")
  k_msr_precip_max <- quo_xpend(default_name("precip"),
    before = "msr", after = "max")
  k_msr_precip_avg <- quo_xpend(default_name("precip"),
    before = "msr", after = "avg")
  k_msr_precip_med <- quo_xpend(default_name("precip"),
    before = "msr", after = "med")

  k_msr_sundur_avg <- quo_xpend(default_name("sundur"),
    before = "msr", after = "avg")

  # by <- c("dim_station", "time_month")
  by <- c(
    dplyr::quo_name(k_dim_station),
    dplyr::quo_name(k_time_month)
  )
  dat <- full_join(dat_temp, dat_sundur, by = by)
  dat <- full_join(dat, dat_precip, by = by)
  dat <- dat %>% arrange(!!k_dim_station, !!k_time_month)
  dat <- dat %>% dplyr::select(
    !!k_dim_station,
    !!k_time_month,

    !!k_msr_temp_min,
    !!k_msr_temp_max,
    !!k_msr_temp_avg,

    !!k_msr_precip_min,
    !!k_msr_precip_max,
    !!k_msr_precip_avg,
    !!k_msr_precip_med,

    !!k_msr_sundur_avg,

    !!k_dim_time_type) %>%
    tidyr::unite(uid, !!k_dim_station, !!k_time_month, sep = "-", remove = FALSE)
  # dat %>% head() %>% View()

  # Remove dups ------
  # TODO 20171812: find out why this is even necessary
  idx_dups <- dat$uid %>% duplicated()
  dat <- dat[!idx_dups, ]

  dat
}

data_trans_join_x <- function(
  dat_temp,
  dat_sundur,
  dat_precip,
  join_fun = dplyr::full_join
) {
  k_dim_station <- quo_prepend(default_name("station"), "dim")
  # k_dim_time_type <- quo_prepend(default_name("time_type"), "dim")
  k_time_month <- default_name("time_month")

  k_msr_temp_min <- quo_xpend(default_name("temp"),
    before = "msr", after = "min")
  k_msr_temp_max <- quo_xpend(default_name("temp"),
    before = "msr", after = "max")
  k_msr_temp_avg <- quo_xpend(default_name("temp"),
    before = "msr", after = "avg")

  k_msr_precip_min <- quo_xpend(default_name("precip"),
    before = "msr", after = "min")
  k_msr_precip_max <- quo_xpend(default_name("precip"),
    before = "msr", after = "max")
  k_msr_precip_avg <- quo_xpend(default_name("precip"),
    before = "msr", after = "avg")
  k_msr_precip_med <- quo_xpend(default_name("precip"),
    before = "msr", after = "med")

  k_msr_sundur_day_avg <- quo_xpend(default_name("sundur"),
    before = "msr", after = "day_avg")
  k_msr_sundur_avg <- quo_xpend(default_name("sundur"),
    before = "msr", after = "avg")

  # by <- c("dim_station", "time_month")
  by <- c(
    dplyr::quo_name(k_dim_station),
    dplyr::quo_name(k_time_month)
  )
  dat <- join_fun(dat_temp, dat_sundur, by = by)
  dat <- join_fun(dat, dat_precip, by = by)
  dat <- dat %>% arrange(!!k_dim_station, !!k_time_month)
  dat <- dat %>% dplyr::select(
    !!k_dim_station,
    !!k_time_month,

    !!k_msr_temp_min,
    !!k_msr_temp_max,
    !!k_msr_temp_avg,

    !!k_msr_precip_min,
    !!k_msr_precip_max,
    !!k_msr_precip_avg,
    !!k_msr_precip_med,

    !!quo_name(k_msr_sundur_avg) := !!k_msr_sundur_day_avg

    # !!k_dim_time_type
  ) %>%
    tidyr::unite(uid, !!k_dim_station, !!k_time_month, sep = "-", remove = FALSE)
  # dat %>% head() %>% View()

  # Remove dups ------
  # TODO 20171812: find out why this is even necessary
  idx_dups <- dat$uid %>% duplicated()
  dat <- dat[!idx_dups, ]

  dat
}

data_trans_join_x_v3 <- function(
  dat_temp,
  dat_sundur,
  dat_precip,
  dat_station,
  join_fun = dplyr::full_join
) {
  # v1 + v2 -----
  dat <- data_trans_join_x(
    dat_temp = dat_temp,
    dat_sundur = dat_sundur,
    dat_precip = dat_precip,
    join_fun = join_fun
  )

  # Add longitude, latitude and high -----
  dat <- dat %>%
    left_join(dat_station %>% select(dim_station, dim_country, dim_longitude,
      dim_latitude, dim_high),
      by = "dim_station")
  # TODO: via quosures

  # Impute missing values -----
  dat <- data_trans_impute_missing_values(dat)

  # Correct imputed negative values -----
  modify_fun <- function(x) {
    x[x < 0] <- 0
    x
  }
  dat <- dat %>% purrr::modify_at(
    c("msr_precip_min", "msr_precip_max", "msr_precip_avg",
      "msr_precip_med", "msr_sundur_avg"),
    modify_fun
  )

  dat
}

# NA ----------------------------------------------------------------------

na_to_mean <- function(x) {
  ifelse(is.na(v <- x),
    round(mean(v, na.rm = TRUE), 1),
    v
  )
}

# DB ----------------------------------------------------------------------

# sapply(dat_model, function(x) sum(is.na(x)))
# sapply(dat_model, function(x) length(unique(x)))

# library(Amelia)
# missmap(dat_model, main = "Missing values vs observed")

# dat_model %>% names()

#' @export
data_trans_db <- function(dat_input) {
  dat_model <- dat_input %>%
    group_by(dim_station) %>%
    mutate(
      msr_temp_min = na_to_mean(msr_temp_min),
      msr_temp_max = na_to_mean(msr_temp_max),
      msr_temp_avg = na_to_mean(msr_temp_avg),
      msr_precip_min = na_to_mean(msr_precip_min),
      msr_precip_max = na_to_mean(msr_precip_max),
      msr_precip_avg = na_to_mean(msr_precip_avg),
      msr_precip_med = na_to_mean(msr_precip_med),
      msr_sundur_avg = na_to_mean(msr_sundur_avg)
    ) %>%
    ungroup()

  dat_model
}

# sapply(dat_model, function(x) sum(is.na(x)))
# sapply(dat_model_2, function(x) sum(is.na(x)))

#' @export
data_trans_db_msr <- function(dat_base) {
  dat_base %>%
    select(time_month, matches("msr_")) %>%
    select(-matches("med"))
}

#' @export
data_trans_db_msr_v2 <- function(dat_base) {
  dim_station <- quo_prepend(default_name("station"), "dim")
  # TODO-20180610: do the same for other column names as well

  dat_base %>%
    select(!!dim_station, time_month, matches("msr_")) %>%
    select(-matches("med"))
}

#' @export
dat_transform_relevant_columns <- function(dat) {
  dat <- dat %>%
    dplyr::select(
      id,
      dim_rank,
      dim_country,
      dim_station_name,
      msr_distance,
      # dim_latitude,
      # dim_longitude,
      time_month,
      diff_time_month,
      msr_temp_min,
      diff_msr_temp_min,
      msr_temp_max,
      diff_msr_temp_max,
      msr_precip_avg,
      diff_msr_precip_avg,
      msr_sundur_avg,
      diff_msr_sundur_avg
    )
}

#' @export
dat_transform_names_to_label <- function(
  dat,
  settings = default_settings()
) {
  mapping_info <- settings$name_mapping
  mapping_info <- mapping_info[names(mapping_info) %in% names(dat)]
  df_mapping <- mapping_info %>% purrr::map("label") %>% purrr::map_df("label_1") %>%
    t() %>% as.data.frame() %>% tibble::rownames_to_column() %>%
    select(key = rowname, label = V1) %>%
    mutate(label = as.character(label))

  df_keys <- dat %>%
    names() %>%
    data.frame(stringsAsFactors = FALSE) %>%
    dplyr::select(key = ".")

  df_labels <- left_join(df_keys, df_mapping, by = "key")
  names(dat) <- df_labels %>% dplyr::pull(label)

  dat
}
