
# Write -------------------------------------------------------------------

data_write_station <- function(
  dat,
  vsn = default_version(),
  settings = default_settings()
) {
  file <- data_con_station(dtype = "tidy",
    vsn = vsn, settings = settings)
  dat %>%
    dplyr::arrange(dim_station) %>%
    saveRDS(file = file)
  # TODO-20180307-1523: align with tidy eval
  file
}

data_write_temperature_min <- function(
  dat,
  vsn = default_version(),
  settings = default_settings()
) {
  k_station <- quo_prepend(default_name("station"), "dim")
  k_time_month <- default_name("time_month")

  file <- data_con_temperature_min(dtype = "tidy",
    vsn = vsn, settings = settings)
  dat %>%
    dplyr::arrange(!!k_station, !!k_time_month) %>%
    saveRDS(file = file)
  file
}

data_write_temperature_max <- function(
  dat,
  vsn = default_version(),
  settings = default_settings()
) {
  k_station <- quo_prepend(default_name("station"), "dim")
  k_time_month <- default_name("time_month")

  file <- data_con_temperature_max(dtype = "tidy",
    vsn = vsn, settings = settings)
  dat %>%
    dplyr::arrange(!!k_station, !!k_time_month) %>%
    saveRDS(file = file)
  file
}

data_write_temperature_comb <- function(
  dat,
  vsn = default_version(),
  settings = default_settings()
) {
  file <- data_con_temperature_comb(dtype = "tidy",
    vsn = vsn, settings = settings)
  dat %>%
    saveRDS(file = file)
  file
}

data_write_sunshine_duration <- function(
  dat,
  vsn = default_version(),
  settings = default_settings()
) {
  k_station <- quo_prepend(default_name("station"), "dim")
  k_time_month <- default_name("time_month")

  file <- data_con_sunshine_duration(dtype = "tidy",
    vsn = vsn, settings = settings)
  dat %>%
    dplyr::arrange(!!k_station, !!k_time_month) %>%
    saveRDS(file = file)
  file
}

data_write_precipitation_historical <- function(
  dat,
  vsn = default_version(),
  settings = default_settings()
) {
  file <- data_con_precipitation_historical(dtype = "tidy",
    vsn = vsn, settings = settings)
  dat %>%
    saveRDS(file = file)
  file
}

data_write_precipitation_recent <- function(
  dat,
  vsn = default_version(),
  settings = default_settings()
) {
  file <- data_con_precipitation_recent(dtype = "tidy",
    vsn = vsn, settings = settings)
  dat %>%
    saveRDS(file = file)
  file
}

data_write_precipitation_comb <- function(
  dat,
  vsn = default_version(),
  settings = default_settings()
) {
  file <- data_con_precipitation_comb(dtype = "tidy",
    vsn = vsn, settings = settings)
  dat %>%
    saveRDS(file = file)
  file
}

data_write_join_full <- function(
  dat,
  vsn = default_version(),
  settings = default_settings()
) {
  file <- data_con_join_full(dtype = "tidy",
    vsn = vsn, settings = settings)
  dat %>%
    saveRDS(file = file)
  file
}

data_write_join_inner <- function(
  dat,
  vsn = default_version(),
  settings = default_settings()
) {
  file <- data_con_join_inner(dtype = "tidy",
    vsn = vsn, settings = settings)
  dat %>%
    saveRDS(file = file)
  file
}

data_write_db <- function(
  dat,
  vsn = default_version(),
  settings = default_settings()
) {
  file <- data_con_db(dtype = "tidy",
    vsn = vsn, settings = settings)
  dat %>%
    saveRDS(file = file)
  file
}

data_write_db_msr <- function(
  dat,
  vsn = default_version(),
  settings = default_settings()
) {
  file <- data_con_db_msr(dtype = "tidy",
    vsn = vsn, settings = settings)
  dat %>%
    saveRDS(file = file)
  file
}

data_write_distance <- function(
  dat,
  vsn = default_version(),
  settings = default_settings()
) {
  file <- data_con_distance(dtype = "tidy",
    vsn = vsn, settings = settings)
  dat %>%
    saveRDS(file = file)
  file
}
