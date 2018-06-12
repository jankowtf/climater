
# Packages ----------------------------------------------------------------

library(dplyr)

# Settings ----------------------------------------------------------------

data_version <- setting_get_version("data_version")

# Read imported data ------------------------------------------------------

dat_station <- data_read_station(dtype = "tidy", vsn = data_version)

# Temperature -------------------------------------------------------------

dat_temp_min <- data_read_temperature_min(dtype = "tidy", vsn = data_version)
dat_temp_max <- data_read_temperature_max(dtype = "tidy", vsn = data_version)

dat_temp <- data_trans_temperature_combine(
  dat_min = dat_temp_min,
  dat_max = dat_temp_max)

dat_temp <- dat_temp %>% mutate_if(is.double, round, 1)

data_write_temperature_comb(dat_temp, vsn = data_version)

# Sunshine duration -------------------------------------------------------

dat_sundur <- data_read_sunshine_duration(dtype = "tidy", vsn = data_version)

# Precipitation -----------------------------------------------------------

dat_precip_hist <- data_read_precipitation_historical(dtype = "tidy",
  vsn = data_version)
dat_precip_recent <- data_read_precipitation_recent(dtype = "tidy",
  vsn = data_version)

dat_precip <- data_trans_precipitation_combine(
  dat_hist = dat_precip_hist,
  dat_recent = dat_precid_recent
)

dat_precip <- dat_precip %>% mutate_if(is.double, round, 1)

data_write_precipitation_comb(dat_precip, vsn = data_version)

# Distances ---------------------------------------------------------------

# dat_distance <- data_trans_distance(dat_station)
#
# data_write_distance(dat_distance, vsn = data_version)
# dat_distance <- data_read_distance(vsn = data_version)

# Join full ---------------------------------------------------------------

dat_join_full <- if (data_version <= "v2") {
  # v1 + v2 -----
  data_trans_join_x(
    dat_temp = dat_temp,
    dat_sundur = dat_sundur,
    dat_precip = dat_precip,
    join_fun = dplyr::full_join
  )
} else if (data_version == "v3") {
  # v3 -----
  data_trans_join_x_v3(
    dat_temp = dat_temp,
    dat_sundur = dat_sundur,
    dat_precip = dat_precip,
    dat_station = dat_station,
    join_fun = dplyr::full_join
  )
}

data_write_join_full(dat_join_full, vsn = data_version)

# Join inner --------------------------------------------------------------

dat_join_inner <- if (data_version <= "v2") {
  # v1 + v2 -----
  data_trans_join_x(
    dat_temp = dat_temp,
    dat_sundur = dat_sundur,
    dat_precip = dat_precip,
    join_fun = dplyr::inner_join
  )
} else if (data_version == "v3") {
  # v3 -----
  data_trans_join_x_v3(
    dat_temp = dat_temp,
    dat_sundur = dat_sundur,
    dat_precip = dat_precip,
    dat_station = dat_station,
    join_fun = dplyr::inner_join
  )
}

data_write_join_inner(dat_join_inner, vsn = data_version)

# Prepare for modeling ----------------------------------------------------

dat_db <- data_trans_db(dat_join_inner)
data_write_db(dat_db, vsn = data_version)

# dat_db_msr <- data_trans_db_msr(dat_db)
dat_db_msr <- data_trans_db_msr_v2(dat_db)
data_write_db_msr(dat_db_msr, vsn = data_version)
