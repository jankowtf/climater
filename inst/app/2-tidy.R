
# Prerequisites -----------------------------------------------------------

library(magrittr)

# Settings ----------------------------------------------------------------

# settings <- default_settings()
data_version <- setting_get_version("data_version")

# Station ----------------------------------------------------------------

# v1 -----
dat_station <- data_read_station(dtype = "raw")
dat_station <- data_tidy_station(dat_station)
data_write_station(dat_station)

# v2 -----
dat_station <- data_read_station(dtype = "raw")
dat_station <- data_tidy_station_v2(dat_station)
data_write_station(dat_station, vsn = data_version)
# dat_station <- data_read_station(dtype = "tidy", vsn = data_version)

# Temperature -------------------------------------------------------------

# v1 -----
dat_temp_min <- data_read_temperature_min(dtype = "raw")
dat_temp_min <- data_tidy_temperature_min(dat_temp_min)
data_write_temperature_min(dat_temp_min)

# v2 -----
dat_temp_min <- data_read_temperature_min(dtype = "raw")
dat_temp_min <- data_tidy_temperature_min_v2(dat_temp_min)
data_write_temperature_min(dat_temp_min, vsn = data_version)

# v1 -----
dat_temp_max <- data_read_temperature_max(dtype = "raw")
dat_temp_max <- data_tidy_temperature_max(dat_temp_max)
data_write_temperature_max(dat_temp_max)

# v2 -----
dat_temp_max <- data_read_temperature_max(dtype = "raw")
dat_temp_max <- data_tidy_temperature_max_v2(dat_temp_max)
data_write_temperature_max(dat_temp_max, vsn = data_version)

# Sunshine duration -------------------------------------------------------

# v1 -----
dat_sundur <- data_read_sunshine_duration(dtype = "raw")
dat_sundur <- data_tidy_sunshine_duration(dat_sundur)
data_write_sunshine_duration(dat_sundur)

# v2 -----
dat_sundur <- data_read_sunshine_duration(dtype = "raw")
dat_sundur <- data_tidy_sunshine_duration_v2(dat_sundur)
data_write_sunshine_duration(dat_sundur, vsn = data_version)

# Precipitation -----------------------------------------------------------

# v1 -----
dat_precip_hist <- data_read_precipitation_historical(dtype = "raw")
dat_precip_hist <- data_tidy_precipitation(dat_precip_hist)
data_write_precipitation_historical(dat_precip_hist)

# v2 -----
dat_precip_hist <- data_read_precipitation_historical(dtype = "raw")
dat_precip_hist <- data_tidy_precipitation_v2(dat_precip_hist)
data_write_precipitation_historical(dat_precip_hist, vsn = data_version)

# v1 -----
dat_precip_recent <- data_read_precipitation_recent(dtype = "raw")
dat_precip_recent <- data_tidy_precipitation(dat_precip_recent)
data_write_precipitation_recent(dat_precip_recent)

# v2 -----
dat_precip_recent <- data_read_precipitation_recent(dtype = "raw")
dat_precip_recent <- data_tidy_precipitation_v2(dat_precip_recent)
data_write_precipitation_recent(dat_precip_recent, vsn = data_version)
