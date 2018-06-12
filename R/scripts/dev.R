
# Anti join check ---------------------------------------------------------

if (FALSE) {
  # Pair temperature/sunshine duration
  res <- anti_join(dat_temp, dat_sundur, by = c("dim_station", "time_month"))
  res <- anti_join(dat_sundur, dat_temp, by = c("dim_station", "time_month"))

  # Pair temperature/precipitation
  res <- anti_join(dat_temp,
    filter(dat_precip, dim_time_type == "recent"), by = c("dim_station", "time_month"))
  res <- anti_join(filter(dat_precip, dim_time_type == "recent"),
    dat_temp, by = c("dim_station", "time_month"))
}
