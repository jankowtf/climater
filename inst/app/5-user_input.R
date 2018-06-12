
# Packages ----------------------------------------------------------------

library(dplyr)

# Top-level settings ------------------------------------------------------

knn <- 3
# How many recommendations would the user like to see

# dist_measures <- default_dist_measures()
dist_measures <- "euclidean"

dist_measure_final <- "euclidean"
# dist_measure_final <- character()
# Any one of default_dist_measures()
# Set this to character() if you would like to get an ensemble recommendation

# Actual numerical input data ---------------------------------------------

dat_input <- dat_db_msr %>% as.data.frame() %>% sample_n(5)

# Add reference location -----
ref_location <- dat_station %>%
  filter(stringr::str_detect(dim_station_name, "Hamburg")) %>%
  select(-dim_station)
dat_input$dim_latitude <- ref_location$dim_latitude
dat_input$dim_longitude <- ref_location$dim_longitude
dat_input$msr_distance <- 1.5

# Alternative input -----

if (FALSE) {
  # Example input from actual DB -----
  set.seed(23981113)
  dat_input <- dat_db_msr %>% as.data.frame() %>% sample_n(1)

  # Example input from actual DB, but only selected columns -----
  dat_input <- dat_input %>% select(time_month, msr_temp_min)

  # Example of completely new input that doesn't match any DB record -----
  dat_input <- data.frame(
    time_month = 9,
    msr_temp_min = 15,
    msr_temp_max = 28,
    msr_temp_avg = 24.5,
    msr_precip_min = 5,
    msr_precip_max = 15,
    msr_precip_avg = 10,
    msr_sundur_avg = 260
  )

  dat_input <- data.frame(
    time_month = 9,
    msr_temp_min = 22,
    msr_temp_avg = 28,
    msr_precip_avg = 10
  )

  dat_input <- data.frame(
    time_month = 5,
    msr_temp_avg = 22
  )
}
