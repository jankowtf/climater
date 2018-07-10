
# Packages ----------------------------------------------------------------

library(magrittr)

# Sanity check sundur -----------------------------------------------------

dat_sundur <- data_read_sunshine_duration_v2(dtype = "raw")
dat_sundur <- data_tidy_sunshine_duration_v2(dat_sundur)

skimr::skim_with(
  numeric = list(
    neg_n = function(x) sum(x < 0, na.rm = TRUE)
  )
)

dat_sundur %>% skimr::skim()
# OK

# Sanity check precip -----------------------------------------------------

dat_precip_recent <- data_read_precipitation_recent_v2(dtype = "raw")
dat_precip_recent %>% skimr::skim()
dat_precip_recent_2 <- data_tidy_precipitation_v2(dat_precip_recent)
dat_precip_recent_2 %>% skimr::skim()
# data_write_precipitation_recent(dat_precip_recent, vsn = data_version)

dat_precip_hist <- data_read_precipitation_historical_v2(dtype = "raw")
dat_precip_hist %>% skimr::skim()
dat_precip_hist_2 <- data_tidy_precipitation_v2(dat_precip_hist)
dat_precip_hist_2 %>% skimr::skim()
# data_write_precipitation_historical(dat_precip_hist, vsn = data_version)

dat_precip <- data_trans_precipitation_combine(
  dat_hist = dat_precip_hist_2,
  dat_recent = dat_precip_recent_2
)

dat_precip <- data_read_precipitation_comb(dtype = "tidy", vsn = data_version)

skimr::skim_with(
  numeric = list(
    neg_n = function(x) sum(x < 0, na.rm = TRUE)
  )
)

dat_precip %>% skimr::skim()
# OK

