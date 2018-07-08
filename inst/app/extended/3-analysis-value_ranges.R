
# Packages ----------------------------------------------------------------

library(skimr)

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
