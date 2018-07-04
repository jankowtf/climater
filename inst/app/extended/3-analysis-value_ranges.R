
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

dat_sundur %>% skimr::skim_to_wide() %>%
  dplyr::filter(type == "numeric") %>%
  purrr::modify_at(c("n", "neg_n"), as.numeric) %>%
  dplyr::select(variable, n, neg_n) %>%
  dplyr::mutate(neg_perc = neg_n / n)

# Abs to get rid of negative values -----
dat_sundur <- data_read_sunshine_duration_v2(dtype = "raw")
dat_sundur <- data_tidy_sunshine_duration_v3(dat_sundur)

dat_sundur %>% skimr::skim()
