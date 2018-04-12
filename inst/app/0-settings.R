settings <- list(
  # Top-level control settings -----
  app_name = "climater",
  # -> Currently not used/important
  overwrite = list(
    tidy = FALSE,
    transform = FALSE
  ),
  # -> controls which parts of the data processing should be overwritten
  output = list(
    show = TRUE
  ),
  # -> controls if modeling results are shown via a temporary HTML file
  versions = list(
    # data_version = "v1"
    # data_version = "v2"
    # -> normalization of station IDs (4-digit to 5-digit)
    data_version = "v3"
    # -> imputation of missing values (via simputation::impute_lm with
    # sequential models)
  ),
  # -> sets the data version to be used
  # Backend settings >> DO NOT CHANGE -----
  data = list(
    dir_raw = "inst/app/data/raw",
    dir_tidy = "inst/app/data/tidy",
    cons = list(
      station = list(
        raw = "stations_list_CLIMAT_data.txt",
        tidy = "station.rds"
      ),
      temperature_min = list(
        raw = "multi-annual/dailyTmin_1961_1990.txt",
        tidy = "temp_min_1961_1990.rds"
      ),
      temperature_max = list(
        raw = "multi-annual/dailyTmax_1961_1990.txt",
        tidy = "temp_max_1961_1990.rds"
      ),
      sunshine_duration = list(
        raw = "multi-annual/sunshine_duration_1961_1990.txt",
        tidy = "sundur_avg_1961_1990.rds"
      ),
      precipitation_historical = list(
        raw = "precipGE1mm_days/historical",
        tidy = "precip_avg_hist.rds"
      ),
      precipitation_recent = list(
        raw = "precipGE1mm_days/recent",
        tidy = "precip_avg_recent.rds"
      ),
      temperature_comb = list(
        tidy = "temp_comb_1961_1990.rds"
      ),
      precipitation_comb = list(
        tidy = "precip_avg_comb.rds"
      ),
      join_full = list(
        tidy = "join_full.rds"
      ),
      join_inner = list(
        tidy = "join_inner.rds"
      ),
      db = list(
        tidy = "db.rds"
      ),
      db_msr = list(
        tidy = "db_msr.rds"
      )
    )
  ),
  name_mapping = list(
    station = list(
      key = dplyr::quo(station),
      label = list()
    ),
    time_start = list(
      key = dplyr::quo(time_start),
      label = list()
    ),
    time_stop = list(
      key = dplyr::quo(time_stop),
      label = list()
    ),
    time_month = list(
      key = dplyr::quo(time_month),
      label = list()
    ),
    time_type = list(
      key = dplyr::quo(time_type),
      label = list()
    ),
    msr_precip_avg = list(
      key = dplyr::quo(msr_precip_avg),
      label = list()
    ),
    temp = list(
      key = dplyr::quo(temp),
      label = list()
    ),
    temp_min = list(
      key = dplyr::quo(temp_min),
      label = list()
    ),
    temp_max = list(
      key = dplyr::quo(temp_max),
      label = list()
    ),
    precip = list(
      key = dplyr::quo(precip),
      label = list()
    ),
    sundur = list(
      key = dplyr::quo(sundur),
      label = list()
    )
  )
)

