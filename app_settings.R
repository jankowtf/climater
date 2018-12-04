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
    show = FALSE
  ),
  # -> controls if modeling results are shown via a temporary HTML file
  versions = list(
    # data_version = "v1"
    # data_version = "v2"
    # -> normalization of station IDs (4-digit to 5-digit)
    data_version = "v3",
    # -> imputation of missing values (via simputation::impute_lm with
    # sequential models)
    frontend_version = "v0.0.0.9005"
  ),
  data_repo = list(
    repo_1 = "C:/users/janko/dropbox (personal)/data/climater",
    repo_2 = "C:/users/Sebastian/climater"
  ),
  # -> sets the data version to be used
  scaling = list(
    # distance_1 = 0
    # distance_1 = 0.001
    distances = list(
      1,
      0.005,
      0.001,
      0.0003,
      0
    ),
    time = list(
      2,
      1,
      0.5,
      0.25,
      0
    )
  ),

  ##############################################################################
  # Backend settings >> DO NOT CHANGE -----
  ##############################################################################
  # TODO 20181122: align with settings approach from FVA project
  data = list(
    # dir_raw = "inst/app/data/raw",
    # dir_tidy = "inst/app/data/tidy",
    dir_raw = "data/raw",
    dir_tidy = "data/tidy",
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
      ),
      distance = list(
        tidy = "distance.rds"
      )
    )
  ),
  name_mapping = list(
    station = list(
      key = dplyr::quo(station),
      label = list(
        label_1 = "Location"
      )
    ),
    station_ref = list(
      key = dplyr::quo(station_ref),
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
    time_type = list(
      key = dplyr::quo(time_type),
      label = list()
    ),
    msr_precip_avg = list(
      key = dplyr::quo(msr_precip_avg),
      label = list(
        label_1 = "Rain days per month (avg.)"
      )
    ),
    precip = list(
      key = dplyr::quo(precip),
      label = list(
        label_1 = "Rain days per month"
      )
    ),
    temp = list(
      key = dplyr::quo(temp),
      label = list()
    ),
    temp_min = list(
      key = dplyr::quo(temp_min),
      label = list(
        label_1 = "Min. temperature"
      )
    ),
    temp_max = list(
      key = dplyr::quo(temp_max),
      label = list(
        label_1 = "Max. temperature"
      )
    ),
    sundur = list(
      key = dplyr::quo(sundur),
      label = list(
        label_1 = "Sunshine hours per day"
      )
    ),
    latitude = list(
      key = dplyr::quo(latitude),
      label = list(
        label_1 = "Latitude"
      )
    ),
    longitude = list(
      key = dplyr::quo(longitude),
      label = list(
        label_1 = "Longitude"
      )
    ),
    distance = list(
      key = dplyr::quo(distance),
      label = list(
        label_1 = "Distance"
      )
    ),

    # Restart 2018-07-08 -----
    input = list(
      key = dplyr::quo(input),
      label = list(
        label_1 = "Input ID"
      )
    ),
    dim_rank = list(
      key = dplyr::quo(dim_rank),
      label = list(
        label_1 = "Rank"
      )
    ),
    dim_latitude = list(
      key = dplyr::quo(dim_latitude),
      label = list(
        label_1 = "Latitude"
      )
    ),
    dim_longitude = list(
      key = dplyr::quo(dim_longitude),
      label = list(
        label_1 = "Longitude"
      )
    ),
    dim_country = list(
      key = dplyr::quo(dim_country),
      label = list(
        label_1 = "Country"
      )
    ),
    dim_station_name = list(
      key = dplyr::quo(dim_station_name),
      label = list(
        label_1 = "Location"
      )
    ),
    msr_distance = list(
      key = dplyr::quo(msr_distance),
      label = list(
        label_1 = "Distance"
      )
    ),
    time_month = list(
      key = dplyr::quo(time_month),
      label = list(
        label_1 = "Month"
      )
    ),
    diff_time_month = list(
      key = dplyr::quo(time_month_diff),
      label = list(
        label_1 = "Month delta"
      )
    ),
    msr_temp_min = list(
      key = dplyr::quo(msr_temp_min),
      label = list(
        label_1 = "Min. temperature"
      )
    ),
    diff_msr_temp_min = list(
      key = dplyr::quo(msr_temp_min_diff),
      label = list(
        label_1 = "Min. temperature delta"
      )
    ),
    msr_temp_max = list(
      key = dplyr::quo(msr_temp_max),
      label = list(
        label_1 = "Max. temperature"
      )
    ),
    diff_msr_temp_max = list(
      key = dplyr::quo(msr_temp_max_diff),
      label = list(
        label_1 = "Max. temperature delta"
      )
    ),
    msr_temp_avg = list(
      key = dplyr::quo(msr_temp_avg),
      label = list(
        label_1 = "Avg. temperature"
      )
    ),
    diff_msr_temp_avg = list(
      key = dplyr::quo(msr_temp_avg_diff),
      label = list(
        label_1 = "Avg. temperature delta"
      )
    ),
    msr_precip_avg = list(
      key = dplyr::quo(msr_precip_avg),
      label = list(
        label_1 = "Rain days per month"
      )
    ),
    diff_msr_precip_avg = list(
      key = dplyr::quo(msr_precip_avg_diff),
      label = list(
        label_1 = "Rain days per month delta"
      )
    ),
    msr_sundur_avg = list(
      key = dplyr::quo(msr_sundur_avg),
      label = list(
        label_1 = "Sunshine hours per day"
      )
    ),
    diff_msr_sundur_avg = list(
      key = dplyr::quo(msr_sundur_avg_diff),
      label = list(
        label_1 = "Sunshine hours per day delta"
      )
    ),
    # fct_scaling = list(
    #   key = dplyr::quo(fct_scaling),
    #   label = list(
    #     label_1 = "Scaling factor"
    #   )
    # )
    id = list(
      key = dplyr::quo(id),
      label = list(
        label_1 = "ID"
      )
    )
  )
)


