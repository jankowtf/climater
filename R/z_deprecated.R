#' @export
model_predict_index <- function(
  model_estimation,
  knn = 3
) {
  .Deprecated("model_predict_index_v3")
  # Get names of computed distance measures -----
  dist_measures <- names(model_estimation[[1]][[1]])

  model_prediction <- lapply(model_estimation, function(estimation) {
    res <- lapply(dist_measures, function(mea) {
      # Select particular distance measure result for all stations -----
      values <- sapply(estimation, "[[", mea)
      names(values) <- 1:length(values)

      # Sort estimation results -----
      # values <- sort(values[values != 0])
      values <- sort(values)

      # Select best predictions based on knn -----
      as.numeric(names(values[1:knn]))
    })
    names(res) <- dist_measures
    res
  })
}

model_predict_index_v2 <- function(
  model_estimation,
  knn = 3
) {
  .Deprecated("model_predict_index_v3")
  # Get names of computed distance measures -----
  dist_measures <- names(model_estimation[[1]]$distance_stat[[1]])
  # TODO-20180705: make this nicer/more robust

  model_prediction <- lapply(model_estimation, function(estimation) {
    index <- lapply(dist_measures, function(mea) {
      # Select particular distance measure result for all stations -----
      values <- sapply(estimation$distance_stat, "[[", mea)
      names(values) <- 1:length(values)

      # Sort estimation results -----
      # values <- sort(values[values != 0])
      values <- sort(values)

      # Select best predictions based on knn -----
      as.numeric(names(values[1:knn]))
    })
    names(index) <- dist_measures

    list(
      distance_stat_index = index,
      dat_distance_geo = estimation$dat_distance_geo
    )
  })

  model_prediction
}

#' @export
model_predict <- function(
  model_prediction_index,
  dat_input,
  dat_db,
  dat_station,
  knn
) {
  .Deprecated("model_predict_v3")
  # Ensure matrix -----
  dat_input <- as.matrix(dat_input)

  dat_result <- lapply(1:length(model_prediction_index), function(idx_input) {
    choice <- model_prediction_index[[idx_input]]
    dist_meassure <- unlist(lapply(names(choice), rep, knn))
    index <- unlist(choice)

    # Get station ID ------
    dim_station <- dat_db$dim_station[index]

    # Get ranks -----
    rank <- rep(seq(knn), length(choice))

    # Input data -----
    dat_input_row <- dat_input[idx_input, ]

    # Prediction data -----
    dat_prediction <- dat_db[index, ] %>%
      select(time_month, matches("msr_")) %>%
      mutate(
        dim_rank = rank,
        dim_station = dim_station,
        dim_dist_measure = dist_meassure,
        diff_time_month = time_month - dat_input_row["time_month"],
        diff_msr_temp_min = msr_temp_min - dat_input_row["msr_temp_min"],
        diff_msr_temp_max = msr_temp_max - dat_input_row["msr_temp_max"],
        diff_msr_temp_avg = msr_temp_avg - dat_input_row["msr_temp_avg"],
        diff_msr_precip_min = msr_precip_min - dat_input_row["msr_precip_min"],
        diff_msr_precip_max = msr_precip_max - dat_input_row["msr_precip_max"],
        diff_msr_precip_avg = msr_precip_avg - dat_input_row["msr_precip_avg"],
        diff_msr_sundur_avg = msr_sundur_avg - dat_input_row["msr_sundur_avg"]
      )

    dat_prediction <- dat_prediction %>% mutate_if(is.double, round, 1)

    # Join stations -----
    dat_prediction_2 <- inner_join(
      dat_prediction,
      dat_station,
      by = "dim_station"
    ) %>%
      select(
        dim_rank,
        dim_country,
        dim_station_name,
        time_month,
        diff_time_month,
        msr_temp_min,
        diff_msr_temp_min,
        msr_temp_max,
        diff_msr_temp_max,
        msr_temp_avg,
        diff_msr_temp_avg,
        msr_precip_min,
        diff_msr_precip_min,
        msr_precip_max,
        diff_msr_precip_max,
        msr_precip_avg,
        diff_msr_precip_avg,
        msr_sundur_avg,
        diff_msr_sundur_avg,
        dim_station,
        dim_latitude,
        dim_longitude,
        dim_high,
        dim_dist_measure,
        everything()
      )

    # Return -----
    list(
      input = dat_input_row,
      prediction = dat_prediction_2
    )
  })
}

model_predict_v2 <- function(
  model_prediction_index,
  dat_input,
  dat_db,
  dat_station,
  knn
) {
  .Deprecated("model_predict_v3")
  # Ensure matrix -----
  # dim_station <- default_name("station") %>% quo_prepend("dim")
  dat_input <- dat_input %>%
    # dplyr::select(-dim_station) %>%
    as.matrix()

  dat_result <- lapply(1:length(model_prediction_index), function(idx_input) {
    choice <- model_prediction_index[[idx_input]]$distance_stat_index
    dist_meassure <- unlist(lapply(names(choice), rep, knn))
    index <- unlist(choice)

    # Get station ID ------
    dim_station <- dat_db$dim_station[index]

    # Get ranks -----
    rank <- rep(seq(knn), length(choice))

    # Input data -----
    dat_input_row <- dat_input[idx_input, ]

    # Prediction data -----
    dat_prediction <- dat_db[index, ] %>%
      select(time_month, matches("msr_")) %>%
      mutate(
        dim_rank = rank,
        dim_station = dim_station,
        dim_dist_measure = dist_meassure,
        diff_time_month = time_month - dat_input_row["time_month"],
        diff_msr_temp_min = msr_temp_min - dat_input_row["msr_temp_min"],
        diff_msr_temp_max = msr_temp_max - dat_input_row["msr_temp_max"],
        diff_msr_temp_avg = msr_temp_avg - dat_input_row["msr_temp_avg"],
        diff_msr_precip_min = msr_precip_min - dat_input_row["msr_precip_min"],
        diff_msr_precip_max = msr_precip_max - dat_input_row["msr_precip_max"],
        diff_msr_precip_avg = msr_precip_avg - dat_input_row["msr_precip_avg"],
        diff_msr_sundur_avg = msr_sundur_avg - dat_input_row["msr_sundur_avg"]
      )

    dat_prediction <- dat_prediction %>% mutate_if(is.double, round, 1)

    dat_distance_geo <- model_prediction_index[[idx_input]]$dat_distance_geo

    # Join stations -----
    dat_prediction_2 <- inner_join(
      dat_prediction,
      dat_station,
      by = "dim_station"
    ) %>%
      left_join(
        dat_distance_geo,
        by = "dim_station"
      ) %>%
      select(
        dim_rank,
        dim_country,
        dim_station_name,
        msr_distance,
        time_month,
        diff_time_month,
        msr_temp_min,
        diff_msr_temp_min,
        msr_temp_max,
        diff_msr_temp_max,
        msr_temp_avg,
        diff_msr_temp_avg,
        msr_precip_min,
        diff_msr_precip_min,
        msr_precip_max,
        diff_msr_precip_max,
        msr_precip_avg,
        diff_msr_precip_avg,
        msr_sundur_avg,
        diff_msr_sundur_avg,
        dim_station,
        dim_latitude,
        dim_longitude,
        dim_high,
        dim_dist_measure,
        everything()
      )

    # Return -----
    list(
      input = dat_input_row,
      prediction = dat_prediction_2
    )
  })
}
