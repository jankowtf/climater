
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

model_run_v2 <- function(
  dat_input,
  dat_db,
  dat_station,
  dist_measures,
  dist_measure_final,
  knn
) {
  .Deprecated("model_run_v3")
  # Model estimation --------------------------------------------------------

  model_estimation <- model_estimate_v4(
    dat_input = dat_input,
    dat_db = dat_db_msr,
    dat_station = dat_station,
    dist_measures = dist_measures
  )

  # Identify best choices ---------------------------------------------------

  model_prediction_index <- model_predict_index_v4(
    model_estimation = model_estimation,
    knn = knn
  )

  # Model prediction --------------------------------------------------------

  model_prediction <- model_predict_v4(
    model_prediction_index,
    dat_station = dat_station,
    knn = knn
  )

  model_prediction_ensemble <- model_predict_ensemble_v2(
    model_prediction = model_prediction
  )

  # Model output ------------------------------------------------------------

  model_result_gathered <- model_output_gathered(model_prediction_ensemble)

  if (settings$output$show) {
    print("Show:")
    print(settings$output$show)
    model_result_render(
      model_result_gathered,
      knn = knn,
      dist_measures = dist_measures,
      dist_measure_final = dist_measure_final
    )
  }

  model_result_gathered
}

model_estimate_v4 <- function(
  dat_input,
  dat_db,
  dat_station,
  dist_measures = default_dist_measures()
) {
  .Deprecated("model_estimate_v5")
  dat_db_out <- dat_db %>%
    dplyr::mutate(
      scaling_factor = NA
    )
  # Argument handling -----
  dist_measures <- match.arg(dist_measures, several.ok = TRUE)

  # Ensure matrix -----
  # dim_station <- default_name("station") %>% quo_prepend("dim")
  dat_input <- dat_input %>%
    # dplyr::select(-dim_station) %>%
    as.matrix()

  # Compute distance for each input row/vector -----
  dat_dist <- lapply(1:nrow(dat_input), function(row_user) {
    dat_input_row <- dat_input[row_user, , drop = FALSE]

    # Distances -----
    dat_distance_geo <- data.frame(
      dim_station = unique(dat_station$dim_station),
      msr_distance = NA,
      stringsAsFactors = FALSE
    )

    # Handle time input -----
    # Only keep weather records that have time == user input
    dat_list <- handle_input_time(
      dat_input = dat_input_row,
      dat_db = dat_db
    )
    # Update input and DB data:
    dat_input_row <- dat_list$dat_input
    dat_db <- dat_list$dat_db

    cols <- c("dim_latitude", "dim_longitude", "msr_distance")
    if (all(cols %in% colnames(dat_input_row))) {
      # Get scaling factor(s) from settings -----
      # scaling_factor <- settings$scaling$distances
      dat_scaling_factor <- tibble(scaling_factor = unlist(settings$scaling$distances))

      # Compute geo distance -----
      # dat_msr_distance <- compute_geo_distance_v2(p_1 = dat_input_row, p_2 = dat_station)
      dat_msr_distance <- dat_scaling_factor %>%
        dplyr::group_by(scaling_factor) %>%
        dplyr::do(
          pipe_compute_geo_distance_v2(
            .,
            dat_input_row = dat_input_row,
            dat_station = dat_station
          )
        ) %>%
        ungroup()

      # Handle distance input -----
      # Only keep stations that have distance <= user input
      dat_list <- handle_input_distance(
        dat_input = dat_input_row,
        dat_msr_distance = dat_msr_distance,
        dat_scaling_factor = dat_scaling_factor
      )
      # Update input and distance data:
      dat_input_row <- dat_list$dat_input
      dat_msr_distance <- dat_list$dat_msr_distance

      # Join distances to DB data -----
      col_dim_station <- as.symbol("dim_station")
      value_dim_station <- dat_msr_distance %>% dplyr::pull(!!col_dim_station)
      # Ensure stations in DB data match those in distance data:
      dat_db <- dat_db %>%
        dplyr::filter(!!col_dim_station %in% value_dim_station)
      # Cache a version of DB data before the join for downstream processing:
      dat_db_out <- dat_db
      # Actual join:
      dat_db <- left_join(
        dat_db,
        dat_msr_distance,
        by = "dim_station"
      )

      dat_distance_geo <- dat_db %>%
        select(
          scaling_factor,
          dim_station,
          msr_distance,
          msr_distance_scaled
        ) %>%
        group_by(
          scaling_factor,
          dim_station
        ) %>%
        summarise(
          msr_distance = unique(msr_distance),
          msr_distance_scaled = unique(msr_distance_scaled)
        ) %>%
        ungroup()
      # TODO 20181122: tidy eval for column names
    } else {
      # PATCH 20181122
      # TODO 20181122: remove patch
      dat_input_row <- cbind(
        data.frame(scaling_factor = NA) %>% as.matrix(),
        dat_input_row
      )
      dat_db$scaling_factor <- NA
      dat_distance_geo$scaling_factor <- NA
    }

    # Drop all dims -----
    dat_db <- dat_db %>%
      select(-matches("dim_")) %>%
      as.matrix()
    # TODO-20180610: encapsulate in function
    idx <- colnames(dat_input_row) %>% str_detect("dim")
    dat_input_row <- dat_input_row[ , !idx, drop = FALSE]

    # estimation_result <- lapply(1:nrow(dat_db), function(row) {
    #   dat_db_this <- dat_db[row, colnames(dat_input_row), drop = FALSE]
    #
    #   # List availble distance measures
    #   # philentropy::getDistMethods()
    #
    #   res <- list(
    #     euclidean = if ("euclidean" %in% dist_measures) {
    #       philentropy:::euclidean(
    #         dat_input_row, dat_db_this, testNA = FALSE)
    #     }
    #   )
    #   res[!sapply(res, is.null)]
    # })

    dat_input_row <- dat_input_row %>% as_tibble()
    names_old <- dat_input_row %>%
      dplyr::select(-scaling_factor) %>%
      names() %>%
      rlang::syms()

    names_new <- c(
      list(rlang::sym("scaling_factor")),
      names_old %>%
        paste0("input_", .) %>%
        rlang::syms()
    )

    names_mapping <- c(
      list(rlang::sym("scaling_factor")),
      names_old
    )
    names(names_mapping) <- names_new
    dat_input_row <- dat_input_row %>% dplyr::rename(!!!names_mapping)

    dat_estimation <- left_join(
      dat_db %>% as_tibble(),
      dat_input_row,
      by = "scaling_factor"
    ) %>%
      dplyr::select(
        scaling_factor,
        everything()
      )

    estimation_result <- dat_estimation %>%
      dplyr::group_by(scaling_factor) %>%
      dplyr::do(
        pipe_model_estimate(
          .,
          dist_measures = dist_measures
        )
      ) %>%
      ungroup()

    list(
      estimation_result = estimation_result,
      dat_distance_geo = dat_distance_geo,
      dat_input = dat_input,
      dat_db = dat_db_out
    )
    # TODO-20180705: tidyfy column name

  })
}

pipe_compute_geo_distance_v2 <- function(
  dat_scaling_factor,
  dat_input_row,
  dat_station
) {
  .Deprecated("pipe_compute_geo_distance_v3")
  dat_msr_distance <- compute_geo_distance_v2(
    p_1 = dat_input_row,
    p_2 = dat_station
  )
  dat_msr_distance <- bind_cols(
    dat_msr_distance,
    dat_msr_distance * dat_scaling_factor %>% dplyr::pull(scaling_factor)
  ) %>%
    dplyr::rename(msr_distance_scaled = msr_distance1)
  # TODO 20181122: tidy eval for column names

  dat_msr_distance$dim_station <- dat_station$dim_station
  dat_msr_distance
}

# https://www.kompf.de/gps/distcalc.html
compute_geo_distance <- function(p_1, p_2) {
  dist <- 6378.388 * acos(
    sin(p_1$dim_latitude*pi/180) * sin(p_2$dim_latitude*pi/180) +
      cos(p_1$dim_latitude*pi/180) * cos(p_2$dim_latitude*pi/180) *
      cos(p_1$dim_longitude*pi/180 - p_2$dim_longitude*pi/180))
}

compute_geo_distance_v2 <- function(p_1, p_2) {
  dist <- 6378.388 * acos(
    sin(pi/180*p_1[ , "dim_latitude"]) * sin(pi/180*p_2[ , "dim_latitude"]) +
      cos(pi/180*p_1[ , "dim_latitude"]) * cos(pi/180*p_2[ , "dim_latitude"]) *
      cos(pi/180*p_1[ , "dim_longitude"] - pi/180*p_2[ , "dim_longitude"]))
  names(dist) <- "msr_distance"
  dist

  # TODO-20180610: robustify against dependency on tibble and column names from
  # settings
}

pipe_compute_geo_distance_v3 <- function(
  dat_scaling_factor,
  dat_input,
  dat_station
) {
  dat_scaling_factor
  dat_msr_distance <- compute_geo_distance_v2(
    p_1 = dat_input,
    p_2 = dat_station
  )
  dat_msr_distance <- bind_cols(
    dat_msr_distance,
    dat_msr_distance * dat_scaling_factor %>% dplyr::pull(scaling_factor)
  ) %>%
    dplyr::rename(msr_distance_scaled = msr_distance1)
  # TODO 20181122: tidy eval for column names

  dat_msr_distance$dim_station <- dat_station$dim_station
  dat_msr_distance
}

handle_input_distance <- function(
  dat_input,
  dat_msr_distance,
  dat_scaling_factor
) {
  .Deprecated("handle_input_distance_v2")
  # Handle distance
  col_msr_distance <- settings$name_mapping$msr_distance$key
  idx <- colnames(dat_input) %>% str_detect(quo_name(col_msr_distance))
  if (any(idx)) {
    value_msr_distance <- dat_input[ , quo_name(col_msr_distance)]
    # dat_msr_distance %>% dplyr::arrange(desc(msr_distance)) %>% head()
    dat_msr_distance <- dat_msr_distance %>% dplyr::filter(
      !!col_msr_distance <= value_msr_distance
    )
  }

  list(
    dat_input = if (
      all(dat_scaling_factor %>% dplyr::pull(scaling_factor) == 0)
    ) {
      # If scaling factor 0 then this implies to drop alltogether
      dat_input[, !idx, drop = FALSE]
    } else {
      do_fun <- function(
        dat_scaling_factor,
        dat_input,
        idx
      ) {
        # Apply scaling so input and DB data match up downstream
        dat_input[, idx] <- dat_input[, idx] * dat_scaling_factor %>%
          dplyr::pull(scaling_factor)
        colnames(dat_input)[idx] <- "msr_distance_scaled"
        # TODO 20181122: tidy eval for column names based on settings
        as_tibble(dat_input)
      }

      dat_scaling_factor %>%
        dplyr::group_by(scaling_factor) %>%
        dplyr::do(
          do_fun(
            .,
            dat_input = dat_input,
            idx = idx
          )
        ) %>%
        ungroup() %>%
        as.matrix()
    },
    dat_msr_distance = dat_msr_distance
  )
}

model_predict_index_v4 <- function(
  model_estimation,
  knn = 3
) {
  .Deprecated("model_predict_index_v5")
  model_prediction <- lapply(model_estimation, function(estimation) {
    index <- estimation$estimation_result %>%
      dplyr::group_by(scaling_factor) %>%
      dplyr::mutate(index = 1:n()) %>%
      dplyr::arrange(estimation) %>%
      dplyr::slice(1:knn) %>%
      dplyr::mutate(rank = 1:n()) %>%
      ungroup()

    list(
      estimation_result_index = index,
      dat_distance_geo = estimation$dat_distance_geo,
      dat_input = estimation$dat_input,
      dat_db = estimation$dat_db
    )
  })

  model_prediction
}

model_predict_v4 <- function(
  model_prediction_index,
  dat_station,
  knn
) {
  .Deprecated("model_apply_v1")
  dat_result <- lapply(1:length(model_prediction_index), function(idx_input) {
    # Ensure matrix -----
    dat_input <- model_prediction_index[[idx_input]]$dat_input
    dat_db <- model_prediction_index[[idx_input]]$dat_db %>%
      as_tibble()

    choice <- model_prediction_index[[idx_input]]$estimation_result_index
    index <- choice %>% dplyr::pull(index)

    # Get station ID ------
    dim_station <- dat_db$dim_station[index]

    # Input data -----
    dat_input_row <- dat_input[idx_input, ]

    # Prediction data -----
    dat_prediction <- dat_db[index, ] %>%
      select(time_month, matches("msr_")) %>%
      mutate(
        dim_station = dim_station,
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

    dat_prediction <- bind_cols(
      choice %>% dplyr::select(scaling_factor, dim_rank = rank),
      dat_prediction
    )

    dat_distance_geo <- model_prediction_index[[idx_input]]$dat_distance_geo

    # Join stations -----
    dat_prediction_2 <- inner_join(
      dat_prediction,
      dat_station,
      by = "dim_station"
    ) %>%
      left_join(
        dat_distance_geo,
        by = c("scaling_factor", "dim_station")
      ) %>%
      select(
        scaling_factor,
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
        everything()
      )

    # Return -----
    list(
      input = dat_input_row,
      prediction = dat_prediction_2
    )
  })
}
