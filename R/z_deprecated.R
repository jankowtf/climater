
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
      fct_scaling = NA
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
      # fct_scaling <- settings$scaling$distances
      dat_fct_scaling <- tibble(fct_scaling = unlist(settings$scaling$distances))

      # Compute geo distance -----
      # dat_msr_distance <- compute_geo_distance_v2(p_1 = dat_input_row, p_2 = dat_station)
      dat_msr_distance <- dat_fct_scaling %>%
        dplyr::group_by(fct_scaling) %>%
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
        dat_fct_scaling = dat_fct_scaling
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
          fct_scaling,
          dim_station,
          msr_distance,
          msr_distance_scaled
        ) %>%
        group_by(
          fct_scaling,
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
        data.frame(fct_scaling = NA) %>% as.matrix(),
        dat_input_row
      )
      dat_db$fct_scaling <- NA
      dat_distance_geo$fct_scaling <- NA
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
      dplyr::select(-fct_scaling) %>%
      names() %>%
      rlang::syms()

    names_new <- c(
      list(rlang::sym("fct_scaling")),
      names_old %>%
        paste0("input_", .) %>%
        rlang::syms()
    )

    names_mapping <- c(
      list(rlang::sym("fct_scaling")),
      names_old
    )
    names(names_mapping) <- names_new
    dat_input_row <- dat_input_row %>% dplyr::rename(!!!names_mapping)

    dat_estimation <- left_join(
      dat_db %>% as_tibble(),
      dat_input_row,
      by = "fct_scaling"
    ) %>%
      dplyr::select(
        fct_scaling,
        everything()
      )

    estimation_result <- dat_estimation %>%
      dplyr::group_by(fct_scaling) %>%
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
  dat_fct_scaling,
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
    dat_msr_distance * dat_fct_scaling %>% dplyr::pull(fct_scaling)
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
  dat_fct_scaling,
  dat_input,
  dat_station
) {
  dat_fct_scaling
  dat_msr_distance <- compute_geo_distance_v2(
    p_1 = dat_input,
    p_2 = dat_station
  )
  dat_msr_distance <- bind_cols(
    dat_msr_distance,
    dat_msr_distance * dat_fct_scaling %>% dplyr::pull(fct_scaling)
  ) %>%
    dplyr::rename(msr_distance_scaled = msr_distance1)
  # TODO 20181122: tidy eval for column names

  dat_msr_distance$dim_station <- dat_station$dim_station
  dat_msr_distance
}

handle_input_distance <- function(
  dat_input,
  dat_msr_distance,
  dat_fct_scaling
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
      all(dat_fct_scaling %>% dplyr::pull(fct_scaling) == 0)
    ) {
      # If scaling factor 0 then this implies to drop alltogether
      dat_input[, !idx, drop = FALSE]
    } else {
      do_fun <- function(
        dat_fct_scaling,
        dat_input,
        idx
      ) {
        # Apply scaling so input and DB data match up downstream
        dat_input[, idx] <- dat_input[, idx] * dat_fct_scaling %>%
          dplyr::pull(fct_scaling)
        colnames(dat_input)[idx] <- "msr_distance_scaled"
        # TODO 20181122: tidy eval for column names based on settings
        as_tibble(dat_input)
      }

      dat_fct_scaling %>%
        dplyr::group_by(fct_scaling) %>%
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
      dplyr::group_by(fct_scaling) %>%
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
      choice %>% dplyr::select(fct_scaling, dim_rank = rank),
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
        by = c("fct_scaling", "dim_station")
      ) %>%
      select(
        fct_scaling,
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

model_estimate_inner <- function(.x) {
  .Deprecated("model_estimate_inner_v2")
  dat_input <- .x$dat_input
  dat_db <- .x$dat_db
  dat_station <- .x$dat_station

  # Handle time input -----
  # Only keep weather records that have time == user input
  dat_list <- handle_input_time(
    dat_input = dat_input,
    dat_db = dat_db
  )
  # Update input and DB data:
  dat_input <- dat_list$dat_input
  dat_db <- dat_list$dat_db

  # Get scaling factor(s) from settings -----
  # fct_scaling <- settings$scaling$distances
  dat_fct_scaling <- tibble(fct_scaling = unlist(settings$scaling$distances))

  # Compute geo distance -----
  # dat_msr_distance <- compute_geo_distance_v2(p_1 = dat_input_row, p_2 = dat_station)
  dat_msr_distance <- compute_geo_distance_v3(
    p_1 = dat_input %>%
      dplyr::select(matches("latitude|longitude")) %>%
      dplyr::summarise_all(unique),
    p_2 = dat_station
  )

  # Handle distance input -----
  # Only keep stations that have distance <= user input
  dat_list <- handle_input_distance_v2(
    dat_input = dat_input,
    dat_msr_distance = dat_msr_distance
  )
  # Update input and distance data:
  dat_input <- dat_list$dat_input
  dat_input_out <- dat_input
  dat_input <- purrr::map_df(
    seq_len(nrow(dat_fct_scaling)), ~dat_input) %>%
    dplyr::mutate(
      fct_scaling = rep(dat_fct_scaling$fct_scaling, nrow(dat_input)) %>%
        sort()
    ) %>%
    dplyr::select(
      fct_scaling,
      everything()
    )
  dat_msr_distance <- dat_list$dat_msr_distance

  # Scale -----
  dat_msr_distance <- dat_fct_scaling %>%
    dplyr::group_by(fct_scaling) %>%
    dplyr::do(
      pipe_compute_geo_distance_v4(
        .,
        dat_msr_distance
      )
    ) %>%
    ungroup()

  # Handle DB -----
  dat_list <- handle_input_db(
    dat_db = dat_db,
    dat_msr_distance = dat_msr_distance
  )
  dat_db <- dat_list$dat_db
  dat_db_out <- dat_list$dat_db_out

  # Create geo distance data frame ----
  dat_distance_geo <- handle_dat_dist_geo(dat_db)

  # Early exit -----
  if (!nrow(dat_db)) {
    model_result <- list(
      estimation_result= tibble(
        fct_scaling = NA,
        dim_station = NA,
        index = NA,
        rank = NA,
        estimation_result = NA
      ),
      dat_distance_geo = dat_distance_geo,
      dat_input = dat_input_out,
      dat_db = dat_db_out
    )
    return(model_result)
  }

  # Drop all dims -----
  dat_list <- handle_drop_dims(dat_db, dat_input)
  dat_input <- dat_list$dat_input
  dat_db <- dat_list$dat_db

  # Align dimensions of input -----
  alignment_factor <- dat_db %>% nrow() / nrow(dat_fct_scaling)

  dat_input <- purrr::map_df(
    seq_len(alignment_factor), ~dat_input
  ) %>%
    dplyr::arrange(fct_scaling)
  # dat_input <- dat_input %>%
  #   dplyr::group_by(fct_scaling) %>%
  #   purrr::map_df(seq_len(nrow(dat_db)), ~dat_input)
  #
  # Compute euclidean distance -----
  # purrr::map2(dat_db, dat_input, pmap_inner_2)
  fct_scaling <- dat_input %>%
    dplyr::pull(fct_scaling)
  dat_input_mat <- dat_input %>%
    dplyr::select(-fct_scaling) %>%
    as.matrix()
  dat_db_mat <- dat_db %>%
    dplyr::arrange(fct_scaling) %>%
    dplyr::select(-fct_scaling) %>%
    as.matrix()
  vec_estimation_result <- sapply(1:nrow(dat_db), function(row) {
    dat_db_this <- dat_db_mat[row, , drop = FALSE]
    dat_input_this <- dat_input_mat[row, , drop = FALSE]
    res <- philentropy:::euclidean(
      dat_input_this, dat_db_this, testNA = FALSE)
  })

  estimation_result <- tibble(
    fct_scaling = fct_scaling,
    dim_station = rep(dat_db_out$dim_station, nrow(dat_fct_scaling)),
    estimation_result = vec_estimation_result
  ) %>%
    dplyr::group_by(fct_scaling) %>%
    # Add distances for arranging:
    dplyr::left_join(dat_distance_geo,
      by = c("fct_scaling", "dim_station")) %>%
    # Add position index:
    dplyr::mutate(index = row_number()) %>%
    dplyr::arrange(fct_scaling, estimation_result, msr_distance) %>%
    dplyr::mutate(rank = row_number()) %>%
    dplyr::select(
      fct_scaling,
      dim_station,
      index,
      rank,
      estimation_result
    ) %>%
    ungroup()

  list(
    estimation_result= estimation_result,
    dat_distance_geo = dat_distance_geo,
    dat_input = dat_input_out,
    dat_db = dat_db_out
  )
}


handle_input_time <- function(
  dat_input,
  dat_db
) {
  .Deprecated("model_handle_input_time_v2")
  col_dim_time <- settings$name_mapping$time_month$key
  idx <- colnames(dat_input) %>% str_detect(quo_name(col_dim_time))
  if (any(idx)) {
    value_dim_time <- dat_input %>% dplyr::pull(!!col_dim_time)
    dat_db <- dat_db %>% dplyr::filter(
      !!col_dim_time == value_dim_time
    )
  }

  list(
    dat_input = dat_input,
    dat_db = dat_db
  )
}

handle_dat_dist_geo <- function(
  dat_db
) {
  .Deprecated("model_handle_dat_dist_geo_v2")
  df_dist_geo <- dat_db %>%
    select(
      fct_scaling,
      dim_station,
      msr_distance,
      msr_distance_scaled
    ) %>%
    group_by(
      fct_scaling,
      dim_station
    ) %>%
    summarise(
      msr_distance = unique(msr_distance),
      msr_distance_scaled = unique(msr_distance_scaled)
    ) %>%
    ungroup()
  # TODO 20181122: tidy eval for column names
}

handle_input_db <- function(
  dat_db,
  dat_msr_distance
) {
  .Deprecated("model_handle_input_db_v2")
  # Join distances to DB data -----
  col_dim_station <- default_name("station") %>% quo_prepend("dim")
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

  list(
    dat_db = dat_db,
    dat_db_out = dat_db_out
  )
}

pipe_compute_geo_distance_v4 <- function(
  dat_fct_scaling,
  dat_msr_distance
) {
  dat_msr_distance %>%
    dplyr::mutate(
      msr_distance_scaled = msr_distance * dat_fct_scaling %>%
        pull(fct_scaling)
    )
}

pipe_compute_geo_distance_v5 <- function(
  dat_fct_scaling,
  dat_msr_distance
) {
  .Deprecated("pipe_compute_geo_distance_v6")
  dat_msr_distance %>%
    dplyr::mutate(
      msr_distance_scaled = msr_distance * dat_fct_scaling %>%
        pull(fct_scaling_dist)
    )
}

handle_drop_dims <- function(dat_db, dat_input) {
  .Deprecated("model_handle_drop_dims_v2")
  dat_input <- dat_input %>%
    dplyr::select(-matches("dim"))
  dat_db <- dat_db %>%
    select(-matches("dim_"))
  # TODO-20180610: encapsulate in function

  cols <- names(dat_input) %>%
    rlang::syms()
  dat_db <- dat_db %>%
    dplyr::select(!!!cols)

  list(
    dat_input = dat_input,
    dat_db = dat_db
  )
}

model_run_v4 <- function(
  dat_input,
  dat_db,
  dat_station,
  knn
) {
  .Deprecated("model_run_v5")
  # Model estimation -----
  model_estimation <- model_estimate_v5(
    dat_input = dat_input,
    dat_db = dat_db,
    dat_station = dat_station
  )

  # Model output -----
  model_output <- model_apply_v2(
    model_estimation,
    dat_station = dat_station,
    knn = knn
  )

  model_output
}

model_estimate_v5 <- function(
  dat_input,
  dat_db,
  dat_station
) {
  .Deprecated("model_estimate_v6")
  # Get scaling factor(s) from settings -----
  if (TRUE) {
    dat_fct_scaling = tibble::tibble(
      fct_scaling_dist = unlist(settings$scaling$distances) %>%
        rep(nrow(dat_input)),
      fct_scaling_time = unlist(settings$scaling$time) %>%
        rep(nrow(dat_input))
    )

    # Expand -----
    dat_fct_scaling <- dat_fct_scaling %>%
      tidyr::expand(fct_scaling_dist, fct_scaling_time)

    # Add columns for join -----
    # dat_fct_scaling <- dat_fct_scaling %>%
    #   dplyr::mutate(
    #     time_month = dat_input %>%
    #       dplyr::select_at(if("time_month" %in% names(.))
    #         "time_month" else integer(0)) %>%
    #       dplyr::pull()
    #   )
    # KEEP AS REFERENCE
    dat_fct_scaling <- dat_fct_scaling %>%
      dplyr::mutate(
        time_month = dat_input %>%
          dplyr::pull(time_month),
        msr_distance = dat_input %>%
          dplyr::pull(msr_distance)
      )

    dat_input <- dat_input %>%
      # dplyr::select(time_month) %>%
      dplyr::left_join(
        dat_fct_scaling,
        by = intersect(names(dat_input), names(dat_fct_scaling))
      ) %>%
      dplyr::mutate(
        time_month_scaled = time_month * fct_scaling_time,
        msr_distance_scaled = msr_distance * fct_scaling_dist
      )
  } else {
    stop("Single set of scaling factors for all vars to be scaled not implemented yet")
    dat_fct_scaling = tibble::tibble(
      dat_fct_scaling = unlist(settings$scaling$distances)
    )
  }

  # dat_list <- model_handle_input_time_v2(
  #   dat_input = dat_input,
  #   dat_db = dat_db
  # )
  # dat_db <- dat_list$dat_db

  # Prepare input for subsequent {purrr} pipes -----
  # dat_estimation_input <- model_prepare_estimation_input(
  #   dat_input,
  #   dat_db,
  #   dat_station
  # )
  #
  # # Compute distance for each input vector -----
  # df_estimate <- dat_estimation_input %>%
  #   purrr::map(model_estimate_inner_v2)

  if (FALSE) {
    dat_db <- purrr::map(
      seq_len(nrow(dat_input)), ~dat_db
    )
    dat_station <- purrr::map(
      seq_len(nrow(dat_input)), ~dat_station
    )
    dat_input <- purrrlyr::by_row(dat_input,
      function(v) list(v)[[1L]], .collate = "list")$.out

    dat_estimation_input <- purrr::pmap(list(dat_input, dat_db, dat_station),
      function(a, b, c) {
        list(
          dat_input = a,
          dat_db = b,
          dat_station = c
        )
      })
    df_estimate <- dat_estimation_input %>%
      purrr::map(model_estimate_inner_v2)
  }

  dat_input <- dat_input %>%
    dplyr::mutate(
      id = str_glue("time={fct_scaling_time}_dist={fct_scaling_dist}")
    ) %>%
    dplyr::select(
      id,
      everything()
    )
  dat_input <- purrrlyr::by_row(dat_input,
    function(v) list(v)[[1L]], .collate = "list")$.out

  df_estimate <- dat_input %>%
    purrr::map(model_estimate_inner_v3,
      dat_db = dat_db, dat_station = dat_station)

  df_estimate
}

model_apply_v2 <- function(
  model_estimation,
  dat_station,
  knn
) {
  .Deprecated("model_apply_v3")
  # dat_result <- lapply(model_estimation, function(estimation) {
  model_output_list <- model_estimation %>%
    purrr::map(function(estimation) {
      # Ensure matrix -----
      dat_db <- estimation$dat_db

      choice <- estimation$estimation_result %>%
        dplyr::filter(rank %in% 1:knn)

      dat_input <- estimation$dat_input
      v_id <- dat_input %>%
        dplyr::distinct(id) %>%
        dplyr::pull()
      dat_input <- purrr::map_df(seq_len(knn), ~dat_input %>%
          dplyr::select(-id)) %>%
        as.matrix()

      # Prediction data -----
      foo <- function(choice, dat_db) {
        dat_db %>%
          dplyr::filter(
            dim_station %in% choice$dim_station
          )
      }

      dat_output <- choice %>%
        dplyr::do(foo(., dat_db)) %>%
        dplyr::ungroup() %>%
        select(dim_station, time_month, matches("msr_")) %>%
        mutate(
          # diff_time_month = time_month - dat_input %>%
          #   dplyr::pull(time_month),
          # diff_msr_temp_min = msr_temp_min - dat_input %>%
          #   dplyr::pull(msr_temp_min),
          # diff_msr_temp_max = msr_temp_max - dat_input %>%
          #   dplyr::pull(msr_temp_max),
          # diff_msr_temp_avg = msr_temp_avg - dat_input %>%
          #   dplyr::pull(msr_temp_avg),
          # diff_msr_precip_min = msr_precip_min - dat_input %>%
          #   dplyr::pull(msr_precip_min),
          # diff_msr_precip_max = msr_precip_max - dat_input %>%
          #   dplyr::pull(msr_precip_max),
          # diff_msr_precip_avg = msr_precip_avg - dat_input %>%
          #   dplyr::pull(msr_precip_avg),
          # diff_msr_sundur_avg = msr_sundur_avg - dat_input %>%
          #   dplyr::pull(msr_sundur_avg)
          diff_time_month = time_month - dat_input["time_month"],
          diff_msr_temp_min = msr_temp_min - dat_input["msr_temp_min"],
          diff_msr_temp_max = msr_temp_max - dat_input["msr_temp_max"],
          diff_msr_temp_avg = msr_temp_avg - dat_input["msr_temp_avg"],
          diff_msr_precip_min = msr_precip_min - dat_input["msr_precip_min"],
          diff_msr_precip_max = msr_precip_max - dat_input["msr_precip_max"],
          diff_msr_precip_avg = msr_precip_avg - dat_input["msr_precip_avg"],
          diff_msr_sundur_avg = msr_sundur_avg - dat_input["msr_sundur_avg"]
        )

      # dat_output <- dat_output %>% mutate_if(is.double, round, 1)
      dat_output <- dat_output %>% mutate_at(vars(matches("^diff")), round, 1)

      dat_output <- bind_cols(
        choice %>%
          dplyr::ungroup() %>%
          dplyr::select(dim_rank = rank),
        dat_output
      ) %>%
        dplyr::mutate(
          id = v_id
        ) %>%
        dplyr::select(
          id,
          dim_rank,
          everything()
        )

      dat_distance_geo <- estimation$dat_distance_geo

      # Join stations -----
      dat_output_2 <- inner_join(
        dat_output,
        dat_station,
        by = "dim_station"
      ) %>%
        left_join(
          dat_distance_geo,
          by = "dim_station"
        ) %>%
        select(
          id,
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
        model_input = dat_input %>% tibble::as_tibble(),
        model_output = dat_output_2
      )
    })
  # names(model_output_list) <- paste0("input_", 1:length(model_output_list))

  model_output <- list(
    model_input = purrr::map_df(model_output_list, "model_input"),
    model_output = purrr::map_df(model_output_list, "model_output")
  )
}

model_estimate_inner_v3 <- function(
  dat_input,
  dat_db,
  dat_station
) {
  # Compute geo distance -----
  dat_msr_distance <- compute_geo_distance_v3(
    p_1 = dat_input %>%
      dplyr::select(matches("latitude|longitude")) %>%
      dplyr::summarise_all(unique),
    p_2 = dat_station
  )

  # Handle distance input -----
  # Only keep stations that have distance <= user input
  dat_list <- handle_input_distance_v2(
    dat_input = dat_input,
    dat_msr_distance = dat_msr_distance
  )
  # Update input and distance data:
  dat_input <- dat_list$dat_input
  dat_input_out <- dat_input
  dat_msr_distance <- dat_list$dat_msr_distance

  v_fct_scaling_dist <- dat_input %>%
    dplyr::distinct(fct_scaling_dist) %>%
    dplyr::pull(fct_scaling_dist)
  tmp <- tibble::tibble(
    dim_station = dat_msr_distance %>% dplyr::pull(dim_station) %>%
      rep(length(v_fct_scaling_dist)) %>%
      sort(),
    fct_scaling_dist = v_fct_scaling_dist %>%
      rep(nrow(dat_msr_distance))
  ) %>%
    dplyr::arrange(desc(fct_scaling_dist))
  dat_msr_distance <- dat_msr_distance %>%
    left_join(tmp, by = "dim_station")

  # Scale -----
  dat_msr_distance <- dat_msr_distance %>%
    dplyr::group_by(fct_scaling_dist) %>%
    dplyr::do(
      pipe_compute_geo_distance_v6(.)
    ) %>%
    ungroup()

  dat_time <- dat_db %>%
    dplyr::select(
      dim_station,
      time_month
    ) %>%
    dplyr::mutate(
      fct_scaling_time = dat_input %>%
        dplyr::pull(fct_scaling_time) %>%
        unique()
    ) %>%
    dplyr::group_by(fct_scaling_time) %>%
    dplyr::do(
      pipe_compute_time_v6(.)
    ) %>%
    ungroup()

  # Handle DB -----
  dat_list <- model_handle_input_db_v3(
    dat_db = dat_db,
    dat_time = dat_time,
    dat_msr_distance = dat_msr_distance
  )
  dat_db <- dat_list$dat_db
  dat_db_out <- dat_list$dat_db_out

  # Create geo distance data frame ----
  dat_distance_geo <- model_handle_dat_dist_geo_v2(dat_db)

  # Early exit -----
  if (!nrow(dat_db)) {
    model_result <- list(
      estimation_result= tibble::tibble(
        fct_scaling = NA,
        dim_station = NA,
        index = NA,
        rank = NA,
        estimation_result = NA
      ),
      dat_distance_geo = dat_distance_geo,
      dat_input = dat_input_out,
      dat_db = dat_db_out
    )
    return(model_result)
  }

  # Scaling factors -----
  fct_scaling_dist <- dat_input %>%
    dplyr::distinct(fct_scaling_dist) %>%
    dplyr::pull()
  fct_scaling_time <- dat_input %>%
    dplyr::distinct(fct_scaling_time) %>%
    dplyr::pull()

  # Drop all dims -----
  dat_list <- model_handle_column_alignment(dat_db, dat_input)
  dat_input <- dat_list$dat_input
  dat_db <- dat_list$dat_db

  # Align dimensions of input -----
  dat_input <- purrr::map_df(
    seq_len(dat_db %>% nrow()), ~dat_input
  )

  # Compute euclidean distance -----
  # purrr::map2(dat_db, dat_input, pmap_inner_2)
  dat_input_mat <- dat_input %>%
    as.matrix()
  dat_db_mat <- dat_db %>%
    as.matrix()
  vec_estimation_result <- sapply(1:nrow(dat_db), function(row) {
    dat_db_this <- dat_db_mat[row, , drop = FALSE]
    dat_input_this <- dat_input_mat[row, , drop = FALSE]
    res <- philentropy:::euclidean(
      dat_input_this, dat_db_this, testNA = FALSE)
  })

  estimation_result <- tibble::tibble(
    # fct_scaling_time = fct_scaling_time,
    # fct_scaling_dist = fct_scaling_dist,
    dim_station = dat_db_out$dim_station,
    estimation_result = vec_estimation_result
  ) %>%
    # dplyr::mutate(
    #   id_scaling = str_glue("time={fct_scaling_time}_dist={fct_scaling_dist}")
    # ) %>%
    # dplyr::select(
    #   id_scaling,
    #   everything()
    # ) %>%
    # dplyr::select(
    #   -fct_scaling_time,
    #   -fct_scaling_dist,
    # ) %>%
  # Add distances for arranging:
  dplyr::left_join(dat_distance_geo,
    by = "dim_station"
  ) %>%
    dplyr::select(
      -fct_scaling_dist,
    ) %>%
    # Add position index:
    dplyr::mutate(index = row_number()) %>%
    dplyr::arrange(estimation_result, msr_distance) %>%
    dplyr::mutate(rank = row_number()) %>%
    dplyr::select(
      dim_station,
      index,
      rank,
      estimation_result
    )

  list(
    estimation_result= estimation_result,
    dat_distance_geo = dat_distance_geo,
    dat_input = dat_input_out,
    dat_db = dat_db_out
  )
}
