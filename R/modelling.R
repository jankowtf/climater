
# Distance measures -------------------------------------------------------

#' @export
model_estimate <- function(
  dat_input,
  dat_db,
  dist_measures = default_dist_measures()
) {
  # Argument handling -----
  dist_measures <- match.arg(dist_measures, several.ok = TRUE)

  # Ensure matrix -----
  dat_input <- as.matrix(dat_input)
  dat_db <- as.matrix(dat_db)

  # Compute distance for each input row/vector -----
  dat_dist <- lapply(1:nrow(dat_input), function(row_user) {
    dat_input_row <- dat_input[row_user, , drop = FALSE]

    lapply(1:nrow(dat_db), function(row) {
      dat_db_this <- dat_db[row, colnames(dat_input_row), drop = FALSE]

      # List availble distance measures
      # philentropy::getDistMethods()

      res <- list(
        euclidean = if ("euclidean" %in% dist_measures) {
          philentropy:::euclidean(
            dat_input_row, dat_db_this, testNA = FALSE)
        },
        manhattan = if ("manhattan" %in% dist_measures) {
          philentropy:::manhattan(
            dat_input_row, dat_db_this, testNA = FALSE)
        },
        jaccard = if ("jaccard" %in% dist_measures) {
          philentropy:::jaccard(
            dat_input_row, dat_db_this, testNA = FALSE)
        },
        avg = if ("avg" %in% dist_measures) {
          philentropy:::avg(
            dat_input_row, dat_db_this, testNA = FALSE)
        },
        squared_euclidean = if ("squared_euclidean" %in% dist_measures) {
          philentropy:::squared_euclidean(
            dat_input_row, dat_db_this, testNA = FALSE)
        },
        chebyshev = if ("chebyshev" %in% dist_measures) {
          philentropy:::chebyshev(
            dat_input_row, dat_db_this, testNA = FALSE)
        }
      )
      res[!sapply(res, is.null)]
    })
  })
}

#' @export
model_estimate_v2 <- function(
  dat_input,
  dat_db,
  dat_station,
  dist_measures = default_dist_measures()
) {
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
      msr_distance = NA
    )

    cols <- c("dim_latitude", "dim_longitude", "msr_distance")
    if (all(cols %in% colnames(dat_input_row))) {
      # dat_input_row <- as.matrix(dat_input)
      msr_distance <- compute_geo_distance_v2(p_1 = dat_input_row, p_2 = dat_station)
      msr_distance$dim_station <- dat_station$dim_station
      dat_db <- left_join(dat_db, msr_distance, by = "dim_station")
      dat_distance_geo <- dat_db %>%
        select(dim_station, msr_distance) %>%
        group_by(dim_station) %>%
        summarise(msr_distance = unique(msr_distance)) %>%
        ungroup()
    }
    dat_db <- dat_db %>%
      select(-matches("dim_")) %>%
      as.matrix()
    # TODO-20180610: encapsulate in function

    # Drop all dims -----
    idx <- colnames(dat_input_row) %>% str_detect("dim")
    dat_input_row <- dat_input_row[ , !idx, drop = FALSE]

    ret <- lapply(1:nrow(dat_db), function(row) {
      dat_db_this <- dat_db[row, colnames(dat_input_row), drop = FALSE]

      # List availble distance measures
      # philentropy::getDistMethods()

      res <- list(
        euclidean = if ("euclidean" %in% dist_measures) {
          philentropy:::euclidean(
            dat_input_row, dat_db_this, testNA = FALSE)
        },
        manhattan = if ("manhattan" %in% dist_measures) {
          philentropy:::manhattan(
            dat_input_row, dat_db_this, testNA = FALSE)
        },
        jaccard = if ("jaccard" %in% dist_measures) {
          philentropy:::jaccard(
            dat_input_row, dat_db_this, testNA = FALSE)
        },
        avg = if ("avg" %in% dist_measures) {
          philentropy:::avg(
            dat_input_row, dat_db_this, testNA = FALSE)
        },
        squared_euclidean = if ("squared_euclidean" %in% dist_measures) {
          philentropy:::squared_euclidean(
            dat_input_row, dat_db_this, testNA = FALSE)
        },
        chebyshev = if ("chebyshev" %in% dist_measures) {
          philentropy:::chebyshev(
            dat_input_row, dat_db_this, testNA = FALSE)
        }
      )
      res[!sapply(res, is.null)]
    })

    list(
      distance_stat = ret,
      dat_distance_geo = dat_distance_geo
    )
    # TODO-20180705: tidyfy column name

  })
}

#' @export
model_estimate_v6 <- function(
  dat_input,
  dat_db,
  dat_station
) {
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
    purrr::map(model_estimate_inner_v4,
      dat_db = dat_db, dat_station = dat_station)

  df_estimate
}

model_prepare_estimation_input <- function(
  dat_input,
  dat_db,
  dat_station
) {
  # Transform to list for subsequent purrr-style processing -----
  dat_input <- dat_input %>%
    tibble::rownames_to_column() %>%
    dplyr::group_by(rowname) %>%
    list(.) %>%
    purrr::map(~.x %>%
        ungroup() %>%
        dplyr::select(-rowname)
    )
  dat_input <- dat_input %>%
    purrr::map(
      ~list(
        dat_input = .x,
        dat_db = dat_db,
        dat_station = dat_station
      )
    )
}

model_estimate_inner_v2 <- function(.x) {
  dat_input <- .x$dat_input
  dat_db <- .x$dat_db
  dat_station <- .x$dat_station

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

  # Handle DB -----
  dat_list <- model_handle_input_db_v2(
    dat_db = dat_db,
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

  # PATCH: remove unwanted cols -----
  # dat_db <- dat_db %>%
  #   dplyr::select(-fct_scaling_dist)

  # Drop all dims -----
  dat_list <- model_handle_drop_dims_v2(dat_db, dat_input)
  dat_input <- dat_list$dat_input
  dat_db <- dat_list$dat_db

  # Align dimensions of input -----
  alignment_factor <- dat_db %>% nrow()

  dat_input <- purrr::map_df(
    seq_len(alignment_factor), ~dat_input
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

#' @export
model_estimate_inner_v4 <- function(
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
    time_month = dat_db_out$time_month,
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
      time_month,
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

pipe_model_estimate <- function(
  dat_estimation,
  dist_measures
) {
  dat_estimation %>%
    dplyr::rowwise() %>%
    dplyr::do(
      pipe_model_estimate_inner(
        .,
        dist_measures = dist_measures
      )
    )
}

pipe_model_estimate_inner <- function(
  dat_estimation,
  dist_measures
) {
  # cols <- colnames(dat_estimation) %>%
  #   rlang::syms() %>%
  #   purrr::map(function(s) expr(!!s))
  dat_estimation <- dat_estimation %>%
    tibble::as_tibble()
  cols_input <- dat_estimation %>%
    dplyr::select(matches("^input_")) %>%
    names() %>%
    rlang::syms()

  cols_db <- cols_input %>%
    as.character() %>%
    str_replace("^input_", "") %>%
    rlang::syms()

  dat_input <- dat_estimation %>%
    dplyr::select(!!!cols_input) %>%
    as.matrix()
  dat_db <- dat_estimation %>%
    dplyr::select(!!!cols_db) %>%
    as.matrix()

  estimation <- philentropy:::euclidean(
    dat_input, dat_db, testNA = FALSE)

  tibble::tibble(
    fct_scaling = dat_estimation$fct_scaling %>% unique(),
    estimation = estimation
  )
}

pipe_compute_geo_distance_v6 <- function(
  dat_msr_distance
) {
  dat_msr_distance %>%
    dplyr::mutate(
      msr_distance_scaled = msr_distance * fct_scaling_dist
    )
}

pipe_compute_time_v6 <- function(
  dat_time
) {
  dat_time %>%
    dplyr::mutate(
      time_month_scaled = time_month * fct_scaling_time
    )
}

#' @export
model_predict_index_v3 <- function(
  model_estimation,
  knn = 3
) {
  # Get names of computed distance measures -----
  dist_measures <- names(model_estimation[[1]]$estimation_result[[1]])
  # TODO-20180705: make this nicer/more robust

  model_prediction <- lapply(model_estimation, function(estimation) {
    index <- lapply(dist_measures, function(mea) {
      # Select particular distance measure result for all stations -----
      values <- sapply(estimation$estimation_result, "[[", mea)
      names(values) <- 1:length(values)

      # Sort estimation results -----
      # values <- sort(values[values != 0])
      values <- sort(values)

      # Select best predictions based on knn -----
      as.numeric(names(values[1:knn]))
    })
    names(index) <- dist_measures

    list(
      estimation_result_index = index,
      dat_distance_geo = estimation$dat_distance_geo,
      dat_input = estimation$dat_input,
      dat_db = estimation$dat_db
    )
  })

  model_prediction
}

#' @export
model_predict_index_v5 <- function(
  model_estimation,
  knn = 3
) {

  model_prediction <- lapply(model_estimation, function(estimation) {
    index <- estimation$estimation_result %>%
      dplyr::group_by(fct_scaling) %>%
      dplyr::mutate(index = 1:n()) %>%
      dplyr::arrange(estimation_result) %>%
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

#' @export
model_predict_v3 <- function(
  model_prediction_index,
  # dat_input,
  # dat_db,
  dat_station,
  knn
) {
  dat_result <- lapply(1:length(model_prediction_index), function(idx_input) {
    # Ensure matrix -----
    dat_input <- model_prediction_index[[idx_input]]$dat_input
    dat_db <- model_prediction_index[[idx_input]]$dat_db %>%
      tibble::as_tibble()

    choice <- model_prediction_index[[idx_input]]$estimation_result_index
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

#' @export
model_apply_v1 <- function(
  model_estimation,
  dat_station,
  knn
) {
  # dat_result <- lapply(model_estimation, function(estimation) {
  model_output_list <- model_estimation %>%
    purrr::map(function(estimation) {
      # Ensure matrix -----
      dat_db <- estimation$dat_db

      choice <- estimation$estimation_result %>%
        dplyr::group_by(fct_scaling) %>%
        dplyr::filter(rank %in% 1:knn)

      dat_input <- estimation$dat_input
      fct_scaling <- choice %>%
        dplyr::ungroup() %>%
        # dplyr::pull(fct_scaling) %>%
        dplyr::distinct(fct_scaling) %>%
        dplyr::pull()
      factor_align_input <- fct_scaling %>%
        dplyr::n_distinct()
      dat_input <- purrr::map_df(seq_len(factor_align_input * knn), ~dat_input) %>%
        dplyr::mutate(
          fct_scaling = rep(fct_scaling, knn) %>% sort()
        ) %>%
        dplyr::group_by(fct_scaling) %>%
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
        ) %>%
        dplyr::mutate(
          fct_scaling = rep(fct_scaling, knn) %>% sort()
        ) %>%
        dplyr::select(
          fct_scaling,
          everything()
        )

      # dat_output <- dat_output %>% mutate_if(is.double, round, 1)
      dat_output <- dat_output %>% mutate_at(vars(matches("^diff")), round, 1)

      dat_output <- bind_cols(
        choice %>%
          dplyr::ungroup() %>%
          dplyr::select(dim_rank = rank),
        dat_output
      ) %>%
        dplyr::select(
          fct_scaling,
          dim_rank,
          everything()
        )
      # dat_output %>% dplyr::left_join(
      #   choice,
      #   by = c("fct_scaling", "dim_station")
      # ) %>%
      #   dplyr::select(
      #     fct_scaling,
      #     dim_station,
      #     rank,
      #     everything()
      #   ) %>%
      #   dplyr::group_by(fct_scaling) %>%
      #   dplyr::arrange(fct_scaling, rank) %>%
      #   dplyr::ungroup()

      dat_distance_geo <- estimation$dat_distance_geo

      # Join stations -----
      dat_output_2 <- inner_join(
        dat_output,
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

#' @export
model_apply_v3 <- function(
  model_estimation,
  dat_station,
  knn
) {
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
            dim_station %in% choice$dim_station,
            time_month %in% choice$time_month
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

#' @export
model_predict_ensemble <- function(
  model_prediction,
  dist_measure = character()
) {
  dat_result_unique <- lapply(model_prediction, function(prediction) {
    dat_prediction <- prediction$prediction

    prediction$prediction_ensemble <- if (!length(dist_measure)) {
      plyr::ddply(dat_prediction, "dim_rank", function(dat) {
        # names(sort(table(dat$dim_station), decreasing = TRUE))
        tmp <- arrange(count(dat, dim_station), desc(n))

        # Resolve ties -----
        # station_id <- if (!all(c(0, diff(tmp$n)) == 0)) {
        station_id <- if (nrow(tmp) == nrow(dat)) {
          sample(tmp$dim_station, 1)
        } else {
          tmp$dim_station[1]
        }

        # Filter based on station ID -----
        res <- filter(dat, dim_station == station_id)[1, ]

        # Denote that this is an ensemble result -----
        res$dim_dist_measure <- "ensemble"

        # Add trust measure for ensemble result -----
        res$msr_ensemble_trust <- tmp$n[1] / nrow(dat)
        res
      })
    } else {
      filter(dat_prediction, dim_dist_measure == dist_measure)
    }
    prediction
  })
}

#' @export
model_predict_ensemble_v2 <- function(
  model_prediction
) {
  dat_result_unique <- lapply(model_prediction, function(prediction) {
    # dat_prediction <- prediction$prediction
    #
    # prediction$prediction_ensemble <- dat_prediction

    prediction
  })
}

#' @export
model_output <- function(model_prediction) {
  model_prediction %>% lapply(
    function(dat) {
      dat$prediction <- as.data.frame(dat$prediction)
      # dat$prediction_ensemble <- as.data.frame(dat$prediction_ensemble)
      dat$prediction_ensemble <- as.data.frame(dat$prediction)
      dat
    })
}

#' @export
model_output_gathered <- function(model_prediction) {
  # Prepare -----
  model_result <- model_output(model_prediction)
  names(model_result) <- sprintf("input_%s", seq(model_result))
  # model_result[[1]] %>% names()

  # Gather
  .gather <- function(dat, what) {
    model_input <- dat %>%
      lapply("[[", what) %>%
      lapply(function(dat) {
        if (inherits(dat, c("integer", "numeric"))) {
          data.frame(as.list(dat))
        } else {
          dat
        }
      }) %>%
      bind_rows(.id = "input")
  }

  model_input <- .gather(model_result, "input")
  model_prediction <- .gather(model_result, "prediction")
  # model_result$input_1$prediction %>% sapply(class) %>% stringr::str_detect("factor")
  model_prediction_ensemble <- .gather(model_result, "prediction_ensemble")

  # Return -----
  list(
    input = model_input,
    prediction = model_prediction,
    prediction_ensemble = model_prediction_ensemble
  )
}

#' @export
model_result_render <- function(
  model_result_gathered,
  knn,
  dist_measures,
  dist_measure_final
) {
  mdown <- c(
    "# Model result `r Sys.time()`",
    "## Input",
    " * `knn`: `r knn`",
    "",
    " * `dist_measures`: `r dist_measures`",
    "",
    " * `dist_measure_final`: `r dist_measure_final`",
    "",
    " * Actual numeric input:",
    "",
    "```{r echo=FALSE}",
    "DT::datatable(
      model_result_gathered$input,
        options = list(
          scrollX = TRUE
        )
    )",
    "```",
    " ",
    "## Output raw",
    "```{r echo=FALSE}",
    "DT::datatable(
      model_result_gathered$prediction,
        options = list(
          scrollX = TRUE
        )
    )",
    "```",
    " ",
    "## Output ensemble",
    "```{r echo=FALSE}",
    "DT::datatable(
      model_result_gathered$prediction_ensemble,
        options = list(
          scrollX = TRUE
        )
    )",
    "```"
  )
  file_stub <- tempfile()
  file_rmd <- paste0(file_stub, ".Rmd")
  file_html <- paste0(file_stub, ".html")

  writeLines(mdown, file_rmd)
  # knitr::knit2html(file_rmd, file_html)
  # knitr::knit(file_rmd, file_html)
  rmarkdown::render(file_rmd, output_file = file_html)
  # if (interactive())
  browseURL(file_html)
}

#' @export
shiny_run_model <- function(
  dat_input,
  dat_db,
  dat_station,
  dist_measures,
  dist_measure_final,
  knn
) {
  # Model estimation --------------------------------------------------------

  model_estimation <- model_estimate_v2(
    dat_input = dat_input,
    dat_db = dat_db_msr,
    dat_station = dat_station,
    dist_measures = dist_measures
  )
  # model_estimation[[1]][[1]]

  # NOTE:
  # * First level: data input vectors
  # * Second level: station
  # * Third level: distance measure values

  # Identify best choices ---------------------------------------------------

  model_prediction_index <- model_predict_index_v2(
    model_estimation = model_estimation,
    knn = knn
  )
  # model_prediction_index

  # Model prediction --------------------------------------------------------

  model_prediction <- model_predict_v2(
    model_prediction_index,
    dat_input = dat_input,
    dat_db = dat_db,
    dat_station = dat_station,
    knn = knn
  )

  model_prediction_ensemble <- model_predict_ensemble(
    model_prediction = model_prediction,
    dist_measure = dist_measure_final
  )
  # model_prediction_ensemble[[1]]$prediction_ensemble$dim_dist_measure

  # Model output ------------------------------------------------------------

  # model_result <- model_output(model_prediction_ensemble)

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

#' @export
model_run <- function(
  dat_input,
  dat_db,
  dat_station,
  dist_measures,
  dist_measure_final,
  knn
) {
  # Model estimation --------------------------------------------------------

  model_estimation <- model_estimate_v3(
    dat_input = dat_input,
    dat_db = dat_db_msr,
    dat_station = dat_station,
    dist_measures = dist_measures
  )
  # model_estimation[[1]][[1]]

  # NOTE:
  # * First level: data input vectors
  # * Second level: station
  # * Third level: distance measure values

  # Identify best choices ---------------------------------------------------

  model_prediction_index <- model_predict_index_v3(
    model_estimation = model_estimation,
    knn = knn
  )
  # model_prediction_index

  # Model prediction --------------------------------------------------------

  model_prediction <- model_predict_v3(
    model_prediction_index,
    # dat_input = dat_input,
    # dat_db = dat_db,
    dat_station = dat_station,
    knn = knn
  )

  model_prediction_ensemble <- model_predict_ensemble(
    model_prediction = model_prediction,
    dist_measure = dist_measure_final
  )
  # model_prediction_ensemble[[1]]$prediction_ensemble$dim_dist_measure

  # Model output ------------------------------------------------------------

  # model_result <- model_output(model_prediction_ensemble)

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

#' @export
model_run_v3 <- function(
  dat_input,
  dat_db,
  dat_station,
  knn
) {
  # Model estimation -----
  model_estimation <- model_estimate_v5(
    dat_input = dat_input,
    dat_db = dat_db,
    dat_station = dat_station
  )

  # Model output -----
  model_output <- model_apply_v1(
    model_estimation,
    dat_station = dat_station,
    knn = knn
  )

  model_output
}

model_run_v5 <- function(
  dat_input,
  dat_db,
  dat_station,
  knn
) {
  withProgress(message = 'Getting recommendations', value = 0, {

    incProgress(1/3, detail = "Estimating...")
    # Model estimation -----
    model_estimation <- model_estimate_v6(
      dat_input = dat_input,
      dat_db = dat_db,
      dat_station = dat_station
    )


    incProgress(2/3, detail = "Selecting...")
    # Model output -----
    model_output <- model_apply_v3(
      model_estimation,
      dat_station = dat_station,
      knn = knn
    )

    incProgress(3/3, detail = "Done...")

    model_output
  })

  model_output
}

#' @export
handle_input_distance_v2 <- function(
  dat_input,
  dat_msr_distance
) {
  # col_msr_distance <- settings$name_mapping$msr_distance$key
  col_msr_distance <- rlang::sym("msr_distance")
  # TODO 20181129: refactor column names from settings

  idx <- colnames(dat_input) %>%
    str_detect(quo_name(col_msr_distance) %>%
        str_glue("$"))
  if (any(idx)) {
    value_msr_distance <- dat_input %>%
      dplyr::distinct(!!col_msr_distance) %>%
      dplyr::pull()
    if (all(!is.na(value_msr_distance))) {
      dat_msr_distance <- dat_msr_distance %>% dplyr::filter(
        !!col_msr_distance <= value_msr_distance
      )
      dat_input <- dat_input %>%
        dplyr::mutate(!!col_msr_distance := 0)
    }
  }

  list(
    dat_input = dat_input,
    dat_msr_distance = dat_msr_distance
  )
}

model_handle_input_time_v2 <- function(
  dat_input,
  dat_db
) {
  col_dim_time <- settings$name_mapping$time_month$key
  idx <- colnames(dat_input) %>% str_detect(quo_name(col_dim_time))
  if (any(idx)) {
    value_dim_time <- dat_input %>% dplyr::pull(!!col_dim_time)
    dat_db <- dat_db %>% dplyr::filter(
      !!col_dim_time %in% value_dim_time
    )
  }

  list(
    dat_input = dat_input,
    dat_db = dat_db
  )
}

model_handle_input_db_v2 <- function(
  dat_db,
  dat_msr_distance
) {
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

model_handle_input_db_v3 <- function(
  dat_db,
  dat_time,
  dat_msr_distance
) {
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

  # Join time to DB data -----
  dat_db <- left_join(
    dat_db,
    dat_time,
    by = c("dim_station", "time_month")
  )

  list(
    dat_db = dat_db,
    dat_db_out = dat_db_out
  )
}

model_handle_dat_dist_geo_v2 <- function(
  dat_db
) {
  df_dist_geo <- dat_db %>%
    select(
      fct_scaling_dist,
      dim_station,
      msr_distance,
      msr_distance_scaled
    ) %>%
    group_by(
      fct_scaling_dist,
      dim_station
    ) %>%
    summarise(
      msr_distance = unique(msr_distance),
      msr_distance_scaled = unique(msr_distance_scaled)
    ) %>%
    ungroup()
  # TODO 20181122: tidy eval for column names
}

model_handle_drop_dims_v2 <- function(dat_db, dat_input) {
  select_non_na <- function(x) {
    all(!is.na(x))
  }
  dat_input <- dat_input %>%
    dplyr::select(-matches("dim|fct")) %>%
    dplyr::select_if(select_non_na)

  dat_db <- dat_db %>%
    select(-matches("dim_"))
  # TODO-20180610: encapsulate in function

  cols <- intersect(names(dat_db), names(dat_input)) %>%
    rlang::syms()
  dat_db <- dat_db %>%
    dplyr::select(!!!cols)

  list(
    dat_input = dat_input,
    dat_db = dat_db
  )
}

model_handle_column_alignment <- function(dat_db, dat_input) {
  select_non_na <- function(x) {
    all(!is.na(x))
  }
  dat_input <- dat_input %>%
    dplyr::select(-matches("dim|fct")) %>%
    dplyr::select_if(select_non_na)

  dat_db <- dat_db %>%
    select(-matches("dim_"))
  # TODO-20180610: encapsulate in function

  cols <- intersect(names(dat_db), names(dat_input))
  idx <- cols %>% str_detect("_scaled$")
  if (any(idx)) {
    cols_scaled <- cols[idx]
    cols_unscaled <- cols_scaled %>%
      sapply(function(x) x %>% str_replace("_scaled$", ""))
    cols <- cols[!cols %in% cols_unscaled]
  }
  cols <- cols %>%
    rlang::syms()
  dat_db <- dat_db %>%
    dplyr::select(!!!cols)
  dat_input <- dat_input %>%
    dplyr::select(!!!cols)

  list(
    dat_input = dat_input,
    dat_db = dat_db
  )
}

pmap_inner_2 <- function(dat_db, dat_input) {
  dat_db <- as.matrix(dat_db)
  dat_input <- as.matrix(dat_input)
  philentropy:::euclidean(
    dat_input, dat_db, testNA = FALSE)
}
