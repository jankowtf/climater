
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
model_estimate_v4 <- function(
  dat_input,
  dat_db,
  dat_station,
  dist_measures = default_dist_measures()
) {
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
    as_tibble()
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

  tibble(
    scaling_factor = dat_estimation$scaling_factor %>% unique(),
    estimation = estimation
  )
}

pipe_compute_geo_distance_v2 <- function(
  dat_scaling_factor,
  dat_input_row,
  dat_station
) {
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
model_predict_index_v4 <- function(
  model_estimation,
  knn = 3
) {

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
      as_tibble()

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
model_predict_v4 <- function(
  model_prediction_index,
  dat_station,
  knn
) {
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
model_run_v2 <- function(
  dat_input,
  dat_db,
  dat_station,
  dist_measures,
  dist_measure_final,
  knn
) {
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

handle_input_distance <- function(
  dat_input,
  dat_msr_distance,
  dat_scaling_factor
) {
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

handle_input_time <- function(
  dat_input,
  dat_db
) {
  col_dim_time <- settings$name_mapping$time_month$key
  idx <- colnames(dat_input) %>% str_detect(quo_name(col_dim_time))
  if (any(idx)) {
    value_dim_time <- dat_input[ , quo_name(col_dim_time)]
    dat_db <- dat_db %>% dplyr::filter(
      !!col_dim_time == value_dim_time
    )
  }

  list(
    # dat_input = dat_input[, !idx],
    dat_input = dat_input,
    dat_db = dat_db
  )
}
