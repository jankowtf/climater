
# Distance measures -------------------------------------------------------

#' @import philentropy
model_estimate_distances <- function(
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

#' @import philentropy
model_estimate_distances_v2 <- function(
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
    cols <- c("dim_latitude", "dim_longitude", "msr_distance")
    if (all(cols %in% colnames(dat_input_row))) {
      # dat_input_row <- as.matrix(dat_input)
      msr_distance <- compute_geo_distance_v2(p_1 = dat_input_row, p_2 = dat_station)
      msr_distance$dim_station <- dat_station$dim_station
      dat_db <- left_join(dat_db, msr_distance, by = "dim_station")
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
  })
}

model_predict_index_distances <- function(
  model_estimation,
  knn = 3
) {
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

model_predict_distances <- function(
  model_prediction_index,
  dat_input,
  dat_db,
  dat_station,
  knn
) {
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

model_predict_distances_v2 <- function(
  model_prediction_index,
  dat_input,
  dat_db,
  dat_station,
  knn
) {
  # Ensure matrix -----
  # dim_station <- default_name("station") %>% quo_prepend("dim")
  dat_input <- dat_input %>%
    # dplyr::select(-dim_station) %>%
    as.matrix()

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

model_predict_ensemble_distances <- function(
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

model_output <- function(model_prediction) {
  model_prediction %>% lapply(
    function(dat) {
      dat$prediction <- as.data.frame(dat$prediction)
      dat$prediction_ensemble <- as.data.frame(dat$prediction_ensemble)
      dat
    })
}

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
