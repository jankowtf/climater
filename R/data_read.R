
# Column names ------------------------------------------------------------

#' @importFrom snakecase to_any_case
#' @export
dat_tidy_names <- function(dat) {
  nms <- names(dat)
  # nms <- snakecase::to_any_case(nms,
  #   replace_special_characters = "[^[:alnum:]]", preprocess = "-|_")
  nms <- snakecase::to_any_case(nms)
  names(dat) <- nms
  dat
}

# Read --------------------------------------------------------------------

#' @import magrittr
#' @export
dat_read_generic <- function(
  con_fun,
  dtype = default_dtype(),
  vsn = default_version(),
  col_types = character(),
  settings = default_settings()
) {
  con <- con_fun(dtype = dtype, vsn = vsn, settings = settings)
  # con <- here(con)
  data_con_check(con = con, throw = TRUE)
  dat <- if (!length(col_types)) {
    rio::import(con)
  } else {
    rio::import(con, colClasses = col_types)
  }
  dat %>%
    dat_tidy_names() %>%
    as_tibble()
}

#' @import dplyr
#' @import stringr
#' @import fs
#' @import rio
#' @export
dat_read_multiple_generic <- function(
  con_fun,
  dtype = default_dtype(),
  vsn = default_version(),
  col_types = character(),
  settings = default_settings()
) {
  # Globals -----
  k_station <- default_name("station")
  k_time_start <- default_name("time_start")
  k_time_stop <- default_name("time_stop")
  # TODO-20180307-1557: encpasulate via `default_*()' functions

  path <- con_fun(dtype = dtype, vsn = vsn, settings = settings)
  stopifnot(file.exists(path))
  stopifnot(fs::is_dir(path))

  paths <- fs::dir_ls(path)
  dat <- plyr::ldply(paths, function(path) {
    # Read actual data -----
    # path <- here(path)
    # dat <- read.csv2(path, stringsAsFactors = FALSE)
    # dat <- rio::import(path) %>% dat_tidy_names()
    dat <- if (!length(col_types)) {
      rio::import(path) %>% dat_tidy_names()
    } else {
      rio::import(path, colClasses = col_types) %>% dat_tidy_names()
    }

    # Capture meta data -----
    meta <- str_split(basename(path), "_", simplify = TRUE)
    meta <- str_replace_all(meta, "\\.txt$", "")

    # Add meta data -----
    dat <- mutate(dat,
      !!quo_name(k_station) := meta[1],
      !!quo_name(k_time_start) := meta[2],
      !!quo_name(k_time_stop) := meta[3]
    )

    # Arrange -----
    dat <- select(
      dat,
      !!k_station,
      !!k_time_start,
      !!k_time_stop,
      everything()
    )

    dat
  }, .progress = "text", .id = NULL)

  dat %>% as_tibble()
}

#' @export
dat_read_x_generic <- function(
  data_con,
  dtype = default_dtype(),
  vsn = default_version(),
  col_types = character(),
  settings = default_settings()
) {
  if (dtype == "raw") {
    dat_read_multiple_generic(con_fun = data_con,
      dtype = dtype, vsn = vsn, col_types = col_types, settings = settings)
  } else if (dtype == "tidy") {
    dat_read_generic(con_fun = data_con,
      dtype = dtype, vsn = vsn, col_types = col_types, settings = settings)
  } else {
    stop(sprintf("Invalid data type: %s", dtype))
  }
}

#' @export
data_read_station <- function(
  dtype = default_dtype(),
  vsn = default_version(),
  settings = default_settings()
) {
  dat_read_generic(con_fun = data_con_station,
    dtype = dtype, vsn = vsn, settings = settings)
}

#' @export
data_read_station_v2 <- function(
  dtype = default_dtype(),
  vsn = default_version(),
  settings = default_settings()
) {
  col_types <- c(
    "WMO-Station ID" = "character",
    StationName = "character",
    Latitude = "numeric",
    Logitude = "numeric",
    High = "numeric",
    Country = "character"
  )
  dat_read_generic(con_fun = data_con_station,
    dtype = dtype, vsn = vsn, col_types = col_types, settings = settings)
}

#' @export
data_read_temperature_min <- function(
  dtype = default_dtype(),
  vsn = default_version(),
  settings = default_settings()
) {
  dat_read_generic(con_fun = data_con_temperature_min,
    dtype = dtype, vsn = vsn, settings = settings)
}

#' @export
data_read_temperature_min_v2 <- function(
  dtype = default_dtype(),
  vsn = default_version(),
  settings = default_settings()
) {
  col_types <- c(
    Station = "character",
    Jan = "numeric",
    Feb = "numeric",
    Mrz = "numeric",
    Apr = "numeric",
    Mai = "numeric",
    Jun = "numeric",
    Jul = "numeric",
    Aug = "numeric",
    Sep = "numeric",
    Okt = "numeric",
    Nov = "numeric",
    Dez = "numeric"
  )
  dat_read_generic(con_fun = data_con_temperature_min,
    dtype = dtype, vsn = vsn, col_types = col_types, settings = settings)
}

#' @export
data_read_temperature_max <- function(
  dtype = default_dtype(),
  vsn = default_version(),
  settings = default_settings()
) {
  dat_read_generic(con_fun = data_con_temperature_max,
    dtype = dtype, vsn = vsn, settings = settings)
}

#' @export
data_read_temperature_max_v2 <- function(
  dtype = default_dtype(),
  vsn = default_version(),
  settings = default_settings()
) {
  col_types <- c(
    Station = "character",
    Jan = "numeric",
    Feb = "numeric",
    Mrz = "numeric",
    Apr = "numeric",
    Mai = "numeric",
    Jun = "numeric",
    Jul = "numeric",
    Aug = "numeric",
    Sep = "numeric",
    Okt = "numeric",
    Nov = "numeric",
    Dez = "numeric"
  )
  dat_read_generic(con_fun = data_con_temperature_max,
    dtype = dtype, vsn = vsn, col_types = col_types, settings = settings)
}

#' @export
data_read_temperature_comb <- function(
  dtype = default_dtype(),
  vsn = default_version(),
  settings = default_settings()
) {
  dat_read_generic(con_fun = data_con_temperature_comb,
    dtype = dtype, vsn = vsn, settings = settings)
}

#' @export
data_read_sunshine_duration <- function(
  dtype = default_dtype(),
  vsn = default_version(),
  settings = default_settings()
) {
  dat_read_generic(con_fun = data_con_sunshine_duration,
    dtype = dtype, vsn = vsn, settings = settings)
}

#' @export
data_read_sunshine_duration_v2 <- function(
  dtype = default_dtype(),
  vsn = default_version(),
  settings = default_settings()
) {
  col_types <- c(
    Station = "character",
    Jan = "numeric",
    Feb = "numeric",
    Mrz = "numeric",
    Apr = "numeric",
    Mai = "numeric",
    Jun = "numeric",
    Jul = "numeric",
    Aug = "numeric",
    Sep = "numeric",
    Okt = "numeric",
    Nov = "numeric",
    Dez = "numeric"
  )
  dat_read_generic(con_fun = data_con_temperature_min,
    dtype = dtype, vsn = vsn, col_types = col_types, settings = settings)
}

#' @export
data_read_precipitation_historical <- function(
  dtype = default_dtype(),
  vsn = default_version(),
  settings = default_settings()
) {
  dat_read_x_generic(
    data_con = data_con_precipitation_historical,
    dtype = dtype,
    vsn = vsn,
    settings = settings
  )
}

#' @export
data_read_precipitation_historical_v2 <- function(
  dtype = default_dtype(),
  vsn = default_version(),
  settings = default_settings()
) {
  col_types <- c(
    Jahr = "character",
    Jan = "numeric",
    Feb = "numeric",
    Mrz = "numeric",
    Apr = "numeric",
    Mai = "numeric",
    Jun = "numeric",
    Jul = "numeric",
    Aug = "numeric",
    Sep = "numeric",
    Okt = "numeric",
    Nov = "numeric",
    Dez = "numeric"
  )
  dat_read_x_generic(
    data_con = data_con_precipitation_historical,
    dtype = dtype,
    vsn = vsn,
    col_types = col_types,
    settings = settings
  )
}

#' @export
data_read_precipitation_recent <- function(
  dtype = default_dtype(),
  vsn = default_version(),
  settings = default_settings()
) {
  dat_read_x_generic(
    data_con = data_con_precipitation_recent,
    dtype = dtype,
    vsn = vsn,
    settings = settings
  )
}

#' @export
data_read_precipitation_recent_v2 <- function(
  dtype = default_dtype(),
  vsn = default_version(),
  settings = default_settings()
) {
  col_types <- c(
    Jahr = "character",
    Jan = "numeric",
    Feb = "numeric",
    Mrz = "numeric",
    Apr = "numeric",
    Mai = "numeric",
    Jun = "numeric",
    Jul = "numeric",
    Aug = "numeric",
    Sep = "numeric",
    Okt = "numeric",
    Nov = "numeric",
    Dez = "numeric"
  )
  dat_read_x_generic(
    data_con = data_con_precipitation_historical,
    dtype = dtype,
    vsn = vsn,
    col_types = col_types,
    settings = settings
  )
}

#' @export
data_read_precipitation_comb <- function(
  dtype = default_dtype(),
  vsn = default_version(),
  settings = default_settings()
) {
  dat_read_generic(con_fun = data_con_precipitation_comb,
    dtype = dtype, vsn = vsn, settings = settings)
}

#' @export
data_read_join_full <- function(
  dtype = "tidy",
  vsn = default_version(),
  settings = default_settings()
) {
  dat_read_generic(con_fun = data_con_join_full,
    dtype = dtype, vsn = vsn, settings = settings)
}

#' @export
data_read_join_inner <- function(
  dtype = "tidy",
  vsn = default_version(),
  settings = default_settings()
) {
  dat_read_generic(con_fun = data_con_join_inner,
    dtype = dtype, vsn = vsn, settings = settings)
}

#' @export
data_read_db <- function(
  dtype = "tidy",
  vsn = default_version(),
  settings = default_settings()
) {
  dat_read_generic(con_fun = data_con_db,
    dtype = dtype, vsn = vsn, settings = settings)
}

#' @export
data_read_db_msr <- function(
  dtype = "tidy",
  vsn = default_version(),
  settings = default_settings()
) {
  dat_read_generic(con_fun = data_con_db_msr,
    dtype = dtype, vsn = vsn, settings = settings)
}

#' @export
data_read_distance <- function(
  dtype = "tidy",
  vsn = default_version(),
  settings = default_settings()
) {
  dat_read_generic(con_fun = data_con_distance,
    dtype = dtype, vsn = vsn, settings = settings)
}
