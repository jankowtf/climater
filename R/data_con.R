
# Data con: get path ------------------------------------------------------

data_con_get_path <- function(
  id,
  dtype = default_dtype(),
  ftype = default_ftype(),
  norm = default_path_norm(),
  vsn = default_version(),
  settings = default_settings()
) {
  dtype <- match.arg(dtype)
  dir <- sprintf("dir_%s", dtype)
  path <- file.path(
    get_global_data_repo(),
    settings$data[[dir]],
    settings$data$cons[[id]][[dtype]]
  )
  if (dtype != "raw") {
    path <- unlist(str_split(path, "\\."))
    path <- sprintf("%s_%s.%s", path[1], vsn, path[2])
  }
  if (length(ftype)) path <- sprintf("%s.%s", path, ftype)
  if (norm) path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  path
}

# Data con: check ---------------------------------------------------------

data_con_check <- function(con, throw = default_throw()) {
  res <- fs::file_exists(con)
  if (throw && !res) {
    stop(sprintf("File connection does not exist: %s", con))
  }
  res
}

# Data cons: actual connections -------------------------------------------

data_con_station <- function(
  dtype = default_dtype(),
  ftype = default_ftype(),
  norm = default_path_norm(),
  vsn = default_version(),
  settings = default_settings()
) {
  data_con_get_path(id = "station", dtype = dtype,
    ftype = ftype, norm = norm, vsn = vsn, settings = settings)
}

data_con_temperature_min <- function(
  dtype = default_dtype(),
  ftype = default_ftype(),
  norm = default_path_norm(),
  vsn = default_version(),
  settings = default_settings()
) {
  data_con_get_path(id = "temperature_min", dtype = dtype,
    ftype = ftype, norm = norm, vsn = vsn, settings = settings)
}

data_con_temperature_max <- function(
  dtype = default_dtype(),
  ftype = default_ftype(),
  norm = default_path_norm(),
  vsn = default_version(),
  settings = default_settings()
) {
  data_con_get_path(id = "temperature_max", dtype = dtype,
    ftype = ftype, norm = norm, vsn = vsn, settings = settings)
}

data_con_temperature_comb <- function(
  dtype = default_dtype(),
  ftype = default_ftype(),
  norm = default_path_norm(),
  vsn = default_version(),
  settings = default_settings()
) {
  data_con_get_path(id = "temperature_comb", dtype = dtype,
    ftype = ftype, norm = norm, vsn = vsn, settings = settings)
}

data_con_sunshine_duration <- function(
  dtype = default_dtype(),
  ftype = default_ftype(),
  norm = default_path_norm(),
  vsn = default_version(),
  settings = default_settings()
) {
  data_con_get_path(id = "sunshine_duration", dtype = dtype,
    ftype = ftype, norm = norm, vsn = vsn, settings = settings)
}

data_con_precipitation_historical <- function(
  dtype = default_dtype(),
  ftype = default_ftype(),
  norm = default_path_norm(),
  vsn = default_version(),
  settings = default_settings()
) {
  data_con_get_path(id = "precipitation_historical", dtype = dtype,
    ftype = ftype, norm = norm, vsn = vsn, settings = settings)
}

data_con_precipitation_recent <- function(
  dtype = default_dtype(),
  ftype = default_ftype(),
  norm = default_path_norm(),
  vsn = default_version(),
  settings = default_settings()
) {
  data_con_get_path(id = "precipitation_recent", dtype = dtype,
    ftype = ftype, norm = norm, vsn = vsn, settings = settings)
}

data_con_precipitation_comb <- function(
  dtype = default_dtype(),
  ftype = default_ftype(),
  norm = default_path_norm(),
  vsn = default_version(),
  settings = default_settings()
) {
  data_con_get_path(id = "precipitation_comb", dtype = dtype,
    ftype = ftype, norm = norm, vsn = vsn, settings = settings)
}

data_con_join_full <- function(
  dtype = default_dtype(),
  ftype = default_ftype(),
  norm = default_path_norm(),
  vsn = default_version(),
  settings = default_settings()
) {
  data_con_get_path(id = "join_full", dtype = dtype,
    ftype = ftype, norm = norm, vsn = vsn, settings = settings)
}

data_con_join_inner <- function(
  dtype = default_dtype(),
  ftype = default_ftype(),
  norm = default_path_norm(),
  vsn = default_version(),
  settings = default_settings()
) {
  data_con_get_path(id = "join_inner", dtype = dtype,
    ftype = ftype, norm = norm, vsn = vsn, settings = settings)
}

data_con_db <- function(
  dtype = default_dtype(),
  ftype = default_ftype(),
  norm = default_path_norm(),
  vsn = default_version(),
  settings = default_settings()
) {
  data_con_get_path(id = "db", dtype = dtype,
    ftype = ftype, norm = norm, vsn = vsn, settings = settings)
}

data_con_db_msr <- function(
  dtype = default_dtype(),
  ftype = default_ftype(),
  norm = default_path_norm(),
  vsn = default_version(),
  settings = default_settings()
) {
  data_con_get_path(id = "db_msr", dtype = dtype,
    ftype = ftype, norm = norm, vsn = vsn, settings = settings)
}

data_con_distance <- function(
  dtype = default_dtype(),
  ftype = default_ftype(),
  norm = default_path_norm(),
  vsn = default_version(),
  settings = default_settings()
) {
  data_con_get_path(id = "distance", dtype = dtype,
    ftype = ftype, norm = norm, vsn = vsn, settings = settings)
}
