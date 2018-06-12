
#' @export
setting_get_version <- function(
  id = "data_version",
  settings = default_settings()
) {
  settings$versions[[id]]
}

#' @export
settings_get_data_repo <- function(
  id = "repo_1",
  settings = default_settings()
) {
  ret <- settings$data_repo[[id]]
  if (is.null(ret)) stop("Invalid data repo setting")
  ret
}
