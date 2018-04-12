
#' @export
setting_get_version <- function(
  id = "data_version",
  settings = default_settings()
) {
  settings$versions[[id]]
}
