
#' @export
set_global_data_repo <- function(
  id = "repo_1",
  settings = default_settings()
) {
  project <- ".climater"
  env <- getOption(project)
  dir <- normalizePath(settings_get_data_repo(id), winslash = "/") %>%
    stringr::str_replace_all("^\\\\+", "//")
  if (is.null(env)) {
    env <- new.env()
    env$data_repo <- dir
    eval(parse(text = sprintf("options(%s = env)", project)))
  } else {
    env$data_repo <- dir
  }
  dir
}

#' @export
get_global_data_repo <- function() {
  env <- getOption(".climater")
  ret <- env$data_repo
  if (is.null(ret)) stop("Invalid global data repo setting")
  ret
}
