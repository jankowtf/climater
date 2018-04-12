#' @export
link_dir_data <- function(dir, project) {
  env <- getOption(project)
  dir <- normalizePath(dir, winslash = "/") %>%
    stringr::str_replace_all("^\\\\+", "//")
  if (is.null(env)) {
    env <- new.env()
    env$dir_data <- dir
    # expr <- substitute(options(NAME = env), list(NAME = project))
    # lazyeval::interp(~options(NAME = env, NAME = project))
    eval(parse(text = sprintf("options(%s = env)", project)))
  } else {
    env$dir_data <- dir
  }
  dir
}

#' @export
get_dir_data <- function(project) {
  env <- getOption(project)
  env$dir_data
}
