
#' @export
default_dtype <- function() {
  c("tidy", "raw")
}

#' @export
default_name_mapping_type <- function() {
  c("key", "label")
}

#' @export
default_ftype <- function() {
  character()
}

#' @export
default_path_norm <- function() {
  FALSE
}

#' @export
default_version <- function() {
  "v1"
}

#' @export
default_settings <- function(file = "00-settings.R") {
  source(here::here(file))$value
}

#' @export
default_throw <- function() {
  TRUE
}

#' @export
default_id_col_rhs <- function() {
  default_name("station")
}

#' @export
default_id_col_lhs <- function() {
  rhs <- default_id_col_rhs()
  lhs <- dplyr::quo(sprintf("dim_%s", dplyr::quo_name(rhs)))
  lhs <- dplyr::quo_name(rlang::eval_tidy(lhs))
}

#' @export
default_name <- function(
  id,
  type = default_name_mapping_type(),
  settings = default_settings()
) {
  type <- match.arg(type)
  res <- settings$name_mapping[[id]][[type]]
  if (is.null(res)) stop(sprintf("Invalid mapping (ID = '%s', type = '%s')",
    id, type))
  res
}

#' @export
default_dist_measures <- function() {
  c(
    "euclidean",
    "manhattan",
    "jaccard",
    "squared_euclidean",
    "chebyshev",
    "avg"
  )
}

default_data_repos <- function() {
  c(
    "default",
    "alternative_1"
  )
}
