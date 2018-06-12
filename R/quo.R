
quo_prepend <- function(what, with) {
  res <- as.name(sprintf("%s_%s", with, dplyr::quo_name(what)))
  res <- dplyr::enquo(res)
  res
}

quo_append <- function(what, with) {
  res <- as.name(sprintf("%s_%s", dplyr::quo_name(what), with))
  res <- dplyr::enquo(res)
  res
}

quo_xpend <- function(what, before, after) {
  res <- as.name(sprintf("%s_%s_%s", before, dplyr::quo_name(what), after))
  res <- dplyr::enquo(res)
  res
}
