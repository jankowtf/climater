
# Distance formula --------------------------------------------------------

# https://www.kompf.de/gps/distcalc.html
#' @export
compute_geo_distance <- function(p_1, p_2) {
  dist <- 6378.388 * acos(
    sin(p_1$dim_latitude*pi/180) * sin(p_2$dim_latitude*pi/180) +
      cos(p_1$dim_latitude*pi/180) * cos(p_2$dim_latitude*pi/180) *
      cos(p_1$dim_longitude*pi/180 - p_2$dim_longitude*pi/180))
}

#' @export
compute_geo_distance_v2 <- function(p_1, p_2) {
  dist <- 6378.388 * acos(
    sin(pi/180*p_1[ , "dim_latitude"]) * sin(pi/180*p_2[ , "dim_latitude"]) +
      cos(pi/180*p_1[ , "dim_latitude"]) * cos(pi/180*p_2[ , "dim_latitude"]) *
      cos(pi/180*p_1[ , "dim_longitude"] - pi/180*p_2[ , "dim_longitude"]))
  names(dist) <- "msr_distance"
  dist

  # TODO-20180610: robustify against dependency on tibble and column names from
  # settings
}
