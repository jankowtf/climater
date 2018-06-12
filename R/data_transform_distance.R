
# Distance formula --------------------------------------------------------

# https://www.kompf.de/gps/distcalc.html
compute_geo_distance <- function(p_1, p_2) {
  dist <- 6378.388 * acos(
    sin(p_1$dim_latitude) * sin(p_2$dim_latitude) +
      cos(p_1$dim_latitude) * cos(p_2$dim_latitude) *
      cos(p_1$dim_longitude - p_2$dim_longitude))
}

compute_geo_distance_v2 <- function(p_1, p_2) {
  dist <- 6378.388 * acos(
    sin(p_1[ , "dim_latitude"]) * sin(p_2[ , "dim_latitude"]) +
      cos(p_1[ , "dim_latitude"]) * cos(p_2[ , "dim_latitude"]) *
      cos(p_1[ , "dim_longitude"] - p_2[ , "dim_longitude"]))
  names(dist) <- "msr_distance"
  dist

  # TODO-20180610: robustify against dependency on tibble and column names from
  # settings
}
