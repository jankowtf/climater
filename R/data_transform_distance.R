
# Distance formula --------------------------------------------------------

# https://www.kompf.de/gps/distcalc.html
#' @export
compute_geo_distance_v3 <- function(p_1, p_2) {
  dim_station <- p_2[["dim_station"]]
  p_1 <- as.matrix(p_1[ , c("dim_latitude", "dim_longitude")])
  p_2 <- as.matrix(p_2[ , c("dim_latitude", "dim_longitude")])
  dist <- 6378.388 * acos(
    sin(pi/180*p_1[ , "dim_latitude"]) * sin(pi/180*p_2[ , "dim_latitude"]) +
      cos(pi/180*p_1[ , "dim_latitude"]) * cos(pi/180*p_2[ , "dim_latitude"]) *
      cos(pi/180*p_1[ , "dim_longitude"] - pi/180*p_2[ , "dim_longitude"]))
  tibble::tibble(
    dim_station = dim_station,
    msr_distance = dist
  )
  # TODO-20180610: robustify against dependency on tibble and column names from
  # settings
}
