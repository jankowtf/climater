library(tidyverse)

months <- rep(c(1:12), 2)
# months[months > 6] <- 12 - months[months > 6]
# x <- sin(2 * pi * months / 12)
#
# sapply(months, function(x) min(x, 12 - x))

month_df <- tibble::tibble(
  month = months,
  month_sin = sin(2 * pi * months / 12),
  month_cos = cos(2 * pi * months / 12)
)

month_df %>%
  dplyr::mutate(
    diff = abs(c(0, diff(month_sin)))
  ) %>%
  View()
