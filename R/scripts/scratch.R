library(tidyverse)

file <- "C:/Users/janko/GD/Code/R/Packages/climater/inst/app/data/raw/precipGE1mm_days/recent/01001_201501_201709.txt"
dat <- read.csv2(file, stringsAsFactors = FALSE)
dat <- rio::import(file) %>% dat_tidy_names()
