---
title: "Climate app demo"
author: "Janko Thyson"
output: html_document
editor_options: 
  chunk_output_type: console
---



## Main script

Note that the actual main script that executes a particular "app usage run" can
be found at `/inst/app/1-main.R`.

The following code simply serves as a more illustrative explanation of how the
individual app parts are linked to each other

## Settings


```r
settings <- default_settings()
set_global_data_repo("repo_1", settings = settings)
```

```
## [1] "C:/Users/janko/dropbox (personal)/Data/climater"
```

## Load DB data


```r
source(here::here("inst/app/4-load.R"))
```

This script loads the following three data objects:

1. Database table for measures


```r
dat_db %>% glimpse()
```

```
## Observations: 10,272
## Variables: 15
## $ uid            <chr> "02080-1", "02080-2", "02080-3", "02080-4", "02...
## $ dim_station    <chr> "02080", "02080", "02080", "02080", "02080", "0...
## $ time_month     <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3,...
## $ msr_temp_min   <dbl> -21.2, -19.8, -15.6, -8.7, -0.7, 6.2, 8.4, 6.3,...
## $ msr_temp_max   <dbl> -21.2, -19.8, -15.6, -8.7, -0.7, 6.2, 8.4, 6.3,...
## $ msr_temp_avg   <dbl> -21.2, -19.8, -15.6, -8.7, -0.7, 6.2, 8.4, 6.3,...
## $ msr_precip_min <dbl> 0.000000, 0.000000, 1.000000, 2.000000, 2.00000...
## $ msr_precip_max <dbl> 16.00000, 19.00000, 12.00000, 13.00000, 14.0000...
## $ msr_precip_avg <dbl> 8.000000, 6.800000, 6.600000, 6.200000, 6.90000...
## $ msr_precip_med <dbl> 7.000000, 6.000000, 6.000000, 6.000000, 7.00000...
## $ msr_sundur_avg <dbl> -0.7, -0.7, -0.5, -0.3, 0.0, 0.2, 0.3, 0.2, 0.0...
## $ dim_country    <chr> "Sweden", "Sweden", "Sweden", "Sweden", "Sweden...
## $ dim_longitude  <dbl> 22.45, 22.45, 22.45, 22.45, 22.45, 22.45, 22.45...
## $ dim_latitude   <dbl> 68.45, 68.45, 68.45, 68.45, 68.45, 68.45, 68.45...
## $ dim_high       <dbl> 330, 330, 330, 330, 330, 330, 330, 330, 330, 33...
```

```r
dat_db %>% head()
```

```
## # A tibble: 6 x 15
##   uid     dim_station time_month msr_temp_min msr_temp_max msr_temp_avg
##   <chr>   <chr>            <int>        <dbl>        <dbl>        <dbl>
## 1 02080-1 02080                1      -21.2        -21.2        -21.2  
## 2 02080-2 02080                2      -19.8        -19.8        -19.8  
## 3 02080-3 02080                3      -15.6        -15.6        -15.6  
## 4 02080-4 02080                4       -8.70        -8.70        -8.70 
## 5 02080-5 02080                5       -0.700       -0.700       -0.700
## 6 02080-6 02080                6        6.20         6.20         6.20 
## # ... with 9 more variables: msr_precip_min <dbl>, msr_precip_max <dbl>,
## #   msr_precip_avg <dbl>, msr_precip_med <dbl>, msr_sundur_avg <dbl>,
## #   dim_country <chr>, dim_longitude <dbl>, dim_latitude <dbl>,
## #   dim_high <dbl>
```

2. Database table for stations 


```r
dat_station %>% glimpse()
```

```
## Observations: 4,285
## Variables: 6
## $ dim_station      <chr> "01001", "01005", "01007", "01008", "01025", ...
## $ dim_station_name <chr> "Jan Mayen", "Isfjord Radio", "Ny-Alesund", "...
## $ dim_country      <chr> "Norway", "Norway", "Norway", "Norway", "Norw...
## $ dim_latitude     <dbl> 70.94, 78.06, 78.92, 78.25, 69.68, 69.65, 74....
## $ dim_longitude    <dbl> -8.67, 13.63, 11.93, 15.50, 18.91, 18.94, 19....
## $ dim_high         <dbl> 9, 9, 8, 27, 9, 114, 16, 131, 15, 9, 13, 15, ...
```

```r
dat_station %>% head()
```

```
## # A tibble: 6 x 6
##   dim_station dim_station_name dim_country dim_latitude dim_longitude
##   <chr>       <chr>            <chr>              <dbl>         <dbl>
## 1 01001       Jan Mayen        Norway              70.9         -8.67
## 2 01005       Isfjord Radio    Norway              78.1         13.6 
## 3 01007       Ny-Alesund       Norway              78.9         11.9 
## 4 01008       Svalbard         Norway              78.2         15.5 
## 5 01025       Tromso/Langnes   Norway              69.7         18.9 
## 6 01026       Tromso           Norway              69.6         18.9 
## # ... with 1 more variable: dim_high <dbl>
```

## Example user input


```r
source(here::here("inst/app/5-user_input.R"))
```

Open script `inst/app/5-user_input.R` to change the example user input

1. Top level input


```r
knn
```

```
## [1] 3
```

```r
dist_measures
```

```
## [1] "euclidean"
```

```r
dist_measure_final
```

```
## [1] "euclidean"
```

2. Actual numeric input


```r
dat_input %>% glimpse()
```

```
## Observations: 5
## Variables: 12
## $ dim_station    <chr> "96491", "62056", "85577", "82331", "47168"
## $ time_month     <int> 12, 9, 10, 7, 6
## $ msr_temp_min   <dbl> 23.4, 16.6, 9.9, 22.7, 18.0
## $ msr_temp_max   <dbl> 23.4, 16.6, 9.9, 22.7, 18.0
## $ msr_temp_avg   <dbl> 23.4, 16.6, 9.9, 22.7, 18.0
## $ msr_precip_min <dbl> 10, 0, 0, 0, 4
## $ msr_precip_max <dbl> 24, 4, 5, 18, 13
## $ msr_precip_avg <dbl> 19.6, 1.6, 1.4, 8.4, 8.5
## $ msr_sundur_avg <dbl> 0.8, 0.6, 0.3, 0.7, 0.6
## $ dim_latitude   <dbl> 53.63, 53.63, 53.63, 53.63, 53.63
## $ dim_longitude  <dbl> 9.99, 9.99, 9.99, 9.99, 9.99
## $ msr_distance   <dbl> 1.5, 1.5, 1.5, 1.5, 1.5
```

```r
dat_input %>% head()
```

```
##       dim_station time_month msr_temp_min msr_temp_max msr_temp_avg
## 10116       96491         12         23.4         23.4         23.4
## 5865        62056          9         16.6         16.6         16.6
## 9034        85577         10          9.9          9.9          9.9
## 7999        82331          7         22.7         22.7         22.7
## 4470        47168          6         18.0         18.0         18.0
##       msr_precip_min msr_precip_max msr_precip_avg msr_sundur_avg
## 10116             10             24           19.6            0.8
## 5865               0              4            1.6            0.6
## 9034               0              5            1.4            0.3
## 7999               0             18            8.4            0.7
## 4470               4             13            8.5            0.6
##       dim_latitude dim_longitude msr_distance
## 10116        53.63          9.99          1.5
## 5865         53.63          9.99          1.5
## 9034         53.63          9.99          1.5
## 7999         53.63          9.99          1.5
## 4470         53.63          9.99          1.5
```

## Recommendation object {.tabset}

### Estimation

Actual estimation and prediction based on distance measures











