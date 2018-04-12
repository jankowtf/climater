# climater

The goal of `climater` is to recommend locations that match your climate preferences.

## Cloning

```
git clone https://github/rappster/climater
```

## Installation

```r
devtools::install_github("rappster/climater", auth_token = "<personal_access_token>")
```

## Launch the app (rough draft!)

### From within the RStudio project 

Requires prior cloning and running `devtools::load_all()`

``` r
rmarkdown::run("inst/app/8-app.Rmd")
```

### From plain R script 

Requires installation prior via  `devtools::install_github("rappster/climater", auth_token = "<personal_access_token>")`

``` r
library(climater)
rmarkdown::run(system.file("app/8-app.Rmd", package = "climater"))
```
