
library(stringr)
pkg_name <- devtools::as.package(".")$package
# devtools::build(path = str_glue("../../_Sources/{pkg_name}.tar.gz"))
devtools::build(path = "../../_Sources")
