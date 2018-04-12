
# Settings ----------------------------------------------------------------

settings <- default_settings()

# Data import -------------------------------------------------------------

if (settings$overwrite$tidy) {
  source(here::here("inst/app/2-tidy.R"))
}
# Only needed once / whenever data needs to be updated

# Transform ---------------------------------------------------------------

if (settings$overwrite$transform) {
  source(here::here("inst/app/3-transform.R"))
}

# Load --------------------------------------------------------------------

source(here::here("inst/app/4-load.R"))

# Model -------------------------------------------------------------------

source(here::here("inst/app/5-user_input.R"))

# Model -------------------------------------------------------------------

source(here::here("inst/app/6-model.R"))
