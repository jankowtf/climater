
# Settings ----------------------------------------------------------------

settings <- default_settings()
data_version <- setting_get_version("data_version")

# Data repo ---------------------------------------------------------------

set_global_data_repo("repo_1", settings = settings)
# set_global_data_repo("repo_2")

# Data import -------------------------------------------------------------

if (settings$overwrite$tidy) {
  source(here::here("inst/app/02-tidy.R"))
}
# Only needed once / whenever data needs to be updated

# Transform ---------------------------------------------------------------

if (settings$overwrite$transform) {
  source(here::here("inst/app/03-transform.R"))
}

# Load --------------------------------------------------------------------

source(here::here("inst/app/04-load.R"))

# User input --------------------------------------------------------------

source(here::here("inst/app/05-user_input.R"))

# Model -------------------------------------------------------------------

source(here::here("inst/app/06-model.R"))
