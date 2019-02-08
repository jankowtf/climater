# climater 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
* Package initalization aspects (DESCRIPTION, README, travis, covr, /inst/examples, /inst/apps, package dependencies)
* Ported database-related code from `{climateapp}` (old package).
* Wrapped file paths with `here::here()` in `/inst/app/0-settings.R`

# climater 0.0.0.9006

* Added data con for distances (`/R/data_con.R/data_con_distance`)
* Added write function for distances (`/R/data_write.R/data_write_distance`)
* Added read function for distances (`/R/data_write.R/data_read_distance`)
* Added  name mappings for `latitude`, `longitude`, `distance` and `station_ref` in `/0-settings.R`
* Added `compute_geo_distance` in `/R/data_transform_distances.R` that encapsulates the distance formula described in https://www.kompf.de/gps/distcalc.html
* Added `data_trans_distance` that computes pairwise distance between stations
* Cleared out `/inst/app/data/tidy/´ in order to control `.RDS` files via git LFS
* Added data repo entries to settings file and implemented global data repo controls (`set_global_data_repo()` and
`get_global_data_repo()`)
* Added data repo section to `1-main.R`
* Took out `here::here()` in `/R/data_con.R` functions
* Explicit column types for data imports: `data_read_station_v2`,
`data_read_temperature_min_v2`, `data_read_temperature_max_v2`,
`data_read_sunshine_duration_v2`, `data_read_precipitation_historical_v2`, `data_read_precipitation_recent_v2`
* Added `dim_station` to `dat_db_msr` via `data_trans_db_msr_v2`
* Aligned `/inst/app/8-app.Rmd` to new functionality for distances and took out
some blocks that are currently irrelevant

# climtater 0.0.0.9007

* Updated version number in `DESCRIPTION`
* Added `frontend_version` to settings/versions (`/inst/app/0-settings.R`)
* Fixed transformation bug for `dat_sundur` (added `* 24` in order to compute
values on "hours per day" scale instead of on "day per day" scale). Also rounded
to full integers.
* Added `floor()` and `ceiling()` to inputs in frontend to make user-facing
values nicer
* Hard-coded inputs for `msr_sundur_avg` to range between 0 and 28 with `step =
4` upon user request
* Fixed mapping bug regarding temp_max data 
* Fixed negative sundur values by applying `abs()` via
`data_tidy_sunshine_duration_v3`
* Added numeric inputs to frontend for user-defined latitude and longitude 
* Added radio button input to frontend for toggling between auto-detected and user-defined geo location
* Prediction table only shows relevant columns now
* Restructured frontend a bit to vsually separate the different input blocks 
* Modified modelling functions (`/R/modelling.R`) to include on-the-fly
computations of geo distance in output table

# climater 0.0.0.9008

* Fixed wrong data con link for `sundur`
* Rolled back incorrect transformations of `sundur` that was caused by the wrong
data link above
* Implemented key-to-label mapping
(`R/data_transform.R/dat_transform_names_to_label`) and use labels in prediction
table
* Update frontend version to v0.0.0.9003

# climater 0.0.0.9009

* Removed tick marks from inputs
* Implemented key-to-label mapping for input selection and display of input table
* Changed input selection from checkbox to selectize layout
* Update frontend version to v0.0.0.9004

# climater 0.0.0.9010

* Changed input selection back to checkboxes
* Fixed negative values in `msr_precip_*` that were caused by missing value
imputation via `data_trans_impute_missing_values()`
* Rounded distances to 4 digits
* Update frontend version to v0.0.0.9004
* Experimental version of deployable app script for shinyapps.io (`/inst/app/10-app_deployed.Rmd`)
* Change ennumeration of scripts to double digits

# climater v0.0.2.9000

* Added setting `expand_weight_grid = TRUE/FALSE` which controls whether or not
`expand.grid()` is used to create combinations of all weight factors in
`model_estimate`
* Started to rename functions from versioned name to bare name and deprecated
outdated functions (not finished yet)
* Bump to version v0.0.2.9000

# climater v0.0.2.9001

* Implemented implicit "max. allowed distance based on prime location" (see
`dat_distance_prime_react()`) that acts as lower bound in
`handle_input_distance()` and ensures that no alternative recommendation can be
farther away than the prime location
* Ensured distinct rows in alternative recommendation table (see
`dplyr::distinct()` in `output$model_output_alts <-
shiny::renderDataTable({...})`)
* Ensure ascending ordering by `msr_distance` of alternative recommendations
(see `dplyr::arrange()` in `output$model_output_alts <-
shiny::renderDataTable({...})`)
* Internal: removed versioning scheme from `model_estimate_inner*()`, now back to 
`model_estimate_inner()`
* Internal: removed versioning scheme from `handle_input_distance*()`, now back to 
`handle_input_distance()`
* Internal: bump to version v0.0.2.9001

# climater v0.0.2.9002

* Ensured that prime location isn't contained in alternatives by passing
`dat_model_output_prime` to `model_run` which passes it through to
`model_estimate_inner` which filters the return values accordingly.
* Tried to catch constellations where settings lead to an empty intersection of
the underlying DB via a pragmatic approach of returning `tibble()` as output of
`model_estimate_prime` and `model_estimate` and subsequently checking for empty
tibbles before visualizing. Works in the sense that at least an informative
error message is displayed, but somehow UI does not "switch back" to the regular
state even after a valid settings input is defined. Needs to be further
investigated
* Internal: bump to version v0.0.2.9002

# v0.0.2.9003

* Added CO2 emissions (`msr_co2`) in `/app.R`
* Changed defaut of setting `ui_mode_tabs` to `FALSE`
* Internal: bump to version v0.0.2.9003
