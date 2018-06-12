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
