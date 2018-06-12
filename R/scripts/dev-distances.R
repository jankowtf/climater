# library(ggplot2)
library(tidyverse)

# Own data ----------------------------------------------------------------

file <- "C:/Users/janko/Dropbox (Personal)/Code/R/Packages/_Dev/climater/inst/app/data/tidy/station_v3.rds"
dat <- readRDS(file)
dat %>% glimpse()

# Package: ggmap ----------------------------------------------------------

# loading the required packages
library(ggmap)

# creating a sample data.frame with your lat/lon points
lon <- c(-38.31,-35.5)
lat <- c(40.96, 37.5)
df <- as.data.frame(cbind(lon,lat))

# getting the map
mapgilbert <-
  ggmap::get_map(
    location = c(lon = mean(df$lon), lat = mean(df$lat)),
    zoom = 4,
    maptype = "satellite",
    scale = 2
  )
mapgilbert %>% glimpse()

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(
    data = df,
    aes(
      x = lon,
      y = lat,
      fill = "red",
      alpha = 0.8
    ),
    size = 5,
    shape = 21
  ) +
  guides(fill = FALSE, alpha = FALSE, size = FALSE)


# calculate distance between two place names
arena_dist <- mapdist(from = "Madison Square Garden New York, NY", to = "The Palace of Auburn Hills Auburn Hills, MI")

# output distance in miles
arena_dist$miles

# output "distance" in minutes
arena_dist$minutes


# Package: geosphere ------------------------------------------------------

install.packages("geosphere")
library(geosphere)
distm(c(df$lon[1], df$lat[1]), c(df$lon[2], df$lat[2]), fun = distHaversine)
distm(df, fun = distHaversine)

distances <- distm(dat %>% head() %>% select(dim_longitude, dim_latitude), fun = distHaversine)
distances <- distm(dat %>% select(dim_longitude, dim_latitude), fun = distHaversine)
distances %>% glimpse()
distances %>% class()
distances %>% dim()

Also:
# distHaversine()
# distMeeus()
# distRhumb()
# distVincentyEllipsoid()
# distVincentySphere()

# Openrouteservice --------------------------------------------------------

devtools::install_github("GIScience/openrouteservice-r")
library(openrouteservice)

# one-time API key set-up
# ors_api_key("<your-api-key>")

# query for coordinates
locations <- lapply(c("Heidelberg", "Kraków"), ors_geocode)
coordinates <- lapply(locations, function(x) x$features[[1]]$geometry$coordinates)

# find route
route <- ors_directions(coordinates, format="geojson")

# route length in kilometres and duration in hours
unlist(route$features[[1]]$properties$summary) / c(1000, 3600)
##    distance    duration
## 1051.861300    9.205167

# draw on map using leaflet
library(leaflet)

leaflet() %>%
  addTiles() %>%
  addGeoJSON(route, fill=FALSE) %>%
  fitBBox(route$bbox)


# Pairwise distance calculations ------------------------------------------
# https://stackoverflow.com/questions/46262194/pairwise-distance-calculation-nested-data-frame

df <- maps::us.cities %>%
  slice(1:20) %>%
  group_by(name) %>%
  nest(long, lat, .key = coords)

df2 <- df %>%
  # Create the grid
  mutate(name1 = name) %>%
  select(starts_with("name")) %>%
  complete(name, name1) %>%
  filter(name != name1) %>%
  left_join(df, by = "name") %>%
  left_join(df, by = c("name1" = "name")) %>%
  # Grid completed. Calcualte the distance by distHaversine
  mutate(distance = map2_dbl(coords.x, coords.y, geosphere::distHaversine))

df3 <- df2 %>%
  select(-starts_with("coord")) %>%
  group_by(name) %>%
  nest()

