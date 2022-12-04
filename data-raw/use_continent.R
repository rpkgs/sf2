library(sf)
library(usethis)

shp_continent = read_sf("data-raw/shp/continent.shp")
use_data(shp_continent, overwrite = TRUE)
