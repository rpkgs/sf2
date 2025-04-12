library(sf)
library(usethis)

read_sf2 <- \(f) read_sf(f) %>% st_set_crs(4326)

river1 <- read_sf2("Z:/ShapeFiles/国家基础地理信息系统数据/hyd1_4l.shp")
river2 <- read_sf2("Z:/ShapeFiles/国家基础地理信息系统数据/hyd2_4l.shp")
river4 <- read_sf2("Z:/ShapeFiles/国家基础地理信息系统数据/River4_polyline.shp")
river5 <- read_sf2("Z:/ShapeFiles/国家基础地理信息系统数据/River5_polyline.shp")
use_data(river1, river2, river4, river5)

river_p1 <- read_sf2("Z:/ShapeFiles/国家基础地理信息系统数据/hyd1_4p.shp")
river_p2 <- read_sf2("Z:/ShapeFiles/国家基础地理信息系统数据/hyd2_4p.shp")
use_data(river_p1, river_p2)
