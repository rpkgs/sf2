#' prj84
#' Default Spatial object projection: WGS84.
#' @importFrom sp CRS
#' @export
prj84 <- CRS("+proj=longlat +datum=WGS84 +no_defs")
# prj84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
# prj84 <- CRS("+init=EPSG:4326")

#' df2sp
#' Convert dataframe data into SpatialPointsDataframe
#' 
#' @param d A data.frame with coordinates information
#' @param formula ~longitude+latitude
#' @inheritParams make_grid
#' 
#' @importFrom sp coordinates proj4string coordinates<- proj4string<-
#' @export
df2sp <- function (d, formula = ~lon + lat, prj){
    if (missing(prj)) prj <- prj84
    coordinates(d) <- formula
    proj4string(d) <- prj
    return(d)
}

#' extractId
#' 
#' @param sp A SpatialPointDataFrame with the station coordinates information
#' @param shpfile A character, shape file path.
#' 
#' @importFrom maptools readShapePoly
#' @importFrom sp SpatialPolygons over
#' @export
extractId <- function(sp, shpfile){
    # formula <- ~lon+lat
    # sp    <- df2sp(station, formula, prj84)
    shp   <- read_shp(shpfile, proj4string = prj84)
    bound <- SpatialPolygons(shp@polygons, proj4string = prj84)
    ## clipped station
    clipId <- which(!is.na(over(sp, bound))) %>% as.numeric

    plot(shp, axes = T)
    plot(sp[clipId, ], add = T)
    clipId#return clipId
}

# #' get_nearGrids
# #' 
# #' find nearest 3*3 grids of points
# #'
# #' with the help of RANN package to quickly select nearest 3*3 grids
# #' use line dist to replace sphere distance
# #' 
# #' @param grid SpatialGridDataFrame object
# #' @param sp SpatialPointDataframe object
# #' @param ngrid how many nearest grids to be extracted?
# #' @return nearId nearest order
# #' 
# #' @importFrom RANN nn2
# #' @export
# get_nearGrids <- function(grid, station, ngrid = 3*3){
#   loc_grid    <- coordinates(grid)
#   loc_station <- coordinates(station)
#   x   <- nn2(loc_grid, loc_station, ngrid)
#   res <- data.frame(stationId = rep(station@data$stationId, ngrid),
#                     nearId = rep(1:ngrid, rep(nrow(station), ngrid)),
#                     idx = as.numeric(x$nn.idx),
#                     dists = as.numeric(x$nn.dists) *6371 * pi/180)
#   # rdist.earth(loc_grid[res$idx, ], loc_station)
#   return(res)
# }
