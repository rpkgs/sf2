#' prj84
#' Default Spatial object projection: WGS84.
#' @importFrom sp CRS
#' @export
prj84 <- CRS("+proj=longlat +datum=WGS84 +no_defs")
# prj84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
# prj84 <- CRS("+init=EPSG:4326")

#' df2sp
#' 
#' Convert dataframe data into SpatialPointsDataframe (Deprecated! Use [df2sf()]
#' instead.)
#' 
#' @param d A data.frame with coordinates information 
#' @param formula ~longitude+latitude 
#' @inheritParams make_grid
#' 
#' @importFrom sp coordinates proj4string coordinates<- proj4string<-
#' 
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
