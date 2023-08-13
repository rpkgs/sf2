#' extractId
#'
#' @param sp A SpatialPointDataFrame with the station coordinates information
#' @param shpfile A character, shape file path.
#'
#' @importFrom maptools readShapePoly
#' @importFrom sp SpatialPolygons over
#' @export
extractId <- function(sp, shpfile) {
  # formula <- ~lon+lat
  # sp    <- df2sp(station, formula, prj84)
  prj84 <- get_crs()
  shp <- read_shp(shpfile, proj4string = prj84)
  bound <- SpatialPolygons(shp@polygons, proj4string = prj84)
  ## clipped station
  clipId <- which(!is.na(over(sp, bound))) %>% as.numeric()

  plot(shp, axes = T)
  plot(sp[clipId, ], add = T)
  clipId # return clipId
}
