#' gridSaveToPoly
#'
#' Write spatialPixelDataframe into polygon shpfile for the convenience of
#' checking in google earth.
#'
#' @param grid The spatialPixelDataframe or spatialGridDataframe object.
#' @param file The string of output shapefile name including path.
#' @importFrom rgdal writeOGR
#' @export
gridSaveToPoly <- function(grid, file){
  poly <-  as(grid, "SpatialPolygonsDataFrame")
  # writePolyShape(poly, file) #, can't write prj info
  writeOGR(poly, dsn = file, layer = basename(file), driver = "ESRI Shapefile")
}
