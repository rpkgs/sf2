#' @importFrom sf as_Spatial st_crs
#' @export
read_polyline <- function(file, lwd = 0.5, ...) {
  if (file.exists(file)) {
    shp <- read_sf(file) %>% as_Spatial()
    st_crs(shp) <- 4326
    # suppressWarnings(shp <- readShapeLines(file))
    list("sp.lines", shp, first = FALSE, lwd = lwd, ...) # china poly
  }
}


#' write and read ESRI Shapefile
#'
#' @param x Spatial* Object from sp package
#' @param file The path of ESRI shapefile
#' @param ... other parameters to `rgdal::writeOGR()`
#' 
#' @export
write_shp <- function(x, file, ...) {
  write_sf(sf::st_as_sf(x), file, layer_options = "ENCODING=UTF-8", ...)
}

#' @rdname write_shp
#' @export
write_shp_sp <- function(x, file, ...) {
  rgdal::writeOGR(x, file, "shape", driver = "ESRI Shapefile", layer_options = "ENCODING=UTF-8", ...)
}

#' gridSaveToPoly
#'
#' Write spatialPixelDataframe into polygon shpfile for the convenience of
#' checking in google earth.
#'
#' @param grid The spatialPixelDataframe or spatialGridDataframe object.
#' @param file The string of output shapefile name including path.
#' @export
gridSaveToPoly <- function(grid, file) {
  poly <- as(grid, "SpatialPolygonsDataFrame")
  # writePolyShape(poly, file) #, can't write prj info
  rgdal::writeOGR(poly, dsn = file, layer = basename(file), driver = "ESRI Shapefile")
}
