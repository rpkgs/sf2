#' write and read ESRI Shapefile
#'
#' @param x Spatial* Object from sp package
#' @param file The path of ESRI shapefile
#' @param ... other parameters to [rgdal::writeOGR()]
#' 
#' @importFrom rgdal writeOGR readOGR
#' @export
write_shp <- function(x, file, ...) {
    writeOGR(x, file, "shape", driver = "ESRI Shapefile", ...)
}

#' read shapefile
#' @param file The path of ESRI shapefile
#' @param ... other parameters to [rgdal::readOGR()]
#' 
#' @export
#' 
#' @seealso [rgdal::readOGR()]
read_shp <- function(file, ...) {
    readOGR(file, verbose = FALSE, ...)
}
