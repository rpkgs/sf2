#' write and read ESRI Shapefile
#'
#' @param x Spatial* Object from sp package
#' @param file The path of ESRI shapefile
#' @param ... other parameters to [rgdal::writeOGR()]
#' 
#' @export
write_shp <- function(x, file, ...) {
    write_sf(sf::st_as_sf(x), file, layer_options = "ENCODING=UTF-8", ...)
}

#' @importFrom rgdal writeOGR readOGR
#' @rdname write_shp
#' @export
write_shp_sp <- function(x, file, ...) {
    writeOGR(x, file, "shape", driver = "ESRI Shapefile", layer_options="ENCODING=UTF-8", ...)
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
