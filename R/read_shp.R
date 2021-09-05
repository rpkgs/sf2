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

#' @importFrom maptools readShapeLines
#' @export
read_polyline <- function(file, lwd = 0.5, ...) {
    if (file.exists(file)) {
        suppressWarnings(shp <- readShapeLines(file))
        proj4string(shp) <- prj84
        list("sp.lines", shp, first = FALSE, lwd = lwd, ...) # china poly
    }
}
