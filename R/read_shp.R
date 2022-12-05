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

#' @importFrom sf as_Spatial st_crs
#' @export
read_polyline <- function(file, lwd = 0.5, ...) {
    if (file.exists(file)) {
        shp <- read_sf(file) %>% as_Spatial()
        st_crs(shp) = 4326
        # suppressWarnings(shp <- readShapeLines(file))
        list("sp.lines", shp, first = FALSE, lwd = lwd, ...) # china poly
    }
}
