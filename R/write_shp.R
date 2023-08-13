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
#' @param ... other parameters to [sf::write_sf()]
#' 
#' @export
write_shp <- function(x, file, ...) {
  write_sf(sf::st_as_sf(x), file, layer_options = "ENCODING=UTF-8", ...)
}
