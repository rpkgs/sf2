#' clip shapefile
#' 
#' @import sf
#' @export
clip_shp <- function(x, y) {
    st_intersection(st_as_sf(x), st_as_sf(y)) %>% as_Spatial()
}

#' clip raster by mask or points
#'
#' @param x Raster* object
#' @param y points represented by a two-column matrix or data.frame, or SpatialPoints*;
#' SpatialPolygons*; SpatialLines; Extent; or a numeric vector representing cell numbers
#'
#' @importFrom raster extract values
#' @export
clip_raster <- function(x, y, return.id = TRUE) {
    vals <- extract(x, y)
    ind <- NULL
    if (return.id) {
        class <- class(x)
        if (class == "RasterLayer") {
            temp <- x
        } else {
            # RasterBrick
            temp <- raster(x, layer = 1)
        }
        values(temp) <- 1:length(temp)
        ind <- extract(temp, y)
    }
    list(data = data.table(vals), index = ind) # return
}
