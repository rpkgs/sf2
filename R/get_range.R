
#' get bbox from shapefile
#' @export
get_range_shp <- function(shp, cellsize) {
    bbox <- shp@bbox
    if (missing(cellsize)) return(bbox[c(1,3,2,4)])
    range = c(floor(bbox[, 1] / cellsize) * cellsize, ceiling(bbox[, 2] / cellsize) * cellsize)[c(1, 3, 2, 4)]
    return(range)
}
