#' @export 
raster2poly <- function(r, I_grid = NULL){
    if (is.null(I_grid)) I_grid <- 1:nrow(r)
    as(r, "SpatialPolygonsDataFrame")
}

#' @export 
raster2SpatialGrid <- function(r, I_grid = NULL){
    if (is.null(I_grid)) I_grid <- 1:nrow(r)
    as(r, "SpatialGridDataFrame")
}
