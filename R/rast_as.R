#' @export 
rast2poly <- function(r, I_grid = NULL){
    if (is.null(I_grid)) I_grid <- 1:nrow(r)
    as(r, "SpatialPolygonsDataFrame")
}

#' @export 
rast2SpatialGrid <- function(r, I_grid = NULL){
    if (is.null(I_grid)) I_grid <- 1:nrow(r)
    as(r, "SpatialGridDataFrame")
}

#' @export
as_rast <- function(x) raster::raster(x) %>% rast()
