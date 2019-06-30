#' get_grid
#' construct SpatialGridDataFrame
#'
#' @param range A numeric vector, `[lat_min, lat_max, lon_min, lon_max]`
#' @param cellsize Numeric vector, grid cell size `[cellsize_lat, cellsize_lon]`. 
#' @param midgrid A vector, `[midgrid_long, midgrid_lat]`. If midgrid = false, then
#' begin point and end point locate on grid lines; If true, then begin point and
#' end point in the middle of grid.
#' @param prj [sp::CRS-class()] Projection.
#' 
#' @examples
#' range <- c(25, 40, 73, 105) # Tibetan Plateau
#' grid  <- get_grid(range, cellsize = 1/12, midgrid = TRUE)
#' @importFrom sp GridTopology SpatialPixelsDataFrame
#' @export
get_grid <- function(range, cellsize, midgrid = c(TRUE, TRUE), prj = prj84) {
    if (length(cellsize) == 1) cellsize = rep(cellsize, 2)
        
    lat_range  <- range[1:2]
    long_range <- range[3:4]
    # lat_range <- c(25, 40); long_range <- c(73, 105)
    
    if (length(midgrid) == 1){
        midgrid <- rep(midgrid, 2)
    }else if(length(midgrid) != 2){
        midgrid <- midgrid[1:2]
        message("error: midgrid length should be 1 or 2!")
    }
    
    offset  <- c(long_range[1], lat_range[1]) + cellsize/2 * (midgrid)
    
    lat     <- seq(offset[2], lat_range[2], by = cellsize[1])
    lon     <- seq(offset[1], long_range[2], by = cellsize[2])
    dims    <- c(length(lon), length(lat))

    grid <- GridTopology(cellcentre.offset = offset,
        cellsize = c(1, 1) * cellsize, cells.dim = dims)
    #SpatialPixelsDataFrame, other than GirdDataframe. They have a big difference!
    grid <- SpatialPixelsDataFrame(grid, data = data.frame(id = seq.int(1, prod(dims))),
                                   proj4string = prj)
    return(grid)
}
