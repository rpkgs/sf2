#' make_grid
#' 
#' construct SpatialGridDataFrame
#' 
#' The input matrix suits for `image`, can be directly input to `make_grid`.
#' 
#' @param range A numeric vector, `[lon_min, lon_max, lat_min, lat_max]`
#' @param cellsize Numeric vector, grid cell size `[cellsize_lon, cellsize_lat]`. 
#' @param midgrid A vector, `[midgrid_lon, midgrid_lat]`. If midgrid = false, then
#' begin point and end point locate on grid lines; If true, then begin point and
#' end point in the middle of grid.
#' @param prj [sp::CRS-class()] Projection.
#' @param type Character, one of `mat` or `vec`.
#' - `mat`: If `image(x)` looks correct, x can be put into grid directly: 
#'      `grid@data <- data.frame(x = as.numeric(x))`
#' - `vec`: If `image(x)` looks correct, x needs to `flipud`: 
#'      `grid@data <- data.frame(x = as.numeric(Ipaper::flipud(x)))`
#' @param fix_lon360 boolean
#' 
#' @examples
#' range <- c(73, 105, 25, 40) # Tibetan Plateau
#' grid  <- make_grid(range, cellsize = 1/2, midgrid = TRUE)
#' @importFrom sp GridTopology SpatialPixelsDataFrame
#' @export
make_grid <- function(range = c(-180, 180, -90, 90), cellsize = 1, midgrid = c(TRUE, TRUE), prj = prj84, 
    type = "mat",
    fix_lon360 = FALSE) 
{
    if (length(cellsize) == 1) cellsize = rep(cellsize, 2)
    
    lon_range <- range[1:2]
    lat_range <- range[3:4]
    # lat_range <- c(25, 40); lon_range <- c(73, 105)
    
    if (length(midgrid) == 1){
        midgrid <- rep(midgrid, 2)
    }else if(length(midgrid) != 2){
        midgrid <- midgrid[1:2]
        message("error: midgrid length should be 1 or 2!")
    }
    
    offset  <- c(lon_range[1], lat_range[1]) + cellsize/2 * (midgrid)

    lon     <- seq(offset[1], lon_range[2], by = cellsize[1])    
    lat     <- seq(offset[2], lat_range[2], by = cellsize[2])
    dims    <- c(length(lon), length(lat))


    #SpatialPixelsDataFrame, other than GirdDataframe. They have a big difference!
    if (type == "mat") {
        grid <- get_grid.lonlat(lon, lat)    
    } else if (type == "vec") {
        grid <- GridTopology(cellcentre.offset = offset,
        cellsize = c(1, 1)*cellsize, cells.dim = dims)
        grid <- SpatialPixelsDataFrame(grid, data = data.frame(id = seq.int(1, prod(dims))),
                                       proj4string = prj)       
    }
    return(grid)
}

#' @export
get_grid <- make_grid

#' @rdname make_grid
#' @export
get_grid.lonlat <- function(lon, lat, fix_lon360 = FALSE){
    lon2  <- seq(min(lon), max(lon), length.out = length(lon))
    lat2  <- seq(min(lat), max(lat), length.out = length(lat))

    # points <- expand.grid(lon, lat)
    points <- expand.grid(lon2, lat2)

    grid <- SpatialPixelsDataFrame(points, data = data.frame(id = 1:nrow(points)), 
        proj4string = prj84)
    if (fix_lon360) grid %<>% fix_lon()

    grid
}
