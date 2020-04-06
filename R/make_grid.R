#' make_grid for global basins
#' 
#' @inheritParams get_grid
#' 
#' @examples 
#' \dontrun{
#' r <- make_grid(range=c(-180, 180, -60, 90), cellsize=1)
#' }
#' @export
make_grid <- function(range = c(-180, 180, -60, 90), cellsize, midgrid = c(TRUE, TRUE)) {
    if (length(cellsize) == 1) cellsize = rep(cellsize, 2)
    if (length(midgrid) == 1) midgrid <- rep(midgrid, 2)

    lon_range <- range[1:2]
    lat_range <- range[3:4]

    offset <- cellsize/2 * (midgrid)
    lon <- seq(lon_range[1]+offset[1], lon_range[2], by =  cellsize[1])
    lat <- seq(lat_range[2]-offset[2], lat_range[1], by = -cellsize[2])

    grid <- get_grid(range, cellsize, midgrid, type = "vec")

    dim <- grid@grid@cells.dim
    nlon <- dim[1]
    nlat <- dim[2]
    mat_IDS <- matrix(1:(nlon * nlat), nlon, nlat)

    obj <- listk(range, cellsize, midgrid, lon, lat, mat_IDS, grid)
    obj
}
