#' make_grid for global basins
#' 
#' @inheritParams get_grid
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

#' @export
write_grid <- function(obj, file) {
    writeGDAL(obj$grid, file)
}

last_lgl <- function(x) {
    ind <- which(x)
    ind[length(ind)]
}

first_lgl <- function(x) {
    ind <- which(x)
    ind[1]
}

#' @export
clip_rect <- function(obj, range, cliped.obj = TRUE) {
    # just slightly greater than the range
    ilon <- seq(last_lgl(obj$lon < range[1]), first_lgl(obj$lon > range[2]))
    ilat <- seq(last_lgl(obj$lat > range[4]), first_lgl(obj$lat < range[3])) # lat in the reverse order
    # browser()
    # lons <- obj$lon[ilon]
    # lats <- obj$lat[ilat]
    # ilon2 <- which(obj$lon <= range[2] & obj$lon >= range[1])
    # ilat2 <- which(obj$lat <= range[4] & obj$lat >= range[3])
    # browser()
    mat_IDS <- obj$mat_IDS[ilon, ilat]
    ids <- as.numeric(mat_IDS)
    cellsize <- obj$cellsize
    if (cliped.obj) {
        # reconstruct SpatialPixelsDataFrame due to writeGDAL error
        # gridTopology <- GridTopology(c(range[1], range[3]) + cellsize/2, c(1,1)*cellsize, c(length(ilon), length(ilat)))
        # data = obj$grid@data[ids,,drop=FALSE]
        # grid2 <- SpatialPixelsDataFrame(gridTopology, data, proj4string = CRS(proj4string(obj$grid)))
        # browser()
        grid2 <- obj$grid[ids, ]
        coors <- coordinates(grid2)
        # s <- SpatialPoints(coors)
        # gridded(s) <- TRUE
        grid <- SpatialPixelsDataFrame(coors, grid2@data, proj4string = prj84)
        # as(grid2, Spatial)
        # center.offset <- coordinates(grid2)[1, ]
        # cellsize <- grid2@grid@cellsize
        # pnts <- as(grid2, SpatialPoints)

        ## need to update
        obj_new <- listk(range, cellsize, lon = obj$lon[ilon], lat = obj$lat[ilat],
            ids, mat_IDS, grid = grid)
        obj_new
    } else {
        listk(ids)
    }
}
