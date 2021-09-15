#' overlap_fraction
#'
#' @importFrom data.table data.table := is.data.table
#' @importFrom terra rast
#' @export
overlap_fraction <- function(
    x, cellsize = 0.1,
    range = NULL, show = TRUE, outfile = NULL)
{
    if (is.null(range)) range <- st_range(x, cellsize)
    grid <- make_grid(range, cellsize)
    r_mask <- as_rast(grid)

    wkb <- sf::st_as_binary(sf::st_geometry(st_as_sf(x)), EWKB = TRUE)
    #BUG: Error in x$.self$finalize() : attempt to apply non-function
    r <- sf.extract::rast_overlap(r_mask, wkb[[1]])

    info <- data.table(values = grid$id) %>% merge(r, all.x = TRUE)
    info[is.na(coverage_fraction), coverage_fraction := 0]
    grid@data <- data.table(info$coverage_fraction)

    if (show) {
        poly_grid <- as_SpatialPolygonsDataFrame(grid)
        sp::plot(grid)
        sp::plot(x[,1], add = TRUE, col = "transparent")
        sp::plot(poly_grid, add = TRUE, lwd = 0.2)
    }
    if (!is.null(outfile)) rgdal::writeGDAL(grid, outfile)
    grid
}
