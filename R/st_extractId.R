#' @export
df2sf <- function(d, coords = c("lon", "lat"), crs = 4326, ...) {
  st_as_sf(d, coords = coords, crs = crs, ...)
}

df2sp = df2sf

#' st_extractId
#'
#' @param x A sf point, or file path
#' @param y A sf polygon, or file path
#' @param plot Boolean. Whether to visualize extracted points?
#' @param ... others passed to [sf::st_within()]
#'
#' @importFrom sf st_within st_geometry read_sf
#' @export
st_extractId <- function(x, y, plot = TRUE, ...) {
  if (is.character(x)) x %<>% read_sf()
  if (is.character(y)) y %<>% read_sf()

  l <- st_within(x, y, ...)
  inds <- which.notempty(l)

  if (plot) {
    plot(st_geometry(y))
    plot(st_geometry(x), add = TRUE)
  }
}


# #' get_nearGrids
# #'
# #' find nearest 3*3 grids of points
# #'
# #' with the help of RANN package to quickly select nearest 3*3 grids
# #' use line dist to replace sphere distance
# #'
# #' @param grid SpatialGridDataFrame object
# #' @param ngrid how many nearest grids to be extracted?
# #' @return nearId nearest order
# #'
# #' @importFrom RANN nn2
# #' @export
# get_nearGrids <- function(grid, station, ngrid = 3*3){
#   loc_grid    <- coordinates(grid)
#   loc_station <- coordinates(station)
#   x   <- nn2(loc_grid, loc_station, ngrid)
#   res <- data.frame(stationId = rep(station@data$stationId, ngrid),
#                     nearId = rep(1:ngrid, rep(nrow(station), ngrid)),
#                     idx = as.numeric(x$nn.idx),
#                     dists = as.numeric(x$nn.dists) *6371 * pi/180)
#   # rdist.earth(loc_grid[res$idx, ], loc_station)
#   return(res)
# }
