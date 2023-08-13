#' extractId
#'
#' @param sp A SpatialPointDataFrame with the station coordinates information
#' @param shpfile A character, shape file path.
#'
#' @importFrom maptools readShapePoly
#' @importFrom sp SpatialPolygons over
#' @export
extractId <- function(sp, shpfile) {
  # formula <- ~lon+lat
  # sp    <- df2sp(station, formula, prj84)
  prj84 <- get_crs()
  shp <- read_shp(shpfile, proj4string = prj84)
  bound <- SpatialPolygons(shp@polygons, proj4string = prj84)
  ## clipped station
  clipId <- which(!is.na(over(sp, bound))) %>% as.numeric()

  plot(shp, axes = T)
  plot(sp[clipId, ], add = T)
  clipId # return clipId
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

# #' clip raster by mask or points
# #'
# #' @param x Raster* object
# #' @param y points represented by a two-column matrix or data.frame, or SpatialPoints*;
# #' SpatialPolygons*; SpatialLines; Extent; or a numeric vector representing cell numbers
# #'
# #' @importFrom raster extract values
# #' @export
# clip_raster <- function(x, y, return.id = TRUE) {
#     vals <- extract(x, y)
#     ind <- NULL
#     if (return.id) {
#         class <- class(x)
#         if (class == "RasterLayer") {
#             temp <- x
#         } else {
#             # RasterBrick
#             temp <- raster(x, layer = 1)
#         }
#         values(temp) <- 1:length(temp)
#         ind <- extract(temp, y)
#     }
#     list(data = data.table(vals), index = ind) # return
# }
