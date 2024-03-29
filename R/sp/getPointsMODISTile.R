#' Get points data from MODIS tiles
#'
#' @param coor 2-column matrix of coordinates (longitude, latitude)
#' @param cellsize 500m products, cellsize is regarded as 1/240; 1km product,
#' cellsize is regarded as 1/120.
#'
#' @return
#' * `h` MODIS tile horizontal id
#' * `v` MODIS tile vertical id
#' * `i` point horizontal position at the tile
#' * `j` point vertical position at the tile
#'
#' @references
#' 1. https://landweb.modaps.eosdis.nasa.gov/cgi-bin/developer/tilemap.cgi
#' 2. https://modis-land.gsfc.nasa.gov/MODLAND_grid.html
#'
#' @examples
#' coor <- cbind(101:120, 21:40)
#' d <- getPointsMODISTile(coor)
#' @export
#' @importFrom data.table data.table
getPointsMODISTile <- function(coor, cellsize = 1 / 240) {
  scale <- 463.312716528 * cellsize * 240 # meter; 1km: 926.625433055833
  nline <- 1 / cellsize * 10 # how many lines per tile?

  proj_sin <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

  # coor: coordinate matrix, (longitude, latitude)
  sin_xy <- rgdal::project(coor, proj_sin)
  sin_ij <- sin_xy / scale

  h <- floor(sin_ij[, 1] / nline) + 18
  v <- 8 - floor(sin_ij[, 2] / nline)
  # calculate the tile and its location
  px <- cbind(
    ceiling(sin_ij[, 1] %% nline), # horizontal
    ceiling(nline - sin_ij[, 2] %% nline)
  ) %>%
    set_colnames(sprintf("s%d_%s", ceiling(scale / 1e2) * 1e2, c("i", "j"))) # vertical

  data.table(coor, h, v, px) # return
}
