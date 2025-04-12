# terra::crds(r, ...)
# #' @export
# rast2poly <- function(r, I_grid = NULL){
#     if (is.null(I_grid)) I_grid <- 1:nrow(r)
#     as(r, "SpatialPolygonsDataFrame")
# }

# #' @export
# rast2poly <- function(r, crs = 4326) {
#   sf_poly <- suppressWarnings({
#     sf::st_as_sf(stars::st_as_stars(r),
#       as_points = FALSE, merge = TRUE
#     ) %>% sf::st_make_valid() # %>% sf::as_Spatial()
#   })
#   sf::st_crs(sf_poly) <- crs
#   sf_poly
# }


#' @export
rast2SpatialGrid <- function(r, I_grid = NULL) {
  if (is.null(I_grid)) I_grid <- 1:nrow(r)
  as(r, "SpatialGridDataFrame")
}

#' @export
as_rast <- function(x) rast(x)


#' @importFrom terra as.array
#' @export
rast_array <- function(r) {
  if (is.character(r)) r %<>% rast()
  aperm(as.array(r), c(2, 1, 3)) %>% flipud()
}

#' rast_coord
#' 
#' @return
#' - `I`   : the order of `base`, `[lon, lat]`, `image` works directly on it.
#' - `cell`: the order of `gdal`, `[lon, rev(lat)]`, returned by `values(r)`,
#' 
#' @importFrom dplyr mutate rename relocate
#' @export
rast_df <- function(r, .area = TRUE, na.rm = FALSE) {
  if (is.character(r)) r %<>% terra::rast()
  if (.area && !("area" %in% names(r))) r$area <- cellSize(r, unit = "km")
  
  r$I = make_rast(rast_range(r), res(r))

  nrow <- ncol(r) # because r is transposed and flipud
  as.data.table(r, xy = TRUE, na.rm = na.rm) %>%
    mutate(
      cell = 1:nrow(.),
      col = ceiling(I / nrow), 
      row = I - (col - 1) * nrow, .before = "x"
    ) %>% dplyr::relocate(I, cell, row, col) %>%
    dplyr::rename(lon = x, lat = y)
}

rast_coord = rast_df


#' @importFrom terra ext res
#' @export
rast_dim <- function(f) {
  r <- rast(f)
  range <- as.vector(ext(r))
  res <- res(r)

  lon <- seq(range[1] + res[1] / 2, range[2], res[1])
  lat <- seq(range[3] + res[2] / 2, range[4], res[2])

  listk(lon, lat)
}

#' @export
value2colormap <- function(x, brks, cols) {
  col2dt <- function(cols) col2rgb(cols) %>% t() %>% as.data.table()
  colormap = col2dt(cols)
  ind   = findInterval(x, brks) # %>% summary()
  colormap[ind, ] |> as.matrix()
}

#' @export
rast2colormap <- function(r, brks, cols, fout = "out.tif") {
  x = values(r)
  r2 = c(r, r, r)
  val = value2colormap(x, brks, colors)
  values(r2) = val
  writeRaster(r2, fout)
}
