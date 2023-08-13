# terra::crds(r, ...)

#' @importFrom terra as.array
#' @export
rast_array <- function(r) {
  if (is.character(r)) r %<>% rast()
  aperm(as.array(r), c(2, 1, 3)) %>% flipud()
}

#' rast_coord
#' 
#' @details 
#' - `I`: the order of `values(r)`
#' - `cell`: the order of `values(r)`
#' 
#' @importFrom dplyr mutate rename relocate
#' @export
rast_df <- function(r, .area = TRUE, na.rm = FALSE) {
  if (is.character(r)) r %<>% terra::rast()
  if (.area && !("area" %in% names(r))) r$area <- cellSize(r, unit = "km")
  
  r$cell = make_rast(rast_range(r), res(r))

  nrow <- ncol(r) # because r is transposed and flipud
  as.data.table(r, xy = TRUE, na.rm = na.rm) %>%
    mutate(
      I = 1:nrow(.),
      col = ceiling(cell / nrow), 
      row = cell - (col - 1) * nrow, .before = "x"
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
