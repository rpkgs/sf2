#' make_rast
#'
#' @param range A numeric vector, `[lon_min, lon_max, lat_min, lat_max]`
#' @param cellsize Numeric vector, grid cell size `[cellsize_lon, cellsize_lat]`.
#' @param nlyrs positive integer. Number of layers
#' @param ... other parameters to [terra::rast()], e.g., names, vals.
#'
#' @seealso [terra::rast()]
#' @importFrom terra ext rast res resample
#' @export
make_rast <- function(range = c(-180, 180, -90, 90), cellsize = 1, nlyrs = 1, vals = NULL, ...) {
  if (length(cellsize) == 1) {
    cellsize <- rep(cellsize, 2)
  }
  e <- ext(range[1], range[2], range[3], range[4])

  if (is.null(vals)) {
    r = rast(e, res = cellsize, nlyrs = nlyrs, ...)
    values(r) = rast_cellId(r)
    r
  } else {
    rast(e, res = cellsize, nlyrs = nlyrs, vals = vals, ...)
  }
}


#' @export
rast_cellId <- function(r) {
  dim = dim(r)[1:2]
  id = matrix(1:prod(dim), dim, byrow = TRUE) %>% fliplr()
  mat2vec(id)
}


#' @export
check_rast_vals <- function(vals) UseMethod("check_rast_vals", vals)

#' @export
check_rast_vals.matrix <- function(vals) {
  image(flipud(t(vals)))
}

#' @export
check_rast_vals.array <- function(vals) {
  image(flipud(t(vals[, , 1])))
}

#' @export
index_mat2vec <- function(dim) {
  dim <- dim[1:2]
  ind <- array(1:prod(dim), dim = dim)
  flipud(ind) %>% as.numeric()
  # gdal: [lon, rev(lat)]
  # base: [lon, lat]
}

#' @export
mat2vec <- function(vals) UseMethod("mat2vec", vals)

#' @export
mat2vec.array <- function(vals) {
  perm = seq_along(dim(vals))
  perm[2] = 1
  perm[1] = 2
  aperm(vals, perm) |> c()
}

#' @export
`array<-` <- function(r, values) {
  rast(r, vals = values)
}
