# `mat2vec`和`vec2mat`共用相同的转换下标, 可以逆向转化。
# dim: nlon, nlat
sp_array <- function(grid_base) {
  dim <- grid_base@grid@cells.dim
  arr <- array(grid_base@data %>% as.matrix(), dim = c(dim, ncol(grid_base)))
  arr
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
  perm <- seq_along(dim(vals))
  perm[2] <- 1
  perm[1] <- 2
  aperm(vals, perm) |> c()
}
