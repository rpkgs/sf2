# `mat2vec`和`vec2mat`共用相同的转换下标, 可以逆向转化。
# dim: nlon, nlat
sp_array <- function(grid_base) {
  dim <- grid_base@grid@cells.dim
  arr <- array(grid_base@data %>% as.matrix(), dim = c(dim, ncol(grid_base)))
  arr
}
