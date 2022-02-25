# `mat2vec`和`vec2mat`共用相同的转换下标, 可以逆向转化。
# dim: nlon, nlat
index_mat2vec.default <- function(dim) {
  dim = dim[1:2]
  ind = array(1:prod(dim), dim = dim)
  flipud(ind) %>% as.numeric()  
}

sp_array <- function(grid_base){
    dim = grid_base@grid@cells.dim
    arr = array(grid_base@data %>% as.matrix(), dim = c(dim, ncol(grid_base)))
    arr
}
