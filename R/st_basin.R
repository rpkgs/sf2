# 水文学--流域处理

#' guess_network
#'
#' @param info: the first two columns should be `[i, contains]`
#' @references https://stackoverflow.com/questions/25130462/get-disjoint-sets-from-a-list-in-r
#' 
#' @examples 
#' \dontrun{
#' info = shp_contains(shp)
#' l_net = guess_networ(info)
#' }
#' @export 
guess_network <- function(info) {
  g <- igraph::graph.data.frame(info, directed = FALSE)
  split(igraph::V(g)$name, igraph::clusters(g)$membership) %>%
    lapply(as.numeric)
}

#' @importFrom sf st_area
#' @export
st_area2 <- function(x) as.numeric(st_area(x))

#' @importFrom sf st_contains
#' @export 
shp_contains <- function(shp, .parallel = FALSE) {
  `%dof%` <- ifelse(.parallel, foreach::`%dopar%`, foreach::`%do%`)
  
  S_area <- st_area2(shp)
  n <- nrow(shp)
  index <- 1:n
  
  info_contain <- foreach(i = index, icount()) %dof%
    {
      runningId(i, 5)
      x <- shp[i, ]
      y <- shp[-i, ]
      suppressMessages({
        inds <- st_contains(x, y)[[1]]
        if (length(inds) > 0) {
          inds <- index[-i][inds]
          frac <- S_area[inds] / S_area[i]
          data.table(i = i, contains = inds, frac) # [frac <= 0.5, index]
        } else {
          NULL
        }
      })
    } %>% do.call(rbind, .)
}
