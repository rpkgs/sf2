#' get range of `sf` object
#'
#' @param x `sf` object
#' @param cellsize double
#' @export
st_range <- function(x, cellsize = 0.1) {
    bbox <- x %>% st_bbox()
    range <- (bbox / cellsize) %>%
        {
            c(floor(.[1]), ceiling(.[3]), floor(.[2]), ceiling(.[4]))
        } * cellsize
    range
}
