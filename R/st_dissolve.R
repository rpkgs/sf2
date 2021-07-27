#' st_dissolve
#' 
#' @export st_dissolve
st_dissolve <- function (x, by = NULL, ...) UseMethod("st_dissolve")

#' @rdname st_dissolve
#' @export
st_dissolve.sf = function(x, by = NULL, ...) {
    if (is.null(by) || !(by %in% colnames(shp))) by = colnames(x)[1]
    x %>% dplyr::group_by_at(by) %>% dplyr::summarise(...)
}

#' @rdname st_dissolve
#' @export
st_dissolve.character <- function(x, by = NULL, ...) {
    shp <- sf::read_sf(x)
    outfile <- gsub(".shp$", "_dissolved.shp", x)
    # # print(shp)
    shp2 = st_dissolve(shp, by)
    write_shp(shp2, outfile)
}
