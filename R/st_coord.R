#' @importFrom sf st_drop_geometry st_coordinates
#' @export 
as.data.table.sf <- function(x) {
    loc = st_coord(x)
    x %>% st_drop_geometry() %>% cbind(loc, .)
}

#' @inheritParams sf::st_coordinates
#' @export 
st_coord <- function(x) {
    st_coordinates(x) %>%
        set_colnames(c("lon", "lat")) %>%
        as.data.table()
}
