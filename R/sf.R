#' @export
st_dissolve <- function(x, by = NULL, ...) {
    x %>%
        dplyr::group_by_at(by) %>%
        dplyr::summarise(...)
}
