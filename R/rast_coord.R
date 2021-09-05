#' rast_coord
#' 
#' @export
rast_coord <- function(r, .area = TRUE) {
    if (is.character(r)) r %<>% raster::raster()
    pos <- raster::coordinates(r) %>%
        cbind(values(raster::area(r)), values(r)) %>%
        set_colnames(c("lon", "lat", "area", "value")) %>%
        data.table()
    
    r_temp <- r
    values(r_temp) <- 1:prod(dim(r)[1:2])
    I_grid <- raster_array(r_temp) %>% as.numeric()

    nrow <- ncol(r) # because r is transposed and flipud
    pos[I_grid, ] %>%
        mutate(
            I = 1:nrow(.),
            col = ceiling(I / nrow),
            row = I - (col - 1) * nrow
        ) %>%
        reorder_name("I", tailvars = c("row", "col"))
    # values(r_temp) = pos$lat
    # lat <- raster_array(r_temp) %>% as.numeric()
    # data.table(I = seq_along(lon), lon, lat)
}

#' @export
rast_array <- function (r) {
    if (is.character(r)) r %<>% raster::raster()
    aperm(as.array(r), c(2, 1, 3)) %>% flipud()
}
