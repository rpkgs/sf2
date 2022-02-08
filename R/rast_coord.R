#' rast_coord
#' 
#' @importFrom raster raster values
#' @importFrom dplyr mutate
#' @export
rast_coord <- function(r, .area = TRUE) {
    if (is.character(r)) r %<>% raster::raster()
    r %<>% raster()
    pos <- raster::coordinates(r) %>%
        cbind(values(raster::area(r)), values(r)) %>%
        set_colnames(c("lon", "lat", "area", "value")) %>%
        data.table()
    
    r_temp <- r
    values(r_temp) <- 1:prod(dim(r)[1:2])
    I_grid <- rast_array(r_temp) %>% as.numeric()

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

reorder_name <- function (d, headvars = c("site", "date", "year", "doy", "d8", 
    "d16"), tailvars = "") 
{
    names <- names(d)
    headvars %<>% intersect(names)
    tailvars %<>% intersect(names)
    varnames <- c(headvars, setdiff(names, union(headvars, tailvars)), 
        tailvars)
    if (is.data.table(d)) {
        d[, varnames, with = F]
    }
    else if (is.data.frame(d)) {
        d[, varnames]
    }
    else if (is.list(d)) {
        d[varnames]
    }
    else {
        stop("Unknown data type!")
    }
}
