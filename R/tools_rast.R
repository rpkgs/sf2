is.terra <- function(r) inherits(r, "SpatRaster")
is.raster <- function(r) inherits(r, "BasicRaster")
.isRaster <- function(r) is.terra(r) | is.raster(r)

# ' check_rast
# ' @param type one of `c("terra", "raster")`
check_rast <- function(r, type = "raster") {
    fun = switch(type, raster = raster::raster, terra = terra::rast)
    if ( is.character(r) ||
         (type == "raster" && !is.raster(r)) ||
         (type == "terra"  && !is.terra(r)) ) {
        r %<>% fun()
    }
    r
}

.values <- function(r) {
    if (is.terra(r)) {
        terra::values(r)[, 1]
    } else if (is.raster(r)) {
        raster::values(r)
    }
}

.area <- function(r, ...) {
    if (is.terra(r)) {
        terra::cellSize(r, unit = "km", ...)
    } else if (is.raster(r)) {
        raster::area(r, ...) # km2
    }
}

.coords <- function(r, ...) {
    if (is.terra(r)) {
        terra::crds(r, ...)
    } else if (is.raster(r)) {
        raster::coordinates(r, ...) # km2
    }
}

#' @importFrom terra as.array
#' @export
rast_array <- function(r) {
    if (is.character(r)) r %<>% check_rast()
    aperm(as.array(r), c(2, 1, 3)) %>% flipud()
}

#' @importFrom raster raster area as.array values values<-
#' @export
rast_df <- function(r){
    r %<>% check_rast()
    pos <- .coords(r) %>%
        cbind(.values(.area(r)),  .values(r)) %>%
        set_colnames(c("lon", "lat", "area", "value")) %>% data.table()

    r_temp <- r
    values(r_temp) <- 1:prod(dim(r)[1:2])
    I_grid <- rast_array(r_temp) %>% as.numeric()
    nrow <- ncol(r)
    pos[I_grid, ] %>% 
        mutate(I = 1:nrow(.), col = ceiling(I/nrow), row = I - (col - 1) * nrow) %>% 
        reorder_name("I", tailvars = c("row", "col"))
}

#' @export
rast_dim <- function (file) {
    # subfix = stringr::str_extract(basename(file), "(?<=\\.).{2,4}$")
    # if (subfix == "nc") {
    #     ncdim_get(file)
    # }
    suppressWarnings(x <- rgdal::GDALinfo(file))
    lon = x %>% {
        seq(.["ll.x"] + .["res.x"]/2, by = .["res.x"],
            length.out = .["columns"])
    }
    lat = x %>% {
        seq(.["ll.y"] + .["res.y"]/2, by = .["res.y"],
            length.out = .["rows"])
    }
    listk(lon, lat)
}
