# source("test/main_pkgs.R")

# row of lat, col of lon
get_pos_i <- function(x, offset = 0, Iratio = FALSE, cellsize_s = 0.05, cellsize_l = 0.5) {
    # fix a pos
    delta <- -cellsize_s/2
    ratio <- (x + offset + delta)/cellsize_l + 1 #
    if (!Iratio) {
        floor(ratio)
    } else {
        ratio
    }
}

# centre in the left margin
get_pos <- function(d, cellsize_s = 0.05){
    d[, .(lat, lon)] %>% 
        add(cellsize_s/2) %>%
        mutate(I_lon = get_pos_i(lon, 180),
               I_lat = get_pos_i(lat, 90),
               # r_lon = get_pos_i(lon, 180, TRUE),
               # r_lat = get_pos_i(lat, 90, TRUE),
               I = I_lon + (I_lat-1)*720)
}

pos2sp <- function(x) {
    if (!is.data.frame(x))
        x %<>% set_colnames(c("lon", "lat")) %>% as.data.table()

    coordinates(x) <- ~lon+lat
    gridded(x) <- TRUE
    proj4string(x) <- sp2::prj84
    x
}
