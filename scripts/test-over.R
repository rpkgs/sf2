source("test/main_pkgs.R")

# grid@data <- data.frame(x = as.numeric(mat[,,1]))

{
    FUN <- floor
    cellsize_s <- 0.05
    x <- info[basin == "102101A", .(lon, lat)] %>% get_pos()
    x_sp <- pos2sp(x)

    # x@grid@cellcentre.offset %<>% subtract(0.05/2)
    Id <- over(x_sp, grid)
    y <- cbind(x, Id)

    cliped <- grid[Id$id, ]@coords %>% pos2sp()
    cliped2 <- grid[x$I, ]@coords %>% pos2sp()

    plot(x_sp, axes = TRUE, xlim = c(142, 144)); grid()
    plot(cliped, axes = TRUE, add = TRUE)
    plot(cliped2, axes = TRUE, add = TRUE, col = "red")

    y[id != I]
}


load("debug.rda")
r_fract <- overlap_fraction(basin, 0.1) %>% as_rast()
