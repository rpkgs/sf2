library(ncdf4)
library(Ipaper)
library(magrittr)
library(data.table)
library(foreach)
library(iterators)
library(purrr)
library(sp)
library(matrixStats)
library(lubridate)
library(raster)
library(plyr)
source("test/main_pkgs.R")

mutate <- plyr::mutate
dates <- seq(ymd("1979-01-01"), ymd("2012-12-01"), "month")

add_date_df <- function(d){
    d2 <- melt(d, c("source", "region") %>% intersect(colnames(d)), variable.name = "date")
    d2$date %<>% factor(labels = dates) %>% as.character() %>% as.Date()
    d2
}

range <- c(-180, 180, -90, 90) # Tibetan Plateau
grid  <- get_grid(range, cellsize = 1/2, midgrid = TRUE)

grid$area <- area(raster(grid))@data@values
## fix pos in the left margin of grid box

# indir <- system.file("inst/examples/", package = "sp2")
dir_shp <- "inst/examples/shp"
files_shp <- dir(dir_shp,full.names = TRUE) %>%
    set_names(., basename(.)) %>%
    map(. %>% {
        ans <- dir(., "*.csv", recursive = TRUE, full.names = TRUE)
        set_names(ans, basename(dirname(ans)))
    })

files_nc <- dir("inst/examples/nc", "*.nc", full.names = TRUE) %>%
    set_names(., basename(.))

df_shp <- foreach(lst_files = files_shp, icount()) %do% {
    foreach(file = lst_files, icount()) %do% {
        fread(file)
    }
} %>% melt_tree(c("region", "basin"))
gridInfo <- cbind(df_shp[, .(region, basin, weight)], get_pos(df_shp)[, .(I)])
gridInfo %<>% mutate(w = grid$area[.$I] * weight)

gridInfo_lst <- split(gridInfo, gridInfo[, (paste0(region, "-", basin))])
df_grp <- names(gridInfo_lst) %>% {strsplit(., "-")} %>%
    {data.table(region = map_chr(., 1), basin = map_chr(., 2))} %>%
    merge(gridInfo[, .(w = sum(w)), .(region, basin)])

lst <- foreach(file_nc = files_nc, i = icount()) %do% {
    runningId(i)
    fid <- nc_open(file_nc)
    varname <- names(fid$var)[1]
    mat <- ncvar_get(fid, varname)
    dim(mat) <- c(720*360, 408)
    # d <- as.data.table(mat)

    r <- foreach(info = gridInfo_lst, icount()) %do% {
        colWeightedMeans(mat[info$I, , drop = FALSE], info$weight, na.rm = TRUE)
    }

    d <- do.call(rbind, r) %>% as.data.table() %>% cbind(df_grp, .)
    # dim <- map_int(fid$dim, ~length(.x$vals))
}

df <- melt_list(lst, "source") %>% reorder_name("source")
d_all <- ddply(df, .(source, region), function(d){
    mat <- d[, -(1:4)] %>% as.matrix()
    colWeightedMeans(mat, d$w, na.rm = TRUE)
}, .progress = "text")

d_1 <- ddply(df, .(source), function(d){
    mat <- d[, -(1:4)] %>% as.matrix()
    colWeightedMeans(mat, d$w, na.rm = TRUE)
}, .progress = "text")

pdat <- add_date_df(d_1)
ggplot(pdat, aes(date, value)) + geom_line() +
    facet_wrap(~source)

l2 <- listk('basin' = df[, -4], region = d_all, "global-mean" = d_1) %>%
    map(. %>% {
        vars_com <- intersect(colnames(.), c("source", "region", "basin"))
        set_names(., c(vars_com, format(dates, "%Y%m")))
    })
write_list2xlsx(l2[-1], "groundwater.xlsx")
fwrite(df, "groudwater_global_basins.csv")

write_list2xlsx(lst, "gd_global_basins_P_PET.xlsx")

foreach(x = lst, name = names(lst)) %do% {
    outfile <- sprintf("%s.csv", gsub(".nc", "", name))
    fwrite(x, outfile)
}

fids <- foreach(file = files_nc) %do% {
    nc_open(file)
}
