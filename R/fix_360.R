#' bbox[1]
#' @export 
brick.range <- function(array,
                       range = c(-180, 180, -90, 90),
                       crs = "", transpose = FALSE) {
    brick(array,
        xmn = range[1], xmx = range[2],
        ymn = range[3], ymx = range[4],
        crs = crs, transpose = transpose
    )
}

#' @export
nc2brick <- function(file, varname = 1L) {
    fid <- nc_open(file)
    lon <- ncvar_get(fid, "lon")
    lat <- ncvar_get(fid, "lat")

    cellsize_x <- diff(lon) %>% abs() %>% median()
    cellsize_x <- diff(lat) %>% abs() %>% median()

    data <- ncvar_get(nc, varname)

    if (max(lon) > 180) {
        # fix at here
        I_lon <- c(which(lon >= 180), which(lon <= 180))
        data <- aperm(data[I_lon, length(lat):1, ], c(2, 1, 3)) # flipud

        lon <- lon[I_lon]
        lon[lon >= 180] %<>% subtract(360)
    }
}

fix_360.array <- function(array) {
    nlon <- dim(array)[1]
    lon <- seq(0, 360, length.out = nlon)
    info <- fix_360.lon(lon)
    array[info$I_lon, , ]
    # list(array[info$I_lon, , ]
}

fix_360.lon <- function(lon) {
    I_lon <- c(which(lon >= 180), which(lon <= 180) %>% setdiff(1))
    lon <- lon[I_lon]
    lon[lon >= 180] %<>% subtract(360)
    listk(lon, I_lon)
    # lon
}

# Extract_nc <- function(Filename, Shp, Var = 'tem') {
#   nc <- nc_open(Filename)
#   if(max(lon) <= 180) {
#     st_extract(Filename, Shp)[[1]][,-1]
#   } else {
#     Data <- ncvar_get(nc, Var)
#     # DataM <- apply(Data, c(1,2), sum)
#     # fields::image.plot(DataM) # Should 0-360, and upward
#     Ord <- c(which(lon >= 180), which(lon < 180))
#     Data <- aperm(Data[Ord,length(lat):1,], c(2,1,3))
    
#     lon <- lon[Ord]; lon[lon>= 180] <- lon[lon>= 180] - 360
#     Raster <- brick.bbox(data, range = c(range(lon), range(lat)), crs = projection(Shp))
#     exact_extract(Raster, Shp, 'mean')
#   }
# }
