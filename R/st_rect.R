#' st_rect
#' 
#' @param range `[lon_min, lon_max, lat_min, lat_max]`
#' 
#' @examples
#' \dontrun{
#' xlim = c(112, 115)
#' ylim = c(21, 23)
#' range <- c(xlim, ylim)
#' poly = st_rect(range)
#' write_sf(poly, "poly.shp")
#' }
#' @export
st_rect <- function(range, crs = st_crs(4326)){
    xlim = range[1:2]
    ylim = range[3:4]
    coors = matrix(c(xlim, rev(xlim), xlim[1], rep(ylim, each = 2), ylim[1]), ncol = 2)
    pts = list(coors)
    p <- st_polygon(list(coors)) #%T>% plot()
    st_sfc(p, crs = crs)
}
# st_bbox(c(112, 115, 21, 23) %>% set_names(c("xmin", "xmax", "ymin", "ymax")), crs = st_crs(4326))
