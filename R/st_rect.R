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



# #' @export
# make_polygon <- function(range = c(70, 140, 15, 55),
#                          outfile = "extent_china.shp", ...) {
#     xmin <- range[1]
#     xmax <- range[2]
#     ymin <- range[3]
#     ymax <- range[4]
#     points <- rbind(
#         c(xmin, ymin),
#         c(xmin, ymax),
#         c(xmax, ymax),
#         c(xmax, ymin),
#         c(xmin, ymin)
#     )
#     Sr <- Polygon(points)
#     Srs <- Polygons(list(Sr), "sr1")
#     spp <- SpatialPolygons(list(Srs), 1L, prj84)
#     shp <- SpatialPolygonsDataFrame(spp, data = data.frame(id = seq_along(spp)), FALSE)
#     sp::plot(shp, pbg = "white")
#     write_shp_sf(shp, outfile, ...)
# }
