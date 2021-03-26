#' @export 
make_polygon <- function(
    range = c(70, 140, 15, 55),
    outfile = "extent_china.shp", ...)
{
    xmin = range[1]
    xmax = range[2]
    ymin = range[3]
    ymax <- range[4]

    points = rbind(
        c(xmin, ymin),
        c(xmin, ymax),
        c(xmax, ymax),
        c(xmax, ymin),
        c(xmin, ymin))
    Sr = Polygon(points)
    Srs = Polygons(list(Sr), "sr1")
    spp = SpatialPolygons(list(Srs), 1L, prj84)
    shp <- SpatialPolygonsDataFrame(spp, data = data.frame(id = seq_along(spp)), FALSE)

    sp::plot(shp, pbg="white")
    write_shp_sf(shp, outfile, ...)
}
