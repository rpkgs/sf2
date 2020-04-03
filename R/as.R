as_SpatialLines <- function(x) {
    as(x, "SpatialLines")
}

as_SpatialPolygonsDataFrame <- function(x) {
    as(x, "SpatialPolygonsDataFrame")
}

coords2_SpatialPolygons <- function(file) {
    coords <- fread(file) %>% set_colnames(c("lon", "lat"))
    Srl <- Polygons(list(Polygon(coords)), "1")
    s <- SpatialPolygons(list(Srl), pO = 1L, proj4string = prj84)
    SpatialPolygonsDataFrame(s, data.frame(id = 1), match.ID=FALSE)
}
