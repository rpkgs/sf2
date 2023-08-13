#' @export
as_SpatialLines <- function(x) {
    as(x, "SpatialLines")
}

#' @export 
as_SpatialPolygonsDataFrame <- function(x) {
    as(x, "SpatialPolygonsDataFrame")
}

#' @export
as_SpatialPixelsDataFrame <- function(x) {
    as(x, "SpatialPixelsDataFrame")
}

#' @export 
as_SpatialGridDataFrame <- function(x) {
    as(x, "SpatialGridDataFrame")
}

#' @importFrom sp Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
coords2_SpatialPolygons <- function(file) {
    coords <- fread(file) %>% set_colnames(c("lon", "lat"))
    Srl <- Polygons(list(Polygon(coords)), "1")
    s <- SpatialPolygons(list(Srl), pO = 1L, proj4string = get_crs())
    SpatialPolygonsDataFrame(s, data.frame(id = 1), match.ID = FALSE)
}
