#' clip shapefile
#'
#' @import sf
#' @export
st_clip <- function(x, y) {
  st_intersection(st_as_sf(x), st_as_sf(y)) %>% as_Spatial()
}

#' @importFrom sf st_drop_geometry st_coordinates
#' @export
as.data.table.sf <- function(x) {
  loc <- st_coord(x)
  x %>%
    st_drop_geometry() %>%
    cbind(loc, .)
}

#' @inheritParams sf::st_coordinates
#' @export
st_coord <- function(x) {
  st_coordinates(x) %>%
    set_colnames(c("lon", "lat")) %>%
    as.data.table()
}

#' st_rect
#'
#' @param range `[lon_min, lon_max, lat_min, lat_max]`
#'
#' @examples
#' \dontrun{
#' xlim <- c(112, 115)
#' ylim <- c(21, 23)
#' range <- c(xlim, ylim)
#' poly <- st_rect(range)
#' write_sf(poly, "poly.shp")
#' }
#' @export
st_rect <- function(range, crs = st_crs(4326)) {
  xlim <- range[1:2]
  ylim <- range[3:4]
  coors <- matrix(c(xlim, rev(xlim), xlim[1], rep(ylim, each = 2), ylim[1]), ncol = 2)
  pts <- list(coors)
  p <- st_polygon(list(coors)) # %T>% plot()
  st_sfc(p, crs = crs)
}
# st_bbox(c(112, 115, 21, 23) %>% set_names(c("xmin", "xmax", "ymin", "ymax")), crs = st_crs(4326))

#' get range of `sf` object
#'
#' @param x `sf` object
#' @param cellsize double
#' @export
st_range <- function(x, cellsize = 0.1) {
  bbox <- x %>% st_bbox()
  range <- (bbox / cellsize) %>%
    {
      c(floor(.[1]), ceiling(.[3]), floor(.[2]), ceiling(.[4]))
    } * cellsize
  range
}


#' st_dissolve
#'
#' @export st_dissolve
st_dissolve <- function(x, by = NULL, ...) UseMethod("st_dissolve")

# TODO: dissolve all features not work; 25 Feb, 2022
#' @rdname st_dissolve
#' @export
st_dissolve.sf <- function(x, by = NULL, ...) {
  if (is.null(by) || !(by %in% colnames(shp))) by <- colnames(x)[1]
  x %>%
    dplyr::group_by_at(by) %>%
    dplyr::summarise(...)
}

#' @rdname st_dissolve
#' @export
st_dissolve.character <- function(x, by = NULL, ...) {
  shp <- sf::read_sf(x)
  outfile <- gsub(".shp$", "_dissolved.shp", x)
  # # print(shp)
  shp2 <- st_dissolve(shp, by)
  write_shp(shp2, outfile)
}


#' @export
df2sf <- function(d, coords = c("lon", "lat"), crs = 4326, ...) {
  st_as_sf(d, coords = coords, crs = crs, ...)
}

df2sp = df2sf

#' st_extractId
#'
#' @param x A sf point, or file path
#' @param y A sf polygon, or file path
#' @param plot Boolean. Whether to visualize extracted points?
#' @param ... others passed to [sf::st_within()]
#'
#' @importFrom sf st_within st_geometry read_sf
#' @export
st_extractId <- function(x, y, plot = TRUE, ...) {
  if (is.character(x)) x %<>% read_sf()
  if (is.character(y)) y %<>% read_sf()

  l <- st_within(x, y, ...)
  inds <- which.notempty(l)

  if (plot) {
    plot(st_geometry(y))
    plot(st_geometry(x), add = TRUE)
  }
}
