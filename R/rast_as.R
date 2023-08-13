# #' @export
# rast2poly <- function(r, I_grid = NULL){
#     if (is.null(I_grid)) I_grid <- 1:nrow(r)
#     as(r, "SpatialPolygonsDataFrame")
# }

#' @export
rast2poly <- function(r, crs = 4326) {
  sf_poly <- suppressWarnings({
    sf::st_as_sf(stars::st_as_stars(r),
      as_points = FALSE, merge = TRUE
    ) %>% sf::st_make_valid() # %>% sf::as_Spatial()
  })
  sf::st_crs(sf_poly) <- crs
  sf_poly
}


#' @export
rast2SpatialGrid <- function(r, I_grid = NULL) {
  if (is.null(I_grid)) I_grid <- 1:nrow(r)
  as(r, "SpatialGridDataFrame")
}

#' @export
as_rast <- function(x) rast(x)
