#' df2sp
#'
#' Convert dataframe data into SpatialPointsDataframe (Deprecated! Use [df2sf()]
#' instead.)
#'
#' @param d A data.frame with coordinates information
#' @param formula ~longitude+latitude
#' @inheritParams make_grid
#'
#' @importFrom sp coordinates proj4string coordinates<- proj4string<-
#'
#' @export
df2sp <- function(d, formula = ~ lon + lat, prj) {
  if (missing(prj)) prj <- get_crs()
  coordinates(d) <- formula
  proj4string(d) <- prj
  return(d)
}
