#' load_data
#' @name load_data
#' 
#' @details
#' - [load_china_river]
#'  + `river$i$`: river polylines
#'  + `river_p$i$`: river polygons
#' 
#' - [load_china()]: `bou1_4p`
#' - [load_continent]: `shp_continent`
NULL

#' @rdname load_data
#' @export
load_china_river <- function() {
  indir = system.file("data-raw/China/", package = "sf2")
  fs = dir(indir, "*.rda", full.names = TRUE)
  for (f in fs) {
    load(f, envir = .GlobalEnv)
  }
}

#' @rdname load_data
#' @export
load_china <- function() {
  f = system.file("data-raw/bou1_4p.rda", package = "sf2")
  load(f, envir = .GlobalEnv)
}

#' @rdname load_data
#' @export
load_continent <- function() {
  f = system.file("data-raw/shp_continent.rda", package = "sf2")
  load(f, envir = .GlobalEnv)
}
