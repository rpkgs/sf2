#' overlapPolygonGrids
#' 
#' @param shpfile file path of polygon shapefile
#' @param cellsize double
#' @param show Boolean, whether to show cliped grids?
#' 
#' @return 
#' * `lon`: longitude of grids
#' * `lat`: latitude of grids
#' * `id`: id of grids
#' 
#' @examples
#' \dontrun{
#' d <- overlapBasinGrids(shpfile, show = TRUE)
#' }
#' @importFrom rgdal readOGR
#' @importFrom sp proj4string CRS over
#' @export
overlapPolygonGrids <- function(shpfile, cellsize = 1/24, show = FALSE){
    shp <- readOGR(shpfile, verbose = FALSE)

    range <- shp@bbox %>% as.numeric() %>% .[c(2, 4, 1, 3)]
    range[c(1, 3)] %<>% floor()
    range[c(2, 4)] %<>% ceiling()

    grid2 <- get_grid(range, cellsize = cellsize, prj = CRS(proj4string(shp)))
    id <- over(grid2, shp[, "Id"])[[1]]
    I  <- id[!is.na(id)]
    id <- id[I]
  
    d <- coordinates(grid2[I, ]) %>% as.data.table() %>% 
      set_colnames(c("lon", "lat")) %>% cbind(basinId = id) 

    if (show) {
        plot(grid2[I, ])
        plot(shp, add = TRUE)    
    }
    d
}
