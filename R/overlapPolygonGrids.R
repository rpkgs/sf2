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
#' shpfile <- "D:/Documents/Tencent Files/991810576/FileRecv/jianyu/shp/af_0000001.shp"
#' shp <- rgdal::readOGR(shpfile, verbose = FALSE)
#' d <- overlapPolygonGrids(shp, show = TRUE)
#' }
#' @importFrom rgdal readOGR
#' @importFrom sp proj4string CRS over
#' @importFrom data.table as.data.table
#' @importFrom sp plot
#' @export
overlapPolygonGrids <- function(shp, cellsize = 1/24, type = "big", show = FALSE){
    if (is.character(shp)) {
        shp <- readOGR(shp, verbose = FALSE)
    }

    range <- shp@bbox %>% as.numeric() %>% .[c(2, 4, 1, 3)]
    # range[c(1, 3)] %<>% floor()
    # range[c(2, 4)] %<>% ceiling()
    range[c(1, 3)] %<>% {floor(./cellsize)*cellsize}
    range[c(2, 4)] %<>% {ceiling(./cellsize)*cellsize}

    grid <- get_grid(range, cellsize = cellsize, prj = CRS(proj4string(shp)))

    
    if (type == "big") {
        poly_grid <- as(grid, "SpatialPolygonsDataFrame")
        # browser()
        # sf_grid <- st_as_sf(poly_grid)
        # sf_poly  <- st_as_sf(shp)
        # rbenchmark::benchmark(
        #     id <- over(poly_grid, shp[, "Id"])[[1]],
        #     poly_clip <- raster::intersect(poly_grid, shp),
        #     r1 <- st_intersection(sf_grid, sf_poly),
        #     replications = 1)
        poly_clip <- raster::intersect(poly_grid, shp)
        AREA_clip <- raster::area(poly_clip) # not aggregated
        d <- poly_clip@data[, "id", drop = FALSE]
        I <- unique(d$id)

        A_clip    <- split(AREA_clip, d$id) %>% sapply(sum, na.rm = TRUE)
        A_grid    <- raster::area(poly_grid[d$id, ])
        d %<>% cbind(A_clip, A_grid, fraction = A_clip/A_grid)
    } else if (type == "small") {
        id <- over(grid, shp[, "Id"])[[1]]
        I  <- which(!is.na(id))
        id <- id[I]

        d <- coordinates(grid[I, ]) %>% as.data.table() %>%
          set_colnames(c("lon", "lat")) %>% cbind(basinId = id)
    }

    if (show) {
        sp::plot(grid[I, ])
        sp::plot(shp, add = TRUE)
    }
    d
}

#' @rdname overlapPolygonGrids
#' @export
#' @importFrom raster intersect area
overlapPolygonGrids2 <- function(shp, cellsize = 1/24, show = FALSE){


    browser()

    # S_grid    <- raster::area(poly_grid[I_overlap, ])
    browser()
    I  <- id[!is.na(id)]
    id <- id[I]

    d <- coordinates(grid[I, ]) %>% as.data.table() %>%
      set_colnames(c("lon", "lat")) %>% cbind(basinId = id)

    if (show) {
        plot(grid[I, ])
        plot(shp, add = TRUE)
    }
    d
}
