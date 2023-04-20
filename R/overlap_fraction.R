#' overlap_fraction
#' 
#' @importFrom terra plot cellSize rast
#' @importFrom exactextractr coverage_fraction
#' @importFrom data.table as.data.table
#' 
#' @example R/example/ex-overlap.R
#' 
#' @export 
overlap_fraction <- function(
    shp, cellsize = 0.1, range = NULL, outfile = NULL, ...)
{
  if (is.null(range)) range <- st_range(shp, cellsize)
  grid <- make_grid(range, cellsize)
  r_mask <- as_rast(grid)
  r_cell <- rast(r_mask, vals = 1:prod(dim(r_mask)))
  
  res <- c(
    I = r_mask,
    cell = r_cell, 
    fraction = exactextractr::coverage_fraction(r_mask, shp)[[1]], 
    area = cellSize(r_mask, unit = "km")
  ) %>% rast()

  dat = as.data.table(res, xy = TRUE) %>% 
    dplyr::rename(lon = x, lat = y) %>% 
    dplyr::arrange(I) %>% 
    dplyr::mutate(area2 = area * fraction)
  dat = dat[fraction > 0, ]
  
  if (!is.null(outfile)) {
    Ipaper::write_fig({terra::plot(res)}, outfile)

    vals = nctools::ncread_cmip(fin, ntime = 1)$data[[1]] # array
    d = cbind(dat[, .(lon, lat, I)], value = vals[dat$I])
    p = ggplot2::ggplot(d, aes(lon, lat)) +
      ggplot2::geom_raster(aes(fill = value))
    Ipaper::write_fig(p, "temp.pdf", show = F)
  }
  dat
}
