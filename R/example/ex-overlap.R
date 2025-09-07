\dontrun{
library(nctools)

f = "//kong-nas/CMIP6/DATA/China/CN0.5.1_ChinaDaily_025x025/yearly/CN05.1_Tmax_1961_2021_yearly_025x025.nc"

data = ncread(f, "tmax")
image(data[, , 1]) # should works

r = rast(f)
range = as.vector(ext(r))
cellsize = res(r)
}

# shp = basin_Baihe
load_china()
shp = bou1_4p
cellsize <- 0.5
range <- st_range(shp, cellsize)

dat = overlap_fraction(shp, cellsize = cellsize, range, outfile = NULL)
print(dat)
