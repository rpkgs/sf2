# sf2  

<!-- badges: start -->
[![R-CMD-check](https://github.com/rpkgs/sf2/workflows/R-CMD-check/badge.svg)](https://github.com/rpkgs/sf2/actions)
[![codecov](https://codecov.io/gh/rpkgs/sf2/branch/master/graph/badge.svg)](https://codecov.io/gh/rpkgs/sf2)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%203%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)
[![CRAN](http://www.r-pkg.org/badges/version/sf2)](https://cran.r-project.org/package=sf2)
<!-- badges: end -->

`sf2` extends the function of `sf` package.

## Installation
``` r
devtools::install_github("rpkgs/sf2")
```


<u>**Table1.** 空间数据中数据排序要求。</u>

| 函数                                                         | 数据名称 | 数据格式          | 其他说明                                                     |
| ------------------------------------------------------------ | -------- | ----------------- | ------------------------------------------------------------ |
| image, fileds::image.plot, lattice::levelplot                | mat_base | `[lon, lat]`      | `mat_base` to `mat_rast`:<br />`t() %>% fliplr()`, `flipud() %>% t()` |
| raster::raster, terra::rast<br />raster::as.array            | mat_rast | `[rev(lat), lon]` | `mat_rast` to `mat_gdal`:<br />`t() %>% flipud()`, `fliplr() %>% t()` |
| g_base = sf2::make_grid(type = “base”), `default type`       | mat_base | `[lon, lat]`      | **优势：**<br/>(1) lon、lat都是按照从小到大进行排列，符合常规习惯；<br/> (2) 此种格式，R语言基础绘图可以正常出图<br /><br />**劣势：**<br/>与所有空间数据R包相悖。 |
| (a) g_gdal = sf2::make_grid(type = “gdal”), <br/>(b) rgdal、sp、terra、raster的计算函数返回的均是这种数据排列格式，<br />如：raster::values, raster::area, terra::values, terra::as.data.frame，他们是按照g_gdal排序格式，与其构造函数中数据排序无关。<br />(c) **terra::writeCDF保存的nc文件**, ncdf4::ncvar_get获取的array。 | mat_gdal | `[lon, rev(lat)]` | `mat_base` to `mat_gdal`:<br />flipud()<br /><br />`mat_rast` to `mat_gdal`:<br />`t()` |

> 注意：`g_mat`转化为raster::raster或terra::rast之后，则按照新的数据格式排列数据。
