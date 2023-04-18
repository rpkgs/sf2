test_that("overlap_fraction works", {
  skip_on_ci()

  expect_silent({
    # file = system.file("shp/basin_BaiHe.shp", package = "sf2")
    # shp <- read_sf(file)
    shp <- basin_Baihe
    r = overlap_fraction(shp, 0.1)
    # nrow(r) = 632
  })
})
