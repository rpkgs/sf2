test_that("make_rast works", {
  bandnames = paste0("bands", 1:3)
  r = make_rast(vals = seq(1, 360*180), nlyrs = 3, names = bandnames)
  expect_equal(names(r), bandnames)
  expect_equal(res(r), c(1, 1))
  expect_equal(dim(r), c(180, 360, 3)) # nlat, nlon, ntime
  
  # plot empty rast
  expect_true({
    plot(r)
    TRUE
  })
})
