test_that("rast_coord works", {
  bandnames = paste0("bands", 1:3)
  vals = rep(seq(1, 360 * 180), 3)
  r = make_rast(vals = vals, nlyrs = 3, names = bandnames)

  d = rast_coord(r)
  d_1st = subset(d, value == 1)
  expect_equal(nrow(d), 360*180)
  expect_true(with(d_1st, lon == -179.5 && lat == 89.5))
})
