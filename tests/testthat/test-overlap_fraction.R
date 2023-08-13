test_that("overlap_fraction works", {
  d = overlap_fraction(bou1_4p, 1)
  expect_true(abs(sum(d$area2) / 1e6 -  9.490825) <= 1e-5)
  expect_equal(nrow(d), 1140L) 
})
