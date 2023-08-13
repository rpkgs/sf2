test_that("make_rast works", {
  bandnames = paste0("bands", 1:3)
  vals = rep(seq(1, 360 * 180), 3)
  r = make_rast(vals = vals, nlyrs = 3, names = bandnames)
  expect_equal(names(r), bandnames)
  expect_equal(res(r), c(1, 1))
  expect_equal(dim(r), c(180, 360, 3)) # nlat, nlon, ntime
  
  # plot empty rast
  expect_no_error({
    plot(r)
  })

  ## second 
  range <- c(70, 105, 25, 40) # Tibetan Plateau
  cellsize <- 5

  r = make_rast(range, cellsize)

  dim = dim(r)
  data = matrix(1:prod(dim), dim[1:2], byrow = TRUE) %>% fliplr()
  expect_equal(c(values(r)), mat2vec(data))
})
