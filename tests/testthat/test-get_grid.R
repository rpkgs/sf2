test_that("multiplication works", {
    range <- c(25, 40, 73, 105) # Tibetan Plateau
    grid  <- get_grid(range, cellsize = 1/12, midgrid = TRUE)

    expect_equal(nrow(grid), 69120)
    expect_equal(class(grid)[1], "SpatialPixelsDataFrame")
})
