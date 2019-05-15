test_that("multiplication works", {
    coor <- cbind(101:120, 21:40)
    d <- getPointsMODISTile(coor)
    expect_equal(nrow(d), 20)
})
