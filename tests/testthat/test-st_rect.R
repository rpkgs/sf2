test_that("st_rect works", {
  range = c(70, 140, 15, 55) #%>% set_names(c("xmin", "xmax", "ymin", "ymax"))
  r = st_rect(range)
  expect_equal(st_bbox(r) %>% as.numeric(), range[c(1, 3, 2, 4)])
})
