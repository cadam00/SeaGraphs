test_that("get_component_v works", {
  component_v <- SeaGraphs::get_component_v()
  expect_equal(terra::ext(component_v)[1:4],
    c(xmin = 34.2033322208424,
      xmax = 35.129257882307,
      ymin = 43.6516646893225,
      ymax = 44.0127754657068
    )
  )
  expect_equal(dim(component_v), c(13, 25, 1))
})