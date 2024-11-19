test_that("get_component_u works", {
  component_u <- SeaGraphs::get_component_u()
  expect_equal(terra::ext(component_u)[1:4],
    c(xmin = 34.2033322208424,
      xmax = 35.129257882307,
      ymin = 43.6516646893225,
      ymax = 44.0127754657068
    )
  )
  expect_equal(dim(component_u), c(13, 25, 1))
})