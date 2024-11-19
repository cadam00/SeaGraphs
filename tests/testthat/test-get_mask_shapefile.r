test_that("get_get_mask_shapefile works", {
  mask_shapefile <- SeaGraphs::get_mask_shapefile()
  expect_equal(sf::st_bbox(mask_shapefile)[1:4],
    c(xmin = 34.43481363621,
      ymin = 43.74194238342,
      xmax = 34.89777646694,
      ymax = 43.92249777161
    )
  )
  expect_equal(dim(mask_shapefile), c(1, 2))
})