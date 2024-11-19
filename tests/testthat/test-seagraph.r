test_that("seagraph works", {

  library(SeaGraphs)
  set.seed(123, "Mersenne-Twister", sample.kind="Rejection")
  component_u <- get_component_u()
  component_v <- get_component_v()
  graph_result <- seagraph(component_u    = component_u,
                           component_v    = component_v,
                           mask_shapefile = NULL,
                           k_neighbors    = 7)
  expect_equal(head(graph_result$sf$weight),
               c(1.0000000000000, 0.9805285485353, 0.9440645294536,
                 0.9096408232499, 0.8924729023720, 0.8573462464362)
              )

  expect_equal(head(graph_result$sf$weight),
               unname(head(graph_result$edge_list[1:6,"weight"]))
              )

  expect_equal(unname(head(graph_result$edge_list[,"from"])),
               c(1, 3, 4, 6, 8, 9)
              )

  expect_equal(unname(head(graph_result$edge_list[,"to"])),
               c(2, 1, 5, 7, 6, 10)
              )

  expect_equal(graph_result$adj_mat[1:5],
               c(0.0000000000000, 0.0000000000000, 0.9805285485353,
                 0.0000000000000, 0.9714400248821)
              )

  ## Examples of mask usage
  mask_shapefile <- get_mask_shapefile()

  masked_result <- suppressWarnings(
                     seagraph(component_u    = component_u,
                              component_v    = component_v,
                              mask_shapefile = mask_shapefile,
                              k_neighbors    = 7)
                   )

  expect_equal(sf::st_bbox(masked_result$sf)[1:4],
    c(xmin = 34.44407289282,
      ymin = 43.74888682143,
      xmax = 34.88851721033,
      ymax = 43.91555333360
    )
  )

  masked_result <- suppressWarnings(
                     seagraph(component_u    = component_u,
                              component_v    = component_v,
                              mask_shapefile = terra::vect(mask_shapefile),
                              k_neighbors    = 7)
                   )

  expect_equal(sf::st_bbox(masked_result$sf)[1:4],
    c(xmin = 34.44407289282,
      ymin = 43.74888682143,
      xmax = 34.88851721033,
      ymax = 43.91555333360
    )
  )

  ## Check errors
  masked_result <- try(seagraph(
                         component_u    = component_u,
                         component_v    = component_v,
                         mask_shapefile = "Hello",
                         k_neighbors    = 7),
                       silent = TRUE)

  expect_equal(class(masked_result) == "try-error", TRUE)

  terra::crs(component_v) <- ""
  graph_result <- try(seagraph(
                        component_u    = component_u,
                        component_v    = component_v,
                        mask_shapefile = NULL,
                        k_neighbors    = 7),
                      silent = TRUE)

  expect_equal(class(graph_result) == "try-error", TRUE)

  component_v <- get_component_v()
  terra::res(component_v) <- c(1, 1)
  terra::ext(component_v) <- terra::ext(component_u)
  graph_result <- try(seagraph(
                        component_u    = component_u,
                        component_v    = component_v,
                        mask_shapefile = NULL,
                        k_neighbors    = 7),
                      silent = TRUE)

  expect_equal(class(graph_result) == "try-error", TRUE)

  component_v <- get_component_v()
  component_v <- terra::crop(component_v, terra::ext(component_v) / 4)
  graph_result <- try(seagraph(
                        component_u    = component_u,
                        component_v    = component_v,
                        mask_shapefile = NULL,
                        k_neighbors    = 7),
                      silent = TRUE)

  expect_equal(class(graph_result) == "try-error", TRUE)

})
