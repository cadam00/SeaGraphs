test_that("flows_sfn works", {

  library(SeaGraphs)
  set.seed(123, "Mersenne-Twister", sample.kind="Rejection")
  component_u <- get_component_u()
  component_v <- get_component_v()
  graph_result <- seagraph(component_u    = component_u,
                           component_v    = component_v,
                           mask_shapefile = NULL,
                           k_neighbors    = 7)

  flows_sfn_testdata <- readRDS(system.file("test_datasets/flows_sfn.rds",
                                              package="SeaGraphs"))
  expect_equal(flows_sfn(graph_result)$x,
               flows_sfn_testdata$x
  )

  flows_sfn_testcut <- readRDS(system.file("test_datasets/flows_sfncut.rds",
                                             package="SeaGraphs"))
  expect_equal(flows_sfn(graph_result, lowcut = 0.1, uppcut = 0.9)$x,
               flows_sfn_testcut$x
  )

  expect_equal(flows_sfn(graph_result, lowcut = 0.1)$x,
               readRDS(system.file("test_datasets/flows_sfnlowcut.rds",
                                   package="SeaGraphs"))$x
  )

  expect_equal(flows_sfn(graph_result, uppcut = 0.9)$x,
               readRDS(system.file("test_datasets/flows_sfnuppcut.rds",
                                   package="SeaGraphs"))$x
  )

  expect_equal(flows_sfn(graph_result$sfnetwork)$x,
               flows_sfn_testdata$x
  )

  sf_example <- sf::st_as_sf(graph_result$sfnetwork, "edges")
  expect_equal(flows_sfn(sf_example)$x,
               flows_sfn_testdata$x
  )

  # Error cases
  expect_equal(class(try(flows_sfn(sf::st_cast(sf_example, "MULTIPOINT")),
                         silent = TRUE)) == "try-error", TRUE)

  expect_equal(class(try(flows_sfn(sf_example[,c("from", "to")]),
                         silent = TRUE)) == "try-error", TRUE)

  expect_equal(class(try(flows_sfn("Hello"),
                         silent = TRUE)) == "try-error", TRUE)

  expect_equal(class(try(flows_sfn(graph_result, lowcut = 0.9, uppcut = 0.1),
                         silent = TRUE)) == "try-error", TRUE)

})
