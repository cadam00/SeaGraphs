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
  expect_equal(flows_sfn(graph_result)$x$limits,
               flows_sfn_testdata$x$limits
  )

  flows_sfn_testcut <- readRDS(system.file("test_datasets/flows_sfncut.rds",
                                             package="SeaGraphs"))
  expect_equal(flows_sfn(graph_result, lowcut = 0.1, uppcut = 0.9)$x$limits,
               flows_sfn_testcut$x$limits
  )

  expect_equal(flows_sfn(graph_result, lowcut = 0.1)$x$limits,
               readRDS(system.file("test_datasets/flows_sfnlowcut.rds",
                                   package="SeaGraphs"))$x$limits
  )

  expect_equal(flows_sfn(graph_result, uppcut = 0.9)$x$limits,
               readRDS(system.file("test_datasets/flows_sfnuppcut.rds",
                                   package="SeaGraphs"))$x$limits
  )

  expect_equal(flows_sfn(graph_result$sfnetwork)$x$limits,
               flows_sfn_testdata$x$limits
  )

  sf_example <- sf::st_as_sf(graph_result$sfnetwork, "edges")
  expect_equal(flows_sfn(sf_example)$x$limits,
               flows_sfn_testdata$x$limits
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
