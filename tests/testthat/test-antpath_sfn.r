test_that("antpath_sfn works", {
  
  component_u <- SeaGraphs::get_component_u()
  component_v <- SeaGraphs::get_component_v()
  graph_result <- SeaGraphs::seagraph(component_u    = component_u,
                                      component_v    = component_v,
                                      mask_shapefile = NULL,
                                      k_neighbors    = 7)
  
  antpath_sfn_testdata <- readRDS(system.file("test_datasets/antpath_sfn.rds",
                                              package="SeaGraphs"))
  expect_equal(antpath_sfn(graph_result),
               antpath_sfn_testdata
  )
  
  antpath_sfn_testcut <- readRDS(system.file("test_datasets/antpath_sfncut.rds",
                                             package="SeaGraphs"))
  expect_equal(antpath_sfn(graph_result, lowcut = 0.1, uppcut = 0.9),
               antpath_sfn_testcut
  )
  
  expect_equal(antpath_sfn(graph_result, lowcut = 0.1),
               readRDS(system.file("test_datasets/antpath_sfnlowcut.rds",
                                             package="SeaGraphs"))
  )
  
  expect_equal(antpath_sfn(graph_result, uppcut = 0.9),
               readRDS(system.file("test_datasets/antpath_sfnuppcut.rds",
                                             package="SeaGraphs"))
  )
  
  expect_equal(antpath_sfn(graph_result$sfnetwork),
               antpath_sfn_testdata
  )
  
  sf_example <- sf::st_as_sf(graph_result$sfnetwork, "edges")
  expect_equal(antpath_sfn(sf_example),
               antpath_sfn_testdata
  )
  
  # Error cases
  expect_equal(class(try(antpath_sfn(sf::st_cast(sf_example, "MULTIPOINT")),
                         silent = TRUE)) == "try-error", TRUE)
  
  expect_equal(class(try(antpath_sfn(sf_example[,c("from", "to")]),
                           silent = TRUE)) == "try-error", TRUE)
  
  expect_equal(class(try(antpath_sfn("Hello"),
                           silent = TRUE)) == "try-error", TRUE)
  
  expect_equal(class(try(antpath_sfn(graph_result, lowcut = 0.9, uppcut = 0.1),
                           silent = TRUE)) == "try-error", TRUE)
  
})