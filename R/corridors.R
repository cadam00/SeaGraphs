get_component_u <- function()
  rast(system.file("external/component_u.tiff", package="SeaGraphs"))
get_component_v <- function()
  rast(system.file("external/component_v.tiff", package="SeaGraphs"))
get_mask_shapefile <- function()
  st_read(
    system.file("external/mask_shapefile/mask_shapefile.shp",
    package="SeaGraphs")
  )

melt_neighbours <- function(input_to_melt){
  dim_input_to_melt <- dim(input_to_melt)
  nn_results_edges <- matrix(c(rep(seq.int(dim_input_to_melt[1]),
                                   times = dim_input_to_melt[2]),
                               input_to_melt),
                             nrow = dim_input_to_melt[1] * dim_input_to_melt[2],
                             ncol = 2
  )
  colnames(nn_results_edges) <- c("from", "to")
  nn_results_edges <-
    nn_results_edges[nn_results_edges[,1] != nn_results_edges[,2],]
  return(cbind(nn_results_edges[!duplicated(
                                  t(vapply(seq.int(nrow(nn_results_edges)),
                                           function(i)
                                             sort(nn_results_edges[i,]),
                                           numeric(2)))),],
               "weight" = 1)
         )
}

# Function to normalize a column to the range [0.0..., 1]
normalize_dividing <- function(x) {
  #(x - min(x)) / (max(x) - min(x))
  #(x - 0) / (max(x) - 0) #we consider as the minimum value being 0
  # and all of them being positive integers
  x/max(x)
}

seagraph <- function(component_u, component_v,
                     mask_shapefile = NULL,
                     k_neighbors = 7L
                     ){

  if ( (k_neighbors <= 0) )
    stop("k_neighbors must must be a positive number.")
  if (k_neighbors != as.integer(k_neighbors))
    stop("k_neighbors must be an integer number.")

  crs_component_u <- crs(component_u)
  crs_component_v <- crs(component_v)

  if (crs_component_u != crs_component_v)
    stop(
      paste0("Different coordinate system between component_u and component_v.",
             " Check with crs(component_u) and crs(component_v).")

    )

  if (ext(component_u) != ext(component_v))
    stop(
      paste0("Different extent between component_u and component_v.",
             " Check with ext(component_u) and ext(component_v).")

    )

  if (any(res(component_u) != res(component_v)))
    stop(
      paste0("Different resolution between component_u and component_v.",
             " Check with res(component_u) and res(component_v).")

    )

  if (!is.null(mask_shapefile)){

    is_mask_shapefile_sf         <- is(mask_shapefile, "sf")
    is_mask_shapefile_SpatVector <- is(mask_shapefile, "SpatVector")

    if (is_mask_shapefile_sf) {
      if (st_crs(mask_shapefile)$wkt != crs_component_u){
        warning(
          paste0(
            "Different resolution or coordinates among component_u and",
             " mask_shapefile:\nproject crs(component_u) on mask_shapefile"
          )
        )
        mask_shapefile <- st_transform(mask_shapefile, crs = crs_component_u)
      }
    } else if (is_mask_shapefile_SpatVector) {
      if (crs(mask_shapefile) != crs_component_u){
        warning(
          paste0(
            "Different resolution or coordinates among component_u and",
            " mask_shapefile:\nproject crs(component_v) on mask_shapefile")
        )
        mask_shapefile <- project(mask_shapefile, crs_component_u)
      }

    } else {
      stop("mask_shapefile must be either 'sf' or 'SpatVector' class.")
    }
    component_u <- crop(component_u, mask_shapefile, mask=TRUE)
    component_v <- crop(component_v, mask_shapefile, mask=TRUE)
  }

  centroids_within_polygon <- st_as_sf(as.data.frame(crds(component_u)),
                                       coords = c("x","y"),
                                       crs=crs_component_u)[[1]]

  distances_nn      <- st_distance(centroids_within_polygon)
  nrow_distances_nn <- nrow(distances_nn)
  input_grid_graph  <- matrix(ncol = k_neighbors, nrow = nrow_distances_nn)
  two_kp1           <- seq(from=2, to=(k_neighbors+1), by=1)
  for (i in seq.int(nrow_distances_nn)){
    input_grid_graph[i,] <- order(distances_nn[i,])[two_kp1]
  }

  edge_list_i <- melt_neighbours(input_grid_graph)

  vect_centroids_within_polygon <- vect(centroids_within_polygon)
  points_component_u <- extract(component_u, vect_centroids_within_polygon)
  points_component_v <- extract(component_v, vect_centroids_within_polygon)

  nrow_edge_list_i <- nrow(edge_list_i)
  edges_list       <- vector(mode = "list", length  = nrow_edge_list_i)
  values           <- numeric(nrow_edge_list_i)
  for (i in seq.int(nrow_edge_list_i)){
    first_point     <- edge_list_i[i,1]
    second_point    <- edge_list_i[i,2]
    edges_list[[i]] <- st_linestring(
                         c(centroids_within_polygon[[first_point]],
                           centroids_within_polygon[[second_point]])
                       )

    xy_current_vector_first  <- c(points_component_u[first_point,2],
                                  points_component_v[first_point,2])
    xy_current_vector_second <- c(points_component_u[second_point,2],
                                  points_component_v[second_point,2])
    xy_current_vector        <- (xy_current_vector_first +
                                 xy_current_vector_second) / 2

    coords_basic_linestring <- st_coordinates(edges_list[[i]])

    # Calculate the direction vector
    xy_direction_vector <- coords_basic_linestring[2, c(1,2)] -
                           coords_basic_linestring[1, c(1,2)]

    ## https://en.wikipedia.org/wiki/Vector_projection
    ##                                        #Definitions_in_terms_of_a_and_b
    ## Scalar projection of a_ onto b_ = ||a_|| * cos(theta) =
    ##                                                    dot(a_, b_) / ||b_||
    values[i] <- sum(xy_current_vector * xy_direction_vector) /
                 sqrt(sum(xy_direction_vector ^ 2))
  }

  sfc_lines <- do.call(st_sfc, edges_list)

  values[is.na(values)] <- 0

  # if(normalize_weights){values <- normalize_dividing(values)}
  sfc_lines        <- st_as_sf(sfc_lines)
  sfc_lines$weight <- values

  negative             <- sfc_lines$weight < 0
  sfc_lines[negative,] <- st_reverse(sfc_lines[negative,])

  sfc_lines$weight <- normalize_dividing(abs(sfc_lines$weight))

  result <- list("sfnetwork" = NULL,
                 "sf"        = NULL,
                 "edge_list" = NULL,
                 "adj_mat"   = NULL,
                 "ID_coords" = NULL)

  result$sfnetwork <- as_sfnetwork(sfc_lines,
                                   directed=TRUE,
                                   crs = crs_component_u)

  length_sfnetwork <- length(result$sfnetwork)

  st_crs(sfc_lines) <- crs_component_u
  result$sf         <- sfc_lines

  result$edge_list <- as.matrix(st_drop_geometry(st_as_sf(result$sfnetwork,
                                                          "edges")))

  seq_length_sfnetwork <- seq.int(length_sfnetwork)

  result$adj_mat                         <- matrix(0,
                                                   nrow=length_sfnetwork,
                                                   ncol=length_sfnetwork)
  result$adj_mat[result$edge_list[,1:2]] <- result$edge_list[,3]
  rownames(result$adj_mat) <- colnames(result$adj_mat) <- seq_length_sfnetwork

  result$ID_coords <- cbind("ID" = seq_length_sfnetwork,
                            as.data.frame(st_coordinates(
                              st_as_sf(result$sfnetwork, "nodes"))))

  class(result) <- c("SeaGraph", class(result))

  return(result)

}

####################################################

antpath_sfn <- function(result, lowcut = NULL, uppcut = NULL){

  if (is(result, "SeaGraph")){
    result_edges_sf_small      <- st_as_sf(result$sfnetwork, "edges")
  } else if (is(result, "sfnetwork")){
    result_edges_sf_small      <- st_as_sf(result, "edges")
  } else if (is(result, "sf") &&
             is(result, "data.frame")){
    if (!all(c("from", "to", "weight") %in% names(result)))
      stop("For 'sf' inputs, use 'from', 'to' and 'weight' colnames.")
    if (!all(st_is(result, 'LINESTRING')))
      stop("'sf' class inputs must be rows of 'LINESTRING' geometry type.")
    result_edges_sf_small      <- result[,c("from", "to", "weight")]
  } else {
    stop("Input must be class of 'SeaGraph', 'sfnetwork' or 'sf'.")
  }

  is_not_null_lowcut  <- !is.null(lowcut)
  is_not_null_uppcut  <- !is.null(uppcut)

  if (is_not_null_lowcut && is_not_null_uppcut){
    toclip <- quantile(result_edges_sf_small$weight, c(lowcut, uppcut))
    if (lowcut > uppcut) stop("It must be lowcut <= uppcut.")
    result_edges_sf_small <- result_edges_sf_small[
      (result_edges_sf_small$weight > toclip[1]) &
        (result_edges_sf_small$weight < toclip[2]),]
  } else if (is_not_null_lowcut){
    result_edges_sf_small <- result_edges_sf_small[
      result_edges_sf_small$weight >
        quantile(result_edges_sf_small$weight, lowcut),]
  } else if (is_not_null_uppcut){
    result_edges_sf_small <- result_edges_sf_small[
      result_edges_sf_small$weight <
        quantile(result_edges_sf_small$weight, uppcut),]
  }

  #https://github.com/trafficonese/leaflet.extras2/blob/master/inst/examples/
  #                                                              arrowhead_app.R
  leaflet() |>
    addTiles() |>
    addProviderTiles("Esri.WorldImagery") |>
    addPolylines(data = result_edges_sf_small, color = "blue", weight = 1) |>
    addAntpath(data = result_edges_sf_small,
                                group = "group",
                                color = "red",
                                weight = result_edges_sf_small$weight,
                                opacity = 0.5)
}

flows_sfn <- function(result, lowcut = NULL, uppcut = NULL){

  if (is(result, "SeaGraph")){
    result_edges_sf_small      <- st_as_sf(result$sfnetwork, "edges")
  } else if (is(result, "sfnetwork")){
    result_edges_sf_small      <- st_as_sf(result, "edges")
  } else if (is(result, "sf") &&
             is(result, "data.frame")){
    if (!all(c("from", "to", "weight") %in% names(result)))
      stop("For 'sf' inputs, use 'from', 'to' and 'weight' colnames.")
    if (!all(st_is(result, 'LINESTRING')))
      stop("'sf' class inputs must be rows of 'LINESTRING' geometry type.")
    result_edges_sf_small      <- result[,c("from", "to", "weight")]
  } else {
    stop("Input must be class of 'SeaGraph', 'sfnetwork' or 'sf'.")
  }

  coords_df           <- as.data.frame(st_coordinates(result_edges_sf_small))
  evens               <- rep(c(FALSE, TRUE), length.out=nrow(coords_df))
  coords_df           <- cbind(coords_df[!evens,c(3,1,2)],
                               coords_df[evens,c(1,2)])
  names(coords_df)    <- c("L1", "lng0", "lat0", "lng1", "lat1")
  rownames(coords_df) <- NULL
  coords_df$flow      <- result_edges_sf_small$weight

  is_not_null_lowcut  <- !is.null(lowcut)
  is_not_null_uppcut  <- !is.null(uppcut)

  if (is_not_null_lowcut && is_not_null_uppcut){
    toclip <- quantile(coords_df$flow, c(lowcut, uppcut))
    if (lowcut > uppcut) stop("It must be lowcut <= uppcut.")
    coords_df <- coords_df[(coords_df$flow > toclip[1]) &
                             (coords_df$flow < toclip[2]),]
  } else if (is_not_null_lowcut){
    coords_df <- coords_df[coords_df$flow > quantile(coords_df$flow, lowcut),]
  } else if (is_not_null_uppcut){
    coords_df <- coords_df[coords_df$flow < quantile(coords_df$flow, uppcut),]
  }

  # Define a color palette based on the flow values
  flow_palette <- colorNumeric(palette = "YlOrRd", domain = coords_df$flow)

  # Create the leaflet map and add flows
  leaflet() |>
    addTiles() |>
    addProviderTiles("Esri.WorldImagery") |>
    addFlows(
      coords_df$lng0, coords_df$lat0, coords_df$lng1, coords_df$lat1,
      flow = coords_df$flow,#^2,
      time = NULL,  # If you have time data, you can add it here
      maxFlow = max(coords_df$flow) * 5,  # Adjust as necessary
      popupOptions = list(closeOnClick = FALSE, autoClose = FALSE),
      color = flow_palette(coords_df$flow),
      minThickness = 0
    )
}


