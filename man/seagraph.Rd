\name{seagraph}

\alias{seagraph}

\title{Sea Currents To Connectivity Transformation}

\description{
Sea Currents To Connectivity Transformation
}

\usage{
seagraph(component_u, component_v, mask_shapefile = NULL, k_neighbors = 7L)
}
\arguments{
\item{component_u}{
\code{SpatRaster} object with horizontal direction.
}
\item{component_v}{
\code{SpatRaster} object with vertical direction.
}
\item{mask_shapefile}{
Optional \code{sf} or SpatVector object for masking \code{component_u} and
\code{component_v} \code{SpatRaster} objects. The default value is \code{NULL},
indicating that no mask is performed.
}
\item{k_neighbors}{
\code{integer} object with the number \eqn{k} of nearest neighbors to use. The
default is \code{7L}.
}
}

\details{
Sea currents data of the input are transformed into weighted directed graph
connectivity data. Each centroid of \code{component_u} and \code{component_v} is
considered as a separate graph node. These connectivity data are provided in
sfnetwork, \code{sf}, edge list and adjacency matrix form. An additional
\code{data.frame} with correspondence between indices and coordinates for edge
list and adjacency matrix is returned as well. The weights in all outputs are
minmax-scaled in \eqn{[0-1]}.

Extent, resolution and coordinate system of both must have the same for both
\code{component_u} and \code{component_v}; otherwise an error stops the
function's execution. In case that a \code{mask_shapefile} is provided, then it
is internally assured that it has the same resolution or coordinates with
\code{component_u} and a warning is prompted.
}

\value{
\code{SeaGraph} object, which is a list containing the following elements:
\itemize{
\item{sfnetwork}: sfnetwork object representing both graph and coordinates of
the connectivity map.
\item{sf}: \code{sf} and \code{data.frame} object representing 'LINESTRING' rows
of connections between nodes.
\item{edge_list}: \code{matrix} object representing the edge list, where  source
('from'), destination ('to') and weight ('weight') for each connection are
returned.
\item{adj_mat}: \code{matrix} object representing the weighted adjacency
matrix of the nodes.
\item{ID_coords}: \code{data.frame} object with the correspondence between
indices and coordinates for edge list (edge_list) and adjacency matrix
(adj_mat).
}
}

\references{
Nagkoulis, N., Adam, C., Mamoutos, I., Katsanevakis, S., and Mazaris, A. D.
(2025). An ecological connectivity dataset for Black Sea obtained from sea
currents. \emph{Data in Brief}, \emph{58}, 111268.
\doi{10.1016/j.dib.2024.111268}
}

\seealso{
\code{
\link[SeaGraphs]{antpath_sfn},
\link[SeaGraphs]{flows_sfn}
}
}

\examples{
graph_result <- seagraph(component_u    = get_component_u(),
                         component_v    = get_component_v(),
                         k_neighbors    = 7)
## Example of mask usage
masked_result <- seagraph(component_u    = get_component_u(),
                          component_v    = get_component_v(),
                          mask_shapefile = get_mask_shapefile(),
                          k_neighbors    = 7)
}
