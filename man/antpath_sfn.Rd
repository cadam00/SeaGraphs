\name{antpath_sfn}

\alias{antpath_sfn}

\title{Antpath plot}

\description{
Antpath plot of connections
}

\usage{
antpath_sfn(result, lowcut = NULL, uppcut = NULL)
}
\arguments{
\item{result}{
\code{SeaGraph} object (i.e. output of \link[SeaGraphs]{seagraph}) or
\code{sfnetwork} or \code{sf} object containing 'from', 'to' and 'weight'
column names.
}
\item{lowcut}{
Optional percentile of threshold for not plotting connections below this value.
}
\item{uppcut}{
Optional percentile of threshold for not plotting connections above this value.
}
}

\value{
A leaflet antmap map output.
}

\examples{
graph_result <- seagraph(component_u    = get_component_u(),
                         component_v    = get_component_v(),
                         mask_shapefile = NULL,
                         k_neighbors    = 7)

antpath_sfn(graph_result)

antpath_sfn(graph_result, lowcut = 0.1, uppcut = 0.9)
}