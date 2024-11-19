\name{get_mask_shapefile}

\alias{get_mask_shapefile}

\title{Example mask shapefile}

\description{
Example mask shapefile.
}

\usage{
get_mask_shapefile()
}

\details{
Example of input \code{mask_shapefile} used for functions. It is a bounding box
subset of \link[SeaGraphs]{get_component_u} and
\link[SeaGraphs]{get_component_v} \code{SpatRaster} objects.
}

\value{
\code{sf} and \code{data.frame} object.
}

\examples{
mask_shapefile <- get_mask_shapefile()
plot(mask_shapefile)
}