\name{get_component_v}

\alias{get_component_v}

\title{Example component v}

\description{
Example vertical direction raster.
}

\usage{
get_component_v()
}

\details{
Example of input \code{component_v} used for functions. It is a cropped area
of yearly aggregated and normalized vertical component at Black Sea.
}

\value{
\code{SpatRaster} object.
}

\references{
Lima, L., Aydogdu, A., Escudier, R., Masina, S., Ciliberti, S. A., Azevedo, D.,
Peneva, E. L., Causio, S., Cipollone, A., Clementi, E., Cretí, S., Stefanizzi,
L., Lecci, R., Palermo, F., Coppini, G., Pinardi, N., and Palazov, A. (2020).
Black Sea Physical Reanalysis (CMEMS BS-Currents) (Version 1) [Data set].
Copernicus Monitoring Environment Marine Service (CMEMS).
\doi{10.25423/CMCC/BLKSEA_MULTIYEAR_PHY_007_004}. Last Access: 07/11/2024.

Schulzweida, U. (2023). CDO User Guide (23.0). Zenodo.
\doi{10.5281/zenodo.10020800}.
}

\seealso{
\code{
\link[SeaGraphs]{get_component_u},
\link[SeaGraphs]{get_mask_shapefile}
}
}

\examples{
component_v <- get_component_v()
terra::plot(component_v)
}
