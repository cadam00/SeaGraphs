# Sea Currents To Connectivity Transformation

Sea Currents To Connectivity Transformation

## Usage

``` r
seagraph(component_u, component_v, mask_shapefile = NULL, k_neighbors = 7L)
```

## Arguments

- component_u:

  `SpatRaster` object with horizontal direction.

- component_v:

  `SpatRaster` object with vertical direction.

- mask_shapefile:

  Optional `sf` or SpatVector object for masking `component_u` and
  `component_v` `SpatRaster` objects. The default value is `NULL`,
  indicating that no mask is performed.

- k_neighbors:

  `integer` object with the number \\k\\ of nearest neighbors to use.
  The default is `7L`.

## Details

Sea currents data of the input are transformed into weighted directed
graph connectivity data. Each centroid of `component_u` and
`component_v` is considered as a separate graph node. These connectivity
data are provided in sfnetwork, `sf`, edge list and adjacency matrix
form. An additional `data.frame` with correspondence between indices and
coordinates for edge list and adjacency matrix is returned as well. The
weights in all outputs are minmax-scaled in \\\[0-1\]\\.

Extent, resolution and coordinate system of both must have the same for
both `component_u` and `component_v`; otherwise an error stops the
function's execution. In case that a `mask_shapefile` is provided, then
it is internally assured that it has the same resolution or coordinates
with `component_u` and a warning is prompted.

## Value

`SeaGraph` object, which is a list containing the following elements:

- sfnetwork: sfnetwork object representing both graph and coordinates of
  the connectivity map.

- sf: `sf` and `data.frame` object representing 'LINESTRING' rows of
  connections between nodes.

- edge_list: `matrix` object representing the edge list, where source
  ('from'), destination ('to') and weight ('weight') for each connection
  are returned.

- adj_mat: `matrix` object representing the weighted adjacency matrix of
  the nodes.

- ID_coords: `data.frame` object with the correspondence between indices
  and coordinates for edge list (edge_list) and adjacency matrix
  (adj_mat).

## References

Nagkoulis, N., Adam, C., Mamoutos, I., Katsanevakis, S., and Mazaris, A.
D. (2025). An ecological connectivity dataset for Black Sea obtained
from sea currents. *Data in Brief*, *58*, 111268.
[doi:10.1016/j.dib.2024.111268](https://doi.org/10.1016/j.dib.2024.111268)

## See also

` `[`antpath_sfn`](https://cadam00.github.io/SeaGraphs/reference/antpath_sfn.md)`, `[`flows_sfn`](https://cadam00.github.io/SeaGraphs/reference/flows_sfn.md)` `

## Examples

``` r
graph_result <- seagraph(component_u    = get_component_u(),
                         component_v    = get_component_v(),
                         k_neighbors    = 7)
## Example of mask usage
masked_result <- seagraph(component_u    = get_component_u(),
                          component_v    = get_component_v(),
                          mask_shapefile = get_mask_shapefile(),
                          k_neighbors    = 7)
#> Reading layer `mask_shapefile' from data source 
#>   `/home/runner/work/_temp/Library/SeaGraphs/external/mask_shapefile/mask_shapefile.shp' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 34.43481 ymin: 43.74194 xmax: 34.89778 ymax: 43.9225
#> Geodetic CRS:  WGS 84
#> Warning: Different resolution or coordinates among component_u and mask_shapefile:
#> project crs(component_u) on mask_shapefile
```
