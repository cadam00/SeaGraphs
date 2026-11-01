# Example mask shapefile

Example mask shapefile.

## Usage

``` r
get_mask_shapefile()
```

## Details

Example of input `mask_shapefile` used for functions. It is a bounding
box subset of
[get_component_u](https://cadam00.github.io/SeaGraphs/reference/get_component_u.md)
and
[get_component_v](https://cadam00.github.io/SeaGraphs/reference/get_component_v.md)
`SpatRaster` objects.

## Value

`sf` and `data.frame` object.

## Examples

``` r
mask_shapefile <- get_mask_shapefile()
#> Reading layer `mask_shapefile' from data source 
#>   `/home/runner/work/_temp/Library/SeaGraphs/external/mask_shapefile/mask_shapefile.shp' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 34.43481 ymin: 43.74194 xmax: 34.89778 ymax: 43.9225
#> Geodetic CRS:  WGS 84
plot(mask_shapefile)
```
