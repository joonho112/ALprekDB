# Haversine Great-Circle Distance in Meters

Vectorized great-circle distance between pairs of points on the surface
of a sphere using the haversine formula. Uses mean Earth radius
6,371,000 m.

If the `geosphere` package is available, the function delegates to
[`geosphere::distHaversine()`](https://rdrr.io/pkg/geosphere/man/distHaversine.html)
as a sanity-checked implementation (geosphere uses a slightly different
mean radius and a numerically stable form); otherwise it falls back to a
vendored haversine that uses `2 * R * asin(min(1, sqrt(a)))` to remain
numerically stable near antipodal points.

## Usage

``` r
alprek_haversine_m(lat1, lon1, lat2, lon2)
```

## Arguments

- lat1:

  Numeric vector. Latitude of point 1, in decimal degrees.

- lon1:

  Numeric vector. Longitude of point 1, in decimal degrees.

- lat2:

  Numeric vector. Latitude of point 2, in decimal degrees.

- lon2:

  Numeric vector. Longitude of point 2, in decimal degrees.

## Value

Numeric vector of distances in meters, same length as the inputs.

## Details

All four arguments must have the same length. `NA` inputs propagate to
`NA` outputs. The function does not validate that latitudes are in
`[-90, 90]` or longitudes in `[-180, 180]`; callers should pre-validate
if strict bounds are required.

## Examples

``` r
# Birmingham, AL <-> Mobile, AL is ~320 km
alprek_haversine_m(33.5186, -86.8104, 30.6954, -88.0399)
#> [1] 334595.1

# Vectorized over multiple pairs
alprek_haversine_m(
  lat1 = c(33.5186, 33.2098),
  lon1 = c(-86.8104, -87.5692),
  lat2 = c(30.6954, 33.5186),
  lon2 = c(-88.0399, -86.8104)
)
#> [1] 334595.1  78389.2
```
