# Export Geocode Object to RDS

Serializes the full S3 object (data + log + meta) using
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html). Best for re-loading
in R; round-trip identical.

## Usage

``` r
geocode_export_rds(x, path = NULL, compress = TRUE)
```

## Arguments

- x:

  An `alprek_geocode_master`, `alprek_geocode_panel`, or
  `alprek_geocode_reconciled` object.

- path:

  Character. Output path. If `NULL`, auto-generates
  `output/geocode/geocode_<run_id>.rds`.

- compress:

  Logical. Compress? Default `TRUE`.

## Value

Invisible character path.
