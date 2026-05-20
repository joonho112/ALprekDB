# Export Applications Object to RDS

Serializes the full S3 object (both grains + meta + log). Best for
re-loading in R; round-trip identical.

## Usage

``` r
applications_export_rds(x, path = NULL, compress = TRUE)
```

## Arguments

- x:

  An `alprek_applications_master` or `alprek_applications_panel`.

- path:

  Character. Output path. If `NULL`, auto-generates.

- compress:

  Logical. Compress? Default `TRUE`.

## Value

Invisible file path.
