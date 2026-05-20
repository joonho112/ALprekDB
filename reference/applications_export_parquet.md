# Export Applications Data to Parquet

Requires `arrow`. Mirrors
[`budget_export_parquet()`](https://joonho112.github.io/ALprekDB/reference/budget_export_parquet.md).

## Usage

``` r
applications_export_parquet(
  x,
  path = NULL,
  compression = "snappy",
  grain = c("apps", "capacity")
)
```

## Arguments

- x:

  An `alprek_applications_master` or `alprek_applications_panel`.

- path:

  Character. Output path. If `NULL`, auto-generates.

- compression:

  Character. Default `"snappy"`.

- grain:

  Character. `"apps"` (default) or `"capacity"`.

## Value

Invisible file path.
