# Read a geocode panel from DuckDB

Reconstructs an `alprek_geocode_panel` from the `geocode_panel` table.
Ordered factors (`lat_precision`, `coord_model_status`,
`precision_tier`) round-trip via the column type registry.

## Usage

``` r
db_read_geocode_panel(conn, run_ids = NULL)
```

## Arguments

- conn:

  A DBI connection.

- run_ids:

  Optional character vector. When `NULL`, returns all runs present.

## Value

An `alprek_geocode_panel` object.
