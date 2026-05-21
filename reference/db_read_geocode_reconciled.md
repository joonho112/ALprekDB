# Read a reconciled geocode object back from DuckDB

Reconstructs an `alprek_geocode_reconciled` from the
`geocode_reconciled` table. Ordered factor levels for `lat_precision`
and `coord_model_status` round-trip via the column type registry. The
`reconciliation_log` is reconstructed as an empty placeholder (the
original log is not persisted in v0.8.0; downstream consumers that need
it should call
[`geocode_reconcile()`](https://joonho112.github.io/ALprekDB/reference/geocode_reconcile.md)
again).

## Usage

``` r
db_read_geocode_reconciled(conn, run_id = NULL)
```

## Arguments

- conn:

  A DBI connection from
  [`db_init()`](https://joonho112.github.io/ALprekDB/reference/db_init.md).

- run_id:

  Optional character. When `NULL`, the most-recent run.

## Value

An `alprek_geocode_reconciled` object.
