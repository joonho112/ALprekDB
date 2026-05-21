# Write a reconciled geocode object to DuckDB

Persists `alprek_geocode_reconciled$data` into the `geocode_reconciled`
table, partitioned by `geocode_run_id`. Includes all 10 authoritative
columns from
[`geocode_reconcile()`](https://joonho112.github.io/ALprekDB/reference/geocode_reconcile.md)
plus the `lineage_id` lineage key from Step 3.1 (so the row-level
lineage survives the round-trip).

Also calls
[`db_write_geocode_lineage()`](https://joonho112.github.io/ALprekDB/reference/db_write_geocode_lineage.md).

## Usage

``` r
db_write_geocode_reconciled(conn, reconciled, run_id = NULL, overwrite = FALSE)
```

## Arguments

- conn:

  A DBI connection from
  [`db_init()`](https://joonho112.github.io/ALprekDB/reference/db_init.md).

- reconciled:

  An `alprek_geocode_reconciled` object.

- run_id:

  Optional character. When `NULL`, derived from `reconciled$meta`.

- overwrite:

  Logical. Default `FALSE`.

## Value

Invisible character vector of tables written.
