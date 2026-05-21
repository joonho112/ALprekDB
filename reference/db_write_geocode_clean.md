# Write a cleaned geocode object to DuckDB

Persists `alprek_geocode_clean$data` into the `geocode_clean` table,
partitioned by `geocode_run_id`. The run_id is either passed explicitly
via `run_id =` (preferred) or derived from `clean$meta` using the same
`<source>_v1_<YYYY-MM>` scheme that
[`geocode_transform()`](https://joonho112.github.io/ALprekDB/reference/geocode_transform.md)
emits, so a `clean -> transform -> write` and a `clean -> write` pair
share the same partition.

Also calls
[`db_write_geocode_lineage()`](https://joonho112.github.io/ALprekDB/reference/db_write_geocode_lineage.md)
to record one lineage row per write.

## Usage

``` r
db_write_geocode_clean(conn, clean, run_id = NULL, overwrite = FALSE)
```

## Arguments

- conn:

  A DBI connection from
  [`db_init()`](https://joonho112.github.io/ALprekDB/reference/db_init.md).

- clean:

  An `alprek_geocode_clean` object.

- run_id:

  Optional character scalar overriding the derived `geocode_run_id`.
  When `NULL` (default), derived from `clean$meta`.

- overwrite:

  Logical. If `TRUE`, drop any existing rows for this `geocode_run_id`
  before writing. Default `FALSE` — duplicates are rejected.

## Value

Invisible character vector of tables written.
