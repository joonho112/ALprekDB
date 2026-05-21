# Read a cleaned geocode object back from DuckDB

Reconstructs an `alprek_geocode_clean` from the `geocode_clean` table
for a single `geocode_run_id`. When `run_id` is `NULL`, the most-recent
run (lexicographic max — run_ids encode YYYY-MM dates) is returned.

## Usage

``` r
db_read_geocode_clean(conn, run_id = NULL)
```

## Arguments

- conn:

  A DBI connection from
  [`db_init()`](https://joonho112.github.io/ALprekDB/reference/db_init.md).

- run_id:

  Optional character scalar. The run to load.

## Value

An `alprek_geocode_clean` object.
