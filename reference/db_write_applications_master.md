# Write Applications Master to DuckDB

Persists an `alprek_applications_master` into DuckDB across up to four
tables:

- `applications_clean` - per-cycle, applications-grain rows
  (`master$data`).

- `applications_capacity` - per-cycle, capacity-grain rows
  (`master$capacity_data`); only created when `master$capacity_data` is
  non-NULL.

- `applications_lineage` - one row per write, capturing `cycle_year`,
  `file_sha256`, `git_sha`, `reconciled_at`, `transformed_at`, and
  `written_at` for downstream traceability.

- `applications_derived_log` - per-cycle derivation audit rows from
  `master$derived_log`.

Type registry is updated through the shared
[`.db_register_column_types()`](https://joonho112.github.io/ALprekDB/reference/dot-db_register_column_types.md)
helper so reads reconstruct factor / integer / numeric columns
correctly.

## Usage

``` r
db_write_applications_master(conn, master, overwrite = FALSE)
```

## Arguments

- conn:

  A DBI connection (from
  [`db_init()`](https://joonho112.github.io/ALprekDB/reference/db_init.md)).

- master:

  An `alprek_applications_master`.

- overwrite:

  Logical. If `TRUE`, drop and recreate the tables before writing.
  Default `FALSE` - duplicates `(cycle_year)` rows are rejected.

## Value

Invisible character vector of tables written.
