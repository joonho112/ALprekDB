# Write Applications Panel to DuckDB

Persists an `alprek_applications_panel` (multi-cycle) to
`applications_panel` + `applications_capacity_panel` (if capacity
present) + `applications_lineage` (one row per cycle).

## Usage

``` r
db_write_applications_panel(conn, panel, overwrite = FALSE)
```

## Arguments

- conn:

  A DBI connection.

- panel:

  An `alprek_applications_panel`.

- overwrite:

  Logical. Default `FALSE`.

## Value

Invisible character vector of tables written.
