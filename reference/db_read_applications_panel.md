# Read Applications Panel from DuckDB

Reconstructs an `alprek_applications_panel` from DuckDB.

## Usage

``` r
db_read_applications_panel(conn, cycle_years = NULL)
```

## Arguments

- conn:

  A DBI connection.

- cycle_years:

  Optional character vector. If `NULL`, returns all cycles present.

## Value

An `alprek_applications_panel`.
