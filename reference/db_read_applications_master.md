# Read Applications Master from DuckDB

Reconstructs an `alprek_applications_master` from DuckDB. Filters on
`cycle_year`; if `cycle_year` is `NULL`, returns the most recent cycle
present.

## Usage

``` r
db_read_applications_master(conn, cycle_year = NULL)
```

## Arguments

- conn:

  A DBI connection.

- cycle_year:

  Character or `NULL`. The cycle to load.

## Value

An `alprek_applications_master`.
