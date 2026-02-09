# Close ALprekDB DuckDB Connection

Properly closes the DuckDB database connection and shuts down the
database driver.

## Usage

``` r
db_close(conn)
```

## Arguments

- conn:

  A DBI connection object from
  [`db_init()`](https://joonho112.github.io/ALprekDB/reference/db_init.md).

## Value

Invisible `TRUE` on success.
