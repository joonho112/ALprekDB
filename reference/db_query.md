# Execute SQL Query on ALprekDB Database

Executes an arbitrary SQL query and returns the result as a tibble.
Useful for custom aggregation, filtering, and analysis directly in the
database.

## Usage

``` r
db_query(conn, sql)
```

## Arguments

- conn:

  A DBI connection object from
  [`db_init()`](https://joonho112.github.io/ALprekDB/reference/db_init.md).

- sql:

  Character. SQL query string.

## Value

A tibble with the query results.

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- db_init("output/alprekdb.duckdb", read_only = TRUE)
result <- db_query(conn, "
  SELECT school_year, COUNT(*) as n,
         AVG(grand_total) as mean_budget
  FROM master_classroom
  GROUP BY school_year
  ORDER BY school_year
")
db_close(conn)
} # }
```
