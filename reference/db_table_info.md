# Get Column Metadata for a Database Table

Returns column names, DuckDB types, and R type information (from the
column type registry) for a given table.

## Usage

``` r
db_table_info(conn, table)
```

## Arguments

- conn:

  A DBI connection object from
  [`db_init()`](https://joonho112.github.io/ALprekDB/reference/db_init.md).

- table:

  Character. Table name.

## Value

A tibble with columns: `column_name`, `duckdb_type`, `r_type`,
`is_factor`.
