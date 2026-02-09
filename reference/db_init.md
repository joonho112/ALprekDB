# Initialize ALprekDB DuckDB Database

Creates a new or opens an existing DuckDB database for storing ALprekDB
panel data. New databases are initialized with the ALprekDB schema
(metadata and column type registry tables). Existing databases are
validated for schema compatibility.

## Usage

``` r
db_init(path, read_only = FALSE)
```

## Arguments

- path:

  Character. File path for the DuckDB database. Use `":memory:"` for an
  in-memory database (data lost when connection is closed).

- read_only:

  Logical. Open in read-only mode? Default `FALSE`.

## Value

A DBI connection object to the DuckDB database.

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- db_init("output/alprekdb.duckdb")
db_list_tables(conn)
db_close(conn)
} # }
```
