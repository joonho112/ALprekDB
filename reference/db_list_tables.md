# List User Tables in ALprekDB Database

Lists all user data tables, excluding internal metadata tables (those
prefixed with `_alprek_`).

## Usage

``` r
db_list_tables(conn)
```

## Arguments

- conn:

  A DBI connection object from
  [`db_init()`](https://joonho112.github.io/ALprekDB/reference/db_init.md).

## Value

Character vector of table names.
