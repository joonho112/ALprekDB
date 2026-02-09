# Write Master Linked Dataset to DuckDB

Writes both classroom-level and student-level data from a master linked
dataset to the database.

## Usage

``` r
db_write_master(conn, master_obj, overwrite = FALSE)
```

## Arguments

- conn:

  A DBI connection object from
  [`db_init()`](https://joonho112.github.io/ALprekDB/reference/db_init.md).

- master_obj:

  An `alprek_linkage_master` object.

- overwrite:

  Logical. If `TRUE`, drops and recreates the tables. Default `FALSE`.

## Value

Invisible character vector of table names written.
