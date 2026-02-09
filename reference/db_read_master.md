# Read Master Linked Dataset from DuckDB

Reads both classroom-level and student-level master data from the
database and reconstructs the `alprek_linkage_master` S3 object.
Optionally filters by school year.

## Usage

``` r
db_read_master(conn, years = NULL)
```

## Arguments

- conn:

  A DBI connection object from
  [`db_init()`](https://joonho112.github.io/ALprekDB/reference/db_init.md).

- years:

  Character vector or `NULL`. School years to include. `NULL` reads all
  years.

## Value

An `alprek_linkage_master` S3 object.

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- db_init("output/alprekdb.duckdb", read_only = TRUE)
master <- db_read_master(conn)
master$classroom_level
master$student_level
db_close(conn)
} # }
```
