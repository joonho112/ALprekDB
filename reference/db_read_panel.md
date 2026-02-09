# Read Panel Data from DuckDB

Reads panel data from the database and reconstructs the original S3
object with proper R types (factors, Dates, integers). Optionally
filters by school year.

## Usage

``` r
db_read_panel(conn, module = c("budget", "classroom", "student"), years = NULL)
```

## Arguments

- conn:

  A DBI connection object from
  [`db_init()`](https://joonho112.github.io/ALprekDB/reference/db_init.md).

- module:

  Character. Which module to read: `"budget"`, `"classroom"`, or
  `"student"`.

- years:

  Character vector or `NULL`. School years to include (e.g.,
  `c("2023-2024", "2024-2025")`). `NULL` reads all years.

## Value

An `alprek_budget_panel`, `alprek_classroom_panel`, or
`alprek_student_panel` S3 object.

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- db_init("output/alprekdb.duckdb", read_only = TRUE)
budget <- db_read_panel(conn, "budget")
student <- db_read_panel(conn, "student", years = "2024-2025")
db_close(conn)
} # }
```
