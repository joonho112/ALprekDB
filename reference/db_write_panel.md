# Write Panel Data to DuckDB

Writes an ALprekDB panel object to the database. Automatically
determines the table name from the S3 class. Factor columns are
converted to character for storage; their levels are preserved in the
column type registry for reconstruction on read.

## Usage

``` r
db_write_panel(conn, panel_obj, overwrite = FALSE)
```

## Arguments

- conn:

  A DBI connection object from
  [`db_init()`](https://joonho112.github.io/ALprekDB/reference/db_init.md).

- panel_obj:

  An `alprek_budget_panel`, `alprek_classroom_panel`, or
  `alprek_student_panel` object.

- overwrite:

  Logical. If `TRUE`, drops and recreates the table. Default `FALSE`.

## Value

Invisible table name.
