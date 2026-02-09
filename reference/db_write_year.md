# Append Single Year to Existing Database Table

Appends data for a single year to an existing table. Validates that the
year does not already exist and that columns are compatible. Factor
levels are merged (union of existing + new).

## Usage

``` r
db_write_year(conn, panel_obj, table = NULL)
```

## Arguments

- conn:

  A DBI connection object from
  [`db_init()`](https://joonho112.github.io/ALprekDB/reference/db_init.md).

- panel_obj:

  An `alprek_budget_panel`, `alprek_classroom_panel`, or
  `alprek_student_panel` object containing data for a single year.

- table:

  Character or `NULL`. Override table name. If `NULL` (default),
  auto-detected from S3 class.

## Value

Invisible table name.
