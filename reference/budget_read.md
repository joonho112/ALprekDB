# Read Pre-K Budget Data

Reads a Pre-K budget Excel file, normalizes column names, drops footer
rows, detects format, and returns an `alprek_budget_raw` S3 object. Does
NOT reshape or aggregate – that is
[`budget_clean()`](https://joonho112.github.io/ALprekDB/reference/budget_clean.md)'s
job.

## Usage

``` r
budget_read(path, school_year = NULL, sheet = NULL, remove_footer = TRUE)
```

## Arguments

- path:

  Character. Path to the Excel file.

- school_year:

  Character. School year in `"YYYY-YYYY"` format. If `NULL`, attempts to
  infer from the filename.

- sheet:

  Character or numeric. Sheet name or number. If `NULL`, reads the first
  sheet.

- remove_footer:

  Logical. If `TRUE` (default), removes summary/footer rows (e.g., rows
  where Classroom Code is NA or Classroom Name starts with "Count:").

## Value

An `alprek_budget_raw` S3 object (list) with elements:

- `data`: tibble of raw budget data with cleaned column names.

- `meta`: list of metadata (path, school_year, year, format, etc.).

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- budget_read("rptClassBudgets 2021-2022.xlsx", school_year = "2021-2022")
raw <- budget_read("24-25 FCPK Budgets.xlsx") # auto-infers school year
} # }
```
