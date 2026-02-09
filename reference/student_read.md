# Read Pre-K Student/Child Detail Data

Reads an FCPK Student/Child Details Excel file, normalizes column names,
drops footer rows, detects format (legacy vs new), and returns an
`alprek_student_raw` S3 object.

## Usage

``` r
student_read(
  path,
  school_year = NULL,
  sheet = "rptChildren_Excel",
  remove_footer = TRUE
)
```

## Arguments

- path:

  Character. Path to the Excel file.

- school_year:

  Character. School year in `"YYYY-YYYY"` format. If `NULL`, attempts to
  infer from the filename.

- sheet:

  Character or numeric. Sheet name or number. Defaults to
  `"rptChildren_Excel"` which is the standard sheet name for student
  detail files.

- remove_footer:

  Logical. If `TRUE` (default), removes summary/footer rows (rows where
  ADECE ID is NA).

## Value

An `alprek_student_raw` S3 object (list) with elements:

- `data`: tibble of raw student data with whitespace-squeezed column
  names.

- `meta`: list of metadata (path, school_year, year, format, etc.).

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- student_read("FCPK Student Details 21-22.xlsx")
raw <- student_read("24-25 FCPK Child Details.xlsx", school_year = "2024-2025")
} # }
```
