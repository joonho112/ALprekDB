# Read Pre-K Classroom Detail Data

Reads an FCPK Classroom Details Excel file, normalizes column names,
drops footer rows, detects format (legacy vs new), and returns an
`alprek_classroom_raw` S3 object.

## Usage

``` r
classroom_read(
  path,
  school_year = NULL,
  sheet = "rptRIF",
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

  Character or numeric. Sheet name or number. Defaults to `"rptRIF"`
  which is the standard sheet name for classroom detail files.

- remove_footer:

  Logical. If `TRUE` (default), removes summary/footer rows (rows where
  Classroom Code is NA).

## Value

An `alprek_classroom_raw` S3 object (list) with elements:

- `data`: tibble of raw classroom data with original column names
  (whitespace-squeezed but not renamed).

- `meta`: list of metadata (path, school_year, year, format, etc.).

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- classroom_read("FCPK Classroom Details 21-22.xlsx")
raw <- classroom_read("24-25 Classroom Details.xlsx", school_year = "2024-2025")
} # }
```
