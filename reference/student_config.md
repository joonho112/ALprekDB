# Create a Student Processing Configuration

Creates a typed configuration object that controls the student
processing pipeline.

## Usage

``` r
student_config(
  school_year,
  path,
  sheet = "rptChildren_Excel",
  include_pii = FALSE,
  remove_footer = TRUE,
  output_dir = NULL
)
```

## Arguments

- school_year:

  Character. School year in `"YYYY-YYYY"` format (required).

- path:

  Character. Path to the Student/Child Details Excel file (required).

- sheet:

  Character or numeric. Excel sheet to read. Default
  `"rptChildren_Excel"`.

- include_pii:

  Logical. Include PII columns? Default `FALSE`.

- remove_footer:

  Logical. Drop footer/summary rows? Default `TRUE`.

- output_dir:

  Character. Output directory. Default `NULL` (auto-generates).

## Value

An `alprek_student_config` S3 object.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- student_config(
  school_year = "2024-2025",
  path = "data/24-25 FCPK Child Details.xlsx"
)
result <- student_process(cfg)
} # }
```
