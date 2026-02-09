# Create a Classroom Processing Configuration

Creates a typed configuration object that controls the classroom
processing pipeline.

## Usage

``` r
classroom_config(
  school_year,
  classroom_path,
  sheet = "rptRIF",
  include_dob = FALSE,
  remove_footer = TRUE,
  output_dir = NULL
)
```

## Arguments

- school_year:

  Character. School year in `"YYYY-YYYY"` format (required).

- classroom_path:

  Character. Path to the classroom Excel file (required).

- sheet:

  Character or numeric. Excel sheet to read. Default `"rptRIF"`.

- include_dob:

  Logical. Include Date of Birth columns? Default `FALSE`.

- remove_footer:

  Logical. Drop footer/summary rows? Default `TRUE`.

- output_dir:

  Character. Output directory. Default `NULL` (auto-generates).

## Value

An `alprek_classroom_config` S3 object.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- classroom_config(
  school_year = "2024-2025",
  classroom_path = "data/24-25 Classroom Details.xlsx"
)
result <- classroom_process(cfg)
} # }
```
