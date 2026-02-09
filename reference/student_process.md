# Run Complete Student Processing Pipeline

Convenience function that runs the full pipeline for a single year: read
-\> clean -\> validate.

## Usage

``` r
student_process(config, export = FALSE, export_formats = "csv")
```

## Arguments

- config:

  An `alprek_student_config` object.

- export:

  Logical. Export results? Default `FALSE`.

- export_formats:

  Character vector of export formats. Options: `"csv"`, `"excel"`,
  `"rds"`, `"stata"`, `"parquet"`. Default `"csv"`.

## Value

A list with elements: `raw`, `clean`, `validation`.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- student_config("2024-2025", "data/24-25 FCPK Child Details.xlsx")
result <- student_process(cfg)
result$clean$data
} # }
```
