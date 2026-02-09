# Run Complete Classroom Processing Pipeline

Convenience function that runs the full pipeline for a single year: read
-\> clean -\> validate.

## Usage

``` r
classroom_process(config, export = FALSE, export_formats = "csv")
```

## Arguments

- config:

  An `alprek_classroom_config` object.

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
cfg <- classroom_config("2024-2025", "data/24-25 Classroom Details.xlsx")
result <- classroom_process(cfg)
result$clean$data
} # }
```
