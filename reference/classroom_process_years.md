# Run Complete Pipeline for Multiple Years

Processes multiple years and combines them into a panel dataset.

## Usage

``` r
classroom_process_years(configs, export = FALSE, export_formats = "csv")
```

## Arguments

- configs:

  A list of `alprek_classroom_config` objects.

- export:

  Logical. Export panel results? Default `FALSE`.

- export_formats:

  Character vector of export formats.

## Value

A list with elements: `by_year`, `panel`, `validation_summary`.

## Examples

``` r
if (FALSE) { # \dontrun{
configs <- list(
  classroom_config("2021-2022", "data/FCPK Classroom Details 21-22.xlsx"),
  classroom_config("2022-2023", "data/FCPK Classroom Details 22-23.xlsx"),
  classroom_config("2023-2024", "data/FCPK Classroom Details 23-24.xlsx"),
  classroom_config("2024-2025", "data/24-25 Classroom Details.xlsx")
)
result <- classroom_process_years(configs)
result$panel$data
} # }
```
