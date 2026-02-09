# Run Complete Pipeline for Multiple Years

Processes multiple years and combines them into a panel dataset.

## Usage

``` r
student_process_years(configs, export = FALSE, export_formats = "csv")
```

## Arguments

- configs:

  A list of `alprek_student_config` objects.

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
  student_config("2021-2022", "data/FCPK Student Details 21-22.xlsx"),
  student_config("2022-2023", "data/FCPK Student Details 22-23.xlsx"),
  student_config("2023-2024", "data/FCPK Student Details 23-24.xlsx"),
  student_config("2024-2025", "data/24-25 FCPK Child Details.xlsx")
)
result <- student_process_years(configs)
result$panel$data
} # }
```
