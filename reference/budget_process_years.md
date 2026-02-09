# Run Complete Pipeline for Multiple Years

Processes multiple years and combines them into a panel dataset.

## Usage

``` r
budget_process_years(configs, export = FALSE, export_formats = "csv")
```

## Arguments

- configs:

  A list of `alprek_budget_config` objects.

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
  budget_config("2021-2022", "data/rptClassBudgets 2021-2022.xlsx"),
  budget_config("2022-2023", "data/rptClassBudgets 2022-2023.xlsx"),
  budget_config("2023-2024", "data/rptClassBudgets 2023-2024.xlsx"),
  budget_config("2024-2025", "data/24-25 FCPK Budgets.xlsx")
)
result <- budget_process_years(configs)
result$panel$data  # multi-year panel
} # }
```
