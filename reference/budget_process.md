# Run Complete Budget Processing Pipeline

Convenience function that runs the full pipeline for a single year: read
-\> clean -\> validate -\> transform.

## Usage

``` r
budget_process(config, export = FALSE, export_formats = "csv")
```

## Arguments

- config:

  An `alprek_budget_config` object.

- export:

  Logical. Export results? Default `FALSE`.

- export_formats:

  Character vector of export formats. Options: `"csv"`, `"parquet"`,
  `"excel"`, `"rds"`. Default `"csv"`.

## Value

A list with elements: `raw`, `long`, `validation`, `master`.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- budget_config("2024-2025", "data/24-25 FCPK Budgets.xlsx")
result <- budget_process(cfg)
result$master$data  # analysis-ready data
} # }
```
