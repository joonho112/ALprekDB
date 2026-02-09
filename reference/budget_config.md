# Create a Budget Processing Configuration

Creates a typed configuration object that controls the budget processing
pipeline.

## Usage

``` r
budget_config(
  school_year,
  budget_path,
  sheet = NULL,
  output_dir = NULL,
  tolerance = 1,
  fill_na_zero = TRUE,
  remove_footer = TRUE,
  verbose = TRUE
)
```

## Arguments

- school_year:

  Character. School year in `"YYYY-YYYY"` format (required).

- budget_path:

  Character. Path to the budget Excel file (required).

- sheet:

  Character or numeric. Excel sheet to read. Default `NULL` (first
  sheet).

- output_dir:

  Character. Output directory. Default `NULL` (auto-generates).

- tolerance:

  Numeric. Dollar tolerance for reconciliation. Default `1.00`.

- fill_na_zero:

  Logical. Fill NA budget cells with 0? Default `TRUE`.

- remove_footer:

  Logical. Drop footer/summary rows? Default `TRUE`.

- verbose:

  Logical. Print progress messages? Default `TRUE`.

## Value

An `alprek_budget_config` S3 object.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- budget_config(
  school_year = "2024-2025",
  budget_path = "data/24-25 FCPK Budgets.xlsx"
)
result <- budget_process(cfg)
} # }
```
