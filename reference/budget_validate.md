# Validate Budget Data Quality

Performs comprehensive data quality checks on cleaned budget data.
Returns a validation report with pass/fail status for each check.

## Usage

``` r
budget_validate(budget_long_obj, tolerance = 1, strict = FALSE)
```

## Arguments

- budget_long_obj:

  An `alprek_budget_long` object from
  [`budget_clean()`](https://joonho112.github.io/ALprekDB/reference/budget_clean.md).

- tolerance:

  Numeric. Dollar tolerance for reconciliation mismatches. Default is
  `1.00`.

- strict:

  Logical. If `TRUE`, treats warnings as errors (overall result fails if
  any warning occurs). Default is `FALSE`.

## Value

An `alprek_budget_validation` S3 object (list) with elements:

- `passed`: logical overall result.

- `n_errors`, `n_warnings`, `n_info`: counts by severity.

- `checks`: tibble of individual check results.

- `issues`: tibble of specific rows with problems.

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- budget_read("rptClassBudgets 2023-2024.xlsx", "2023-2024")
cleaned <- budget_clean(raw)
validation <- budget_validate(cleaned)
print(validation)
} # }
```
