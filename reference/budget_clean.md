# Clean Budget Data to Long Format

Transforms raw budget data into a long-format intermediate
representation: one row per (classroom x category_detail x source_type).
Also produces reconciliation totals for validation.

## Usage

``` r
budget_clean(raw, category_groups = NULL, tolerance = 1)
```

## Arguments

- raw:

  An `alprek_budget_raw` object from
  [`budget_read()`](https://joonho112.github.io/ALprekDB/reference/budget_read.md).

- category_groups:

  Optional tibble with `category_detail` and `category_group` columns.
  Defaults to the package codebook.

- tolerance:

  Numeric. Dollar tolerance for reconciliation diagnostics. Default is
  `1.00`.

## Value

An `alprek_budget_long` S3 object (list) with elements:

- `long`: tibble in long format with budget amounts.

- `totals`: tibble with reconciliation diagnostics per classroom.

- `meta`: list of metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- budget_read("rptClassBudgets 2023-2024.xlsx", "2023-2024")
cleaned <- budget_clean(raw)
cleaned$long      # long format data
cleaned$totals    # reconciliation diagnostics
} # }
```
