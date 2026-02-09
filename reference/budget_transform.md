# Transform Budget Data to Wide Format with Derived Variables

Transforms long-format budget data into a wide, analysis-ready master
dataset with one row per classroom-year. Adds derived variables
including category totals, percentage shares, and delivery type
groupings.

## Usage

``` r
budget_transform(
  budget_long_obj,
  add_groups = TRUE,
  add_shares = TRUE,
  allocate_payroll = TRUE
)
```

## Arguments

- budget_long_obj:

  An `alprek_budget_long` object from
  [`budget_clean()`](https://joonho112.github.io/ALprekDB/reference/budget_clean.md).

- add_groups:

  Logical. Add delivery type groupings (binary and 3-way)? Default is
  `TRUE`.

- add_shares:

  Logical. Add percentage share columns? Default is `TRUE`.

- allocate_payroll:

  Logical. Allocate payroll taxes proportionally to lead/aux teacher
  benefits? Applies only to legacy format. Default is `TRUE`.

## Value

An `alprek_budget_master` S3 object (list) with elements:

- `data`: tibble in wide format, one row per classroom-year.

- `meta`: list of metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- budget_read("rptClassBudgets 2023-2024.xlsx", "2023-2024")
cleaned <- budget_clean(raw)
master <- budget_transform(cleaned)
master$data
} # }
```
