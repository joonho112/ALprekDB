# Detect Budget File Format

Determines whether a budget data frame uses the legacy (2021-2024) or
new (2024-2025+) format based on column name patterns.

## Usage

``` r
budget_detect_format(df)
```

## Arguments

- df:

  A data frame (raw, from Excel import).

## Value

Character string: `"legacy"` or `"new"`.

## Details

Detection logic:

- **Legacy**: Any column name ending in `"From OSR Funds"`.

- **New**: Any column name containing `"OSR"` AND any column containing
  `"Proration"`.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- readxl::read_excel("rptClassBudgets 2021-2022.xlsx")
budget_detect_format(df)
# "legacy"
} # }
```
