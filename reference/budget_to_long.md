# Convert Master/Panel to Long Format

Converts a wide-format master or panel dataset back to long format for
visualization or further analysis.

## Usage

``` r
budget_to_long(x, include_zeros = TRUE)
```

## Arguments

- x:

  An `alprek_budget_master` or `alprek_budget_panel` object.

- include_zeros:

  Logical. Include zero-amount rows? Default is `TRUE`.

## Value

A tibble in long format.
