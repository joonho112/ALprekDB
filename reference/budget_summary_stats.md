# Calculate Summary Statistics by Group

Calculates descriptive statistics for budget data grouped by one or more
variables.

## Usage

``` r
budget_summary_stats(x, by = "school_year")
```

## Arguments

- x:

  An `alprek_budget_master` or `alprek_budget_panel` object.

- by:

  Character vector of grouping variable names. Default is
  `"school_year"`.

## Value

A tibble with summary statistics.
