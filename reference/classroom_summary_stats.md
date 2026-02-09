# Summary Statistics for Classroom Data

Computes summary statistics for classroom data, either by school year
(default) or overall.

## Usage

``` r
classroom_summary_stats(x, by = "school_year")
```

## Arguments

- x:

  An `alprek_classroom_clean` or `alprek_classroom_panel` object.

- by:

  Character. Grouping variable. Default `"school_year"`. Use `NULL` for
  overall statistics.

## Value

A tibble of summary statistics.
