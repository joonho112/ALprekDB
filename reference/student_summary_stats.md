# Summary Statistics for Student Data

Computes summary statistics for student data, either by school year
(default) or overall.

## Usage

``` r
student_summary_stats(x, by = "school_year")
```

## Arguments

- x:

  An `alprek_student_clean` or `alprek_student_panel` object.

- by:

  Character. Grouping variable. Default `"school_year"`. Use `NULL` for
  overall statistics.

## Value

A tibble of summary statistics.
