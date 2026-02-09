# Summary Statistics for Linkage Data

Computes summary statistics for linked data by school year.

## Usage

``` r
linkage_summary_stats(x, by = "school_year")
```

## Arguments

- x:

  An `alprek_linkage_master`, `alprek_linkage_classroom`, or
  `alprek_linkage_student` object.

- by:

  Character. Grouping variable. Default `"school_year"`.

## Value

A tibble of summary statistics.
