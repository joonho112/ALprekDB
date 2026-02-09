# Aggregate Student Data to Classroom Level

Computes classroom-level summary statistics from student panel data,
including demographic proportions, mean scores, and assessment
completion rates.

## Usage

``` r
linkage_aggregate_students(student)
```

## Arguments

- student:

  An `alprek_student_panel` or `alprek_linkage_student` object.

## Value

A tibble with one row per `(school_year, classroom_code)` and aggregated
student statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
agg <- linkage_aggregate_students(student_panel)
# Merge with classroom-level data
} # }
```
