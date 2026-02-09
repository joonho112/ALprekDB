# Create Master Linked Dataset

Creates a fully linked master dataset at two levels:

1.  **Classroom-level**: classroom + budget + student aggregates +
    derived vars

2.  **Student-level**: student + classroom + budget columns

## Usage

``` r
linkage_create_master(budget, classroom, student)
```

## Arguments

- budget:

  An `alprek_budget_panel` object.

- classroom:

  An `alprek_classroom_panel` object.

- student:

  An `alprek_student_panel` object.

## Value

An `alprek_linkage_master` S3 object (list) with elements:

- `classroom_level`: tibble with 1 row per classroom-year.

- `student_level`: tibble with 1 row per student-year.

- `diagnostics`: list of all join diagnostics.

- `meta`: list with metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
master <- linkage_create_master(budget_panel, classroom_panel, student_panel)
master$classroom_level
master$student_level
} # }
```
