# Join Student and Classroom Data

Performs a left join of student panel data with classroom panel data,
using `school_year` and `classroom_code` as join keys. Classroom-only
columns are appended to the student data (shared columns use student as
the authoritative source).

## Usage

``` r
linkage_student_classroom(student, classroom)
```

## Arguments

- student:

  An `alprek_student_panel` object.

- classroom:

  An `alprek_classroom_panel` object.

## Value

An `alprek_linkage_student` S3 object with elements:

- `data`: tibble of joined student + classroom data.

- `diagnostics`: list with join statistics.

- `meta`: list with metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
sc <- linkage_student_classroom(student_panel, classroom_panel)
sc$data
sc$diagnostics
} # }
```
