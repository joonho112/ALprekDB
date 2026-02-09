# Combine Multiple Years into a Student Panel Dataset

Binds multiple single-year `alprek_student_clean` objects into a
longitudinal panel dataset. Unlike the classroom module, no imputation
is applied since student attributes change year-to-year.

## Usage

``` r
student_bind_years(..., clean_list = NULL)
```

## Arguments

- ...:

  `alprek_student_clean` objects to combine.

- clean_list:

  Optional list of `alprek_student_clean` objects. Alternative to `...`
  for programmatic use.

## Value

An `alprek_student_panel` S3 object (list) with elements:

- `data`: tibble of combined panel data.

- `years`: character vector of school years included.

- `n_total`: total number of student-year observations.

- `n_unique_students`: count of unique ADECE IDs.

- `by_year`: list with per-year metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
panel <- student_bind_years(clean_2122, clean_2223, clean_2324)
panel$data
} # }
```
