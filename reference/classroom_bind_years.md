# Combine Multiple Years into a Classroom Panel Dataset

Binds multiple single-year `alprek_classroom_clean` objects into a
longitudinal panel dataset. Applies forward-fill imputation for
geographic coordinates and year_first_funded within site/classroom
groups.

## Usage

``` r
classroom_bind_years(..., clean_list = NULL)
```

## Arguments

- ...:

  `alprek_classroom_clean` objects to combine.

- clean_list:

  Optional list of `alprek_classroom_clean` objects. Alternative to
  `...` for programmatic use.

## Value

An `alprek_classroom_panel` S3 object (list) with elements:

- `data`: tibble of combined panel data.

- `years`: character vector of school years included.

- `n_total`: total number of classroom-year observations.

- `by_year`: list with per-year metadata.

- `imputation_log`: tibble of imputed values.

## Examples

``` r
if (FALSE) { # \dontrun{
panel <- classroom_bind_years(clean_2122, clean_2223, clean_2324)
panel$data
panel$imputation_log
} # }
```
