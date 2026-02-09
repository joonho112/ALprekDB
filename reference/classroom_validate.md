# Validate Cleaned Classroom Data

Performs comprehensive data quality checks on cleaned classroom data.
Returns a validation report with pass/fail status for each check.
Validation is advisory — it does not block downstream processing.

## Usage

``` r
classroom_validate(clean_obj, strict = FALSE)
```

## Arguments

- clean_obj:

  An `alprek_classroom_clean` object from
  [`classroom_clean()`](https://joonho112.github.io/ALprekDB/reference/classroom_clean.md).

- strict:

  Logical. If `TRUE`, treats warnings as errors. Default `FALSE`.

## Value

An `alprek_classroom_validation` S3 object (list) with elements:

- `passed`: logical overall result.

- `n_errors`, `n_warnings`, `n_info`: counts by severity.

- `checks`: tibble of individual check results.

- `issues`: tibble of specific rows with problems.

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- classroom_read("FCPK Classroom Details 21-22.xlsx")
clean <- classroom_clean(raw)
val <- classroom_validate(clean)
print(val)
} # }
```
