# Validate Cleaned Student Data

Performs comprehensive data quality checks on cleaned student data.
Returns a validation report with pass/fail status for each check.
Validation is advisory — it does not block downstream processing.

## Usage

``` r
student_validate(clean_obj, strict = FALSE)
```

## Arguments

- clean_obj:

  An `alprek_student_clean` object from
  [`student_clean()`](https://joonho112.github.io/ALprekDB/reference/student_clean.md).

- strict:

  Logical. If `TRUE`, treats warnings as errors. Default `FALSE`.

## Value

An `alprek_student_validation` S3 object (list) with elements:

- `passed`: logical overall result.

- `n_errors`, `n_warnings`, `n_info`: counts by severity.

- `checks`: tibble of individual check results.

- `issues`: tibble of specific rows with problems.

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- student_read("FCPK Student Details 21-22.xlsx", "2021-2022")
clean <- student_clean(raw)
val <- student_validate(clean)
print(val)
} # }
```
