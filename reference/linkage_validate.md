# Validate Linkage Results

Performs data quality checks on linkage results (joined data). Returns a
validation report with pass/fail status for each check.

## Usage

``` r
linkage_validate(linkage_obj, strict = FALSE)
```

## Arguments

- linkage_obj:

  An `alprek_linkage_classroom`, `alprek_linkage_student`, or
  `alprek_linkage_master` object.

- strict:

  Logical. If `TRUE`, treats warnings as errors. Default `FALSE`.

## Value

An `alprek_linkage_validation` S3 object (list) with elements:

- `passed`: logical overall result.

- `n_errors`, `n_warnings`, `n_info`: counts by severity.

- `checks`: tibble of individual check results.

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- linkage_classroom_budget(classroom_panel, budget_panel)
validation <- linkage_validate(cb)
print(validation)
} # }
```
