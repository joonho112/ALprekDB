# Validate Cleaned or Reconciled Applications Data

Comprehensive data-quality checks on a single cleaned ADECE applications
object (`alprek_applications_clean`) or the merged
`alprek_applications_reconciled` object. Mirrors
[`budget_validate()`](https://joonho112.github.io/ALprekDB/reference/budget_validate.md)
and
[`classroom_validate()`](https://joonho112.github.io/ALprekDB/reference/classroom_validate.md)
API: each check is logged with a structured row (`check_name`,
`check_description`, `status` one of `PASS`, `ERROR`, `WARN`, `INFO`,
`n_issues`, `details`), and offending rows accumulate in `$issues`.

Validation is scoped to the **data contract layer**: column existence,
value ranges, cross-field consistency, codebook membership, provenance.
Geocoding / ACS / Bayesian-modelling checks live in downstream packages.

## Usage

``` r
applications_validate(x, strict = FALSE, tolerance = 1)
```

## Arguments

- x:

  One of:

  - `alprek_applications_clean` (cleaned per-kind)

  - `alprek_applications_reconciled` (merged renewals + new_apps)

  - `alprek_applications_linkage` (classroom_panel x applications join)

- strict:

  Logical. If `TRUE`, treats warnings as overall failure. Default
  `FALSE`.

- tolerance:

  Numeric. Dollar tolerance for cross-field reconciliation checks.
  Default `1.00`.

## Value

An `alprek_applications_validation` S3 list with elements:

- `passed`: logical (overall result)

- `n_errors`, `n_warnings`, `n_info`: integer counts

- `kind`: e.g., `"renewals"`, `"new_apps"`, `"non_renewals"`,
  `"capacity"`, `"reconciled"`

- `checks`: tibble of check results

- `issues`: tibble of offending rows (one row per issue, with
  `issue_type` plus key context columns)

## Examples

``` r
if (FALSE) { # \dontrun{
clean <- applications_clean(applications_read_renewals(path, "2026-2027"))
v <- applications_validate(clean)
print(v)
v$checks
v$issues
} # }
```
