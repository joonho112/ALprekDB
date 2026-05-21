# Validate Linkage Results

Performs data quality checks on linkage results (joined data). Returns a
validation report with pass/fail status for each check.

**Geocode extension (v0.8.0).** When the input is an
`alprek_linkage_master` whose `classroom_level` carries the prefixed
`geocode_*` columns produced by
[`linkage_create_master()`](https://joonho112.github.io/ALprekDB/reference/linkage_create_master.md)
with a `geocode` panel, the validator additionally runs five
geocode-specific checks on the master:

- `geocode_coverage_classroom` – WARN if `geocode_lat_final` non-NA
  coverage falls below `geocode_coverage_min` (default 0.95).

- `followup_reason_completeness` – ERROR if any row with
  `geocode_needs_followup_geocoding == TRUE` has NA
  `geocode_followup_reason`.

- `county_check_agreement` – WARN if Melissa-vs-classroom county
  agreement rate falls below `county_agreement_min` (default 0.95).
  Gracefully skipped when no county join column is present.

- `new_site_followup_visibility` – INFO count of bucket-D applications
  (NA `matched_classroom_code`) that need followup geocoding. Only fires
  when the master was built with an `applications` panel.

- `model_ready_threshold` – WARN if `pct_model_ready` (from
  `diagnostics$geocode_coverage`) is below `model_ready_min` (default
  0.70).

The geocode checks gracefully skip on 3-arg (no `geocode`) master
objects and on non-master linkage results.

## Usage

``` r
linkage_validate(
  linkage_obj,
  strict = FALSE,
  geocode_coverage_min = 0.95,
  county_agreement_min = 0.95,
  model_ready_min = 0.7
)
```

## Arguments

- linkage_obj:

  An `alprek_linkage_classroom`, `alprek_linkage_student`, or
  `alprek_linkage_master` object.

- strict:

  Logical. If `TRUE`, treats warnings as errors. Default `FALSE`.

- geocode_coverage_min:

  Numeric in `[0, 1]`. Minimum acceptable `geocode_lat_final` non-NA
  coverage on classroom-level rows for the `geocode_coverage_classroom`
  check. Default `0.95`.

- county_agreement_min:

  Numeric in `[0, 1]`. Minimum acceptable Melissa-vs-classroom county
  agreement rate for the `county_check_agreement` check. Default `0.95`.

- model_ready_min:

  Numeric in `[0, 1]`. Minimum acceptable share of `model_ready` rows
  for the `model_ready_threshold` check. Default `0.70` (the real-data
  target is `0.80`).

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

# Master with geocode panel: extra geocode checks fire
master <- linkage_create_master(budget, classroom, student, geocode = gp)
linkage_validate(master)
} # }
```
