# Build the geocode-specific check rows for `linkage_validate()`.

Returns a (possibly empty) list of
[`.make_check()`](https://joonho112.github.io/ALprekDB/reference/dot-make_check.md)-shaped
objects. The checks gracefully no-op when the input is not an
`alprek_linkage_master`, when its `classroom_level` has no `geocode_*`
columns (3-arg master path), or when an individual check's required
column(s) are absent on the master.

## Usage

``` r
.linkage_validate_geocode_checks(
  linkage_obj,
  coverage_min = 0.95,
  agreement_min = 0.95,
  model_ready_min = 0.7
)
```

## Details

The five checks (each produces 0 or 1 result row):

1.  `geocode_coverage_classroom` – WARN if non-NA `geocode_lat_final`
    coverage \< `coverage_min`.

2.  `followup_reason_completeness` – ERROR if any row with
    `geocode_needs_followup_geocoding == TRUE` has NA
    `geocode_followup_reason`.

3.  `county_check_agreement` – WARN if Melissa-vs-classroom county
    agreement rate \< `agreement_min`. Skipped if no county comparison
    column is materialized on the master.

4.  `new_site_followup_visibility` – INFO surfacing the count of
    bucket-D applications needing followup geocoding. Skipped if no
    applications branch fired.

5.  `model_ready_threshold` – WARN if `pct_model_ready` (from
    `diagnostics$geocode_coverage`) is below `model_ready_min`.
