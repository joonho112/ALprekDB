# Validate Cleaned Geocode Data

Comprehensive data-quality checks on an `alprek_geocode_clean` object
(output of
[`geocode_clean()`](https://joonho112.github.io/ALprekDB/reference/geocode_clean.md)).
Mirrors the API of
[`applications_validate()`](https://joonho112.github.io/ALprekDB/reference/applications_validate.md),
[`budget_validate()`](https://joonho112.github.io/ALprekDB/reference/budget_validate.md),
and
[`classroom_validate()`](https://joonho112.github.io/ALprekDB/reference/classroom_validate.md):
each check is logged with a structured row (`check_id`, `description`,
`status` one of `PASS`/`ERROR`/`WARN`/`INFO`, `n_issues`, `details`),
and offending rows accumulate in `$issues`.

Validation is scoped to the **data contract layer**: column existence,
value ranges, AL geographic bounds, codebook membership, key
consistency, provenance. Per-row coordinate reconciliation (ADECE vs
Melissa, distance-tier-driven decisions, follow-up routing) is the
responsibility of
[`geocode_reconcile()`](https://joonho112.github.io/ALprekDB/reference/geocode_reconcile.md)
in Phase 4.

### 15 Checks

1.  `required_columns` (ERROR): all 29 expected columns present.

2.  `row_id_unique` (ERROR): `row_id` has no duplicates.

3.  `row_id_format` (WARN): `row_id` matches `{YYYY-YYYY}_{site_code}`
    or `{YYYY-YYYY}_new_NNNN`.

4.  `school_year_canonical` (ERROR): values in the canonical 5-level
    set.

5.  `site_code_missingness_in_new_only` (ERROR): `site_code` NA rows
    must all carry `school_year == "*_new"`.

6.  `melissa_lat_lng_present` (ERROR): Melissa `LAT`/`LNG` are 100%
    non-NA.

7.  `has_latlon_consistency` (ERROR): `has_latlon == !is.na(latitude)`.

8.  `melissa_coord_in_al_bounds` (ERROR): non-NA Melissa coords inside
    AL bounding box.

9.  `adece_coord_in_al_bounds` (WARN): non-NA ADECE coords inside AL
    bounding box.

10. `resultcode_canonical` (WARN): RESULTCODE in `{GS01..GS08}`.

11. `statuscode_canonical` (WARN): STATUSCODE in observed codebook.

12. `resultcode_statuscode_consistency` (WARN): 1:1 pairings observed in
    the STATUSCODE codebook.

13. `errorcode_all_na_in_v080` (INFO): all `ERRORCODE` values are NA
    (v0.8.0 contract; future deliveries may populate).

14. `provenance_complete` (ERROR): `meta` carries `file_sha256`,
    `cycle_year`, `receipt_date`, `git_sha`.

15. `lineage_id_complete` (ERROR): row-level `lineage_id` exists, is
    non-blank, and is unique.

Plus a final `summary_coverage` (INFO) check reporting RESULTCODE
coverage %, follow-up queue size estimate, and PLACENAME missingness.

## Usage

``` r
geocode_validate(clean, strict = FALSE, config = NULL)
```

## Arguments

- clean:

  An `alprek_geocode_clean` object from
  [`geocode_clean()`](https://joonho112.github.io/ALprekDB/reference/geocode_clean.md).

- strict:

  Logical. If `TRUE`, treats warnings as overall failure. Default
  `FALSE`.

- config:

  Optional `alprek_geocode_config` (from
  [`geocode_config()`](https://joonho112.github.io/ALprekDB/reference/geocode_config.md)).
  When provided, supplies `al_lat_bounds` / `al_lng_bounds`. RESULTCODE
  canonicality remains fixed to the documented Melissa set
  `{GS01..GS08}`; master acceptability is enforced later from the
  RESULTCODE codebook.

## Value

An `alprek_geocode_validation` S3 list with elements:

- `passed`: logical (overall result).

- `n_errors`, `n_warnings`, `n_info`: integer counts.

- `checks`: tibble with columns `check_id`, `description`, `status`,
  `n_issues`, `details`.

- `issues`: tibble with columns `row_id`, `check_id`, `severity`,
  `value`, `expected`, `note`.

## See also

[`geocode_clean()`](https://joonho112.github.io/ALprekDB/reference/geocode_clean.md),
[`geocode_config()`](https://joonho112.github.io/ALprekDB/reference/geocode_config.md),
[`alprek_geocode_al_fips_counties()`](https://joonho112.github.io/ALprekDB/reference/alprek_geocode_al_fips_counties.md),
[`alprek_geocode_resultcode_meaning()`](https://joonho112.github.io/ALprekDB/reference/alprek_geocode_resultcode_meaning.md).

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- geocode_read(path, cycle_year = "2026-2027",
                    receipt_date = "2026-03-04")
clean <- geocode_clean(raw)
v <- geocode_validate(clean)
print(v)
v$checks
v$issues
} # }
```
