# Transform Reconciled Geocode Data Into Master Object (Step 5.1)

Adds 5 derived analytical variables on top of the reconciled geocode
output. Mirrors the
[`applications_transform()`](https://joonho112.github.io/ALprekDB/reference/applications_transform.md)
/
[`budget_transform()`](https://joonho112.github.io/ALprekDB/reference/budget_transform.md)
pattern in this package: a thin, deterministic data-layer transform that
produces an `alprek_geocode_master` S3 object consumable by downstream
linkage, export, and modeling code.

**Derived variables (in order):**

1.  `precision_tier` (ordered factor; rooftop \> parcel \> zip4 \> zip5
    \> centroid \> area \> unknown \> none) - aliased from
    `lat_precision` with descending-order levels for sorting. The
    underlying tier value is unchanged; only the factor-level order is
    inverted so that `sort(precision_tier)` puts highest-precision rows
    first.

2.  `in_alabama` (logical; NA-able) - `lat_final` in `c(30, 36)` and
    `lng_final` in `c(-89, -84)`. Both bounds inclusive. `NA` when
    either `lat_final` or `lng_final` is `NA`.

3.  `county_check_match` (logical; NA-able) - compares Melissa
    `COUNTYNAME` against an `adece_county` sidecar (if present in the
    reconciled `$data`; e.g., G16 fixture). Returns `NA` for every row
    in Step 5.1 standalone runs because the ADECE county column is not
    part of the 29-column Melissa contract. Linkage diagnostics can use
    this column when a county sidecar has been materialized; the default
    v0.8.0 master join does not require it. Comparison is
    case-insensitive ([`tolower()`](https://rdrr.io/r/base/chartr.html)
    both sides).

4.  `coord_age_years` (integer; NA-able) -
    `cycle_year_first - school_year_first`. `cycle_year` and
    `school_year` are both `"YYYY-YYYY"` (with optional `"_new"` suffix
    on `school_year`). The first 4 characters of each are coerced to
    integer; the suffix is ignored because
    [`substr()`](https://rdrr.io/r/base/substr.html) only takes the
    leading year. Negative or implausible values are preserved
    (analytical responsibility, not data-layer correction).

5.  `geocode_run_id` (character) - panel-stable identifier built from
    `config$vendor`, `"v1"`, and
    `format(config$delivery_date, "%Y-%m")`. Default for the v0.8.0
    single-snapshot path: `"melissa_v1_2026-03"`. Every row in a single
    transform shares the same value (the snapshot is one run); panels
    built across release cycles (Step 5.2) carry distinct
    `geocode_run_id` values.

**Phase 5 contract:** Exports must carry both `coord_model_status` (from
Step 4.3) and `lineage_id` (from Step 3.1). Rows with
`coord_model_status != "model_ready"` remain visible in `$data` and must
NOT be silently promoted into downstream SAE-ready master tables. This
transform preserves both columns untouched.

## Usage

``` r
geocode_transform(reconciled, config = NULL)
```

## Arguments

- reconciled:

  An `alprek_geocode_reconciled` object from
  [`geocode_reconcile()`](https://joonho112.github.io/ALprekDB/reference/geocode_reconcile.md).

- config:

  Optional `alprek_geocode_config` (from
  [`geocode_config()`](https://joonho112.github.io/ALprekDB/reference/geocode_config.md)).
  When `NULL`, a minimal default config is constructed from
  `reconciled$meta` (`vendor = meta$source %||% "melissa"`,
  `delivery_date = meta$receipt_date`). Used to build `geocode_run_id`.

## Value

An `alprek_geocode_master` S3 list with elements:

- `data`: tibble of reconciled data + 5 new derived columns. Preserves
  `lineage_id` and `coord_model_status` columns intact.

- `transform_log`: tibble (one row per derivation rule) with columns
  `rule`, `n_affected`, `details`, `severity` (one of
  `"INFO"`/`"WARN"`/`"ERROR"`).

- `meta`: list inheriting from `reconciled$meta` plus `transformed_at`
  and `geocode_run_id`.

## Implementation notes

- `precision_tier` is an ordered factor with levels listed
  **descending** (highest precision first). This makes
  `sort(precision_tier)` put rooftop rows first. The underlying
  `lat_precision` column (ascending order, set by
  [`geocode_reconcile()`](https://joonho112.github.io/ALprekDB/reference/geocode_reconcile.md))
  is left in place.

- `county_check_match` reads an optional `adece_county` column that test
  fixtures (e.g., G16) attach during the clean-\>reconcile pass.
  Production callers should leave this column absent and rely on Phase
  6.1 linkage to materialize the comparison.

- `geocode_run_id` matches the `geocode_run_id` token mentioned in the
  Phase 5 plan book chapter (`06-phase5-transform-panel-export.qmd`) and
  in the future
  [`geocode_bind_years()`](https://joonho112.github.io/ALprekDB/reference/geocode_bind_years.md)
  (Step 5.2) panel scaffolding.

## See also

[`geocode_reconcile()`](https://joonho112.github.io/ALprekDB/reference/geocode_reconcile.md),
[`geocode_config()`](https://joonho112.github.io/ALprekDB/reference/geocode_config.md).

## Examples

``` r
if (FALSE) { # \dontrun{
raw   <- geocode_read(path = "...", cycle_year = "2026-2027",
                      receipt_date = "2026-03-04")
clean <- geocode_clean(raw)
rec   <- geocode_reconcile(clean)
mst   <- geocode_transform(rec)
mst
} # }
```
