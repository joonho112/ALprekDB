# Bind Multiple Geocode Master Snapshots Into a Panel (Step 5.2)

Stacks one or more `alprek_geocode_master` objects (each one the output
of a single Melissa delivery / vendor run) into a longitudinal
`alprek_geocode_panel`.

**Important — what "panel" means here.** A single Melissa delivery is
*already* a 5-year long panel (`school_year` ∈ {2021-2022, ...,
2025-2026_new}) — that within-delivery long shape is materialized
upstream of this function. `geocode_bind_years()` is for binding
**multiple Melissa runs across release cycles** (e.g., a future v0.9.0
delivery on top of the current v0.8.0 delivery). For v0.8.0, only one
Melissa run exists, so the typical call collapses to a degenerate
identity (`geocode_bind_years(master_v1)` returns a 1-run panel whose
`$data` is the input's `$data`).

Each input master's `geocode_run_id` (built in
[`geocode_transform()`](https://joonho112.github.io/ALprekDB/reference/geocode_transform.md)
from `vendor_v1_YYYY-MM`) becomes the panel-row discriminator. By
design, the same `row_id` may appear in multiple runs (a renewal site
re-geocoded each release cycle); the unique key in the bound panel is
`(row_id, geocode_run_id)`.

## Usage

``` r
geocode_bind_years(masters)
```

## Arguments

- masters:

  A single `alprek_geocode_master` object OR a `list` of them.
  Mixed-class lists are rejected.

## Value

An `alprek_geocode_panel` S3 list with elements:

- `data` — bound rows; for single-run input this is `masters$data`
  verbatim (degenerate identity). For multi-run input, rows are
  [`dplyr::bind_rows`](https://dplyr.tidyverse.org/reference/bind_rows.html)'d;
  the `geocode_run_id` column distinguishes them.

- `meta` — list with `n_runs`, `run_ids` (character), `snapshot_dates`
  (Date vector), `snapshot_file_sha256s` (character), `bound_at`,
  `n_rows_total`, `n_rows_per_run` (named integer), `vendors`
  (character).

- `binding_log` — per-run tibble: `geocode_run_id`, `snapshot_date`,
  `file_sha256`, `n_rows`, `n_columns`.

## Schema compatibility

Phase 5 contract: every master is expected to carry the same 29 + 10
"standard" geocode columns (29-col Melissa contract + 10 derived from
reconcile + transform). If runs differ on column membership (e.g., a
future delivery introduces a new RESULTCODE level or an extra Melissa
field), the function emits a `WARN` row in `$binding_log` and still
binds via
[`dplyr::bind_rows`](https://dplyr.tidyverse.org/reference/bind_rows.html)
(which tolerates missing columns by filling `NA`). It does NOT error.

## Key uniqueness

`(row_id, geocode_run_id)` MUST be unique in the bound panel. If a run
contains internally-duplicated `row_id` values (a Phase 3 validator
should have caught this upstream), the function errors with a list of
the colliding keys.

## Phase 5 column preservation

`lineage_id` (stable row lineage from Step 3.1) and `coord_model_status`
(ordered factor from Step 4.3) are preserved in the bound panel exactly
as they appear in each input master. No silent promotion or dropping.

## See also

[`geocode_transform()`](https://joonho112.github.io/ALprekDB/reference/geocode_transform.md),
[`geocode_reconcile()`](https://joonho112.github.io/ALprekDB/reference/geocode_reconcile.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Degenerate single-run panel (v0.8.0 typical use)
mst_v1 <- geocode_transform(geocode_reconcile(geocode_clean(geocode_read("..."))))
panel  <- geocode_bind_years(mst_v1)
panel

# Future multi-run use (v0.9.0+)
panel2 <- geocode_bind_years(list(mst_v1, mst_v2))
} # }
```
