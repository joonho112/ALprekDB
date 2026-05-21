# Link Geocode Panel to Applications Master (Step 6.1)

Joins an `alprek_geocode_panel` onto an `alprek_applications_master`.
For applications that have a resolved `matched_site_code` (renewals +
bucket-C new applications), joins on `(matched_site_code, school_year)`.
For bucket-D "new" applications (no site_code yet), joins on `row_id`
because the Melissa file's `2025-2026_new_NNNN` row_ids correspond
directly to bucket-D applications.

**Two-phase join.** The function performs the site_code join first, then
routes the remaining unmatched applications through a `row_id` join
against the geocode panel. The two phases are tracked separately in the
diagnostics.

**fuzzy fallback (optional, off by default).** `stringdist` (already in
`DESCRIPTION`) is available if a future enhancement wants to match on
`organization_name` / Melissa `site_name` for stragglers. The current
implementation only uses exact key joins; a `fuzzy_threshold` argument
reserves the API for later.

**Preserved through the join:** `coord_model_status`, `lineage_id` (both
applications-side and geocode-side under `geocode_lineage_id`), and
`geocode_run_id`.

**No row inflation.** `nrow(out$data) == nrow(applications$data)`.
Geocode panel is deduplicated to one row per `(site_code, school_year)`
and one row per `row_id` (deterministic on `geocode_run_id`).

## Usage

``` r
linkage_geocode_applications(
  geocode_panel,
  applications,
  fuzzy_threshold = NULL
)
```

## Arguments

- geocode_panel:

  An `alprek_geocode_panel` object.

- applications:

  An `alprek_applications_master` object.

- fuzzy_threshold:

  Reserved for a future fuzzy-name fallback (NULL, ignored currently).
  When non-NULL and `stringdist` is available, the function will try to
  match stragglers on Melissa `site_name` vs. `organization_name`.
  Default `NULL`.

## Value

An `alprek_geocode_linkage_applications` S3 list:

- `data` – applications data + 12 attached geocode columns (prefixed
  `geocode_*`). `nrow == nrow(applications$data)`.

- `diagnostics` – tibble with `metric`, `value`, `group_by`.

- `meta` – list with `linked_at`, run identifiers, match counts.

## See also

[`linkage_geocode_classroom()`](https://joonho112.github.io/ALprekDB/reference/linkage_geocode_classroom.md).

## Examples

``` r
if (FALSE) { # \dontrun{
lk <- linkage_geocode_applications(panel_g, app_master)
lk$diagnostics
} # }
```
