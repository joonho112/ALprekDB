# Link Geocode Panel to Classroom Panel (Step 6.1)

Joins an `alprek_geocode_panel` onto an `alprek_classroom_panel` so that
every classroom-year row inherits the per-site authoritative coordinates
produced by the geocode reconciler. Geocoding happens at the **site**
grain (Melissa addresses are site-level), but the classroom panel is at
classroom-year grain, so multiple classrooms at the same site share one
geocode row.

**Join logic.** Left-join `classroom_panel$data` onto a slim view of the
geocode panel keyed by `(site_code, school_year)`. Classroom rows that
have no matching geocode row keep `NA` for the 12 attached columns; the
per-classroom diagnostic surfaces the unmatched count.

**Preserved through the join:**

- `coord_model_status` – the ordered factor from
  [`geocode_reconcile()`](https://joonho112.github.io/ALprekDB/reference/geocode_reconcile.md).
  Analysts MUST see model-readiness at the classroom-year row, otherwise
  a downstream SAE pipeline cannot honor the "do not promote provisional
  rows" rule.

- `lineage_id` – both the classroom-side (kept verbatim if present) and
  the geocode-side (attached as `geocode_lineage_id`). The two are
  separate strings; downstream models choose which one to anchor on.

- `geocode_run_id` – panel-stable identifier from
  [`geocode_transform()`](https://joonho112.github.io/ALprekDB/reference/geocode_transform.md).

**No row inflation.** `nrow(out$data) == nrow(classroom_panel$data)`. If
the geocode panel happens to have multiple rows per
`(site_code, school_year)` (e.g., a renewal site re-geocoded in two
release cycles), the join deduplicates by taking the most recent run per
(site_code, school_year) using a stable `geocode_run_id` sort
(lexicographic on `YYYY-MM` token) so the join stays 1:1.

**Renaming.** All 12 attached columns are prefixed `geocode_` (e.g.,
`geocode_lat_final`, `geocode_lat_source`, `geocode_lineage_id`,
`geocode_run_id`) to avoid collisions with classroom-panel columns like
`latitude` or `lineage_id`. The classroom panel's own ADECE `latitude` /
`longitude` columns are left untouched (Decision §11.4: escape hatch /
inspection).

## Usage

``` r
linkage_geocode_classroom(geocode_panel, classroom_panel)
```

## Arguments

- geocode_panel:

  An `alprek_geocode_panel` object from
  [`geocode_bind_years()`](https://joonho112.github.io/ALprekDB/reference/geocode_bind_years.md).

- classroom_panel:

  An `alprek_classroom_panel` object.

## Value

An `alprek_geocode_linkage_classroom` S3 list:

- `data` – classroom panel rows + 12 attached geocode columns (prefixed
  `geocode_*`). `nrow == nrow(classroom_panel$data)`.

- `diagnostics` – tibble with `metric`, `value`, `group_by`. Includes
  `n_classroom_total`, `n_matched`, `n_unmatched_geocode`,
  `n_unmatched_classroom`, and coverage broken out by `school_year` and
  `lat_source`.

- `meta` – list with `linked_at`, input panel meta summaries,
  `n_geocode_rows_in`, `n_classroom_rows_in`, `match_rate`.

## Behavior on missing keys

- Classroom rows with `site_code = NA` cannot join; they appear as
  unmatched. The diagnostic `n_unmatched_classroom` includes them.

- Geocode rows with `site_code = NA` (the bucket-D `_new` cohort) are
  excluded from the classroom join entirely; they are surfaced through
  [`linkage_geocode_applications()`](https://joonho112.github.io/ALprekDB/reference/linkage_geocode_applications.md)
  instead.

- Geocode rows whose `(site_code, school_year)` does not appear in the
  classroom panel show up in `n_unmatched_geocode`.

## See also

[`linkage_geocode_applications()`](https://joonho112.github.io/ALprekDB/reference/linkage_geocode_applications.md),
[`geocode_bind_years()`](https://joonho112.github.io/ALprekDB/reference/geocode_bind_years.md),
[`classroom_bind_years()`](https://joonho112.github.io/ALprekDB/reference/classroom_bind_years.md).

## Examples

``` r
if (FALSE) { # \dontrun{
panel_g <- geocode_bind_years(geocode_master)
panel_c <- classroom_bind_years(c2122, c2223, c2324, c2425)
lk <- linkage_geocode_classroom(panel_g, panel_c)
lk
lk$diagnostics
} # }
```
