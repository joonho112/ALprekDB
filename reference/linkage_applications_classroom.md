# Link Applications Master to Classroom Panel

Joins an `alprek_applications_master` object to an existing
`alprek_classroom_panel`. For each classroom-school-year row in the
panel, attaches the per-application columns describing whether the
classroom **applied this cycle**, in which **bucket**, and (where
available) the **tier carry-forward** and **capacity-grain indicators**
for the matching site.

Join logic:

- **Renewals (bucket A/B)**: exact join on
  `(matched_classroom_code, cycle_year to school_year_target)`. Carries
  `tier_prev_dollars`, `tier_prev_rank`, `tier_prev_band`, `bucket`,
  `match_method`, `match_score`.

- **New applications (bucket C/D)**: bucket C is aggregated to the
  matched site via `matched_site_code`, even when reconciliation also
  recorded the nearest `matched_classroom_code`. Bucket D has no matched
  site and remains in `$unmatched_applications`.

- **Capacity-grain merge**: optional left-join on `site_code` to attach
  `capacity_utilization`, `waitlist_ratio`, `is_oversubscribed`.

This is the **data-layer** linkage only - it carries application context
into the panel without computing geocoded/ACS/Bayesian features (those
are downstream packages).

## Usage

``` r
linkage_applications_classroom(
  applications,
  classroom,
  target_school_year = NULL,
  attach_capacity = NULL
)
```

## Arguments

- applications:

  An `alprek_applications_master` object (from
  [`applications_transform()`](https://joonho112.github.io/ALprekDB/reference/applications_transform.md)).

- classroom:

  An `alprek_classroom_panel` object. Joins only happen on the school
  year(s) inferred from the application's `cycle_year` (e.g.,
  `cycle_year = "2026-2027"` maps to a target school year of
  `"2025-2026"` for renewal-prior linkage, but here we use the cycle's
  own year for `applied_this_cycle` semantics).

- target_school_year:

  Optional character. The school_year value in `classroom$data` to
  attach applications context to. Default: derive from
  `applications$meta$cycle_year` (e.g., `"2026-2027"` -\>
  `"2026-2027"`). The "prior" classroom panel used by reconcile is one
  year behind; this join uses the *current* cycle's classroom row.

- attach_capacity:

  Logical. Attach `capacity_utilization` / `waitlist_ratio` /
  `is_oversubscribed` via `site_code`? Default `TRUE` when applications
  has `capacity_data`.

## Value

An `alprek_applications_linkage` S3 list:

- `classroom_level`: tibble - `classroom$data` rows for the
  `target_school_year` joined with application columns (left-join, so
  classrooms that didn't apply still appear with
  `applied_this_cycle = FALSE`)

- `unmatched_applications`: tibble - bucket D rows (truly new
  applications with no `matched_classroom_code` and no
  `matched_site_code`); downstream geocoding package will resolve these

- `diagnostics`: tibble - join counts (n_classroom_rows,
  n_applications_in, n_matched, n_only_classroom,
  n_applications_direct_classroom, n_applications_site_aggregated,
  n_only_application_unmatched)

- `meta`: `linked_at`, `cycle_year`, `target_school_year`,
  `attached_capacity`

## Examples

``` r
if (FALSE) { # \dontrun{
mst <- applications_transform(rec, cap_clean)
panel <- readRDS("output/classroom/classroom_panel_2021-2025.rds")
lk <- linkage_applications_classroom(mst, panel)
lk
lk$classroom_level
} # }
```
