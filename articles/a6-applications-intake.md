# A6 - Applications intake (read → reconcile → validate → link)

## Overview

The **applications** module (added in v0.7.0) covers ADECE’s annual
classroom-applications workbook — the per-cycle file ADECE Tammy Walker
sends each spring with `renewals`, `new`, `Non-Renew`, and capacity
sheets. It complements the budget / classroom / student modules with the
same read → detect → clean → reconcile → validate → transform → panel →
export → linkage → DuckDB pipeline.

This vignette walks through that pipeline end-to-end on **synthetic
data** so users can run every chunk without ADECE files. Real-data calls
look identical — just point the read functions at the real `.xlsx`.

``` r

library(ALprekDB)
```

### Scope (v0.7.0)

**In-scope** (this module):

- 4 input kinds: renewals, new applications, non-renewals, capacity
- Cycle-1 (2026-2027) column mappings + status code + funding type
  codebooks
- 4-bucket reconciliation against a prior classroom panel (Jaro-Winkler
  fuzzy match, county-blocked)
- 18 validation checks + 17-case edge taxonomy
- Derived variables: `is_renewal`, `is_new`, `tier_prev_*`,
  `capacity_utilization`, `waitlist_ratio`, `is_oversubscribed`
- Multi-cycle panel binding, 5-format export, DuckDB persistence,
  linkage to classroom_panel

**Out-of-scope** (planned as separate downstream packages):

- Geocoding (3-source consensus, OSRM isochrone)
- ACS area-weighted aggregation (tidycensus, census tracts, MOE → SE)
- Bayesian small-area estimation of economic-need tiers
- Tier binning, posterior summary, credibility intervals
- Spatial joins, lat/lon validation

A finding that the package fails to do geocoding/ACS/Bayesian work is
*intentional*. Those layers will arrive in separate v0.8.0+ packages
that consume this module’s output.

## Step 1 — Read 4 input kinds

The synthetic generator returns the same standardized cycle-1 schema you
would get from the real workbook after Step 3 below.

``` r

synth <- alprek_synthetic_applications(
  n_renewals       = 12,
  n_new            = 4,
  n_non_renewals   = 3,
  n_capacity_sites = 10,
  cycle_year       = "2026-2027",
  seed             = 42L
)
names(synth)
#> [1] "renewals"     "new_apps"     "non_renewals" "capacity"     "meta"
str(synth$meta)
#> List of 4
#>  $ cycle_year     : chr "2026-2027"
#>  $ cycle_year_prev: chr "2025-2026"
#>  $ seed           : int 42
#>  $ counts         : Named int [1:4] 12 4 3 10
#>   ..- attr(*, "names")= chr [1:4] "renewals" "new_apps" "non_renewals" "capacity"
```

In real use the four kinds come from one ADECE master xlsx:

``` r

path <- "Copy of 2026-27 Classroom Applications_tw04202026 (003).xlsx"
ren_raw  <- applications_read_renewals(path,    cycle_year = "2026-2027",
                                          receipt_date = "2026-04-20")
new_raw  <- applications_read_new(path,         cycle_year = "2026-2027",
                                          receipt_date = "2026-04-20")
nr_raw   <- applications_read_nonrenewal(path,  cycle_year = "2026-2027",
                                          receipt_date = "2026-04-20")
cap_raw  <- applications_read_capacity(path,    cycle_year = "2026-2027",
                                          receipt_date = "2026-04-20")
```

Each `applications_read_*()` returns an `alprek_applications_raw` S3
with `$data` plus a `$meta` slot capturing **file SHA-256**,
**cycle_year**, **receipt_date**, **sheet**, **n_rows**, **n_cols**,
**read_at**, and **git_sha** — the full provenance chain downstream
packages need.

## Step 2 — Detect format

[`applications_detect_format()`](https://joonho112.github.io/ALprekDB/reference/applications_detect_format.md)
examines marker columns to classify the input as `"cycle1"` (2026-2027),
`"cycle0"` (2025-2026), or `"unknown"`:

``` r

applications_detect_format(ren_raw)
#> [1] "cycle1"
```

This branching keeps the column-map resolver future-proof: when ADECE
renames columns in cycle 2027-2028, you add an
`applications_column_map_renewals_cycle2.csv` and an extra marker
pattern.

## Step 3 — Clean (standardize + parse + drop noise rows)

[`applications_clean()`](https://joonho112.github.io/ALprekDB/reference/applications_clean.md)
renames raw columns to the codebook’s `standard_name`, parses numerics
(currency-aware), drops Debugger Trace noise rows, and adds
`raw_row_index` + stable `lineage_id` (SHA-256 of file_sha256, sheet,
raw_row_index, cycle_year) for downstream tracing.

``` r

# For a vignette, wrap synthetic rows in the clean S3 shell:
make_clean <- function(df, kind, cycle_year = "2026-2027") {
  df <- tibble::as_tibble(df)
  df$raw_row_index <- seq_len(nrow(df))
  df$lineage_id    <- sprintf("synth-%s-%04d", kind, df$raw_row_index)
  df$data_source   <- sprintf("ADECE-%s-sheet",
                                switch(kind, renewals = "renewals",
                                              new_apps = "new",
                                              non_renewals = "nonrenewals",
                                              capacity = "capacity"))
  structure(list(
    data         = df,
    cleaning_log = tibble::tibble(),
    meta = list(kind = kind, cycle_year = cycle_year, cycle = "cycle1",
                  n_rows_in = nrow(df), n_rows_out = nrow(df),
                  n_rows_dropped = 0L,
                  file_sha256 = "synthetic", git_sha = "synthetic",
                  cleaned_at = format(Sys.time()))
  ), class = "alprek_applications_clean")
}

ren  <- make_clean(synth$renewals,     "renewals")
new  <- make_clean(synth$new_apps,     "new_apps")
nr   <- make_clean(synth$non_renewals, "non_renewals")
cap  <- make_clean(synth$capacity,     "capacity")

ren
#> <alprek_applications_clean>
#>   Kind:         renewals 
#>   Cycle:       cycle1 (2026-2027)
#>   Rows: in=12 out=12 dropped=0
#>   Cols:         18 
#>   Cleaned at:   2026-05-20 12:44:49
```

Real-data call:

``` r

ren <- applications_clean(ren_raw)
```

## Step 4 — Reconcile (4-bucket assignment)

[`applications_reconcile()`](https://joonho112.github.io/ALprekDB/reference/applications_reconcile.md)
partitions every input row into one of four buckets using exact +
Jaro-Winkler fuzzy matching, blocked by county, against a prior
classroom panel. Every fuzzy decision (chosen + top-3 runners-up) is
logged in `$reconciliation_log` — this is the **audit chain** for
analyst review and re-runs.

``` r

# Without a prior panel, use degraded mode for demonstrations:
rec <- applications_reconcile(ren, new, allow_degraded = TRUE)
rec$summary
#> # A tibble: 5 × 3
#>   bucket  label                                   n
#>   <chr>   <chr>                               <int>
#> 1 A       Renewal, exact match                    0
#> 2 B       Renewal, fuzzy recovered                0
#> 3 C       New app, fuzzy matched                  0
#> 4 D       Truly new                               0
#> 5 unknown Not reconciled (degraded mode only)    16
```

With a real prior classroom panel:

``` r

panel <- readRDS("output/classroom/classroom_panel_2021-2025.rds")
rec <- applications_reconcile(ren, new,
                                 prior_classroom_panel = panel,
                                 fuzzy_threshold = 0.85,
                                 seed = 20260519L)
rec
#> <alprek_applications_reconciled>
#>   Prior school year: 2024-2025
#>   Fuzzy threshold:   0.85
#>   Inputs:  renewals=1495  new_apps=122
#>   Buckets: A=774 B=667 C=60 D=116
#>   Audit-log rows:    3438
```

Bucket semantics:

| Bucket | Meaning | Downstream action |
|----|----|----|
| **A** | Renewal, exact match on `(org, prior project, county)` | Carry existing `classroom_code` forward |
| **B** | Renewal, fuzzy ≥ threshold | Flag for analyst review |
| **C** | New app fuzzy-matched to an existing classroom | Likely an additional classroom at an existing program |
| **D** | No candidate ≥ threshold | Truly new — downstream geocoding required |

## Step 5 — Validate (18 base checks + linkage checks)

[`applications_validate()`](https://joonho112.github.io/ALprekDB/reference/applications_validate.md)
dispatches on input class:

``` r

v_ren  <- applications_validate(ren)
v_ren
#> <alprek_applications_validation>
#>   Kind:    renewals
#>   Overall: PASSED
#>   Errors: 0 | Warnings: 0 | Info: 1
#> 
#>   Checks:
#>     [+] Required renewal columns present
#>     [+] No negative values in funding/award columns
#>     [+] tier_adjustment within +/- $50,000
#>     [+] draft_award = draft_base_award + tier_adjustment (within $1.00)
#>     [+] All county values are Alabama counties
#>     [+] funding_type/funding_type_prior values match codebook
#>     [+] region matches 'Region 1' .. 'Region 9'
#>     [+] process_name in status_codes codebook
#>     [+] File SHA-256 + per-row data_source recorded
#>     [+] raw_row_index + lineage_id + git_sha recorded
#>     [+] Cleaned data has at least one row
#>     [i] Counties represented (>= 30 expected statewide) -- only 7 distinct counties
```

For real cycle-1 data, the renewals sheet typically validates with 0
errors + 1 WARN (`funding_type_in_codebook` — see codebook drift in
`inst/extdata/codebooks/applications_funding_types.csv`).

The same function handles `alprek_applications_reconciled` and
`alprek_applications_linkage` objects (Step 9), adding linkage-specific
checks for unmatched bucket semantics, retained row lineage, and
diagnostic conservation.

## Step 6 — Transform (derived variables)

[`applications_transform()`](https://joonho112.github.io/ALprekDB/reference/applications_transform.md)
adds **data-layer** derived variables (no geocoding / ACS / Bayesian —
those are downstream). The result is an `alprek_applications_master`
carrying both **application-grain** and **capacity-grain** data.

``` r

mst <- applications_transform(rec, capacity_clean = cap)
mst
#> <alprek_applications_master>
#>   Cycle:        2026-2027
#>   Apps rows:    16 (35 cols)
#>   Capacity:     10 rows (13 cols)
#>   Derived log:  10 entries
#>   Tier bands:   $    0 / $2,550 / $3,570 / $4,590 / $5,610
#>   Transformed:  2026-05-20 12:44:50 UTC
```

Applications-grain columns added: `is_renewal`, `is_new`,
`cycle_year_std`, `applied_this_cycle`, `tier_prev_dollars`,
`tier_prev_rank` (1-5), `tier_prev_band` (`high`/`medium`/`low`).

Capacity-grain columns added: `capacity_utilization`, `waitlist_ratio`,
`is_oversubscribed`.

The `$derived_log` slot records every derivation:

``` r

mst$derived_log
#> # A tibble: 10 × 5
#>    variable             formula                             n_non_na  n_na note 
#>    <chr>                <chr>                                  <int> <int> <chr>
#>  1 is_renewal           source_sheet == 'renewals'                16     0 NA   
#>  2 is_new               source_sheet == 'new_apps'                16     0 NA   
#>  3 cycle_year_std       from meta$cycle_year                      16     0 NA   
#>  4 applied_this_cycle   TRUE per row (panel join semantics)       16     0 Diff…
#>  5 tier_prev_dollars    carry-forward = tier_adjustment           12     4 NA   
#>  6 tier_prev_rank       1..5 from tier_adjustment threshol…       12     4 Rank…
#>  7 tier_prev_band       high/medium/low from tier_prev_rank       12     4 NA   
#>  8 capacity_utilization if_else(capacity > 0 & !is.na(enro…       10     0 n_ze…
#>  9 waitlist_ratio       if_else(capacity > 0, waitlist/cap…       10     0 NA   
#> 10 is_oversubscribed    waitlist > 0 OR enrollment > capac…       10     0 n_ov…
```

## Step 7 — Multi-cycle panel binding

When you have two or more cycle masters,
[`applications_bind_years()`](https://joonho112.github.io/ALprekDB/reference/applications_bind_years.md)
stacks them into a long panel preserving both grains:

``` r

mst_2526 <- applications_transform(rec_2526, capacity_clean = cap_2526)
mst_2627 <- applications_transform(rec_2627, capacity_clean = cap_2627)
panel <- applications_bind_years(mst_2526, mst_2627)
panel
#> <alprek_applications_panel>
#>   Cycles:        2025-2026, 2026-2027
#>   Apps rows:     3210
#>   Capacity rows: 1640
#>   ...
```

`applications_track_classrooms(panel)` returns a wide tibble of
`(classroom_key × cycle_year)` boolean presence + `first_cycle` /
`last_cycle` — useful for tracking continuity, new entrants, exits.

## Step 8 — Export (5 formats)

``` r

applications_export_csv(mst,     "output/apps_2627.csv")
applications_export_parquet(mst, "output/apps_2627.parquet")
applications_export_excel(mst,   "output/apps_2627.xlsx")  # 3 sheets
applications_export_rds(mst,     "output/apps_2627.rds")
applications_export_stata(mst,   "output/apps_2627.dta")
```

The Excel export writes `Applications`, `Capacity`, and `Summary` sheets
in one workbook. CSV / Parquet / Stata pick a single grain via the
`grain = "apps"` (default) or `"capacity"` argument. When `path = NULL`,
the export helpers create an `output/applications_<cycle>_<grain>.<ext>`
path, matching the rest of the package’s export convention.

## Step 9 — Linkage to classroom_panel

[`linkage_applications_classroom()`](https://joonho112.github.io/ALprekDB/reference/linkage_applications_classroom.md)
attaches the application context to your existing
`alprek_classroom_panel`. Each panel row gets columns like
`app_applied_this_cycle`, `app_bucket`, `app_tier_prev_band`,
`capacity_utilization`, and `site_n_new_apps`. Bucket A/B renewals join
directly by `matched_classroom_code`; bucket C new applications
aggregate at `matched_site_code`; bucket D rows (no classroom_code) are
kept in a separate `$unmatched_applications` slot for downstream
geocoding.

``` r

lk <- linkage_applications_classroom(mst, panel,
                                        target_school_year = "2024-2025")
lk
#> <alprek_applications_linkage>
#>   Cycle:                2026-2027
#>   Target school year:   2024-2025
#>   Classroom_level rows: 1493
#>   Unmatched apps:       116
#>   Capacity attached:    TRUE
#>   Diagnostics:
#>     n_classroom_rows               1493
#>     n_applications_in              1617
#>     n_matched_to_classroom         1269
#>     n_only_classroom               224
#>     n_applications_direct_classroom 1441
#>     n_applications_site_aggregated 60
#>     n_applications_accounted       1617
#>     n_only_application_unmatched   116
```

You can validate the linkage object the same way:

``` r

applications_validate(lk)
#> <alprek_applications_validation>
#>   Kind:    linkage
#>   Overall: PASSED
#>   ...
```

## Step 10 — DuckDB persistence

For long-term storage and SQL access, ALprekDB writes applications data
into the shared DuckDB file alongside budget / classroom / student
panels.

``` r

conn <- db_init("output/alprekdb.duckdb")

# Master (per cycle) → 4 tables:
db_write_applications_master(conn, mst)
#> ✔ Wrote 1617 rows to 'applications_clean'
#> ✔ Wrote 819 rows to 'applications_capacity'
#> ✔ Wrote 9 rows to 'applications_derived_log'
#> ✔ Lineage row appended to 'applications_lineage'

# Or the multi-cycle panel → 3 tables:
db_write_applications_panel(conn, panel, overwrite = TRUE)

# Read back as native S3:
mst_back   <- db_read_applications_master(conn, cycle_year = "2026-2027")
panel_back <- db_read_applications_panel(conn)

DBI::dbDisconnect(conn, shutdown = TRUE)
```

The shared `_alprek_column_types` registry preserves column types
(integer, factor levels, etc.) across the round-trip.

## What this module does NOT do

| Concern | Where it lives |
|----|----|
| Geocoding (3-source consensus, OSRM isochrone) | Future `ALprekGeocode` package |
| ACS area-weighted aggregation | Future `ALprekACS` package |
| Bayesian SAE of economic-need tiers | Future `ALprekSAE` package |
| Tier binning (`ntile(gr, 6)`) | Same — downstream of SAE |

This split is intentional: the data contract layer above must be stable
and well-tested before spatial / statistical layers consume it.

## See also

- [`vignette("a1-getting-started", package = "ALprekDB")`](https://joonho112.github.io/ALprekDB/articles/a1-getting-started.md)
  — package overview
- [`vignette("a3-linkage-analysis", package = "ALprekDB")`](https://joonho112.github.io/ALprekDB/articles/a3-linkage-analysis.md)
  — broader linkage patterns this module integrates with
- [`vignette("a4-duckdb-sql", package = "ALprekDB")`](https://joonho112.github.io/ALprekDB/articles/a4-duckdb-sql.md)
  — DuckDB schema + SQL examples
- [`vignette("m2-validation-framework", package = "ALprekDB")`](https://joonho112.github.io/ALprekDB/articles/m2-validation-framework.md)
  — how
  [`applications_validate()`](https://joonho112.github.io/ALprekDB/reference/applications_validate.md)
  mirrors the package-wide framework
