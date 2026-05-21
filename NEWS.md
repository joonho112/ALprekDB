# ALprekDB (development version)

# ALprekDB 0.8.0 (2026-05-20)

## Added — Geocode module (new)

This release adds a new **geocode module** for integrating commercial-grade
Melissa.com address-geocoding deliveries with the existing classroom and
applications modules. The module follows the same architecture as the
budget, classroom, student, and applications modules.

* New R source files implement the full pipeline:
  `R/geocode-config.R`, `R/geocode-codebooks.R`, `R/geocode-read.R`,
  `R/geocode-detect.R`, `R/geocode-clean.R`, `R/geocode-validate.R`,
  `R/geocode-reconcile.R`, `R/geocode-transform.R`, `R/geocode-panel.R`,
  `R/geocode-export.R`, `R/utils-geo.R`, `R/linkage-geocode.R`,
  `R/db-geocode.R`, `R/data-synthetic-geocode.R` (14 files total).
* `geocode_read()` ingests Melissa delivery workbooks and captures
  file SHA-256, git SHA, receipt date, sheet, per-row `raw_row_index`,
  and a stable per-row `lineage_id`.
* `geocode_detect_format()` distinguishes Melissa delivery variants
  by marker columns. `geocode_compare_deliveries()` produces a
  delivery-to-delivery diff (row count, column drift, coordinate
  movement) so future Melissa runs can be reviewed against a
  documented baseline.
* `geocode_clean()` standardizes columns via the
  `geocode_column_map_melissa_v1` codebook and parses the Melissa
  RESULTCODE / STATUSCODE / ERRORCODE multi-value strings into
  validated character vectors.
* `geocode_validate()` runs 15 base data-contract checks including
  codebook-driven RESULTCODE and STATUSCODE membership, Alabama
  FIPS-county membership, and coordinate ranges. Validation uses
  structured ERROR / WARN / INFO severity, row-level `$issues`
  accumulation, and fixture-backed regression tests.
* `geocode_reconcile()` partitions every Melissa row against the
  ADECE classroom coordinates using a documented decision matrix
  (`Decision §11.x`): accept Melissa, retain ADECE, retain both,
  or flag for follow-up. `geocode_followup_queue()` extracts rows
  for manual review and attaches privacy attributes because
  `site_street` contains full PII-sensitive addresses.
* `geocode_transform()` derives `coord_model_status` (a
  model-readiness gate) and other run-level variables on a new
  `alprek_geocode_master` S3 object.
* `geocode_bind_years()` stacks multiple Melissa runs into an
  `alprek_geocode_panel`.
* Six export functions: `geocode_export_csv()`,
  `geocode_export_parquet()`, `geocode_export_excel()`,
  `geocode_export_rds()`, `geocode_export_stata()`, and
  `geocode_export_followup_queue()` (dedicated PII-aware exporter
  for the manual-review queue).
* `linkage_geocode_classroom()` joins reconciled geocode columns
  onto the existing `alprek_classroom_panel`, preserving row-level
  lineage and producing an `alprek_geocode_linkage_classroom` S3.
  `linkage_geocode_applications()` joins reconciled coordinates
  onto applications-master rows, enabling bucket-D follow-up paths.
* DuckDB persistence: `db_write_geocode_clean()`,
  `db_read_geocode_clean()`, `db_write_geocode_reconciled()`,
  `db_read_geocode_reconciled()`, `db_write_geocode_panel()`,
  `db_read_geocode_panel()`, `db_write_geocode_lineage()`,
  `db_read_geocode_lineage()`. Adds four new DuckDB tables that
  share the existing `_alprek_column_types` registry.
* `alprek_synthetic_geocode()` generates synthetic Melissa-shaped
  geocode rows for vignettes, examples, and tests using the same
  fake-code conventions as the other synthetic generators.
* `alprek_haversine_m()` computes great-circle distances in meters
  between coordinate pairs (used by reconciliation and tests).
* Seven new codebooks under `inst/extdata/codebooks/`:
  `geocode_column_map_melissa_v1.csv`,
  `melissa_resultcode_codes.csv`, `melissa_statuscode_codes.csv`,
  `melissa_errorcode_codes.csv`, `geocode_al_fips_counties.csv`,
  `geocode_source_manifest.csv`, and `geocode_edge_cases.csv`
  (18 documented edge cases). The six public reference codebooks
  are paired with loaders: `alprek_geocode_column_map()`,
  `alprek_geocode_resultcode_meaning()`,
  `alprek_geocode_statuscode_meaning()`,
  `alprek_geocode_errorcode_meaning()`,
  `alprek_geocode_al_fips_counties()`,
  `alprek_geocode_source_manifest()`.
  `geocode_edge_cases.csv` documents validation/reconciliation
  fixtures and is covered by extdata schema tests.
* New A7 vignette: `vignette("a7-geocoding-quality")` — end-to-end
  walkthrough on synthetic data, including delivery comparison,
  validation, reconciliation, follow-up queue export, and
  classroom-linkage diagnostics.
* Approximately 1,052 additional package test results relative to
  v0.7.0 cover read, format detection, cleaning, validation,
  reconciliation, transform, panel, export, linkage, DuckDB
  persistence, and end-to-end smoke paths.

## Changed

* `linkage_create_master()` signature is extended with two new
  optional arguments — `geocode = NULL` and `applications = NULL`.
  The change is **backward compatible**: calling with only the
  three v0.7.0 required panels produces output identical to v0.7.0.
* When `geocode` is supplied, `classroom_level` gains 12 prefixed
  `geocode_*` columns (the authoritative reconcile columns plus
  `geocode_run_id` and `geocode_lineage_id`). ADECE `latitude` /
  `longitude` columns are intentionally preserved alongside as an
  inspection escape-hatch.
* DESCRIPTION declares `geosphere` under `Suggests` (used by tests
  and by the haversine utility's optional fallback path).

## Out-of-scope (planned downstream releases)

To keep this release focused on integrating a single commercial
geocoding delivery into the data-contract layer, the following are
intentionally **not** in v0.8.0:

* Multi-source geocoding consensus
* ACS area-weighted aggregation (tidycensus, census tracts, MOE -> SE)
* OSRM isochrone and travel-time analysis
* Bayesian small-area estimation of economic-need tiers
* Live geocoding API calls from R

## Scope correction (from v0.7.0)

The v0.7.0 release notes declared geocoding "Out-of-scope (planned
downstream packages)" with three sub-bullets: "three-source consensus,
OSRM isochrone, ArcGIS fallback." With the arrival of commercial-grade
Melissa.com geocode coverage (delivery dated 2026-03-04), three-source
consensus is no longer required for the data-contract layer. v0.8.0
integrates the Melissa delivery directly into ALprekDB as the
geocode module. OSRM isochrone and multi-source consensus remain
out-of-scope and are deferred to downstream releases.

## Known limitations

* The current release reconciles against a **single** Melissa
  delivery. Multi-source consensus is not implemented; rows where
  Melissa and ADECE disagree are routed through the follow-up queue
  rather than resolved by a third source.
* Melissa rows whose `RESULTCODE` includes `GS03` (Postal-level
  geocode rather than rooftop) are always flagged for follow-up.
  Empirical median ADECE-Melissa disagreement at `GS03` rows is
  approximately 4 km, well above the configured rooftop threshold.
* `site_street` is the full street address and is PII-sensitive.
  `geocode_followup_queue()` returns an in-memory queue with
  privacy attributes; the dedicated
  `geocode_export_followup_queue()` exporter writes the internal-use
  CSV header and defaults to `internal_use = TRUE`.

## Empirical findings

* Median ADECE-Melissa distance across reconciled rows is
  approximately 102 m; 95% of rows are within approximately
  3.66 km; 33 rows fall above the 10 km gross-disagreement cutoff
  and are routed to the follow-up queue.
* Site-year geocode rows are 80.1% `model_ready` on the v1 Melissa
  delivery. Classroom-year master rows are 63.3% `model_ready`
  after linkage because the classroom panel includes rows without
  a matched Melissa site-year record; this triggers the default
  model-ready validation warning by design.

## External review

* Incorporated findings from an external review of the v0.8.0
  preview: added `coord_model_status` for downstream model-readiness
  gating; propagated row-level `lineage_id` across read, clean,
  reconcile, transform, panel, linkage, and DuckDB paths; switched
  RESULTCODE / STATUSCODE validation to codebook-driven membership
  rather than hard-coded lists; and tightened the follow-up queue's
  PII defaults.

# ALprekDB 0.7.0 (2026-05-19)

## Added — Applications module (new)

This release adds a new **applications module** for processing the
ADECE annual classroom-applications workbook (renewals, new
applications, non-renewals, and per-site capacity). The module follows
the same architecture as the existing budget, classroom, and student
modules.

* New read functions for the four input kinds:
  `applications_read_renewals()`, `applications_read_new()`,
  `applications_read_nonrenewal()`, `applications_read_capacity()`.
  Each captures file SHA-256, git SHA, receipt date, sheet,
  per-row `raw_row_index`, and a stable per-row `lineage_id` for
  downstream tracing.
* `applications_detect_format()` distinguishes cycle-1 (2026-2027) and
  cycle-0 (2025-2026) formats by marker columns.
* `applications_clean()` standardizes columns via codebook column
  maps (one per kind per cycle), parses currency-aware numerics,
  drops capacity report aggregate rows
  (`rule = "drop_capacity_aggregate"`), and filters Debugger Trace
  noise.
* `applications_reconcile()` partitions every input row into one of
  four buckets — A (renewal exact-match), B (renewal fuzzy-recovered
  ≥ threshold), C (new fuzzy-matched to existing classroom), D
  (truly new) — using Jaro-Winkler similarity blocked by county.
  Every fuzzy decision plus up to three runner-up candidates is
  logged in `$reconciliation_log` (the **audit chain**, with
  `decision_source`, `decision_timestamp`, `decision_seed`,
  `candidate_classroom_code`, `candidate_site_code`, `candidate_rank`,
  `score_margin`).
* `applications_validate()` runs 18 base data-contract checks across
  the four input kinds and reconciled objects, plus linkage-specific
  checks for classroom joins, unmatched buckets, row-lineage retention,
  and diagnostic conservation. Validation uses structured ERROR / WARN
  / INFO severity, row-level `$issues` accumulation, and fixture-backed
  regression tests.
* `applications_transform()` adds data-layer derived variables
  (`is_renewal`, `is_new`, `cycle_year_std`, `applied_this_cycle`,
  `tier_prev_dollars`, `tier_prev_rank`, `tier_prev_band`,
  `capacity_utilization`, `waitlist_ratio`, `is_oversubscribed`) into
  a new `alprek_applications_master` S3 object.
* `applications_bind_years()` stacks multiple cycles into an
  `alprek_applications_panel` preserving both applications-grain and
  capacity-grain rows. `applications_track_classrooms()` summarizes
  cross-cycle classroom presence.
* Five export functions: `applications_export_csv()`,
  `applications_export_parquet()`, `applications_export_excel()`,
  `applications_export_rds()`, `applications_export_stata()`.
* `linkage_applications_classroom()` joins applications context onto
  the existing `alprek_classroom_panel`, preserving row-level lineage,
  aggregating bucket C new applications at the matched site, and
  producing an `alprek_applications_linkage` S3 with classroom-level
  rows plus an `$unmatched_applications` slot for bucket D rows that
  need downstream geocoding.
* DuckDB persistence: `db_write_applications_master()`,
  `db_read_applications_master()`, `db_write_applications_panel()`,
  `db_read_applications_panel()`. Adds six new DuckDB tables
  (`applications_clean`, `applications_capacity`,
  `applications_panel`, `applications_capacity_panel`,
  `applications_lineage`, `applications_derived_log`) that share the existing
  `_alprek_column_types` registry.
* `alprek_synthetic_applications()` generates synthetic 4-kind ADECE
  applications input for vignettes, examples, and tests using the
  same fake-code conventions as the other synthetic generators.
* New codebooks under `inst/extdata/codebooks/`:
  `applications_source_manifest.csv`, `applications_status_codes.csv`
  (extended in this release with `2026 - 2027 First Class Pre-K New
  Classroom Application - Round 2` and `2026-27 First Class Pre-K
  New Classroom`), `applications_funding_types.csv` (extended with
  `New Classroom Funding`, `Reduced Enrollment`, and
  `Classroom Funding;Supplemental Funding`),
  `applications_edge_cases.csv` (17 documented edge cases).
* New column-map CSVs (cycle-1) under `inst/extdata/mappings/`:
  `applications_column_map_renewals_cycle1.csv`,
  `applications_column_map_new_cycle1.csv`,
  `applications_column_map_nonrenewals_cycle1.csv` (positional),
  `applications_column_map_capacity_cycle1.csv`.
* New A6 vignette: `vignette("a6-applications-intake")` — end-to-end
  walkthrough on synthetic data.
* `R/utils-provenance.R`: `alprek_file_hash()`, `alprek_git_sha()`,
  `alprek_provenance_record()`, internal `.alprek_lineage_id()` —
  shared provenance helpers used by the applications module.
* Application-specific tests cover validation, transform, panel,
  export, linkage, DuckDB persistence, and env-gated real-data smoke
  paths across seven test files
  (`test-applications-validate.R`,
  `test-applications-transform.R`,
  `test-applications-panel.R`,
  `test-applications-export.R`,
  `test-applications-linkage.R`,
  `test-applications-duckdb.R`,
  `test-realdata-integration.R`).

## Changed

* DESCRIPTION now imports `stringdist` (for Jaro-Winkler fuzzy
  matching) and `digest` (for stable per-row `lineage_id` hashes).
* `_pkgdown.yml` adds an "Applications Module (v0.7.0)" reference
  section and an A6 Applied Track entry.
* README adds Applications rows to the data-coverage and module tables,
  an applications quick-start, and an explicit out-of-scope declaration
  for geocoding, ACS, and Bayesian tier estimation.
* External review remediation is reflected in code and tests: fuzzy
  matching scores normalized strings, no-panel reconciliation requires
  explicit degraded mode, capacity aggregate rows are dropped before the
  site-level layer, and stable row-level `lineage_id` values are
  propagated through read, clean, reconcile, transform, linkage, and
  DuckDB paths.

## Out-of-scope (planned downstream packages)

To keep this release focused on the data-contract layer, the
following are intentionally **not** in v0.7.0:

* Geocoding (three-source consensus, OSRM isochrone, ArcGIS fallback)
* ACS area-weighted aggregation (tidycensus, census tracts, MOE → SE)
* Bayesian small-area estimation of economic-need tiers
* Tier binning (`ntile(gr, 6)`)
* Posterior summary, credibility intervals
* Address parsing beyond text normalization, lat/lon validation

These are planned as separate `ALprek*` packages that will consume
the applications-module output via the lineage chain
(`lineage_id` → `application_id` → `matched_classroom_code` →
`matched_site_code`).

## Known limitations

* Bucket D rows (truly new applications) have no
  `matched_classroom_code` and no `matched_site_code`. They are
  retained in `linkage_applications_classroom()` output's
  `$unmatched_applications` slot for downstream geocoding to
  resolve.
* The `funding_type` and `process_name` codebooks now include
  cycle-1 observed variants but may need further extension as
  ADECE varies labels across future cycles. Unknown labels emit a
  WARN, not an ERROR, by design.

# ALprekDB 0.6.0 (2026-05-19)

## Major changes

* Added coverage-aware linkage metadata for asymmetric module coverage. Classroom
  and student panels can now retain years without matching canonical budget
  coverage, and missing budget years are recorded as coverage metadata rather
  than imputed or zero-filled.
* Updated linkage validation to distinguish true overlap-year orphan records
  from expected missing-module coverage gaps.
* Added clearer linkage diagnostics, including per-year orphan summaries and
  overlap-year versus all-year match-rate reporting.
* Added reconstruction of coverage-aware linkage diagnostics when reading linked
  master outputs back from DuckDB.

## Workflow and privacy

* Added a reusable `targets` workflow template for synthetic examples and
  opt-in local real-data processing.
* Added environment-gated real-data tests and workflow paths so CI and public
  examples run on synthetic data by default.
* Added privacy/provenance guardrails for local source-data directories, output
  folders, DuckDB files, target caches, environment files, and working logs.
* Disabled row-level real-data output writing by default in the workflow
  template; row-level outputs require an explicit local opt-in.

## Data processing

* Updated classroom-code validation and synthetic classroom-code generation to
  use the canonical six-digit program-code format.
* Hardened budget source detection and reading against temporary lock files,
  marker-only sheets, ambiguous budget-version columns, request/application
  exports, interim snapshots, and noncanonical missing-year substitutes.
* Improved budget amount parsing for currency strings, missing-value labels,
  commas, and parenthetical negatives without base coercion warnings.
* Hardened student format detection against partial exports and improved parsing
  of household-size text and mixed values.
* Improved user-facing warning and progress messages to keep noisy parse details
  out of normal workflows while preserving actionable validation feedback.

## Database

* Preserved `POSIXct` columns during DuckDB write/read round trips.
* Improved `db_read_master()` reconstruction of master objects, linkage
  coverage metadata, and validation diagnostics.
* Kept DuckDB and DBI optional so the package can still be used without database
  dependencies installed.

## Documentation

* Replaced the previous vignette set with two tracks:
  applied workflow vignettes for getting started, panel construction, linkage,
  DuckDB/SQL, and `targets`; and methodological vignettes for architecture,
  validation, codebooks/mappings, privacy, and provenance.
* Updated README and pkgdown home content around synthetic-first examples,
  private workflow boundaries, asymmetric release coverage, and known
  limitations.
* Updated pkgdown navigation and reference grouping so linkage diagnostics,
  exports, DuckDB helpers, codebooks, synthetic data, and display methods are
  easier to find.
* Audited the PLOS manuscript PDF link and kept only the curated site-level
  manuscript PDF exposed through pkgdown.

## Testing and CI

* Added GitHub Actions coverage for R CMD check with real-data execution
  disabled in CI.
* Added synthetic `targets` template smoke tests.
* Added tests for coverage-aware linkage, database diagnostic reconstruction,
  privacy defaults, template parsing, external-data schemas, canonical classroom
  code shapes, and source-format guardrails.

## Known limitations

* Raw ADECE records are not distributed with the package and must remain in
  private local workflows.
* The current real-data manifest has budget coverage through 2024-25 and
  classroom/student coverage through 2025-26; the absent 2025-26 budget source
  is treated as a documented coverage limitation.
* Validation warnings may reflect source-data quality issues or expected
  coverage gaps and still require substantive analyst review.
* Student-level outputs remain confidential even when direct PII columns are
  excluded.
* `db_read_master()` reconstructs master outputs from stored master tables; exact
  persistence of budget-only right-side orphan rows is not guaranteed in this
  release.

# ALprekDB 0.5.0 (2026-02-09)

## New features

### Database module (DuckDB)
* Added `db_init()` and `db_close()` for creating/opening DuckDB databases with
  schema versioning and validation.
* Added `db_write_panel()` and `db_write_master()` to persist processed panel
  data and linked master datasets.
* Added `db_write_year()` for incremental year-by-year data addition without
  full reprocessing; validates no duplicate years.
* Added `db_read_panel()` and `db_read_master()` with full R type
  reconstruction (factor levels, Date, integer) via internal column type
  registry.
* Added `db_list_tables()`, `db_table_info()`, and `db_query()` for database
  inspection and arbitrary SQL queries.
* DuckDB and DBI are optional dependencies (`Suggests`); the package works
  without them.

### Synthetic data generators
* Added `alprek_synthetic_budget()`, `alprek_synthetic_classroom()`, and
  `alprek_synthetic_student()` for generating realistic panel data without
  confidential ADECE files.
* All three generators share classroom codes (via `seed`) so outputs are
  linkable with `linkage_create_master()`.

### Vignettes
* Added "Getting Started with ALprekDB" vignette covering installation,
  codebooks, and a quick-start pipeline.
* Added "Building Multi-Year Panel Data" vignette for budget, classroom, and
  student panel construction workflows.
* Added "Cross-Module Linkage and Analysis" vignette demonstrating master
  dataset creation, transform enrichment, and DuckDB integration.
* Added "Package Architecture and Data Dictionary" vignette with S3 class
  hierarchy, data dictionary, and all 37 validation checks.

## Package stats
* 85 exported functions, 38 R source files, 25 test files, ~954 tests.
* `R CMD check`: 0 errors, 0 warnings, 0 notes.


# ALprekDB 0.4.0 (2026-02-06)

## New features

### Linkage module
* Added `linkage_classroom_budget()` to join classroom and budget panels by
 `school_year` + `classroom_code`, with automatic column deduplication
  (authoritative source strategy).
* Added `linkage_student_classroom()` to join student and classroom panels.
* Added `linkage_aggregate_students()` to compute classroom-level summaries
  from student data (38 aggregate variables including demographics, GOLD gains,
  chronic absence rates, service density, and eDECA gains).
* Added `linkage_create_master()` to produce a two-level master dataset:
  `$classroom_level` (~208 columns) and `$student_level` (~445 columns),
  with derived `per_child_budget` and `per_seat_budget`.
* Added `linkage_validate()` with 8 diagnostic checks (match rate, orphans,
  key uniqueness, NA introduction, year coverage, region consistency).
* Added `linkage_summary_stats()` for descriptive statistics on linked data.
* Added `linkage_export_csv()`, `linkage_export_excel()`,
  `linkage_export_rds()`, `linkage_export_stata()`, and
  `linkage_export_parquet()`.

### Student transform enrichment
* Added `student_transform()` with 27 derived analysis variables:
  - 12 GOLD gain scores (6 domains x raw + scale)
  - 6 K-readiness transition indicators (Emerging -> Accomplished)
  - 2 chronic absence variables (flag + percentage, configurable threshold)
  - 2 service density variables (`n_services`, `risk_index`)
  - 5 eDECA pre-post T-score gains
* Transform is opt-in with per-category toggles; preserves the input S3 class.
* Extended `linkage_aggregate_students()` with 14 additional classroom-level
  aggregates derived from transform variables.


# ALprekDB 0.3.0 (2026-02-02)

## New features

### Student module
* Added `student_read()` with auto format detection (legacy 202-column vs new
  270-column formats) and footer removal.
* Added `student_clean()` with 15 cleaning steps: demographics standardization,
  gross income parsing (dual-format: simple ranges and FPL-based), delivery type
  normalization, service indicator binary encoding, attendance correction
  (abs + extreme cap), IEP2 enhanced indicator derivation, and 4 assessment
  batteries (GOLD 6-domain, PPVT, eDECA pre/post, ASQ).
* Added `student_validate()` with 12 advisory checks.
* Added `student_bind_years()` for multi-year panel construction (no imputation;
  student attributes change yearly).
* Added `student_track()` for tracking student presence across years.
* Added `student_summary_stats()` for descriptive statistics.
* Added `student_process()`, `student_process_years()`, and `student_config()`
  convenience functions.
* Added `student_export_csv()`, `student_export_excel()`,
  `student_export_rds()`, `student_export_stata()`, and
  `student_export_parquet()`.

### New codebooks
* Added `alprek_student_race_mapping()` for student race/ethnicity
  standardization (7 levels from cross-year variants).
* Added `alprek_student_delivery_mapping()` for student delivery type
  normalization (handles 2024-25 mixed capitalization).

### Data quality decisions
* DOB retained by default (`include_pii = FALSE` removes names, guardian info,
  and state/student IDs but preserves DOB for age analysis).
* eDECA Post-test scores preserved (unlike prior ad-hoc scripts that dropped
  them; 2024-25 has 95% Post data).
* Attendance: negative values corrected with `abs()`; values >180 days set to
  NA with warning.


# ALprekDB 0.2.0 (2026-01-27)

## New features

### Classroom module
* Added `classroom_read()` with auto format detection (legacy ~100-column vs
  new ~125-column formats).
* Added `classroom_clean()` with degree classification (8 credential levels),
  race/ethnicity normalization, experience calculation, and coordinate
  standardization.
* Added `classroom_validate()` with 10 advisory checks.
* Added `classroom_bind_years()` with forward-fill imputation for geographic
  coordinates and `year_first_funded` within site groups; imputation log
  tracks every change.
* Added `classroom_process()`, `classroom_process_years()`, and
  `classroom_config()` convenience functions.
* Added `classroom_export_csv()`, `classroom_export_excel()`,
  `classroom_export_rds()`, `classroom_export_stata()`, and
  `classroom_export_parquet()`.

### New codebooks
* Added `alprek_degree_patterns()` for teacher credential classification.
* Added `alprek_race_mapping()` for race/ethnicity standardization.
* Added `alprek_language_mapping()` for fluent language field cleaning.


# ALprekDB 0.1.0 (2026-01-22)

## New features

### Budget module
* Added `budget_read()` with auto format detection (legacy ~176-column vs new
  ~28-column formats).
* Added `budget_clean()` to transform raw Excel data to long-format
  intermediate with payroll tax proportional allocation (legacy) and Additional
  Funds 1 & 2 aggregation.
* Added `budget_validate()` with 7 checks including reconciliation diagnostics
  ($1.00 tolerance).
* Added `budget_transform()` for wide master with derived share variables.
* Added `budget_bind_years()` for multi-year panel construction.
* Added `budget_track_classrooms()` for tracking classroom presence across years.
* Added `budget_summary_stats()` for descriptive statistics.
* Added `budget_process()`, `budget_process_years()`, and `budget_config()`
  convenience functions.
* Added `budget_export_csv()`, `budget_export_parquet()`,
  `budget_export_excel()`, and `budget_export_rds()`.

### Foundation
* Package skeleton with `testthat`, MIT license, GitHub-ready structure.
* Data-driven codebook system with CSV files in `inst/extdata/`:
  `alprek_category_groups()`, `alprek_delivery_types()`, `alprek_county_codes()`.
* Utility functions: `parse_classroom_code()`, `parse_classroom_codes()`,
  `alprek_infer_school_year()`, `alprek_clean_colnames()`.
* Messaging system: `msg_info()`, `msg_success()`, `msg_warn()`.
