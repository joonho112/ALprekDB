# Changelog

## ALprekDB (development version)

## ALprekDB 0.7.0 (2026-05-19)

### Added — Applications module (new)

This release adds a new **applications module** for processing the ADECE
annual classroom-applications workbook (renewals, new applications,
non-renewals, and per-site capacity). The module follows the same
architecture as the existing budget, classroom, and student modules.

- New read functions for the four input kinds:
  [`applications_read_renewals()`](https://joonho112.github.io/ALprekDB/reference/applications_read_renewals.md),
  [`applications_read_new()`](https://joonho112.github.io/ALprekDB/reference/applications_read_new.md),
  [`applications_read_nonrenewal()`](https://joonho112.github.io/ALprekDB/reference/applications_read_nonrenewal.md),
  [`applications_read_capacity()`](https://joonho112.github.io/ALprekDB/reference/applications_read_capacity.md).
  Each captures file SHA-256, git SHA, receipt date, sheet, per-row
  `raw_row_index`, and a stable per-row `lineage_id` for downstream
  tracing.
- [`applications_detect_format()`](https://joonho112.github.io/ALprekDB/reference/applications_detect_format.md)
  distinguishes cycle-1 (2026-2027) and cycle-0 (2025-2026) formats by
  marker columns.
- [`applications_clean()`](https://joonho112.github.io/ALprekDB/reference/applications_clean.md)
  standardizes columns via codebook column maps (one per kind per
  cycle), parses currency-aware numerics, drops capacity report
  aggregate rows (`rule = "drop_capacity_aggregate"`), and filters
  Debugger Trace noise.
- [`applications_reconcile()`](https://joonho112.github.io/ALprekDB/reference/applications_reconcile.md)
  partitions every input row into one of four buckets — A (renewal
  exact-match), B (renewal fuzzy-recovered ≥ threshold), C (new
  fuzzy-matched to existing classroom), D (truly new) — using
  Jaro-Winkler similarity blocked by county. Every fuzzy decision plus
  up to three runner-up candidates is logged in `$reconciliation_log`
  (the **audit chain**, with `decision_source`, `decision_timestamp`,
  `decision_seed`, `candidate_classroom_code`, `candidate_site_code`,
  `candidate_rank`, `score_margin`).
- [`applications_validate()`](https://joonho112.github.io/ALprekDB/reference/applications_validate.md)
  runs 18 base data-contract checks across the four input kinds and
  reconciled objects, plus linkage-specific checks for classroom joins,
  unmatched buckets, row-lineage retention, and diagnostic conservation.
  Validation uses structured ERROR / WARN / INFO severity, row-level
  `$issues` accumulation, and fixture-backed regression tests.
- [`applications_transform()`](https://joonho112.github.io/ALprekDB/reference/applications_transform.md)
  adds data-layer derived variables (`is_renewal`, `is_new`,
  `cycle_year_std`, `applied_this_cycle`, `tier_prev_dollars`,
  `tier_prev_rank`, `tier_prev_band`, `capacity_utilization`,
  `waitlist_ratio`, `is_oversubscribed`) into a new
  `alprek_applications_master` S3 object.
- [`applications_bind_years()`](https://joonho112.github.io/ALprekDB/reference/applications_bind_years.md)
  stacks multiple cycles into an `alprek_applications_panel` preserving
  both applications-grain and capacity-grain rows.
  [`applications_track_classrooms()`](https://joonho112.github.io/ALprekDB/reference/applications_track_classrooms.md)
  summarizes cross-cycle classroom presence.
- Five export functions:
  [`applications_export_csv()`](https://joonho112.github.io/ALprekDB/reference/applications_export_csv.md),
  [`applications_export_parquet()`](https://joonho112.github.io/ALprekDB/reference/applications_export_parquet.md),
  [`applications_export_excel()`](https://joonho112.github.io/ALprekDB/reference/applications_export_excel.md),
  [`applications_export_rds()`](https://joonho112.github.io/ALprekDB/reference/applications_export_rds.md),
  [`applications_export_stata()`](https://joonho112.github.io/ALprekDB/reference/applications_export_stata.md).
- [`linkage_applications_classroom()`](https://joonho112.github.io/ALprekDB/reference/linkage_applications_classroom.md)
  joins applications context onto the existing `alprek_classroom_panel`,
  preserving row-level lineage, aggregating bucket C new applications at
  the matched site, and producing an `alprek_applications_linkage` S3
  with classroom-level rows plus an `$unmatched_applications` slot for
  bucket D rows that need downstream geocoding.
- DuckDB persistence:
  [`db_write_applications_master()`](https://joonho112.github.io/ALprekDB/reference/db_write_applications_master.md),
  [`db_read_applications_master()`](https://joonho112.github.io/ALprekDB/reference/db_read_applications_master.md),
  [`db_write_applications_panel()`](https://joonho112.github.io/ALprekDB/reference/db_write_applications_panel.md),
  [`db_read_applications_panel()`](https://joonho112.github.io/ALprekDB/reference/db_read_applications_panel.md).
  Adds six new DuckDB tables (`applications_clean`,
  `applications_capacity`, `applications_panel`,
  `applications_capacity_panel`, `applications_lineage`,
  `applications_derived_log`) that share the existing
  `_alprek_column_types` registry.
- [`alprek_synthetic_applications()`](https://joonho112.github.io/ALprekDB/reference/alprek_synthetic_applications.md)
  generates synthetic 4-kind ADECE applications input for vignettes,
  examples, and tests using the same fake-code conventions as the other
  synthetic generators.
- New codebooks under `inst/extdata/codebooks/`:
  `applications_source_manifest.csv`, `applications_status_codes.csv`
  (extended in this release with
  `2026 - 2027 First Class Pre-K New Classroom Application - Round 2`
  and `2026-27 First Class Pre-K New Classroom`),
  `applications_funding_types.csv` (extended with
  `New Classroom Funding`, `Reduced Enrollment`, and
  `Classroom Funding;Supplemental Funding`),
  `applications_edge_cases.csv` (17 documented edge cases).
- New column-map CSVs (cycle-1) under `inst/extdata/mappings/`:
  `applications_column_map_renewals_cycle1.csv`,
  `applications_column_map_new_cycle1.csv`,
  `applications_column_map_nonrenewals_cycle1.csv` (positional),
  `applications_column_map_capacity_cycle1.csv`.
- New A6 vignette:
  [`vignette("a6-applications-intake")`](https://joonho112.github.io/ALprekDB/articles/a6-applications-intake.md)
  — end-to-end walkthrough on synthetic data.
- `R/utils-provenance.R`:
  [`alprek_file_hash()`](https://joonho112.github.io/ALprekDB/reference/alprek_file_hash.md),
  [`alprek_git_sha()`](https://joonho112.github.io/ALprekDB/reference/alprek_git_sha.md),
  [`alprek_provenance_record()`](https://joonho112.github.io/ALprekDB/reference/alprek_provenance_record.md),
  internal `.alprek_lineage_id()` — shared provenance helpers used by
  the applications module.
- Application-specific tests cover validation, transform, panel, export,
  linkage, DuckDB persistence, and env-gated real-data smoke paths
  across seven test files (`test-applications-validate.R`,
  `test-applications-transform.R`, `test-applications-panel.R`,
  `test-applications-export.R`, `test-applications-linkage.R`,
  `test-applications-duckdb.R`, `test-realdata-integration.R`).

### Changed

- DESCRIPTION now imports `stringdist` (for Jaro-Winkler fuzzy matching)
  and `digest` (for stable per-row `lineage_id` hashes).
- `_pkgdown.yml` adds an “Applications Module (v0.7.0)” reference
  section and an A6 Applied Track entry.
- README adds Applications rows to the data-coverage and module tables,
  an applications quick-start, and an explicit out-of-scope declaration
  for geocoding, ACS, and Bayesian tier estimation.
- Reconciliation hardening: fuzzy matching scores normalized strings,
  no-panel reconciliation requires explicit degraded mode, capacity
  aggregate rows are dropped before the site-level layer, and stable
  row-level `lineage_id` values are propagated through read, clean,
  reconcile, transform, linkage, and DuckDB paths.

### Out-of-scope (planned downstream packages)

To keep this release focused on the data-contract layer, the following
are intentionally **not** in v0.7.0:

- Geocoding (three-source consensus, OSRM isochrone, ArcGIS fallback)
- ACS area-weighted aggregation (tidycensus, census tracts, MOE → SE)
- Bayesian small-area estimation of economic-need tiers
- Tier binning (`ntile(gr, 6)`)
- Posterior summary, credibility intervals
- Address parsing beyond text normalization, lat/lon validation

These are planned as separate `ALprek*` packages that will consume the
applications-module output via the lineage chain (`lineage_id` →
`application_id` → `matched_classroom_code` → `matched_site_code`).

### Known limitations

- Bucket D rows (truly new applications) have no
  `matched_classroom_code` and no `matched_site_code`. They are retained
  in
  [`linkage_applications_classroom()`](https://joonho112.github.io/ALprekDB/reference/linkage_applications_classroom.md)
  output’s `$unmatched_applications` slot for downstream geocoding to
  resolve.
- The `funding_type` and `process_name` codebooks now include cycle-1
  observed variants but may need further extension as ADECE varies
  labels across future cycles. Unknown labels emit a WARN, not an ERROR,
  by design.

## ALprekDB 0.6.0 (2026-05-19)

### Major changes

- Added coverage-aware linkage metadata for asymmetric module coverage.
  Classroom and student panels can now retain years without matching
  canonical budget coverage, and missing budget years are recorded as
  coverage metadata rather than imputed or zero-filled.
- Updated linkage validation to distinguish true overlap-year orphan
  records from expected missing-module coverage gaps.
- Added clearer linkage diagnostics, including per-year orphan summaries
  and overlap-year versus all-year match-rate reporting.
- Added reconstruction of coverage-aware linkage diagnostics when
  reading linked master outputs back from DuckDB.

### Workflow and privacy

- Added a reusable `targets` workflow template for synthetic examples
  and opt-in local real-data processing.
- Added environment-gated real-data tests and workflow paths so CI and
  public examples run on synthetic data by default.
- Added privacy/provenance guardrails for local source-data directories,
  output folders, DuckDB files, target caches, environment files, and
  working logs.
- Disabled row-level real-data output writing by default in the workflow
  template; row-level outputs require an explicit local opt-in.

### Data processing

- Updated classroom-code validation and synthetic classroom-code
  generation to use the canonical six-digit program-code format.
- Hardened budget source detection and reading against temporary lock
  files, marker-only sheets, ambiguous budget-version columns,
  request/application exports, interim snapshots, and noncanonical
  missing-year substitutes.
- Improved budget amount parsing for currency strings, missing-value
  labels, commas, and parenthetical negatives without base coercion
  warnings.
- Hardened student format detection against partial exports and improved
  parsing of household-size text and mixed values.
- Improved user-facing warning and progress messages to keep noisy parse
  details out of normal workflows while preserving actionable validation
  feedback.

### Database

- Preserved `POSIXct` columns during DuckDB write/read round trips.
- Improved
  [`db_read_master()`](https://joonho112.github.io/ALprekDB/reference/db_read_master.md)
  reconstruction of master objects, linkage coverage metadata, and
  validation diagnostics.
- Kept DuckDB and DBI optional so the package can still be used without
  database dependencies installed.

### Documentation

- Replaced the previous vignette set with two tracks: applied workflow
  vignettes for getting started, panel construction, linkage,
  DuckDB/SQL, and `targets`; and methodological vignettes for
  architecture, validation, codebooks/mappings, privacy, and provenance.
- Updated README and pkgdown home content around synthetic-first
  examples, private workflow boundaries, asymmetric release coverage,
  and known limitations.
- Updated pkgdown navigation and reference grouping so linkage
  diagnostics, exports, DuckDB helpers, codebooks, synthetic data, and
  display methods are easier to find.
- Audited the PLOS manuscript PDF link and kept only the curated
  site-level manuscript PDF exposed through pkgdown.

### Testing and CI

- Added GitHub Actions coverage for R CMD check with real-data execution
  disabled in CI.
- Added synthetic `targets` template smoke tests.
- Added tests for coverage-aware linkage, database diagnostic
  reconstruction, privacy defaults, template parsing, external-data
  schemas, canonical classroom code shapes, and source-format
  guardrails.

### Known limitations

- Raw ADECE records are not distributed with the package and must remain
  in private local workflows.
- The current real-data manifest has budget coverage through 2024-25 and
  classroom/student coverage through 2025-26; the absent 2025-26 budget
  source is treated as a documented coverage limitation.
- Validation warnings may reflect source-data quality issues or expected
  coverage gaps and still require substantive analyst review.
- Student-level outputs remain confidential even when direct PII columns
  are excluded.
- [`db_read_master()`](https://joonho112.github.io/ALprekDB/reference/db_read_master.md)
  reconstructs master outputs from stored master tables; exact
  persistence of budget-only right-side orphan rows is not guaranteed in
  this release.

## ALprekDB 0.5.0 (2026-02-09)

### New features

#### Database module (DuckDB)

- Added
  [`db_init()`](https://joonho112.github.io/ALprekDB/reference/db_init.md)
  and
  [`db_close()`](https://joonho112.github.io/ALprekDB/reference/db_close.md)
  for creating/opening DuckDB databases with schema versioning and
  validation.
- Added
  [`db_write_panel()`](https://joonho112.github.io/ALprekDB/reference/db_write_panel.md)
  and
  [`db_write_master()`](https://joonho112.github.io/ALprekDB/reference/db_write_master.md)
  to persist processed panel data and linked master datasets.
- Added
  [`db_write_year()`](https://joonho112.github.io/ALprekDB/reference/db_write_year.md)
  for incremental year-by-year data addition without full reprocessing;
  validates no duplicate years.
- Added
  [`db_read_panel()`](https://joonho112.github.io/ALprekDB/reference/db_read_panel.md)
  and
  [`db_read_master()`](https://joonho112.github.io/ALprekDB/reference/db_read_master.md)
  with full R type reconstruction (factor levels, Date, integer) via
  internal column type registry.
- Added
  [`db_list_tables()`](https://joonho112.github.io/ALprekDB/reference/db_list_tables.md),
  [`db_table_info()`](https://joonho112.github.io/ALprekDB/reference/db_table_info.md),
  and
  [`db_query()`](https://joonho112.github.io/ALprekDB/reference/db_query.md)
  for database inspection and arbitrary SQL queries.
- DuckDB and DBI are optional dependencies (`Suggests`); the package
  works without them.

#### Synthetic data generators

- Added
  [`alprek_synthetic_budget()`](https://joonho112.github.io/ALprekDB/reference/alprek_synthetic_budget.md),
  [`alprek_synthetic_classroom()`](https://joonho112.github.io/ALprekDB/reference/alprek_synthetic_classroom.md),
  and
  [`alprek_synthetic_student()`](https://joonho112.github.io/ALprekDB/reference/alprek_synthetic_student.md)
  for generating realistic panel data without confidential ADECE files.
- All three generators share classroom codes (via `seed`) so outputs are
  linkable with
  [`linkage_create_master()`](https://joonho112.github.io/ALprekDB/reference/linkage_create_master.md).

#### Vignettes

- Added “Getting Started with ALprekDB” vignette covering installation,
  codebooks, and a quick-start pipeline.
- Added “Building Multi-Year Panel Data” vignette for budget, classroom,
  and student panel construction workflows.
- Added “Cross-Module Linkage and Analysis” vignette demonstrating
  master dataset creation, transform enrichment, and DuckDB integration.
- Added “Package Architecture and Data Dictionary” vignette with S3
  class hierarchy, data dictionary, and all 37 validation checks.

### Package stats

- 85 exported functions, 38 R source files, 25 test files, ~954 tests.
- `R CMD check`: 0 errors, 0 warnings, 0 notes.

## ALprekDB 0.4.0 (2026-02-06)

### New features

#### Linkage module

- Added
  [`linkage_classroom_budget()`](https://joonho112.github.io/ALprekDB/reference/linkage_classroom_budget.md)
  to join classroom and budget panels by `school_year` +
  `classroom_code`, with automatic column deduplication (authoritative
  source strategy).
- Added
  [`linkage_student_classroom()`](https://joonho112.github.io/ALprekDB/reference/linkage_student_classroom.md)
  to join student and classroom panels.
- Added
  [`linkage_aggregate_students()`](https://joonho112.github.io/ALprekDB/reference/linkage_aggregate_students.md)
  to compute classroom-level summaries from student data (38 aggregate
  variables including demographics, GOLD gains, chronic absence rates,
  service density, and eDECA gains).
- Added
  [`linkage_create_master()`](https://joonho112.github.io/ALprekDB/reference/linkage_create_master.md)
  to produce a two-level master dataset: `$classroom_level` (~208
  columns) and `$student_level` (~445 columns), with derived
  `per_child_budget` and `per_seat_budget`.
- Added
  [`linkage_validate()`](https://joonho112.github.io/ALprekDB/reference/linkage_validate.md)
  with 8 diagnostic checks (match rate, orphans, key uniqueness, NA
  introduction, year coverage, region consistency).
- Added
  [`linkage_summary_stats()`](https://joonho112.github.io/ALprekDB/reference/linkage_summary_stats.md)
  for descriptive statistics on linked data.
- Added
  [`linkage_export_csv()`](https://joonho112.github.io/ALprekDB/reference/linkage_export_csv.md),
  [`linkage_export_excel()`](https://joonho112.github.io/ALprekDB/reference/linkage_export_excel.md),
  [`linkage_export_rds()`](https://joonho112.github.io/ALprekDB/reference/linkage_export_rds.md),
  [`linkage_export_stata()`](https://joonho112.github.io/ALprekDB/reference/linkage_export_stata.md),
  and
  [`linkage_export_parquet()`](https://joonho112.github.io/ALprekDB/reference/linkage_export_parquet.md).

#### Student transform enrichment

- Added
  [`student_transform()`](https://joonho112.github.io/ALprekDB/reference/student_transform.md)
  with 27 derived analysis variables:
  - 12 GOLD gain scores (6 domains x raw + scale)
  - 6 K-readiness transition indicators (Emerging -\> Accomplished)
  - 2 chronic absence variables (flag + percentage, configurable
    threshold)
  - 2 service density variables (`n_services`, `risk_index`)
  - 5 eDECA pre-post T-score gains
- Transform is opt-in with per-category toggles; preserves the input S3
  class.
- Extended
  [`linkage_aggregate_students()`](https://joonho112.github.io/ALprekDB/reference/linkage_aggregate_students.md)
  with 14 additional classroom-level aggregates derived from transform
  variables.

## ALprekDB 0.3.0 (2026-02-02)

### New features

#### Student module

- Added
  [`student_read()`](https://joonho112.github.io/ALprekDB/reference/student_read.md)
  with auto format detection (legacy 202-column vs new 270-column
  formats) and footer removal.
- Added
  [`student_clean()`](https://joonho112.github.io/ALprekDB/reference/student_clean.md)
  with 15 cleaning steps: demographics standardization, gross income
  parsing (dual-format: simple ranges and FPL-based), delivery type
  normalization, service indicator binary encoding, attendance
  correction (abs + extreme cap), IEP2 enhanced indicator derivation,
  and 4 assessment batteries (GOLD 6-domain, PPVT, eDECA pre/post, ASQ).
- Added
  [`student_validate()`](https://joonho112.github.io/ALprekDB/reference/student_validate.md)
  with 12 advisory checks.
- Added
  [`student_bind_years()`](https://joonho112.github.io/ALprekDB/reference/student_bind_years.md)
  for multi-year panel construction (no imputation; student attributes
  change yearly).
- Added
  [`student_track()`](https://joonho112.github.io/ALprekDB/reference/student_track.md)
  for tracking student presence across years.
- Added
  [`student_summary_stats()`](https://joonho112.github.io/ALprekDB/reference/student_summary_stats.md)
  for descriptive statistics.
- Added
  [`student_process()`](https://joonho112.github.io/ALprekDB/reference/student_process.md),
  [`student_process_years()`](https://joonho112.github.io/ALprekDB/reference/student_process_years.md),
  and
  [`student_config()`](https://joonho112.github.io/ALprekDB/reference/student_config.md)
  convenience functions.
- Added
  [`student_export_csv()`](https://joonho112.github.io/ALprekDB/reference/student_export_csv.md),
  [`student_export_excel()`](https://joonho112.github.io/ALprekDB/reference/student_export_excel.md),
  [`student_export_rds()`](https://joonho112.github.io/ALprekDB/reference/student_export_rds.md),
  [`student_export_stata()`](https://joonho112.github.io/ALprekDB/reference/student_export_stata.md),
  and
  [`student_export_parquet()`](https://joonho112.github.io/ALprekDB/reference/student_export_parquet.md).

#### New codebooks

- Added
  [`alprek_student_race_mapping()`](https://joonho112.github.io/ALprekDB/reference/alprek_student_race_mapping.md)
  for student race/ethnicity standardization (7 levels from cross-year
  variants).
- Added
  [`alprek_student_delivery_mapping()`](https://joonho112.github.io/ALprekDB/reference/alprek_student_delivery_mapping.md)
  for student delivery type normalization (handles 2024-25 mixed
  capitalization).

#### Data quality decisions

- DOB retained by default (`include_pii = FALSE` removes names, guardian
  info, and state/student IDs but preserves DOB for age analysis).
- eDECA Post-test scores preserved (unlike prior ad-hoc scripts that
  dropped them; 2024-25 has 95% Post data).
- Attendance: negative values corrected with
  [`abs()`](https://rdrr.io/r/base/MathFun.html); values \>180 days set
  to NA with warning.

## ALprekDB 0.2.0 (2026-01-27)

### New features

#### Classroom module

- Added
  [`classroom_read()`](https://joonho112.github.io/ALprekDB/reference/classroom_read.md)
  with auto format detection (legacy ~100-column vs new ~125-column
  formats).
- Added
  [`classroom_clean()`](https://joonho112.github.io/ALprekDB/reference/classroom_clean.md)
  with degree classification (8 credential levels), race/ethnicity
  normalization, experience calculation, and coordinate standardization.
- Added
  [`classroom_validate()`](https://joonho112.github.io/ALprekDB/reference/classroom_validate.md)
  with 10 advisory checks.
- Added
  [`classroom_bind_years()`](https://joonho112.github.io/ALprekDB/reference/classroom_bind_years.md)
  with forward-fill imputation for geographic coordinates and
  `year_first_funded` within site groups; imputation log tracks every
  change.
- Added
  [`classroom_process()`](https://joonho112.github.io/ALprekDB/reference/classroom_process.md),
  [`classroom_process_years()`](https://joonho112.github.io/ALprekDB/reference/classroom_process_years.md),
  and
  [`classroom_config()`](https://joonho112.github.io/ALprekDB/reference/classroom_config.md)
  convenience functions.
- Added
  [`classroom_export_csv()`](https://joonho112.github.io/ALprekDB/reference/classroom_export_csv.md),
  [`classroom_export_excel()`](https://joonho112.github.io/ALprekDB/reference/classroom_export_excel.md),
  [`classroom_export_rds()`](https://joonho112.github.io/ALprekDB/reference/classroom_export_rds.md),
  [`classroom_export_stata()`](https://joonho112.github.io/ALprekDB/reference/classroom_export_stata.md),
  and
  [`classroom_export_parquet()`](https://joonho112.github.io/ALprekDB/reference/classroom_export_parquet.md).

#### New codebooks

- Added
  [`alprek_degree_patterns()`](https://joonho112.github.io/ALprekDB/reference/alprek_degree_patterns.md)
  for teacher credential classification.
- Added
  [`alprek_race_mapping()`](https://joonho112.github.io/ALprekDB/reference/alprek_race_mapping.md)
  for race/ethnicity standardization.
- Added
  [`alprek_language_mapping()`](https://joonho112.github.io/ALprekDB/reference/alprek_language_mapping.md)
  for fluent language field cleaning.

## ALprekDB 0.1.0 (2026-01-22)

### New features

#### Budget module

- Added
  [`budget_read()`](https://joonho112.github.io/ALprekDB/reference/budget_read.md)
  with auto format detection (legacy ~176-column vs new ~28-column
  formats).
- Added
  [`budget_clean()`](https://joonho112.github.io/ALprekDB/reference/budget_clean.md)
  to transform raw Excel data to long-format intermediate with payroll
  tax proportional allocation (legacy) and Additional Funds 1 & 2
  aggregation.
- Added
  [`budget_validate()`](https://joonho112.github.io/ALprekDB/reference/budget_validate.md)
  with 7 checks including reconciliation diagnostics (\$1.00 tolerance).
- Added
  [`budget_transform()`](https://joonho112.github.io/ALprekDB/reference/budget_transform.md)
  for wide master with derived share variables.
- Added
  [`budget_bind_years()`](https://joonho112.github.io/ALprekDB/reference/budget_bind_years.md)
  for multi-year panel construction.
- Added
  [`budget_track_classrooms()`](https://joonho112.github.io/ALprekDB/reference/budget_track_classrooms.md)
  for tracking classroom presence across years.
- Added
  [`budget_summary_stats()`](https://joonho112.github.io/ALprekDB/reference/budget_summary_stats.md)
  for descriptive statistics.
- Added
  [`budget_process()`](https://joonho112.github.io/ALprekDB/reference/budget_process.md),
  [`budget_process_years()`](https://joonho112.github.io/ALprekDB/reference/budget_process_years.md),
  and
  [`budget_config()`](https://joonho112.github.io/ALprekDB/reference/budget_config.md)
  convenience functions.
- Added
  [`budget_export_csv()`](https://joonho112.github.io/ALprekDB/reference/budget_export_csv.md),
  [`budget_export_parquet()`](https://joonho112.github.io/ALprekDB/reference/budget_export_parquet.md),
  [`budget_export_excel()`](https://joonho112.github.io/ALprekDB/reference/budget_export_excel.md),
  and
  [`budget_export_rds()`](https://joonho112.github.io/ALprekDB/reference/budget_export_rds.md).

#### Foundation

- Package skeleton with `testthat`, MIT license, GitHub-ready structure.
- Data-driven codebook system with CSV files in `inst/extdata/`:
  [`alprek_category_groups()`](https://joonho112.github.io/ALprekDB/reference/alprek_category_groups.md),
  [`alprek_delivery_types()`](https://joonho112.github.io/ALprekDB/reference/alprek_delivery_types.md),
  [`alprek_county_codes()`](https://joonho112.github.io/ALprekDB/reference/alprek_county_codes.md).
- Utility functions:
  [`parse_classroom_code()`](https://joonho112.github.io/ALprekDB/reference/parse_classroom_code.md),
  [`parse_classroom_codes()`](https://joonho112.github.io/ALprekDB/reference/parse_classroom_codes.md),
  [`alprek_infer_school_year()`](https://joonho112.github.io/ALprekDB/reference/alprek_infer_school_year.md),
  [`alprek_clean_colnames()`](https://joonho112.github.io/ALprekDB/reference/alprek_clean_colnames.md).
- Messaging system: `msg_info()`, `msg_success()`, `msg_warn()`.
