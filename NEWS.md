# ALprekDB (development version)

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
