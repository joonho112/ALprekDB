# ALprekDB

**ALprekDB turns Alabama First Class Pre-K administrative files into
validated, analysis-ready panel datasets while keeping raw data out of
public code.**

ALprekDB is a modular R package for ADECE budget, classroom, and student
records. It handles file-format detection, data-driven codebooks,
validation diagnostics, derived student variables, cross-module linkage,
DuckDB storage, and export helpers. Public examples use fully synthetic
data. Real ADECE data workflows are opt-in, local-only, and designed for
private analysis projects.

## Author

JoonHo Lee, Ph.D.  
Assistant Professor, The University of Alabama  
<jlee296@ua.edu>

## Why ALprekDB?

ALprekDB is built for repeatable Pre-K administrative data work:

- standardize annual ADECE Excel files across changing source formats;
- build longitudinal budget, classroom, and student panels;
- create classroom- and student-level linked master datasets;
- integrate commercial-grade geocoding via Melissa.com deliveries with
  documented reconciliation and follow-up queues;
- record validation, linkage, orphan, and coverage diagnostics;
- support SQL-friendly DuckDB outputs for downstream analysis;
- separate public package documentation from private real-data
  processing.

## Data Coverage in v0.8.0

The v0.8.0 real-data manifest uses asymmetric coverage because the
currently available source set spans different ranges per module.

| Module | Covered school years | Current release notes |
|----|----|----|
| Budget | 2021-22 through 2024-25 | The 2025-26 budget is structurally unavailable and is not inferred, zero-filled, or copied from another source. |
| Classroom | 2021-22 through 2025-26 | Classroom-code validation uses six-digit program codes. |
| Student | 2021-22 through 2025-26 | Student PII is excluded by default in private workflows. |
| Applications (new in v0.7.0) | Cycle-1 (2026-2027) | Reads renewals / new / non-renewals / capacity sheets from the ADECE annual workbook. |
| **Geocode** (new in v0.8.0) | **Melissa delivery 2026-03-04** | Single commercial-grade Melissa.com geocoding delivery reconciled against ADECE classroom coordinates with a documented decision matrix and follow-up queue. **Out-of-scope this release:** multi-source consensus, ACS area-weighted aggregation, OSRM isochrone, Bayesian small-area tier estimation, and live geocoding API calls — those are deferred to downstream releases. |

Aggregate real-data smoke tests currently cover 5,867 budget
classroom-year records, 7,409 classroom-year records, 116,689
student-year records, and 1,617 cycle-1 application rows (1,495
renewals + 122 new) plus 819 site-level capacity rows. These are
aggregate processing counts, not row-level data.

## Installation

``` r

# From GitHub
remotes::install_github("joonho112/ALprekDB")
```

## Quick Start: Synthetic Data

Synthetic examples require no ADECE files and are safe for public
documentation, CI checks, teaching, and package development. They use
fake `9xx` classroom-code prefixes and synthetic county labels so
printed examples cannot be mistaken for confidential ADECE records.

``` r

library(ALprekDB)

budget <- alprek_synthetic_budget(
  n_classrooms = 20,
  n_years = 2,
  seed = 42
)

classroom <- alprek_synthetic_classroom(
  n_classrooms = 20,
  n_years = 2,
  seed = 42
)

student <- alprek_synthetic_student(
  n_students = 100,
  n_classrooms = 20,
  n_years = 2,
  seed = 42
)

master <- linkage_create_master(budget, classroom, student)
linkage_validate(master)
linkage_summary_stats(master)
```

## Quick Start: Applications Module

The applications module starts from ADECE’s annual
classroom-applications workbook and keeps ACS aggregation and Bayesian
tier estimation outside this package. Geocoding is handled by the v0.8.0
geocode module. For a private cycle-1 run, pair the applications
workbook with an existing classroom panel:

``` r

path <- Sys.getenv("ALPREKDB_APPLICATIONS_FILE")

ren <- applications_clean(
  applications_read_renewals(path, cycle_year = "2026-2027",
                             receipt_date = "2026-04-20")
)
new <- applications_clean(
  applications_read_new(path, cycle_year = "2026-2027",
                        receipt_date = "2026-04-20")
)
cap <- applications_clean(
  applications_read_capacity(path, cycle_year = "2026-2027",
                             receipt_date = "2026-04-20")
)

rec <- applications_reconcile(ren, new, prior_classroom_panel = classroom_panel)
applications_validate(rec)

mst <- applications_transform(rec, capacity_clean = cap)
lk <- linkage_applications_classroom(mst, classroom_panel,
                                     target_school_year = rec$meta$prior_school_year)
applications_validate(lk)
```

For a public synthetic walkthrough that does not require ADECE files,
see
[`vignette("a6-applications-intake", package = "ALprekDB")`](https://joonho112.github.io/ALprekDB/articles/a6-applications-intake.md).

## Quick Start: Geocode Module

The geocode module can be exercised with synthetic Melissa-shaped data.
This round-trips through an in-memory workbook so the public workflow
uses the same
[`geocode_read()`](https://joonho112.github.io/ALprekDB/reference/geocode_read.md)
entry point as a private Melissa delivery.

``` r

library(ALprekDB)

path <- tempfile(fileext = ".xlsx")
openxlsx::write.xlsx(
  alprek_synthetic_geocode(n_sites = 5, n_years = 1, seed = 42),
  path,
  sheetName = "Sheet1"
)

raw <- geocode_read(
  path,
  cycle_year = "2026-2027",
  receipt_date = as.Date("2026-03-04")
)
clean <- geocode_clean(raw)
geocode_validate(clean)

cfg <- geocode_config(
  path = path,
  cycle_year = "2026-2027",
  delivery_date = "2026-03-04",
  verbose = FALSE
)
rec <- geocode_reconcile(clean, config = cfg)
geo_master <- geocode_transform(rec, config = cfg)
geo_panel <- geocode_bind_years(geo_master)
```

For a full quality-control walkthrough, see
[`vignette("a7-geocoding-quality", package = "ALprekDB")`](https://joonho112.github.io/ALprekDB/articles/a7-geocoding-quality.md).

## Private Real-Data Workflow

Raw ADECE files are not distributed with ALprekDB and should not be
committed to GitHub, included in package builds, or rendered into public
pkgdown pages. For private processing, keep raw files in a local source
directory such as `ORIGINAL-DATA/` or another directory referenced by
`ALPREKDB_DATA_DIR`.

The package includes a `targets` workflow template:

``` r

template_dir <- system.file("templates", "targets", package = "ALprekDB")

dir.create("alprekdb-private-workflow", showWarnings = FALSE)
file.copy(
  list.files(template_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE),
  "alprekdb-private-workflow",
  recursive = TRUE
)
```

From the private workflow directory, configure paths with environment
variables:

``` sh
export ALPREKDB_RUN_REALDATA=1
export ALPREKDB_DATA_DIR="/path/to/local/ADECE/source/files"
export ALPREKDB_OUTPUT_DIR="output/alprekdb"
```

Then run:

``` r

targets::tar_make()
```

The private workflow keeps student PII out of processed student panels
by default. Row-level RDS outputs and DuckDB writes require an explicit
local opt-in:

``` sh
export ALPREKDB_WRITE_OUTPUTS=1
```

## Modules

| Module | Purpose | Main outputs |
|----|----|----|
| Budget | Read, clean, validate, reshape, and bind per-classroom funding records. | Long budget records and multi-year budget panels. |
| Classroom | Read, clean, validate, and bind classroom characteristics, geography, and staffing records. | Multi-year classroom panels. |
| Student | Read, clean, validate, and bind child-level enrollment, demographics, assessment, attendance, and service records. | Multi-year student panels with PII excluded by default. |
| Applications | Read, detect, clean, reconcile, validate, transform, export, and persist annual ADECE classroom applications. | Application-grain master data, capacity-grain data, reconciliation audit logs, linkage outputs, and DuckDB tables. |
| Geocode | Read, detect, compare, clean, validate, reconcile, transform, bind, export, and persist Melissa.com commercial-grade geocoding deliveries against ADECE classroom coordinates. | Reconciled site-level coordinates, follow-up queue, multi-run geocode panels, linkage outputs (classroom + applications), and DuckDB tables. |
| Transform | Derive analytic student measures such as gains, readiness, chronic absence, and risk indicators. | Enriched student panels. |
| Linkage | Join budget, classroom, student, geocode, and applications panels with explicit orphan and coverage diagnostics. | Classroom-level and student-level master datasets. |
| Database | Persist panels and master datasets in DuckDB for SQL analysis. | Local DuckDB databases and query results. |

## Pipeline Architecture

![ALprekDB public synthetic examples and private real-data
workflow](reference/figures/figure_01_readme.png)

The diagram separates the public package surface from the private
analysis workflow: public examples, tests, vignettes, and pkgdown are
driven by synthetic data, while local ADECE Excel files remain opt-in
inputs for private validation, linkage, aggregate diagnostics, exports,
and optional DuckDB storage.

## Privacy and Provenance Guardrails

- Do not commit raw ADECE files, `_targets/` caches, row-level exports,
  local DuckDB databases, `output/geocode/` follow-up queues, or private
  output folders.
- Use
  [`alprek_synthetic_budget()`](https://joonho112.github.io/ALprekDB/reference/alprek_synthetic_budget.md),
  [`alprek_synthetic_classroom()`](https://joonho112.github.io/ALprekDB/reference/alprek_synthetic_classroom.md),
  and
  [`alprek_synthetic_student()`](https://joonho112.github.io/ALprekDB/reference/alprek_synthetic_student.md)
  for public examples; use
  [`alprek_synthetic_geocode()`](https://joonho112.github.io/ALprekDB/reference/alprek_synthetic_geocode.md)
  for public geocode examples. These generators use fake `9xx`
  classroom-code prefixes or `999P` site-code prefixes and synthetic
  county labels.
- Treat all real student-level outputs as confidential, even when direct
  PII columns are excluded.
- Keep real-data paths in environment variables rather than committed
  scripts.
- Report only aggregate diagnostics, validation summaries, and
  non-disclosive counts in public documentation.
- Preserve package version, source manifest coverage, validation
  summaries, and linkage diagnostics with private analytic outputs.

## Vignettes

### Applied Track

- [A1 - Getting
  started](https://joonho112.github.io/ALprekDB/articles/a1-getting-started.md)
- [A2 - Build
  panels](https://joonho112.github.io/ALprekDB/articles/a2-build-panels.md)
- [A3 - Linkage
  analysis](https://joonho112.github.io/ALprekDB/articles/a3-linkage-analysis.md)
- [A4 - DuckDB and
  SQL](https://joonho112.github.io/ALprekDB/articles/a4-duckdb-sql.md)
- [A5 - Targets
  workflow](https://joonho112.github.io/ALprekDB/articles/a5-targets-workflow.md)
- [A6 - Applications
  intake](https://joonho112.github.io/ALprekDB/articles/a6-applications-intake.md)
- [A7 - Geocoding
  quality](https://joonho112.github.io/ALprekDB/articles/a7-geocoding-quality.md)

### Methodological Track

- [M1 - Architecture
  trilemma](https://joonho112.github.io/ALprekDB/articles/m1-architecture-trilemma.md)
- [M2 - Validation
  framework](https://joonho112.github.io/ALprekDB/articles/m2-validation-framework.md)
- [M3 - Codebooks and
  mappings](https://joonho112.github.io/ALprekDB/articles/m3-codebooks-mappings.md)
- [M4 - Privacy and
  provenance](https://joonho112.github.io/ALprekDB/articles/m4-privacy-provenance.md)

## Classroom Code Format

Every classroom record is identified by a classroom code:
`CCCDNNNNNN.NN`

- `CCC` = county code, such as `001` through `067`;
- `D` = delivery type;
- `NNNNNN` = six-digit program code;
- `NN` = classroom number within site.

The example below uses fake `9xx` prefixes and `9xxxxx` program codes
for public documentation.

``` r

parse_classroom_codes(c("901P900001.01", "967H900002.02"))
```

Delivery type codes are defined by
[`alprek_delivery_types()`](https://joonho112.github.io/ALprekDB/reference/alprek_delivery_types.md):

| Code | Delivery type            |
|------|--------------------------|
| `P`  | Public School            |
| `C`  | Private Child Care       |
| `H`  | Head Start               |
| `O`  | Community Organization   |
| `F`  | Faith-Based Organization |
| `U`  | University Operated      |
| `S`  | Private School           |

## Known Limitations

- ALprekDB does not distribute or expose raw ADECE row-level records.
- Synthetic data are fabricated for demonstration and testing; they
  should not be interpreted as Alabama program estimates.
- The 2025-26 budget file is not available in the current source scope,
  so 2025-26 classroom and student records are retained with explicit
  missing budget coverage.
- Validation checks are advisory diagnostics, not a substitute for
  substantive review of analysis choices.
- Bucket D applications are retained without `matched_classroom_code` /
  `matched_site_code`; the v0.8.0 geocode module is the primary
  downstream path for wiring these against reconciled coordinates.
- The v0.8.0 geocode module reconciles a **single** Melissa.com delivery
  against ADECE coordinates; multi-source consensus, ACS aggregation,
  OSRM isochrone, Bayesian tier estimation, and live geocoding API calls
  remain out-of-scope and are deferred to downstream releases.
- `site_street` is PII-sensitive;
  [`geocode_followup_queue()`](https://joonho112.github.io/ALprekDB/reference/geocode_followup_queue.md)
  returns an in-memory queue with privacy attributes, and
  [`geocode_export_followup_queue()`](https://joonho112.github.io/ALprekDB/reference/geocode_export_followup_queue.md)
  writes `output/geocode/sites_needing_geocoding_<cycle>.csv` with an
  internal-use header and `internal_use = TRUE` by default.
- Real-data processing depends on local file access and the canonical
  source manifest; paths and outputs should remain outside public
  repositories.

## License

MIT
