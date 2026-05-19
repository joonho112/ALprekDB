# M1 - Architecture Trilemma

## Overview

The methodological track explains the design choices behind ALprekDB.
The applied track shows how to run the package; the methodological track
explains why the package is organized this way.

M1 starts with the core architecture problem from the companion PLOS One
manuscript: confidential administrative data processing has to satisfy
three goals at once.

| Goal | What it requires | What can go wrong |
|----|----|----|
| Transparency | Processing rules, validation checks, and documentation are inspectable. | Open code can accidentally expose assumptions about private data. |
| Consistency | The same rules run across years, users, and machines. | Ad hoc scripts drift as formats and personnel change. |
| Privacy | Confidential records remain local and protected. | Logs, examples, and exports can reveal row-level information. |

ALprekDB’s answer is a public-code, private-data architecture. The
package publishes processing logic, codebooks, tests, documentation, and
synthetic examples. Real ADECE records stay outside the package
repository and outside public examples.

``` r

library(ALprekDB)
```

## Two Documentation Tracks

The pkgdown site uses two tracks because package users and reviewers
need different levels of detail.

| Track | Vignettes | Reader question |
|----|----|----|
| Applied | A1-A5 | How do I build panels, link records, query DuckDB, and run the workflow? |
| Methodological | M1-M4 | Why is the package designed this way, and what contracts does it enforce? |

M1 replaces the older architecture article. Detailed column dictionaries
now belong in codebook documentation and reference pages; this article
focuses on the architectural reasoning that should remain stable even
when specific column counts or source formats change.

## Five Design Principles

The architecture is not one mechanism. It is a set of mechanisms that
cover different sides of the trilemma.

| Principle | Main package mechanism | Trilemma role |
|----|----|----|
| Externalize decisions | CSV codebooks and mapping files in `inst/extdata/` | Rules are readable outside R and versioned with code. |
| Validate at each stage | Module-specific validation objects | Quality evidence is structured and repeatable. |
| Separate code from data | Public package source, private data folders | Public review does not require public records. |
| Demonstrate with synthetic data | Synthetic budget, classroom, and student generators | Examples and tests run without confidential inputs. |
| Orchestrate reproducibly | Optional `targets` template | Processing order and cache invalidation are explicit. |

The point is architectural coverage: no single feature solves
transparency, consistency, and privacy by itself, but the combination
makes the workflow auditable, repeatable, and safe enough for public
documentation.

## Codebooks as Boundary Objects

ALprekDB stores two kinds of public reference tables under
`inst/extdata/`:

- column mappings, which translate source-file columns into standardized
  names;
- codebooks, which define delivery types, counties, degree patterns,
  race and language mappings, and budget category groups.

The installed package can report this inventory without touching real
data.

``` r

ext_dir <- system.file("extdata", package = "ALprekDB", mustWork = TRUE)
ext_files <- list.files(
  ext_dir,
  pattern = "[.]csv$",
  recursive = TRUE,
  full.names = TRUE
)

inventory <- data.frame(
  role = ifelse(grepl("/mappings/", ext_files), "column mapping", "codebook"),
  entries = vapply(
    ext_files,
    function(path) nrow(utils::read.csv(path, check.names = FALSE)),
    integer(1)
  )
)

aggregate(
  cbind(files = rep(1L, nrow(inventory)), entries = inventory$entries),
  by = list(role = inventory$role),
  FUN = sum
)
#>             role files entries
#> 1       codebook     8     225
#> 2 column mapping     6     715
```

This design matters for collaboration. A mapping can be inspected as a
CSV file by program staff and loaded by R code during processing. When a
source format changes, the first response is usually a new mapping and a
detection rule, not an undocumented rewrite of downstream analysis code.

## Processing Architecture

The package has three module lanes that converge into linkage and
optional storage.

``` text
Budget sources     -> read -> clean -> validate -> budget panel
Classroom sources  -> read -> clean -> validate -> classroom panel
Student sources    -> read -> clean -> validate -> transform -> student panel

budget panel + classroom panel + student panel
  -> coverage-aware linkage
  -> classroom-level and student-level master objects
  -> validation summaries, aggregate diagnostics, exports, and DuckDB storage
```

The public examples below use synthetic objects only.

``` r

budget <- alprek_synthetic_budget(
  n_classrooms = 12,
  n_years = 2,
  seed = 42
)

classroom <- alprek_synthetic_classroom(
  n_classrooms = 12,
  n_years = 3,
  seed = 42
)

student <- alprek_synthetic_student(
  n_students = 60,
  n_classrooms = 12,
  n_years = 3,
  seed = 42
)

student <- suppressMessages(student_transform(student))
master <- suppressMessages(linkage_create_master(budget, classroom, student))

data.frame(
  object = c("budget", "classroom", "student", "master_classroom", "master_student"),
  class = c(
    class(budget)[1],
    class(classroom)[1],
    class(student)[1],
    class(master$classroom_level)[1],
    class(master$student_level)[1]
  ),
  rows = c(
    nrow(budget$data),
    nrow(classroom$data),
    nrow(student$data),
    nrow(master$classroom_level),
    nrow(master$student_level)
  )
)
#>             object                  class rows
#> 1           budget    alprek_budget_panel   24
#> 2        classroom alprek_classroom_panel   36
#> 3          student   alprek_student_panel  180
#> 4 master_classroom                 tbl_df   36
#> 5   master_student                 tbl_df  180
```

The S3 classes are part of the consistency contract. Processing
functions expect typed inputs and return typed outputs, so a later stage
can detect when an earlier stage was skipped or replaced with a plain
data frame.

## Coverage-Aware Linkage

The v0.6.0 workflow explicitly allows module coverage to differ by year.
In the private real-data template, canonical budget coverage runs
through 2024-2025, while classroom and student coverage run through
2025-2026. The missing 2025-2026 budget is structural: it is not
inferred, copied forward, or zero-filled.

The synthetic generators currently cover years through 2024-2025. The
example below uses an earlier synthetic year to demonstrate the same
missing-budget logic without real files.

``` r

master$diagnostics$coverage$by_year[, c(
  "school_year",
  "has_budget",
  "has_classroom",
  "has_student",
  "budget_status"
)]
#>           school_year has_budget has_classroom has_student  budget_status
#> 2021-2022   2021-2022       TRUE          TRUE        TRUE      available
#> 2022-2023   2022-2023       TRUE          TRUE        TRUE      available
#> 2023-2024   2023-2024      FALSE          TRUE        TRUE missing_budget
```

Coverage diagnostics let the package distinguish an expected module gap
from a true join failure. That distinction is central to longitudinal
administrative data work: a row retained with missing budget fields is
different from a row that failed to match because of a key problem.

## Validation as Aggregate Transparency

Validation objects summarize evidence about data quality without
printing row-level records in public documentation.

``` r

validation <- suppressMessages(linkage_validate(master))

data.frame(
  passed = validation$passed,
  n_errors = validation$n_errors,
  n_warnings = validation$n_warnings,
  n_info = validation$n_info,
  n_checks = nrow(validation$checks)
)
#>   passed n_errors n_warnings n_info n_checks
#> 1   TRUE        0          0      3       13
```

For private workflows, the same pattern applies: validation can report
counts, statuses, and aggregate rates while keeping individual records
out of logs, vignettes, and public issue reports.

## Classroom Codes

Classroom codes are join keys and should be parsed with package
utilities rather than by hand. Public examples should use fake `9xx`
prefixes and fabricated six-digit program codes.

``` r

parse_classroom_codes(c(
  "901P900001.01",
  "967H900002.02",
  "933C900003.03"
))
#> # A tibble: 3 × 5
#>   county_code delivery_type_code program_code class_num delivery_type     
#>   <chr>       <chr>              <chr>        <chr>     <chr>             
#> 1 901         P                  900001       01        Public School     
#> 2 967         H                  900002       02        Head Start        
#> 3 933         C                  900003       03        Private Child Care
```

The delivery-type code map is maintained in the package utility layer
and used consistently across modules.

## Private Workflow Boundary

Private real-data processing is intentionally opt-in. The targets
template uses environment variables to separate public synthetic runs
from local confidential runs.

``` r

Sys.setenv(
  ALPREKDB_RUN_REALDATA = "1",
  ALPREKDB_DATA_DIR = "/private/path/to/ADECE/source/files",
  ALPREKDB_OUTPUT_DIR = "output/alprekdb",
  ALPREKDB_WRITE_OUTPUTS = "0"
)
```

In real-data mode, row-level RDS outputs and DuckDB files are disabled
by default. Student processing defaults to `include_pii = FALSE`, and
classroom processing defaults to `include_dob = FALSE`. Local teams can
opt into row-level outputs only in a private workspace:

``` r

Sys.setenv(ALPREKDB_WRITE_OUTPUTS = "1")
```

That switch is an operational decision, not a publication decision.
Public documentation should stay synthetic or aggregate.

## What This Architecture Does Not Promise

ALprekDB is a processing package, not a data-sharing system. The
architecture does not:

- distribute ADECE source data;
- make confidential records public or anonymous by itself;
- guarantee cryptographic privacy;
- impute unavailable budget years;
- validate every substantive modeling decision an analyst might make;
- guarantee fixed column counts across future source formats;
- treat synthetic data as estimates from the real program.

Those limits are part of the design contract. The package makes
processing logic public and repeatable while leaving data governance,
secure storage, and data access decisions with the authorized
institutions.

## Reading Map

After M1, the remaining methodological vignettes unpack specific parts
of the architecture:

| Vignette | Focus                                                       |
|----------|-------------------------------------------------------------|
| M2       | Validation framework and severity levels                    |
| M3       | Codebooks, mappings, and format-detection contracts         |
| M4       | Privacy, provenance, and public/private workflow boundaries |
