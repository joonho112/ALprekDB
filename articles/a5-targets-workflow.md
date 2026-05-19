# A5 - Targets Workflow

## Overview

A1 through A4 introduce the ALprekDB object flow:

- A1 creates synthetic panels and a first linked master object;
- A2 builds and inspects budget, classroom, and student panels;
- A3 links the panels and reads coverage/orphan diagnostics;
- A4 stores and queries panels and master tables in DuckDB.

A5 wraps that same flow in a `targets` workflow template for repeatable
analysis. The template is shipped under `inst/templates/targets/`; it is
not a package-root `_targets.R` file. Copy it into a private analysis
project before running it.

The template has two modes:

- synthetic mode, the default, which needs no ADECE files;
- private real-data mode, which runs only when explicitly enabled with
  environment variables.

The real-data manifest encoded in the template follows the v0.6.0
coverage policy: budget covers 2021-22 through 2024-25; classroom and
student cover 2021-22 through 2025-26; the 2025-26 budget is
structurally unavailable and is not zero-filled.

``` r

library(ALprekDB)
```

## Template Files

The installed template contains three main files.

``` r

template_dir <- system.file("templates", "targets", package = "ALprekDB")
list.files(template_dir, recursive = TRUE)
#> [1] "_targets.R"        "local.env.example" "R/functions.R"    
#> [4] "README.md"
```

| File | Role |
|----|----|
| `_targets.R` | Defines the target graph and switches between synthetic and real-data modes. |
| `R/functions.R` | Holds helper functions for configuration, manifests, processing, summaries, and outputs. |
| `local.env.example` | Shows optional environment variables for local private workflows. |

## Copy the Template

Copy the template into a private working folder. Do not run real-data
workflows from inside the package source tree.

``` r

template_dir <- system.file("templates", "targets", package = "ALprekDB")

dir.create("alprekdb-private-workflow", showWarnings = FALSE)
file.copy(
  list.files(template_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE),
  "alprekdb-private-workflow",
  recursive = TRUE
)
```

From the copied workflow folder, install `targets` if needed:

``` r

install.packages("targets")
```

DuckDB output also requires the optional database packages:

``` r

install.packages(c("duckdb", "DBI"))
```

## Synthetic Mode

Synthetic mode is the default and is safe for package checks, teaching,
and workflow rehearsal.

``` r

Sys.setenv(
  ALPREKDB_RUN_REALDATA = "0",
  ALPREKDB_SYNTHETIC_N_CLASSROOMS = "20",
  ALPREKDB_SYNTHETIC_N_STUDENTS = "100",
  ALPREKDB_SYNTHETIC_N_YEARS = "2",
  ALPREKDB_SYNTHETIC_SEED = "42"
)
```

``` r
# Shell alternative
export ALPREKDB_RUN_REALDATA=0
export ALPREKDB_SYNTHETIC_N_CLASSROOMS=20
export ALPREKDB_SYNTHETIC_N_STUDENTS=100
export ALPREKDB_SYNTHETIC_N_YEARS=2
export ALPREKDB_SYNTHETIC_SEED=42
```

Then run the workflow from the copied template directory:

``` r

targets::tar_make()
```

Synthetic mode builds these stable downstream targets:

``` text
workflow_config
synthetic_panels
budget_panel
classroom_panel
student_panel
linkage_master
validation_summary
linkage_summary
summary_files
rds_files
duckdb_file
```

In synthetic mode, row-level RDS and DuckDB outputs are allowed by
default because the data are fabricated.

## Real-Data Mode

Real-data mode is explicit. It requires a private source-data folder and
a private output folder.

``` r

Sys.setenv(
  ALPREKDB_RUN_REALDATA = "1",
  ALPREKDB_DATA_DIR = "/private/path/to/ADECE/source/files",
  ALPREKDB_OUTPUT_DIR = "output/alprekdb",
  ALPREKDB_WRITE_OUTPUTS = "0"
)
```

``` r
# Shell alternative
export ALPREKDB_RUN_REALDATA=1
export ALPREKDB_DATA_DIR="/private/path/to/ADECE/source/files"
export ALPREKDB_OUTPUT_DIR="output/alprekdb"
export ALPREKDB_WRITE_OUTPUTS=0
```

When `ALPREKDB_RUN_REALDATA=1`, the workflow fails early if
`ALPREKDB_DATA_DIR` does not exist or if canonical source files are
missing.

Real-data mode uses privacy-preserving defaults:

- student processing uses `include_pii = FALSE`;
- classroom processing uses `include_dob = FALSE`;
- row-level RDS outputs and DuckDB writes are disabled unless explicitly
  enabled;
- public documentation should report aggregate diagnostics, not
  row-level records.

## Output Policy

The template always writes aggregate summaries:

- `validation_summary.csv`
- `linkage_summary.csv`

In real-data mode, row-level RDS outputs and DuckDB output are disabled
by default. Disabled outputs are represented by marker files under the
output control directory.

To intentionally write row-level local outputs in a private project:

``` r

Sys.setenv(ALPREKDB_WRITE_OUTPUTS = "1")
```

``` r
export ALPREKDB_WRITE_OUTPUTS=1
```

Even with direct PII excluded, real student-level outputs remain
confidential.

## Target Graph Lifecycle

The target graph has the same analytic spine in both modes:

``` text
configuration
  -> source manifest or synthetic panels
  -> budget/classroom/student panels
  -> transformed student panel
  -> linkage master
  -> validation and linkage summaries
  -> summary files, optional RDS files, optional DuckDB file
```

Common `targets` commands:

``` r

targets::tar_manifest()
targets::tar_visnetwork()
targets::tar_make()
targets::tar_read(workflow_config)
targets::tar_read(validation_summary)
targets::tar_read(linkage_summary)
targets::tar_read(linkage_master)
targets::tar_outdated()
targets::tar_destroy()
```

Use `tar_destroy()` carefully. In real-data mode, `_targets/` may
contain cached row-level objects and should be treated as confidential
local output.

## Inspect the Template Without Running It

The helper file can be loaded in a clean environment to inspect
configuration logic. This does not run a target graph.

``` r

template_env <- new.env(parent = globalenv())
sys.source(file.path(template_dir, "R", "functions.R"), template_env)

withr::with_envvar(
  c(
    ALPREKDB_RUN_REALDATA = "0",
    ALPREKDB_SYNTHETIC_N_CLASSROOMS = "6",
    ALPREKDB_SYNTHETIC_N_STUDENTS = "24",
    ALPREKDB_SYNTHETIC_N_YEARS = "2",
    ALPREKDB_SYNTHETIC_SEED = "42"
  ),
  {
    template_cfg <- template_env$alprek_targets_config(project_dir = tempdir())
    data.frame(
      run_realdata = template_cfg$run_realdata,
      write_outputs = template_cfg$write_outputs,
      include_pii = template_cfg$include_pii,
      schema = template_cfg$schema,
      synthetic_n_classrooms = template_cfg$synthetic$n_classrooms,
      synthetic_n_students = template_cfg$synthetic$n_students,
      synthetic_n_years = template_cfg$synthetic$n_years,
      synthetic_seed = template_cfg$synthetic$seed
    )
  }
)
#>   run_realdata write_outputs include_pii                  schema
#> 1        FALSE          TRUE       FALSE 0.6.0-workflow-template
#>   synthetic_n_classrooms synthetic_n_students synthetic_n_years synthetic_seed
#> 1                      6                   24                 2             42
```

The real-data manifest can be inspected without requiring the files to
exist by setting `validate = FALSE`. This reveals the coverage policy
without reading raw data.

``` r

manifest <- template_env$alprek_targets_realdata_manifest(
  data_dir = tempdir(),
  validate = FALSE
)

canonical <- manifest[manifest$status == "canonical", ]

aggregate(
  school_year ~ module,
  data = canonical,
  FUN = function(x) paste(x, collapse = ", ")
)
#>      module                                           school_year
#> 1    budget            2021-2022, 2022-2023, 2023-2024, 2024-2025
#> 2 classroom 2021-2022, 2022-2023, 2023-2024, 2024-2025, 2025-2026
#> 3   student 2021-2022, 2022-2023, 2023-2024, 2024-2025, 2025-2026
```

## CI and Local Smoke Checks

Public CI should run only synthetic/default checks. The GitHub workflow
sets real-data mode off and uses temporary output directories.

Local template checks:

``` r

parse("inst/templates/targets/_targets.R")
parse("inst/templates/targets/R/functions.R")
testthat::test_file("tests/testthat/test-targets-template.R")
```

Synthetic workflow smoke check:

``` r

workflow_dir <- tempfile("alprekdb-targets-")
dir.create(workflow_dir)
file.copy(
  list.files(template_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE),
  workflow_dir,
  recursive = TRUE
)

withr::with_dir(workflow_dir, {
  withr::with_envvar(c(
    ALPREKDB_RUN_REALDATA = "0",
    ALPREKDB_OUTPUT_DIR = file.path(tempdir(), "alprekdb-output"),
    ALPREKDB_SYNTHETIC_N_CLASSROOMS = "4",
    ALPREKDB_SYNTHETIC_N_STUDENTS = "12",
    ALPREKDB_SYNTHETIC_N_YEARS = "2",
    ALPREKDB_SYNTHETIC_SEED = "42"
  ), {
    targets::tar_make(callr_function = NULL)
    targets::tar_read(validation_summary)
    targets::tar_read(linkage_summary)
  })
})
```

Private real-data smoke checks should run only on a machine with local
ADECE source files:

``` r

Sys.setenv(
  ALPREKDB_RUN_REALDATA = "1",
  ALPREKDB_DATA_DIR = "/private/path/to/ADECE/source/files",
  ALPREKDB_WRITE_OUTPUTS = "0"
)

testthat::test_file("tests/testthat/test-realdata-integration.R")
```

## Guardrails

Do not commit or publish:

- raw ADECE files;
- `ORIGINAL-DATA/`;
- `local.env`;
- `_targets/`;
- `output/`;
- `.duckdb` files;
- row-level RDS exports;
- logs or rendered artifacts with row-level details.

Do not infer, zero-fill, or copy forward 2025-26 budget records. The
workflow keeps classroom and student coverage explicit and leaves budget
coverage missing until a canonical 2025-26 budget source exists.

## Next Step

The methodological track begins with M1, which explains the package
architecture and design tradeoffs behind the applied workflows.
