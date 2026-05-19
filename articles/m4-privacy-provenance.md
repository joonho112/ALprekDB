# M4 - Privacy and Provenance

## Overview

ALprekDB uses a public-code, private-data model.

The public package can contain:

- R source code;
- tests built from synthetic or minimal fixtures;
- public CSV codebooks and column mappings;
- vignettes that run on synthetic data;
- templates for private workflows.

The public package should not contain:

- raw ADECE source files;
- row-level real-data extracts;
- local workflow caches;
- local DuckDB databases;
- private output folders;
- logs that print person-level records or direct identifiers.

M4 documents that boundary and the provenance information that should
travel with private outputs.

``` r

library(ALprekDB)
```

## Public and Private Artifacts

| Artifact | Public package? | Reason |
|----|----|----|
| R functions and tests | Yes | Processing logic is reviewable. |
| `inst/extdata/` codebooks | Yes | They describe rules, not individual records. |
| Synthetic data examples | Yes | They are fabricated and reproducible. |
| `inst/templates/targets/` | Yes | It is a reusable workflow template. |
| `ORIGINAL-DATA/` | No | Local source-data staging area only. |
| `_targets/` | No | May cache row-level objects in private workflows. |
| `output/` | No | May contain row-level panels or local summaries. |
| `.duckdb` files | No | Local databases can contain row-level records. |
| `local.env` and `.env*` | No | They can contain private paths or workflow switches. |
| rendered logs | No | Development logs may contain local context. |

Ignore rules are guardrails. They reduce accidental inclusion, but users
should still review `git status` and package-build contents before any
public release.

## Workflow Modes

The targets template exposes the privacy boundary through environment
variables.

``` r

template_dir <- system.file("templates", "targets", package = "ALprekDB", mustWork = TRUE)
template_env <- new.env(parent = globalenv())
sys.source(file.path(template_dir, "R", "functions.R"), template_env)

summarize_config <- function(envvars) {
  cfg <- withr::with_envvar(
    envvars,
    template_env$alprek_targets_config(project_dir = tempdir())
  )
  data.frame(
    run_realdata = cfg$run_realdata,
    write_outputs = cfg$write_outputs,
    include_pii = cfg$include_pii,
    schema = cfg$schema
  )
}

config_summary <- rbind(
  cbind(
    mode = "synthetic_default",
    summarize_config(c(
      ALPREKDB_RUN_REALDATA = "0",
      ALPREKDB_WRITE_OUTPUTS = NA
    ))
  ),
  cbind(
    mode = "realdata_default",
    summarize_config(c(
      ALPREKDB_RUN_REALDATA = "1",
      ALPREKDB_DATA_DIR = tempdir(),
      ALPREKDB_WRITE_OUTPUTS = NA
    ))
  ),
  cbind(
    mode = "realdata_output_opt_in",
    summarize_config(c(
      ALPREKDB_RUN_REALDATA = "1",
      ALPREKDB_DATA_DIR = tempdir(),
      ALPREKDB_WRITE_OUTPUTS = "1"
    ))
  )
)

config_summary
#>                     mode run_realdata write_outputs include_pii
#> 1      synthetic_default        FALSE          TRUE       FALSE
#> 2       realdata_default         TRUE         FALSE       FALSE
#> 3 realdata_output_opt_in         TRUE          TRUE       FALSE
#>                    schema
#> 1 0.6.0-workflow-template
#> 2 0.6.0-workflow-template
#> 3 0.6.0-workflow-template
```

The important default is the middle row: real-data mode does not write
row-level RDS or DuckDB outputs unless `ALPREKDB_WRITE_OUTPUTS=1` is
set.

## Local Real-Data Setup

Private workflows should keep paths in environment variables, not
committed scripts.

``` r

Sys.setenv(
  ALPREKDB_RUN_REALDATA = "1",
  ALPREKDB_DATA_DIR = "/private/path/to/ADECE/source/files",
  ALPREKDB_OUTPUT_DIR = "output/alprekdb",
  ALPREKDB_WRITE_OUTPUTS = "0"
)
```

`ALPREKDB_DATA_DIR` may point to a local `ORIGINAL-DATA/` folder or
another secure source-data directory. That folder is for local
processing and provenance review only; it is excluded from git and
package builds.

## Source Manifest Provenance

The private targets template keeps a source manifest. Public
documentation can describe the manifest at the module/year level without
printing source filenames.

``` r

manifest <- template_env$alprek_targets_realdata_manifest(
  data_dir = tempdir(),
  validate = FALSE
)

canonical <- manifest[manifest$status == "canonical", ]

module_counts <- aggregate(
  school_year ~ module,
  data = canonical,
  FUN = length
)
names(module_counts)[names(module_counts) == "school_year"] <- "canonical_files"

module_years <- aggregate(
  school_year ~ module,
  data = canonical,
  FUN = function(x) paste(min(x), max(x), sep = " through ")
)
names(module_years)[names(module_years) == "school_year"] <- "school_years"

merge(module_counts, module_years, by = "module")
#>      module canonical_files                school_years
#> 1    budget               4 2021-2022 through 2024-2025
#> 2 classroom               5 2021-2022 through 2025-2026
#> 3   student               5 2021-2022 through 2025-2026
```

The current v0.6.0 source scope is asymmetric: budget has canonical
coverage through 2024-2025, while classroom and student have canonical
coverage through 2025-2026. The missing 2025-2026 budget is structural
and should be recorded as coverage metadata, not filled in.

## Privacy Defaults

Student and classroom workflows default to excluding direct private
fields from processed outputs.

``` r

student <- alprek_synthetic_student(
  n_students = 10,
  n_classrooms = 4,
  n_years = 1,
  seed = 1
)

known_student_private_fields <- unique(c(
  ALprekDB:::.get_student_pii_columns("legacy"),
  ALprekDB:::.get_student_pii_columns("new")
))

data.frame(
  object = "synthetic_student_panel",
  rows = nrow(student$data),
  columns = ncol(student$data),
  known_private_fields_tracked = length(known_student_private_fields),
  known_private_fields_present = length(intersect(
    known_student_private_fields,
    names(student$data)
  ))
)
#>                    object rows columns known_private_fields_tracked
#> 1 synthetic_student_panel   10     145                           30
#>   known_private_fields_present
#> 1                            0
```

This is a structural check, not a disclosure review. Even when direct
private fields are excluded, real student-level rows remain confidential
because they can still carry sensitive combinations of attributes.

## Output Policy

| Mode | Aggregate summaries | Row-level RDS | DuckDB |
|----|----|----|----|
| Synthetic default | Allowed | Allowed | Allowed when optional packages are installed. |
| Real-data default | Allowed locally | Disabled | Disabled |
| Real-data opt-in | Allowed locally | Enabled locally | Enabled locally |

Use opt-in row-level outputs only in a private analysis project:

``` r

Sys.setenv(ALPREKDB_WRITE_OUTPUTS = "1")
```

Public reports should summarize validation, coverage, and linkage
diagnostics at aggregate levels.

## Provenance Bundle for Private Outputs

A private analysis output should be accompanied by enough context to
reproduce it later. A practical bundle includes:

- package version and workflow schema;
- source manifest module/year coverage;
- processing mode and output policy;
- validation summaries by module;
- linkage validation summary and coverage table;
- codebook/mapping version from git history;
- target graph or run log from the private workflow;
- notes about expected coverage gaps.

``` r

data.frame(
  component = c(
    "package_version",
    "workflow_schema",
    "source_coverage",
    "validation_summary",
    "linkage_coverage",
    "output_policy"
  ),
  public_safe = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  row_level = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
)
#>            component public_safe row_level
#> 1    package_version        TRUE     FALSE
#> 2    workflow_schema        TRUE     FALSE
#> 3    source_coverage        TRUE     FALSE
#> 4 validation_summary        TRUE     FALSE
#> 5   linkage_coverage        TRUE     FALSE
#> 6      output_policy        TRUE     FALSE
```

This provenance bundle can be shared more broadly than the underlying
row-level data because it contains aggregate processing context rather
than records.

## What Public Logs Should Not Include

Public logs and vignettes should not include:

- raw records or row previews from private files;
- direct person identifiers;
- birth dates or contact fields;
- classroom codes copied from confidential files;
- free-text notes from source systems;
- local absolute paths;
- source filenames from private manifests;
- SQL row samples from local databases;
- `_targets/` cache paths or object previews from real-data runs.

When a private run needs a diagnostic, prefer counts, status tables,
coverage by module/year, and non-disclosive validation summaries.

## Release Gate Checks

Before publishing documentation or building a release, run checks that
look for local-only artifacts.

``` r
git status --short --untracked-files=all
git check-ignore -v ORIGINAL-DATA output log _targets local.env
R CMD build --no-build-vignettes .
```

For source packages, inspect the tarball contents for ignored data
folders, local env files, database files, and rendered private logs.
Ignore rules are not a substitute for inspecting what will actually be
distributed.

## Hidden Contract Checks

## Reading Map

| Need                           | Where to go |
|--------------------------------|-------------|
| Public/private architecture    | M1          |
| Aggregate validation evidence  | M2          |
| Codebook and mapping contracts | M3          |
| Private targets workflow       | A5          |
