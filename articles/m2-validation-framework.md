# M2 - Validation Framework

## Overview

Validation in ALprekDB is structured evidence about processing quality.
It is not a claim that administrative records are perfect, and it is not
a substitute for substantive analysis review. Its job is narrower and
more operational:

- make processing assumptions inspectable;
- apply the same checks across repeated runs;
- distinguish data-quality problems from expected coverage gaps;
- report aggregate diagnostics without exposing confidential rows.

This is the validation side of the public-code, private-data
architecture described in M1.

``` r

library(ALprekDB)
```

## Validation Objects

Validation functions return S3 objects with a common structure.

| Field | Meaning |
|----|----|
| `passed` | Overall result under default or strict interpretation. |
| `n_errors` | Number of `ERROR` checks. |
| `n_warnings` | Number of `WARN` checks. |
| `n_info` | Number of `INFO` checks. |
| `checks` | A tibble with one row per check. |
| `issues` | Non-linkage validators also include specific issue rows when available. |

The `checks` tibble uses a stable set of columns:

``` text
check_name
check_description
status
n_issues
details
```

The number of checks depends on the validation object. At the current
source state, the core counts are:

| Validator | Current check count |
|----|---:|
| [`budget_validate()`](https://joonho112.github.io/ALprekDB/reference/budget_validate.md) | 7 |
| [`classroom_validate()`](https://joonho112.github.io/ALprekDB/reference/classroom_validate.md) | 10 |
| [`student_validate()`](https://joonho112.github.io/ALprekDB/reference/student_validate.md) | 12 |
| [`linkage_validate()`](https://joonho112.github.io/ALprekDB/reference/linkage_validate.md) on classroom-budget linkage | 10 |
| [`linkage_validate()`](https://joonho112.github.io/ALprekDB/reference/linkage_validate.md) on student-classroom linkage | 11 |
| [`linkage_validate()`](https://joonho112.github.io/ALprekDB/reference/linkage_validate.md) on a master object | 13 |

Code that consumes validation results should use
`nrow(validation$checks)` rather than hard-coding a total count.

## Severity Levels

| Status | Meaning | Public interpretation |
|----|----|----|
| `PASS` | The check met its rule, or the rule is not applicable for this object. | No action required. |
| `WARN` | A potential quality issue or coverage caveat needs review. | Default mode may proceed; strict mode fails. |
| `ERROR` | A required contract was violated or a serious linkage problem was detected. | Do not proceed until repaired or explicitly resolved. |
| `INFO` | Context that should be reported but is not a failure. | Preserve in logs and summaries. |

`INFO` is especially important for administrative data. A missing module
year can be expected coverage metadata rather than a failed join.

## Synthetic Example With Coverage Gaps

This vignette uses synthetic data only. The example intentionally gives
budget and student panels fewer years than the classroom panel so the
validation framework has coverage information to report.

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
  n_years = 2,
  seed = 42
)

student <- suppressMessages(student_transform(student))
master <- suppressMessages(linkage_create_master(budget, classroom, student))
validation <- suppressMessages(linkage_validate(master))
```

``` r

validation_summary <- function(x) {
  data.frame(
    passed = x$passed,
    n_errors = x$n_errors,
    n_warnings = x$n_warnings,
    n_info = x$n_info,
    n_checks = nrow(x$checks)
  )
}

validation_summary(validation)
#>   passed n_errors n_warnings n_info n_checks
#> 1   TRUE        0          1      4       13
```

The validation object reports the status counts. It does not require
printing student records, classroom rows, source paths, or private
identifiers.

## Strict Mode

Default validation passes when there are no `ERROR` checks. Strict mode
passes only when there are no `ERROR` and no `WARN` checks. `INFO`
checks never make an object fail.

``` r

validation_default <- suppressMessages(linkage_validate(master, strict = FALSE))
validation_strict <- suppressMessages(linkage_validate(master, strict = TRUE))

data.frame(
  mode = c("default", "strict"),
  passed = c(validation_default$passed, validation_strict$passed),
  errors = c(validation_default$n_errors, validation_strict$n_errors),
  warnings = c(validation_default$n_warnings, validation_strict$n_warnings),
  info = c(validation_default$n_info, validation_strict$n_info),
  checks = c(nrow(validation_default$checks), nrow(validation_strict$checks))
)
#>      mode passed errors warnings info checks
#> 1 default   TRUE      0        1    4     13
#> 2  strict  FALSE      0        1    4     13
```

Strict mode is useful when a workflow needs a clean handoff before
downstream analysis. Default mode is useful when a warning is expected
and documented.

## Coverage-Aware Linkage Diagnostics

The master object stores coverage by school year.

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
#> 2023-2024   2023-2024      FALSE          TRUE       FALSE missing_budget
```

The corresponding validation checks separate expected coverage gaps from
true overlap-year linkage problems.

``` r

coverage_checks <- c(
  "budget_overlap_orphans",
  "budget_missing_coverage",
  "student_classroom_missing_coverage",
  "student_classroom_overlap_orphans",
  "empty_classrooms",
  "na_introduced",
  "year_coverage"
)

validation$checks[
  validation$checks$check_name %in% coverage_checks,
  c("check_name", "status", "n_issues", "details")
]
#> # A tibble: 7 × 4
#>   check_name                         status n_issues details                    
#>   <chr>                              <chr>     <int> <chr>                      
#> 1 budget_overlap_orphans             PASS          0 0 classroom row(s) without…
#> 2 budget_missing_coverage            INFO          1 Budget unavailable: 2023-2…
#> 3 student_classroom_missing_coverage WARN         12 Classroom available for al…
#> 4 student_classroom_overlap_orphans  PASS          0 0 student classroom code(s…
#> 5 empty_classrooms                   INFO         12 0 empty classroom row(s) i…
#> 6 na_introduced                      PASS          0 100% have budget data in o…
#> 7 year_coverage                      INFO          1 Years: 2021-2022, 2022-202…
```

The key distinction is:

- missing budget coverage is explicit metadata;
- rows in missing budget years retain missing budget-derived fields;
- overlap-year classroom-budget or student-classroom orphans are real
  linkage problems;
- classroom years without student coverage are warnings because
  classroom rows are retained without linked student rows;
- student years without classroom coverage are errors because student
  rows lack the classroom context required for the linked master.

## Aggregate Transparency

Validation output should be summarized before it leaves a private
workflow. Status counts and issue totals are usually enough for public
documentation.

``` r

aggregate(
  n_issues ~ status,
  data = validation$checks,
  FUN = sum
)
#>   status n_issues
#> 1   INFO       38
#> 2   PASS        0
#> 3   WARN       12
```

Analysts can keep the full validation object in a private run log, but
public vignettes and reports should avoid row-level excerpts from
confidential data.

## Hidden Contract Checks

The vignette itself checks the object contracts without printing private
data.

## What Public Vignettes Should Not Print

Public validation examples should not print:

- source-data file paths;
- row-level private records;
- student identifiers;
- names, dates of birth, or staff contact fields;
- raw classroom codes from confidential files;
- full private exports;
- real ADECE counts unless they have already been cleared for
  publication.

Synthetic values are useful for testing the pipeline, but they are not
empirical estimates from the real program.

## Reading Map

| Need                                  | Where to go |
|---------------------------------------|-------------|
| Linkage workflow examples             | A3          |
| DuckDB round-trip validation          | A4          |
| Targets workflow validation summaries | A5          |
| Codebook and mapping contracts        | M3          |
| Privacy and provenance policy         | M4          |
