# A2 - Build Panels

## Overview

ALprekDB builds three panel objects before linkage:

- a budget panel with one row per classroom-year funding record;
- a classroom panel with one row per classroom-year site/classroom
  record;
- a student panel with one row per student-year record.

In v0.6.0, the real-data source scope is intentionally asymmetric:

| Module | Covered school years | Panel policy |
|----|----|----|
| Budget | 2021-22 through 2024-25 | Do not infer, copy forward, or zero-fill 2025-26 budget records. |
| Classroom | 2021-22 through 2025-26 | Retain classroom rows even when budget is unavailable. |
| Student | 2021-22 through 2025-26 | Exclude PII by default and retain student rows even when budget is unavailable. |

This vignette uses synthetic data for executable examples. The private
real-data snippets show the same object contract without exposing raw
ADECE files.

``` r

library(ALprekDB)
```

## Build Synthetic Panels

The synthetic generators return panel objects directly. They are useful
for learning the package structure and testing analysis code, not for
estimating Alabama program quantities. Generated classroom identifiers
use fake `9xx` prefixes and synthetic county labels to keep public
printed output visibly separate from confidential ADECE rows.

``` r

n_classrooms <- 12
n_students <- 60
seed <- 42

budget_panel <- alprek_synthetic_budget(
  n_classrooms = n_classrooms,
  n_years = 2,
  seed = seed
)

classroom_panel <- alprek_synthetic_classroom(
  n_classrooms = n_classrooms,
  n_years = 3,
  seed = seed
)

student_panel <- alprek_synthetic_student(
  n_students = n_students,
  n_classrooms = n_classrooms,
  n_years = 3,
  seed = seed
)

budget_panel
#> <alprek_budget_panel>
#>   Years: 2021-2022, 2022-2023 
#>   Total rows: 24 
#>      2021-2022 : 12 classrooms ( legacy )
#>      2022-2023 : 12 classrooms ( legacy )
classroom_panel
#> <alprek_classroom_panel>
#>   Years: 2021-2022, 2022-2023, 2023-2024 
#>   Total observations: 36 
#>   Columns: 44 
#>      2021-2022 : 12 classrooms ( legacy )
#>      2022-2023 : 12 classrooms ( legacy )
#>      2023-2024 : 12 classrooms ( legacy )
student_panel
#> <alprek_student_panel>
#>   Years: 2021-2022, 2022-2023, 2023-2024 
#>   Total observations: 180 
#>   Unique students: 180 
#>   Columns: 145 
#>      2021-2022 : 60 students ( legacy , 145 cols)
#>      2022-2023 : 60 students ( legacy , 145 cols)
#>      2023-2024 : 60 students ( legacy , 145 cols)
```

This synthetic setup deliberately gives budget two years and
classroom/student three years. It mirrors the real v0.6.0 lesson: a
later classroom/student year can be retained even when budget coverage
is unavailable.

## Panel Object Anatomy

Every panel object has a `$data` table and a `$years` vector.
Module-specific metadata differs slightly.

``` r

panel_anatomy <- function(x, module) {
  data.frame(
    module = module,
    class = paste(class(x), collapse = ", "),
    fields = paste(names(x), collapse = ", "),
    years = paste(as.character(x$years), collapse = ", "),
    rows = nrow(x$data),
    columns = ncol(x$data),
    stringsAsFactors = FALSE
  )
}

rbind(
  panel_anatomy(budget_panel, "budget"),
  panel_anatomy(classroom_panel, "classroom"),
  panel_anatomy(student_panel, "student")
)
#>      module                  class
#> 1    budget    alprek_budget_panel
#> 2 classroom alprek_classroom_panel
#> 3   student   alprek_student_panel
#>                                             fields
#> 1                    data, years, n_years, by_year
#> 2    data, years, n_total, by_year, imputation_log
#> 3 data, years, n_total, n_unique_students, by_year
#>                             years rows columns
#> 1            2021-2022, 2022-2023   24      38
#> 2 2021-2022, 2022-2023, 2023-2024   36      44
#> 3 2021-2022, 2022-2023, 2023-2024  180     145
```

The main difference is that budget panels expose `$n_years`, while
classroom and student panels expose `$n_total`. Classroom panels also
carry an `$imputation_log`; student panels carry `$n_unique_students`.

## Budget Panel

Budget panels track OSR and other funding sources by classroom-year.

``` r

head(budget_panel$data[c(
  "school_year",
  "classroom_code",
  "delivery_type",
  "grand_total",
  "share_osr"
)])
#> # A tibble: 6 × 5
#>   school_year classroom_code delivery_type            grand_total share_osr
#>   <fct>       <chr>          <fct>                          <dbl>     <dbl>
#> 1 2021-2022   917S990410.02  Private School                 96360     0.957
#> 2 2021-2022   923P921851.02  Public School                  98535     0.969
#> 3 2021-2022   924O990971.02  Community Organization         91968     0.972
#> 4 2021-2022   946P993831.03  Public School                  83923     0.961
#> 5 2021-2022   948F929292.01  Faith-Based Organization       97344     0.980
#> 6 2021-2022   948H902453.02  Head Start                    107592     0.964
```

``` r

budget_summary_stats(budget_panel)[c(
  "school_year",
  "n",
  "grand_total_mean",
  "grand_total_median",
  "share_osr_mean"
)]
#> # A tibble: 2 × 5
#>   school_year     n grand_total_mean grand_total_median share_osr_mean
#>   <fct>       <int>            <dbl>              <dbl>          <dbl>
#> 1 2021-2022      12           93049.             94164           0.971
#> 2 2022-2023      12           89652.             91796.          0.970
```

``` r

head(budget_track_classrooms(budget_panel))
#> # A tibble: 6 × 7
#>   classroom_code delivery_type            county_code `2021-2022` `2022-2023`
#>   <chr>          <fct>                    <chr>       <lgl>       <lgl>      
#> 1 917S990410.02  Private School           917         TRUE        TRUE       
#> 2 923P921851.02  Public School            923         TRUE        TRUE       
#> 3 924O990971.02  Community Organization   924         TRUE        TRUE       
#> 4 946P993831.03  Public School            946         TRUE        TRUE       
#> 5 948F929292.01  Faith-Based Organization 948         TRUE        TRUE       
#> 6 948H902453.02  Head Start               948         TRUE        TRUE       
#> # ℹ 2 more variables: n_years_present <dbl>, all_years <lgl>
```

## Classroom Panel

Classroom panels hold site, geography, staffing, delivery type, and
classroom metadata.

``` r

head(classroom_panel$data[c(
  "school_year",
  "classroom_code",
  "county_code",
  "delivery_type",
  "site_name",
  "latitude",
  "longitude"
)])
#> # A tibble: 6 × 7
#>   school_year classroom_code county_code delivery_type        site_name latitude
#>   <fct>       <chr>          <chr>       <fct>                <chr>        <dbl>
#> 1 2021-2022   917S990410.02  917         Private School       Site 6        34.5
#> 2 2021-2022   923P921851.02  923         Public School        Site 9        34.1
#> 3 2021-2022   924O990971.02  924         Community Organizat… Site 3        33.4
#> 4 2021-2022   946P993831.03  946         Public School        Site 8        34.6
#> 5 2021-2022   948F929292.01  948         Faith-Based Organiz… Site 1        31.3
#> 6 2021-2022   948H902453.02  948         Head Start           Site 7        31.6
#> # ℹ 1 more variable: longitude <dbl>
```

``` r

classroom_summary_stats(classroom_panel)
#> # A tibble: 3 × 6
#>   school_year n_classrooms mean_total_grant median_total_grant pct_public_school
#>   <fct>              <int>            <dbl>              <dbl>             <dbl>
#> 1 2021-2022             12          121291.            120770.              16.7
#> 2 2022-2023             12          114116.            117246               16.7
#> 3 2023-2024             12          114505.            103180.              16.7
#> # ℹ 1 more variable: lead_degree_coverage <dbl>
```

The imputation log records forward-filled coordinates and first-funded
years within site groups when those operations are needed.

``` r

classroom_panel$imputation_log
#> # A tibble: 0 × 5
#> # ℹ 5 variables: classroom_code <chr>, school_year <chr>, variable <chr>,
#> #   imputed_value <chr>, method <chr>
```

## Student Panel

Student panels contain one row per student-year. Direct PII is excluded
by default in private real-data workflows; synthetic examples use
fabricated student identifiers.

``` r

head(student_panel$data[c(
  "school_year",
  "classroom_code",
  "adece_id",
  "gender",
  "race",
  "poverty_dum",
  "days_absent_total"
)])
#> # A tibble: 6 × 7
#>   school_year classroom_code adece_id gender race  poverty_dum days_absent_total
#>   <fct>       <chr>          <chr>    <fct>  <fct>       <int>             <int>
#> 1 2021-2022   988F982646.03  S000101  Female Black           1                13
#> 2 2021-2022   948H902453.02  S000201  Male   White           1                 5
#> 3 2021-2022   964U948339.01  S000301  Female Black           0                11
#> 4 2021-2022   917S990410.02  S000401  Male   White           0                10
#> 5 2021-2022   999O968087.02  S000501  Male   Black           1                 6
#> 6 2021-2022   964U948339.01  S000601  Male   Lati…           1                10
```

``` r

student_summary_stats(student_panel)
#> # A tibble: 3 × 8
#>   school_year n_students pct_male pct_poverty pct_iep mean_days_absent
#>   <fct>            <int>    <dbl>       <dbl>   <dbl>            <dbl>
#> 1 2021-2022           60     55          60      15                9.8
#> 2 2022-2023           60     45          60      13.3             10  
#> 3 2023-2024           60     61.7        71.7    13.3             10.9
#> # ℹ 2 more variables: gold_lit_fall_pct <dbl>, gold_lit_spring_pct <dbl>
```

[`student_transform()`](https://joonho112.github.io/ALprekDB/reference/student_transform.md)
adds derived analysis variables such as assessment gains, chronic
absence, service density, and risk index when the source columns are
available.

``` r

enriched_student_panel <- student_transform(student_panel)
#> ℹ Deriving advanced analysis variables for 180 students
#> ✔ Added 22 derived columns (145 total columns)
#> ℹ Skipped 5 domain(s) due to missing source columns

head(enriched_student_panel$data[c(
  "school_year",
  "classroom_code",
  "gold_literacy_gain_raw",
  "chronic_absence",
  "n_services",
  "risk_index"
)])
#> # A tibble: 6 × 6
#>   school_year classroom_code gold_literacy_gain_raw chronic_absence n_services
#>   <fct>       <chr>                           <dbl>           <int>      <int>
#> 1 2021-2022   988F982646.03                       3               0          4
#> 2 2021-2022   948H902453.02                       8               0          4
#> 3 2021-2022   964U948339.01                      11               0          1
#> 4 2021-2022   917S990410.02                      11               0          3
#> 5 2021-2022   999O968087.02                      10               0          5
#> 6 2021-2022   964U948339.01                      17               0          4
#> # ℹ 1 more variable: risk_index <int>
```

``` r

enriched_student_panel$transform_log
#> $gold_gains_added
#> [1] 12
#> 
#> $kready_transitions_added
#> [1] 6
#> 
#> $chronic_absence_added
#> [1] 2
#> 
#> $service_density_added
#> [1] 2
#> 
#> $edeca_gains_added
#> [1] 0
#> 
#> $n_cols_added
#> [1] 0
#> 
#> $skipped
#> [1] "edeca_initiative_gain" "edeca_self_reg_gain"   "edeca_attachment_gain"
#> [4] "edeca_tpf_gain"        "edeca_behavior_gain"
```

## Coverage Caveat Before Linkage

The panel builder should preserve module-specific coverage. Later
linkage steps make the coverage gap explicit rather than fabricating
budget records.

``` r

coverage_master <- linkage_create_master(
  budget = budget_panel,
  classroom = classroom_panel,
  student = enriched_student_panel
)
#> ℹ Creating master linked dataset
#> ℹ Step 1/4: Joining classroom + budget
#> ℹ Joining classroom + budget data
#> ✔ Classroom-Budget join: 24/24 matched in overlapping budget years (100%); 24/36 matched overall
#> ℹ   Budget unavailable for year(s): 2023-2024; 12 classroom row(s) retained with missing budget columns
#> ℹ Step 2/4: Aggregating students to classroom level
#> ℹ Aggregating student data to classroom level
#> ✔ Aggregated 180 students into 36 classroom-year groups
#> ℹ Step 3/4: Building classroom-level master
#> ℹ Step 4/4: Building student-level master
#> ℹ Joining student + classroom data
#> ✔ Student-Classroom join: 36/36 classroom codes matched (100%)
#> ℹ   Result: 180 students x 181 columns
#> ✔ Master dataset created:
#> ℹ   Classroom-level: 36 rows x 112 cols
#> ℹ   Student-level: 180 rows x 211 cols
#> ℹ   Budget coverage unavailable for year(s): 2023-2024; budget-derived fields remain NA for those years

coverage_master$meta$coverage$by_year[c(
  "school_year",
  "has_budget",
  "has_classroom",
  "has_student",
  "n_budget_rows",
  "n_classroom_rows",
  "n_student_rows",
  "budget_status"
)]
#>           school_year has_budget has_classroom has_student n_budget_rows
#> 2021-2022   2021-2022       TRUE          TRUE        TRUE            12
#> 2022-2023   2022-2023       TRUE          TRUE        TRUE            12
#> 2023-2024   2023-2024      FALSE          TRUE        TRUE             0
#>           n_classroom_rows n_student_rows  budget_status
#> 2021-2022               12             60      available
#> 2022-2023               12             60      available
#> 2023-2024               12             60 missing_budget
```

``` r

linkage_validate(coverage_master)
#> ✔ Linkage validation passed (13 checks: 0 errors, 0 warnings)
#> <alprek_linkage_validation>
#>   Overall: PASSED 
#>   Errors: 0 | Warnings: 0 | Info: 3 
#> 
#>   Checks:
#>     [ ✓ ] Required join keys present
#>     [ ✓ ] No duplicate classroom-year keys
#>     [ ✓ ] Budget-classroom join match rate >= 95% in overlapping budget years -- Match rate: 100%
#>     [ i ] Orphan (unmatched) observations -- 12 orphan(s) found; 0 in overlapping years; 12 due to missing budget years
#>     [ ✓ ] Budget-classroom orphans in overlapping budget years -- 0 classroom row(s) without budget; 0 budget row(s) without classroom in overlapping years
#>     [ i ] Budget coverage gaps are explicit -- Budget unavailable: 2023-2024
#>     [ ✓ ] Student and classroom coverage gaps are explicit -- Classroom available for all student years | Student available for all classroom years
#>     [ ✓ ] Student classroom codes match classroom records in overlapping years -- 0 student classroom code(s) missing classroom records; 0 student row(s) affected in overlapping years
#>     [ ✓ ] Classrooms with no linked student rows are retained -- 0 empty classroom row(s) in overlapping years; 0 classroom row(s) in missing student years
#>     [ ✓ ] Budget data availability in overlapping coverage years -- 100% have budget data in overlapping years; 12 row(s) are in missing budget year(s) and excluded from this rate
#>     [ i ] Expected years present -- Years: 2021-2022, 2022-2023, 2023-2024 | Budget unavailable: 2023-2024
#>     [ ✓ ] Row count matches expected (left join preserves left rows) -- Expected: 36, Got: 36
#>     [ ✓ ] Region consistency check -- N/A (region or region_num not both present)
```

In real v0.6.0 processing, this same pattern applies to 2025-26:
classroom and student rows are retained, and budget-derived fields
remain missing until a canonical 2025-26 budget source exists.

## Private Real-Data Manifest

For private processing, define a local manifest from
environment-variable paths. Keep the manifest in a private project and
keep `include_pii = FALSE` and `include_dob = FALSE` unless a controlled
analysis explicitly requires otherwise.

``` r

data_dir <- Sys.getenv("ALPREKDB_DATA_DIR")

budget_files <- c(
  "2021-2022" = "budget_2021_2022.xlsx",
  "2022-2023" = "budget_2022_2023.xlsx",
  "2023-2024" = "budget_2023_2024.xlsx",
  "2024-2025" = "budget_2024_2025.xlsx"
)

classroom_files <- c(
  "2021-2022" = "classroom_2021_2022.xlsx",
  "2022-2023" = "classroom_2022_2023.xlsx",
  "2023-2024" = "classroom_2023_2024.xlsx",
  "2024-2025" = "classroom_2024_2025.xlsx",
  "2025-2026" = "classroom_2025_2026.xlsx"
)

student_files <- c(
  "2021-2022" = "student_2021_2022.xlsx",
  "2022-2023" = "student_2022_2023.xlsx",
  "2023-2024" = "student_2023_2024.xlsx",
  "2024-2025" = "student_2024_2025.xlsx",
  "2025-2026" = "student_2025_2026.xlsx"
)
```

Notice that there is no 2025-26 budget entry. That absence is an input
contract, not a missing coding step.

## Private Real-Data Processing

The `*_process_years()` helpers return a result object with a `$panel`
element and a `$validation_summary` element. These examples are not
executed in public documentation.

``` r

budget_configs <- Map(
  function(year, file) {
    budget_config(
      school_year = year,
      budget_path = file.path(data_dir, file),
      verbose = FALSE
    )
  },
  names(budget_files),
  budget_files
)

classroom_configs <- Map(
  function(year, file) {
    classroom_config(
      school_year = year,
      classroom_path = file.path(data_dir, file),
      include_dob = FALSE
    )
  },
  names(classroom_files),
  classroom_files
)

student_configs <- Map(
  function(year, file) {
    student_config(
      school_year = year,
      path = file.path(data_dir, file),
      include_pii = FALSE
    )
  },
  names(student_files),
  student_files
)
```

``` r

budget_processed <- budget_process_years(budget_configs, export = FALSE)
classroom_processed <- classroom_process_years(classroom_configs, export = FALSE)
student_processed <- student_process_years(student_configs, export = FALSE)

budget_panel <- budget_processed$panel
classroom_panel <- classroom_processed$panel
student_panel <- student_transform(student_processed$panel)

budget_processed$validation_summary
classroom_processed$validation_summary
student_processed$validation_summary
```

Row-level exports, local DuckDB files, and `_targets/` caches should
remain outside Git and outside public package artifacts. Use
`ALPREKDB_WRITE_OUTPUTS=1` only in a private workflow when those outputs
are intentionally needed.

## Output Contract

| Helper family | Main output contract |
|----|----|
| [`budget_process()`](https://joonho112.github.io/ALprekDB/reference/budget_process.md) | `raw`, `long`, `validation`, and `master` |
| [`classroom_process()`](https://joonho112.github.io/ALprekDB/reference/classroom_process.md) | `raw`, `clean`, and `validation` |
| [`student_process()`](https://joonho112.github.io/ALprekDB/reference/student_process.md) | `raw`, `clean`, and `validation` |
| `*_process_years()` | `by_year`, `panel`, and `validation_summary` |
| Budget panels | `$data`, `$years`, `$n_years`, and `$by_year` |
| Classroom panels | `$data`, `$years`, `$n_total`, `$by_year`, and `$imputation_log` |
| Student panels | `$data`, `$years`, `$n_total`, `$n_unique_students`, and `$by_year` |

## Next Step

A3 links the three panel objects into classroom-level and student-level
master datasets, then reads the orphan and coverage diagnostics created
during that join.
