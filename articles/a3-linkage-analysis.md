# A3 - Linkage Analysis

## Overview

ALprekDB links budget, classroom, and student panels into a two-level
master object:

- `classroom_level`: one row per classroom-year, with budget fields and
  classroom-level student aggregates;
- `student_level`: one row per student-year, with classroom and budget
  attributes attached.

The main linkage rule is simple: missing coverage years are metadata,
while overlap-year orphans are linkage problems to inspect. In v0.6.0,
the real-data workflow keeps 2025-26 classroom and student rows even
though the canonical 2025-26 budget file is not available.

``` r

library(ALprekDB)
```

## Build Synthetic Panels

This public example intentionally gives budget fewer years than
classroom and student panels. The final synthetic year stands in for the
v0.6.0 2025-26 budget caveat. The synthetic classroom identifiers use
fake `9xx` prefixes so row-level excerpts in this vignette remain
visibly separate from confidential ADECE records.

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

student_panel <- student_transform(student_panel)
#> ℹ Deriving advanced analysis variables for 180 students
#> ✔ Added 22 derived columns (145 total columns)
#> ℹ Skipped 5 domain(s) due to missing source columns
```

## Direct Joins

The master helper performs all joins, but the direct join helpers are
useful for understanding diagnostics.

``` r

classroom_budget <- linkage_classroom_budget(
  classroom = classroom_panel,
  budget = budget_panel
)
#> ℹ Joining classroom + budget data
#> ✔ Classroom-Budget join: 24/24 matched in overlapping budget years (100%); 24/36 matched overall
#> ℹ   Budget unavailable for year(s): 2023-2024; 12 classroom row(s) retained with missing budget columns

classroom_budget
#> <alprek_linkage_classroom>
#>   Years: 2021-2022, 2022-2023, 2023-2024 
#>   Rows: 36 
#>   Columns: 74 
#>   Overlap-year match rate: 100 %
#>   All-year match rate: 66.7 %
#>   Classroom orphans: 12 | Budget orphans: 0
```

``` r

student_classroom <- linkage_student_classroom(
  student = student_panel,
  classroom = classroom_panel
)
#> ℹ Joining student + classroom data
#> ✔ Student-Classroom join: 36/36 classroom codes matched (100%)
#> ℹ   Result: 180 students x 181 columns

student_classroom
#> <alprek_linkage_student>
#>   Years: 2021-2022, 2022-2023, 2023-2024 
#>   Students: 180 
#>   Columns: 181 
#>   Classroom match rate: 100 %
#>   Empty classrooms: 0
```

## Create the Master Object

[`linkage_create_master()`](https://joonho112.github.io/ALprekDB/reference/linkage_create_master.md)
builds the classroom-level and student-level analysis tables and stores
the diagnostics from both join paths.

``` r

master <- linkage_create_master(
  budget = budget_panel,
  classroom = classroom_panel,
  student = student_panel
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

master
#> <alprek_linkage_master>
#>   Years: 2021-2022, 2022-2023, 2023-2024 
#>   Classroom-level: 36 rows x 112 cols
#>   Student-level: 180 rows x 211 cols
#>   Budget overlap match: 100 %
#>   Budget all-year match: 66.7 %
#>   Classroom match: 100 %
```

## Coverage Diagnostics

Coverage is summarized by year. A missing budget year is retained as an
explicit coverage status; it is not turned into a zero budget.

``` r

coverage_by_year[c(
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

[`linkage_summary_stats()`](https://joonho112.github.io/ALprekDB/reference/linkage_summary_stats.md)
keeps budget means as `NA` when a year has no budget coverage.

``` r

linkage_summary_stats(master)
#> # A tibble: 3 × 6
#>   school_year     n mean_grand_total mean_per_child_budget mean_n_children
#>   <fct>       <int>            <dbl>                 <dbl>           <dbl>
#> 1 2021-2022      12            93049                 20535               5
#> 2 2022-2023      12            89652                 21737               5
#> 3 2023-2024      12               NA                    NA               5
#> # ℹ 1 more variable: pct_with_budget <dbl>
```

## Orphan Diagnostics

The classroom-budget diagnostics distinguish overlap-year orphans from
rows retained because an entire budget year is unavailable.

``` r

classroom_budget$diagnostics$orphan_summary_by_year
#>   school_year coverage_status n_classroom_rows n_budget_rows
#> 1   2021-2022         overlap               12            12
#> 2   2022-2023         overlap               12            12
#> 3   2023-2024  missing_budget               12             0
#>   n_classrooms_with_budget n_classrooms_without_budget
#> 1                       12                           0
#> 2                       12                           0
#> 3                        0                          12
#>   n_budget_rows_without_classroom match_rate
#> 1                               0          1
#> 2                               0          1
#> 3                               0          0
```

``` r

paste_or_none <- function(x) {
  if (length(x) == 0) "none" else paste(x, collapse = ", ")
}

data.frame(
  metric = c(
    "classroom rows without budget in overlapping years",
    "classroom rows retained because budget year is missing",
    "budget rows without classroom in overlapping years",
    "missing budget years"
  ),
  value = c(
    classroom_budget$diagnostics$n_left_orphan_overlap_years,
    classroom_budget$diagnostics$n_left_orphan_missing_budget_years,
    classroom_budget$diagnostics$n_right_orphan_overlap_years,
    paste_or_none(classroom_budget$diagnostics$missing_budget_years)
  ),
  stringsAsFactors = FALSE
)
#>                                                   metric     value
#> 1     classroom rows without budget in overlapping years         0
#> 2 classroom rows retained because budget year is missing        12
#> 3     budget rows without classroom in overlapping years         0
#> 4                                   missing budget years 2023-2024
```

The student-classroom diagnostics show whether student classroom codes
matched classroom records and whether any classrooms had no linked
student rows.

``` r

student_classroom$diagnostics$orphan_summary_by_year
#>   school_year coverage_status n_student_rows n_student_classrooms
#> 1   2021-2022         overlap             60                   12
#> 2   2022-2023         overlap             60                   12
#> 3   2023-2024         overlap             60                   12
#>   n_classroom_rows n_student_classrooms_with_classroom
#> 1               12                                  12
#> 2               12                                  12
#> 3               12                                  12
#>   n_student_classrooms_without_classroom n_student_rows_without_classroom
#> 1                                      0                                0
#> 2                                      0                                0
#> 3                                      0                                0
#>   n_classrooms_without_students match_rate
#> 1                             0          1
#> 2                             0          1
#> 3                             0          1
```

``` r

data.frame(
  metric = c(
    "student classroom codes missing in overlapping years",
    "student rows affected in overlapping years",
    "classrooms with no linked student rows",
    "missing classroom years",
    "missing student years"
  ),
  value = c(
    student_classroom$diagnostics$n_student_orphan_overlap_years,
    student_classroom$diagnostics$n_student_orphan_overlap_year_rows,
    student_classroom$diagnostics$n_classroom_orphan,
    paste_or_none(student_classroom$diagnostics$missing_classroom_years),
    paste_or_none(student_classroom$diagnostics$missing_student_years)
  ),
  stringsAsFactors = FALSE
)
#>                                                 metric value
#> 1 student classroom codes missing in overlapping years     0
#> 2           student rows affected in overlapping years     0
#> 3               classrooms with no linked student rows     0
#> 4                              missing classroom years  none
#> 5                                missing student years  none
```

## Validation Checks

[`linkage_validate()`](https://joonho112.github.io/ALprekDB/reference/linkage_validate.md)
reports pass, warning, error, and info checks. In this example the
missing budget year is informational because it is an explicit coverage
gap, not a broken classroom-budget key in an overlapping year.

``` r

validation
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

``` r

validation$checks[c(
  "check_name",
  "status",
  "n_issues",
  "details"
)]
#> # A tibble: 13 × 4
#>    check_name                         status n_issues details                   
#>    <chr>                              <chr>     <int> <chr>                     
#>  1 required_columns                   PASS          0 NA                        
#>  2 key_uniqueness                     PASS          0 NA                        
#>  3 match_rate                         PASS          0 Match rate: 100%          
#>  4 orphan_count                       INFO         12 12 orphan(s) found; 0 in …
#>  5 budget_overlap_orphans             PASS          0 0 classroom row(s) withou…
#>  6 budget_missing_coverage            INFO          1 Budget unavailable: 2023-…
#>  7 student_classroom_missing_coverage PASS          0 Classroom available for a…
#>  8 student_classroom_overlap_orphans  PASS          0 0 student classroom code(…
#>  9 empty_classrooms                   PASS          0 0 empty classroom row(s) …
#> 10 na_introduced                      PASS          0 100% have budget data in …
#> 11 year_coverage                      INFO          1 Years: 2021-2022, 2022-20…
#> 12 row_count_consistency              PASS          0 Expected: 36, Got: 36     
#> 13 region_consistency                 PASS          0 N/A (region or region_num…
```

## Student Aggregation

The classroom-level master includes student aggregates created by
[`linkage_aggregate_students()`](https://joonho112.github.io/ALprekDB/reference/linkage_aggregate_students.md).
You can also call the aggregation helper directly.

``` r

student_aggregates <- linkage_aggregate_students(student_panel)
#> ℹ Aggregating student data to classroom level
#> ✔ Aggregated 180 students into 36 classroom-year groups

head(student_aggregates[c(
  "school_year",
  "classroom_code",
  "n_children",
  "pct_poverty",
  "mean_age",
  "pct_chronic_absence",
  "mean_risk_index"
)])
#> # A tibble: 6 × 7
#>   school_year classroom_code n_children pct_poverty mean_age pct_chronic_absence
#>   <fct>       <chr>               <int>       <dbl>    <dbl>               <dbl>
#> 1 2021-2022   917S990410.02           7        42.9     3                      0
#> 2 2021-2022   923P921851.02           3        66.7     3.33                   0
#> 3 2021-2022   924O990971.02           4        75       3.75                  25
#> 4 2021-2022   946P993831.03           5        60       2.6                    0
#> 5 2021-2022   948F929292.01           4        50       3                      0
#> 6 2021-2022   948H902453.02           7        42.9     3                      0
#> # ℹ 1 more variable: mean_risk_index <dbl>
```

## Classroom-Level Analysis

Budget-derived fields are available for years with budget coverage. They
remain missing for years without budget coverage.

``` r

cl <- master$classroom_level

head(cl[c(
  "school_year",
  "classroom_code",
  "delivery_type",
  "n_children",
  "grand_total",
  "per_child_budget",
  "per_seat_budget",
  "pct_poverty",
  "mean_days_absent"
)])
#> # A tibble: 6 × 9
#>   school_year classroom_code delivery_type            n_children grand_total
#>   <fct>       <chr>          <fct>                         <int>       <dbl>
#> 1 2021-2022   917S990410.02  Private School                    7       96360
#> 2 2021-2022   923P921851.02  Public School                     3       98535
#> 3 2021-2022   924O990971.02  Community Organization            4       91968
#> 4 2021-2022   946P993831.03  Public School                     5       83923
#> 5 2021-2022   948F929292.01  Faith-Based Organization          4       97344
#> 6 2021-2022   948H902453.02  Head Start                        7      107592
#> # ℹ 4 more variables: per_child_budget <dbl>, per_seat_budget <dbl>,
#> #   pct_poverty <dbl>, mean_days_absent <dbl>
```

``` r

available_budget <- !is.na(cl$per_child_budget)

aggregate(
  cbind(per_child_budget, pct_poverty, mean_days_absent) ~ school_year,
  data = cl[available_budget, ],
  FUN = function(x) round(mean(x, na.rm = TRUE), 2)
)
#>   school_year per_child_budget pct_poverty mean_days_absent
#> 1   2021-2022         20535.21       61.88             9.73
#> 2   2022-2023         21737.36       58.12             9.97
```

## Student-Level Analysis

The student-level master carries classroom and budget context onto each
student-year row. Use those fields for descriptive summaries and
modeling inputs, not as automatic causal evidence.

``` r

sl <- master$student_level

head(sl[c(
  "school_year",
  "classroom_code",
  "delivery_type",
  "poverty_dum",
  "days_absent_total",
  "gold_literacy_gain_raw",
  "grand_total"
)])
#> # A tibble: 6 × 7
#>   school_year classroom_code delivery_type         poverty_dum days_absent_total
#>   <fct>       <chr>          <fct>                       <int>             <int>
#> 1 2021-2022   988F982646.03  Faith-Based Organiza…           1                13
#> 2 2021-2022   948H902453.02  Head Start                      1                 5
#> 3 2021-2022   964U948339.01  University Operated             0                11
#> 4 2021-2022   917S990410.02  Private School                  0                10
#> 5 2021-2022   999O968087.02  Community Organizati…           1                 6
#> 6 2021-2022   964U948339.01  University Operated             1                10
#> # ℹ 2 more variables: gold_literacy_gain_raw <dbl>, grand_total <dbl>
```

``` r

aggregate(
  gold_literacy_gain_raw ~ school_year + delivery_type,
  data = sl,
  FUN = function(x) round(mean(x, na.rm = TRUE), 2)
)
#>    school_year            delivery_type gold_literacy_gain_raw
#> 1    2021-2022            Public School                  10.38
#> 2    2022-2023            Public School                  10.14
#> 3    2023-2024            Public School                   9.00
#> 4    2021-2022       Private Child Care                  12.22
#> 5    2022-2023       Private Child Care                  10.86
#> 6    2023-2024       Private Child Care                  10.62
#> 7    2021-2022               Head Start                  11.38
#> 8    2022-2023               Head Start                  10.46
#> 9    2023-2024               Head Start                   8.62
#> 10   2021-2022   Community Organization                  12.38
#> 11   2022-2023   Community Organization                  13.75
#> 12   2023-2024   Community Organization                  12.40
#> 13   2021-2022 Faith-Based Organization                  12.00
#> 14   2022-2023 Faith-Based Organization                  10.45
#> 15   2023-2024 Faith-Based Organization                   8.22
#> 16   2021-2022      University Operated                  11.14
#> 17   2022-2023      University Operated                  12.00
#> 18   2023-2024      University Operated                  14.25
#> 19   2021-2022           Private School                  10.71
#> 20   2022-2023           Private School                  13.60
#> 21   2023-2024           Private School                  12.10
```

## Private Real-Data Linkage

In a private project, build panels as shown in A2, then link and
validate them. Real-data chunks stay non-executed in public
documentation.

``` r

output_dir <- Sys.getenv("ALPREKDB_OUTPUT_DIR")

master <- linkage_create_master(
  budget = budget_processed$panel,
  classroom = classroom_processed$panel,
  student = student_transform(student_processed$panel)
)

validation <- linkage_validate(master)
coverage <- master$meta$coverage$by_year

validation
coverage
```

Row-level exports should be written only in a private workflow with
explicit local opt-in. For master CSV export, pass a base filename, not
a bare directory: the exporter writes `_classroom.csv` and
`_student.csv` suffixes.

``` r

if (identical(Sys.getenv("ALPREKDB_WRITE_OUTPUTS"), "1")) {
  linkage_export_rds(
    master,
    file.path(output_dir, "linkage_master.rds")
  )

  linkage_export_csv(
    master,
    file.path(output_dir, "linkage_master.csv")
  )
}
```

## Next Step

A4 shows how to persist panels and master objects in DuckDB and query
them with SQL.
