# A1 - Getting started with ALprekDB

## Overview

ALprekDB is a public R package for private Alabama First Class Pre-K
administrative data workflows. It turns annual ADECE budget, classroom,
and student files into validated panels, linked master datasets, and
optional DuckDB outputs.

Public examples in the package use synthetic data. Real ADECE files and
row-level outputs should stay in private local projects.

The v0.6.0 real-data scope is asymmetric:

| Module | Covered school years | Notes |
|----|----|----|
| Budget | 2021-22 through 2024-25 | 2025-26 budget data are not inferred or zero-filled. |
| Classroom | 2021-22 through 2025-26 | Classroom codes use six-digit program codes. |
| Student | 2021-22 through 2025-26 | Student PII is excluded by default in private workflows. |

## Installation

``` r

# install.packages("remotes")
remotes::install_github("joonho112/ALprekDB")
```

``` r

library(ALprekDB)
```

## Classroom Codes

Budget, classroom, and student records are linked by classroom code. The
current contract is `CCCDNNNNNN.NN`:

- `CCC` is a three-digit county code;
- `D` is the delivery type code;
- `NNNNNN` is a six-digit program code;
- `NN` is the classroom number within site.

The examples below use fake `9xx` prefixes and `9xxxxx` program codes
for public documentation.

``` r

codes <- c("901P900001.01", "967H900002.02", "933C900003.01")
parse_classroom_codes(codes)
#> # A tibble: 3 × 5
#>   county_code delivery_type_code program_code class_num delivery_type     
#>   <chr>       <chr>              <chr>        <chr>     <chr>             
#> 1 901         P                  900001       01        Public School     
#> 2 967         H                  900002       02        Head Start        
#> 3 933         C                  900003       01        Private Child Care
```

Delivery type labels are data-driven:

``` r

alprek_delivery_types()
#> # A tibble: 7 × 3
#>   code  name                     name_short    
#>   <chr> <chr>                    <chr>         
#> 1 P     Public School            Public        
#> 2 C     Private Child Care       Private CC    
#> 3 H     Head Start               Head Start    
#> 4 O     Community Organization   Community     
#> 5 F     Faith-Based Organization Faith-Based   
#> 6 U     University Operated      University    
#> 7 S     Private School           Private School
```

## Codebooks and Mappings

ALprekDB uses CSV codebooks stored in `inst/extdata/` for column
mappings, budget categories, delivery types, race/ethnicity mappings,
and other standardization rules. The helpers expose those mappings as
ordinary tibbles.

``` r

head(alprek_category_groups(), 10)
#> # A tibble: 10 × 3
#>    category_detail                           category_group        notes        
#>    <chr>                                     <chr>                 <chr>        
#>  1 Lead Teacher Salary                       lead_teacher_salary   ""           
#>  2 Lead Teacher Benefits                     lead_teacher_benefits ""           
#>  3 Aux Teacher Salary                        aux_teacher_salary    ""           
#>  4 Aux Teacher Benefits                      aux_teacher_benefits  ""           
#>  5 Payroll Taxes                             payroll_taxes         "Legacy only…
#>  6 Substitutes                               instructional_support ""           
#>  7 Background Checks                         instructional_support ""           
#>  8 Professional Development Registration     instructional_support ""           
#>  9 Professional Development Mileage          instructional_support ""           
#> 10 Professional Development Lodging And Food instructional_support ""
```

## Synthetic Quick Start

The synthetic generators create panel objects directly. When the same
`n_classrooms`, `n_years`, and seed are used, the generated budget,
classroom, and student panels share classroom codes that can be linked.
Public synthetic outputs intentionally use fake `9xx` classroom-code
prefixes and synthetic county labels so printed rows cannot be mistaken
for confidential ADECE records.

``` r

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

budget
#> <alprek_budget_panel>
#>   Years: 2021-2022, 2022-2023 
#>   Total rows: 40 
#>      2021-2022 : 20 classrooms ( legacy )
#>      2022-2023 : 20 classrooms ( legacy )
classroom
#> <alprek_classroom_panel>
#>   Years: 2021-2022, 2022-2023 
#>   Total observations: 40 
#>   Columns: 44 
#>      2021-2022 : 20 classrooms ( legacy )
#>      2022-2023 : 20 classrooms ( legacy )
student
#> <alprek_student_panel>
#>   Years: 2021-2022, 2022-2023 
#>   Total observations: 200 
#>   Unique students: 200 
#>   Columns: 145 
#>      2021-2022 : 100 students ( legacy , 145 cols)
#>      2022-2023 : 100 students ( legacy , 145 cols)
```

A quick look at the classroom panel shows the shared keys that make
linkage possible:

``` r

head(classroom$data[c(
  "school_year",
  "classroom_code",
  "county_code",
  "delivery_type",
  "site_name"
)])
#> # A tibble: 6 × 5
#>   school_year classroom_code county_code delivery_type            site_name
#>   <fct>       <chr>          <chr>       <fct>                    <chr>    
#> 1 2021-2022   902C980851.01  902         Private Child Care       Site 16  
#> 2 2021-2022   917C983315.01  917         Private Child Care       Site 6   
#> 3 2021-2022   919S923529.03  919         Private School           Site 14  
#> 4 2021-2022   923C914181.01  923         Private Child Care       Site 9   
#> 5 2021-2022   924F968087.02  924         Faith-Based Organization Site 3   
#> 6 2021-2022   925U950844.01  925         University Operated      Site 15
```

## Link the Modules

[`linkage_create_master()`](https://joonho112.github.io/ALprekDB/reference/linkage_create_master.md)
creates a two-level master object:

- `classroom_level`: one row per classroom-year, with budget fields and
  classroom-level student aggregates;
- `student_level`: one row per student-year, with classroom and budget
  attributes attached.

``` r

master <- linkage_create_master(
  budget = budget,
  classroom = classroom,
  student = student
)
#> ℹ Creating master linked dataset
#> ℹ Step 1/4: Joining classroom + budget
#> ℹ Joining classroom + budget data
#> ✔ Classroom-Budget join: 40/40 matched (100%)
#> ℹ Step 2/4: Aggregating students to classroom level
#> ℹ Aggregating student data to classroom level
#> ✔ Aggregated 200 students into 40 classroom-year groups
#> ℹ Step 3/4: Building classroom-level master
#> ℹ Step 4/4: Building student-level master
#> ℹ Joining student + classroom data
#> ✔ Student-Classroom join: 40/40 classroom codes matched (100%)
#> ℹ   Result: 200 students x 181 columns
#> ✔ Master dataset created:
#> ℹ   Classroom-level: 40 rows x 112 cols
#> ℹ   Student-level: 200 rows x 211 cols

master
#> <alprek_linkage_master>
#>   Years: 2021-2022, 2022-2023 
#>   Classroom-level: 40 rows x 112 cols
#>   Student-level: 200 rows x 211 cols
#>   Budget match: 100 %
#>   Classroom match: 100 %
```

The linkage object carries validation and coverage diagnostics:

``` r

linkage_validate(master)
#> ✔ Linkage validation passed (13 checks: 0 errors, 0 warnings)
#> <alprek_linkage_validation>
#>   Overall: PASSED 
#>   Errors: 0 | Warnings: 0 | Info: 1 
#> 
#>   Checks:
#>     [ ✓ ] Required join keys present
#>     [ ✓ ] No duplicate classroom-year keys
#>     [ ✓ ] Budget-classroom join match rate >= 95% in overlapping budget years -- Match rate: 100%
#>     [ i ] Orphan (unmatched) observations -- No orphans
#>     [ ✓ ] Budget-classroom orphans in overlapping budget years -- 0 classroom row(s) without budget; 0 budget row(s) without classroom in overlapping years
#>     [ ✓ ] Budget coverage gaps are explicit -- Budget available for all classroom years
#>     [ ✓ ] Student and classroom coverage gaps are explicit -- Classroom available for all student years | Student available for all classroom years
#>     [ ✓ ] Student classroom codes match classroom records in overlapping years -- 0 student classroom code(s) missing classroom records; 0 student row(s) affected in overlapping years
#>     [ ✓ ] Classrooms with no linked student rows are retained -- 0 empty classroom row(s) in overlapping years; 0 classroom row(s) in missing student years
#>     [ ✓ ] Budget data availability in overlapping coverage years -- 100% have budget data in overlapping years
#>     [ ✓ ] Expected years present -- Years: 2021-2022, 2022-2023 | Budget available for all joined years
#>     [ ✓ ] Row count matches expected (left join preserves left rows) -- Expected: 40, Got: 40
#>     [ ✓ ] Region consistency check -- N/A (region or region_num not both present)
```

``` r

linkage_summary_stats(master)
#> # A tibble: 2 × 6
#>   school_year     n mean_grand_total mean_per_child_budget mean_n_children
#>   <fct>       <int>            <dbl>                 <dbl>           <dbl>
#> 1 2021-2022      20            93399                 25846               5
#> 2 2022-2023      20            90912                 23414               5
#> # ℹ 1 more variable: pct_with_budget <dbl>
```

``` r

master$meta$coverage$by_year[c(
  "school_year",
  "has_budget",
  "has_classroom",
  "has_student",
  "n_budget_rows",
  "n_classroom_rows",
  "n_student_rows"
)]
#>           school_year has_budget has_classroom has_student n_budget_rows
#> 2021-2022   2021-2022       TRUE          TRUE        TRUE            20
#> 2022-2023   2022-2023       TRUE          TRUE        TRUE            20
#>           n_classroom_rows n_student_rows
#> 2021-2022               20            100
#> 2022-2023               20            100
```

The classroom-level master is the usual starting point for classroom or
site-level descriptive analysis:

``` r

head(master$classroom_level[c(
  "school_year",
  "classroom_code",
  "delivery_type",
  "n_children",
  "grand_total",
  "per_child_budget",
  "per_seat_budget"
)])
#> # A tibble: 6 × 7
#>   school_year classroom_code delivery_type            n_children grand_total
#>   <fct>       <chr>          <fct>                         <int>       <dbl>
#> 1 2021-2022   902C980851.01  Private Child Care                3       98066
#> 2 2021-2022   917C983315.01  Private Child Care                5       85759
#> 3 2021-2022   919S923529.03  Private School                    3       95075
#> 4 2021-2022   923C914181.01  Private Child Care                4       91493
#> 5 2021-2022   924F968087.02  Faith-Based Organization          7       89198
#> 6 2021-2022   925U950844.01  University Operated               5       92814
#> # ℹ 2 more variables: per_child_budget <dbl>, per_seat_budget <dbl>
```

## First Private Real-Data Recipe

Real-data examples should be run only in a private project. Keep paths
in environment variables, keep row-level outputs out of GitHub, and keep
student PII/date-of-birth fields excluded unless a controlled analysis
explicitly requires them.

``` r

data_dir <- Sys.getenv("ALPREKDB_DATA_DIR")

budget_cfg <- budget_config(
  school_year = "2024-2025",
  budget_path = file.path(data_dir, "budget_2024_2025.xlsx")
)

classroom_cfg <- classroom_config(
  school_year = "2024-2025",
  classroom_path = file.path(data_dir, "classroom_2024_2025.xlsx"),
  include_dob = FALSE
)

student_cfg <- student_config(
  school_year = "2024-2025",
  path = file.path(data_dir, "student_2024_2025.xlsx"),
  include_pii = FALSE
)
```

Process one module first, inspect its validation object, and then expand
to the full multi-module workflow:

``` r

budget_result <- budget_process(budget_cfg, export = FALSE)
budget_result$validation
budget_result$long
budget_result$master

classroom_result <- classroom_process(classroom_cfg, export = FALSE)
classroom_result$validation

student_result <- student_process(student_cfg, export = FALSE)
student_result$validation
```

For linked private analysis, use the multi-year processing helpers even
when starting with a single year. That keeps the object classes
consistent with larger panel workflows.

``` r

budget_panel <- budget_process_years(list(budget_cfg), export = FALSE)$panel
classroom_panel <- classroom_process_years(list(classroom_cfg), export = FALSE)$panel
student_panel <- student_process_years(list(student_cfg), export = FALSE)$panel

master <- linkage_create_master(
  budget = budget_panel,
  classroom = classroom_panel,
  student = student_panel
)

linkage_validate(master)
linkage_summary_stats(master)
```

## Private Targets Workflow

For repeated real-data processing, start from the package’s `targets`
template in a private project directory:

``` r

template_dir <- system.file("templates", "targets", package = "ALprekDB")

dir.create("alprekdb-private-workflow", showWarnings = FALSE)
file.copy(
  list.files(template_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE),
  "alprekdb-private-workflow",
  recursive = TRUE
)
```

Then configure the private workflow outside the package repository:

``` sh
export ALPREKDB_RUN_REALDATA=1
export ALPREKDB_DATA_DIR="/path/to/local/ADECE/source/files"
export ALPREKDB_OUTPUT_DIR="output/alprekdb"
```

Row-level RDS outputs and DuckDB writes require an explicit local
opt-in:

``` sh
export ALPREKDB_WRITE_OUTPUTS=1
```

## Where to Go Next

- A2 - Build panels: budget, classroom, and student panel construction.
- A3 - Linkage analysis: orphan diagnostics and derived measures.
- A4 - DuckDB and SQL: persistent local database workflows.
- A5 - Targets workflow: reproducible private processing.
- M4 - Privacy and provenance: guardrails for private source data and
  outputs.
