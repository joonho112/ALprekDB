# A4 - DuckDB and SQL

## Overview

ALprekDB can persist panel objects and linked master datasets in DuckDB.
This is useful when private workflows need SQL queries, fast reloads, or
a stable local analysis database.

Public examples in this vignette use synthetic data and write only to a
temporary database under
[`tempdir()`](https://rdrr.io/r/base/tempfile.html). Real DuckDB files
built from ADECE data are row-level private outputs and should stay
outside GitHub, pkgdown, and package builds.

The database features require the optional `duckdb` and `DBI` packages.

``` r

library(ALprekDB)
```

## Build Synthetic Linked Data

The synthetic setup mirrors A3: budget covers two years while classroom
and student panels cover three years. The third year demonstrates
missing budget coverage through the database round trip.

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
```

## Create a Temporary Database

Use a temporary path for examples. Private projects should use a
controlled output directory instead.

``` r

db_path <- tempfile(fileext = ".duckdb")
conn <- db_init(db_path)
```

## Write Panels and Master Tables

[`db_write_panel()`](https://joonho112.github.io/ALprekDB/reference/db_write_panel.md)
chooses a table name from the panel object’s S3 class.
[`db_write_master()`](https://joonho112.github.io/ALprekDB/reference/db_write_master.md)
writes the classroom-level and student-level master tables.

``` r

db_write_panel(conn, budget_panel)
#> ✔ Wrote 24 rows x 38 cols to 'budget_panel'
db_write_panel(conn, classroom_panel)
#> ✔ Wrote 36 rows x 44 cols to 'classroom_panel'
db_write_panel(conn, student_panel)
#> ✔ Wrote 180 rows x 145 cols to 'student_panel'
db_write_master(conn, master)
#> ✔ Wrote 36 rows x 112 cols to 'master_classroom'
#> ✔ Wrote 180 rows x 211 cols to 'master_student'

db_list_tables(conn)
#> [1] "budget_panel"     "classroom_panel"  "master_classroom" "master_student"  
#> [5] "student_panel"
```

## Inspect Schema and R Types

ALprekDB stores R type metadata in an internal registry. Factor columns
are stored as character values in DuckDB and reconstructed as factors
when read back into R.

``` r

head(db_table_info(conn, "classroom_panel"), 10)
#> # A tibble: 10 × 4
#>    column_name    duckdb_type r_type    is_factor
#>    <chr>          <chr>       <chr>     <lgl>    
#>  1 classroom_code VARCHAR     character FALSE    
#>  2 classroom_name VARCHAR     character FALSE    
#>  3 school_year    VARCHAR     factor    TRUE     
#>  4 year           INTEGER     integer   FALSE    
#>  5 region         INTEGER     integer   FALSE    
#>  6 county_code    VARCHAR     character FALSE    
#>  7 county_name    VARCHAR     character FALSE    
#>  8 delivery_type  VARCHAR     factor    TRUE     
#>  9 program_code   VARCHAR     character FALSE    
#> 10 site_name      VARCHAR     character FALSE
```

``` r

head(db_table_info(conn, "master_student"), 10)
#> # A tibble: 10 × 4
#>    column_name    duckdb_type r_type    is_factor
#>    <chr>          <chr>       <chr>     <lgl>    
#>  1 school_year    VARCHAR     factor    TRUE     
#>  2 year           INTEGER     integer   FALSE    
#>  3 classroom_code VARCHAR     character FALSE    
#>  4 classroom_name VARCHAR     character FALSE    
#>  5 adece_id       VARCHAR     character FALSE    
#>  6 region_num     INTEGER     integer   FALSE    
#>  7 site_code      VARCHAR     character FALSE    
#>  8 site_name      VARCHAR     character FALSE    
#>  9 program_code   VARCHAR     character FALSE    
#> 10 program_name   VARCHAR     character FALSE
```

## Query with SQL

Use
[`db_query()`](https://joonho112.github.io/ALprekDB/reference/db_query.md)
for SQL analysis. It returns a tibble.

``` r

db_query(conn, "
  SELECT
    school_year,
    COUNT(*) AS n_classrooms,
    ROUND(AVG(grand_total), 0) AS mean_budget,
    ROUND(AVG(per_child_budget), 0) AS mean_per_child_budget
  FROM master_classroom
  GROUP BY school_year
  ORDER BY school_year
")
#> # A tibble: 3 × 4
#>   school_year n_classrooms mean_budget mean_per_child_budget
#>   <chr>              <dbl>       <dbl>                 <dbl>
#> 1 2021-2022             12       93049                 20535
#> 2 2022-2023             12       89652                 21737
#> 3 2023-2024             12          NA                    NA
```

The missing budget year remains present, with budget means returned as
`NA`.

``` r

db_query(conn, "
  SELECT
    school_year,
    COUNT(*) AS n_classrooms,
    SUM(CASE WHEN grand_total IS NULL THEN 1 ELSE 0 END) AS n_without_budget
  FROM master_classroom
  GROUP BY school_year
  ORDER BY school_year
")
#> # A tibble: 3 × 3
#>   school_year n_classrooms n_without_budget
#>   <chr>              <dbl>            <dbl>
#> 1 2021-2022             12                0
#> 2 2022-2023             12                0
#> 3 2023-2024             12               12
```

Student-level tables can be queried directly.

``` r

db_query(conn, "
  SELECT
    school_year,
    COUNT(*) AS n_students,
    ROUND(AVG(poverty_dum) * 100, 1) AS pct_poverty,
    ROUND(AVG(days_absent_total), 1) AS mean_absences
  FROM student_panel
  GROUP BY school_year
  ORDER BY school_year
")
#> # A tibble: 3 × 4
#>   school_year n_students pct_poverty mean_absences
#>   <chr>            <dbl>       <dbl>         <dbl>
#> 1 2021-2022           60        60             9.8
#> 2 2022-2023           60        60            10  
#> 3 2023-2024           60        71.7          10.9
```

The linked student master can support grouped descriptive summaries.

``` r

db_query(conn, "
  SELECT
    school_year,
    delivery_type,
    COUNT(*) AS n_students,
    ROUND(AVG(gold_literacy_gain_raw), 1) AS mean_literacy_gain
  FROM master_student
  GROUP BY school_year, delivery_type
  ORDER BY school_year, delivery_type
  LIMIT 10
")
#> # A tibble: 10 × 4
#>    school_year delivery_type            n_students mean_literacy_gain
#>    <chr>       <chr>                         <dbl>              <dbl>
#>  1 2021-2022   Community Organization            9               12.4
#>  2 2021-2022   Faith-Based Organization          8               12  
#>  3 2021-2022   Head Start                       10               11.4
#>  4 2021-2022   Private Child Care               10               12.2
#>  5 2021-2022   Private School                    7               10.7
#>  6 2021-2022   Public School                     8               10.4
#>  7 2021-2022   University Operated               8               11.1
#>  8 2022-2023   Community Organization           12               13.8
#>  9 2022-2023   Faith-Based Organization         11               10.5
#> 10 2022-2023   Head Start                       14               10.5
```

## Read Objects Back into R

The read helpers reconstruct ALprekDB S3 objects.

``` r

budget_from_db <- db_read_panel(conn, "budget")
classroom_from_db <- db_read_panel(conn, "classroom")
student_from_db <- db_read_panel(conn, "student")
master_from_db <- db_read_master(conn)

data.frame(
  object = c(
    "budget_panel",
    "classroom_panel",
    "student_panel",
    "master_classroom",
    "master_student"
  ),
  original_rows = c(
    nrow(budget_panel$data),
    nrow(classroom_panel$data),
    nrow(student_panel$data),
    nrow(master$classroom_level),
    nrow(master$student_level)
  ),
  read_rows = c(
    nrow(budget_from_db$data),
    nrow(classroom_from_db$data),
    nrow(student_from_db$data),
    nrow(master_from_db$classroom_level),
    nrow(master_from_db$student_level)
  )
)
#>             object original_rows read_rows
#> 1     budget_panel            24        24
#> 2  classroom_panel            36        36
#> 3    student_panel           180       180
#> 4 master_classroom            36        36
#> 5   master_student           180       180
```

Type reconstruction is part of the round trip.

``` r

data.frame(
  check = c(
    "budget year is integer",
    "classroom delivery_type is factor",
    "student dob is Date",
    "master student dob is Date"
  ),
  passed = c(
    is.integer(budget_from_db$data$year),
    is.factor(classroom_from_db$data$delivery_type),
    inherits(student_from_db$data$dob, "Date"),
    inherits(master_from_db$student_level$dob, "Date")
  ),
  stringsAsFactors = FALSE
)
#>                               check passed
#> 1            budget year is integer   TRUE
#> 2 classroom delivery_type is factor   TRUE
#> 3               student dob is Date   TRUE
#> 4        master student dob is Date   TRUE
```

## Reconstruct Coverage Diagnostics

[`db_read_master()`](https://joonho112.github.io/ALprekDB/reference/db_read_master.md)
reconstructs validation-compatible coverage diagnostics from the stored
master tables.

``` r

master_from_db$meta$coverage$by_year[c(
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

db_validation <- linkage_validate(master_from_db)
#> ✔ Linkage validation passed (13 checks: 0 errors, 0 warnings)
db_validation
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

There is one important boundary:
[`db_read_master()`](https://joonho112.github.io/ALprekDB/reference/db_read_master.md)
rebuilds diagnostics from the stored `master_classroom` and
`master_student` tables. It cannot recover budget-only right-side rows
that were not present in the master tables. If a future workflow needs
to audit those rows after reload, keep the source budget panel or
persisted diagnostics alongside the master tables.

## Year Filters

Read helpers can filter to one or more school years.

``` r

classroom_2023 <- db_read_panel(conn, "classroom", years = "2023-2024")
master_2023 <- db_read_master(conn, years = "2023-2024")

data.frame(
  object = c("classroom_2023", "master_classroom_2023", "master_student_2023"),
  rows = c(
    nrow(classroom_2023$data),
    nrow(master_2023$classroom_level),
    nrow(master_2023$student_level)
  )
)
#>                  object rows
#> 1        classroom_2023   12
#> 2 master_classroom_2023   12
#> 3   master_student_2023   60
```

``` r

linkage_validate(master_2023)
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
#>     [ ✓ ] Budget-classroom orphans in overlapping budget years -- No overlapping budget years to evaluate
#>     [ i ] Budget coverage gaps are explicit -- Budget unavailable: 2023-2024
#>     [ ✓ ] Student and classroom coverage gaps are explicit -- Classroom available for all student years | Student available for all classroom years
#>     [ ✓ ] Student classroom codes match classroom records in overlapping years -- 0 student classroom code(s) missing classroom records; 0 student row(s) affected in overlapping years
#>     [ ✓ ] Classrooms with no linked student rows are retained -- 0 empty classroom row(s) in overlapping years; 0 classroom row(s) in missing student years
#>     [ ✓ ] Budget data availability in overlapping coverage years -- 100% have budget data in overlapping years; 12 row(s) are in missing budget year(s) and excluded from this rate
#>     [ i ] Expected years present -- Years: 2023-2024 | Budget unavailable: 2023-2024
#>     [ ✓ ] Row count matches expected (left join preserves left rows) -- Expected: 12, Got: 12
#>     [ ✓ ] Region consistency check -- N/A (region or region_num not both present)
```

## Read-Only Reopen

Once a database exists, it can be opened in read-only mode for analysis.

``` r

db_close(conn)

conn <- db_init(db_path, read_only = TRUE)

db_query(conn, "
  SELECT COUNT(*) AS n_students
  FROM master_student
")
#> # A tibble: 1 × 1
#>   n_students
#>        <dbl>
#> 1        180
```

## Private Real-Data DuckDB Files

Real-data DuckDB files are row-level private outputs. Write them only in
a private project and only when row-level output is intentional.

``` r

output_dir <- Sys.getenv("ALPREKDB_OUTPUT_DIR")
private_db_path <- file.path(output_dir, "alprekdb.duckdb")

if (identical(Sys.getenv("ALPREKDB_RUN_REALDATA"), "1") &&
    identical(Sys.getenv("ALPREKDB_WRITE_OUTPUTS"), "1")) {
  conn <- db_init(private_db_path)
  on.exit(db_close(conn), add = TRUE)

  db_write_panel(conn, budget_processed$panel, overwrite = TRUE)
  db_write_panel(conn, classroom_processed$panel, overwrite = TRUE)
  db_write_panel(conn, student_transform(student_processed$panel), overwrite = TRUE)
  db_write_master(conn, master, overwrite = TRUE)
}
```

Do not commit `.duckdb` files, row-level exports, `_targets/` caches, or
private output folders. Publish aggregate diagnostics and non-disclosive
counts instead.

## Cleanup

## Next Step

A5 shows how to run the package’s companion `targets` workflow for
repeatable synthetic and private real-data processing.
