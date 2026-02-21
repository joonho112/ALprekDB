# Cross-Module Linkage and Analysis

## Overview

The Linkage module joins Budget, Classroom, and Student panels into
master datasets for analysis. This vignette demonstrates the full
linkage pipeline including student aggregation, derived variables, and
DuckDB integration.

``` r
library(ALprekDB)
```

## Generate Linked Synthetic Data

All three generators share classroom codes when called with the same
seed:

``` r
seed <- 42
nc <- 15

budget <- alprek_synthetic_budget(n_classrooms = nc, n_years = 2, seed = seed)
classroom <- alprek_synthetic_classroom(n_classrooms = nc, n_years = 2, seed = seed)
student <- alprek_synthetic_student(n_students = 80, n_classrooms = nc,
                                     n_years = 2, seed = seed)
```

Verify shared classroom codes:

``` r
b_codes <- unique(budget$data$classroom_code)
c_codes <- unique(classroom$data$classroom_code)
s_codes <- unique(student$data$classroom_code)

cat("Budget classrooms:", length(b_codes), "\n")
#> Budget classrooms: 15
cat("Classroom classrooms:", length(c_codes), "\n")
#> Classroom classrooms: 15
cat("Student classrooms:", length(unique(s_codes)), "\n")
#> Student classrooms: 15
cat("All student classrooms in classroom panel:",
    all(s_codes %in% c_codes), "\n")
#> All student classrooms in classroom panel: TRUE
```

## Creating the Master Dataset

[`linkage_create_master()`](https://joonho112.github.io/ALprekDB/reference/linkage_create_master.md)
performs all joins in one step:

``` r
master <- linkage_create_master(budget, classroom, student)
#> ℹ Creating master linked dataset
#> ℹ Step 1/4: Joining classroom + budget
#> ℹ Joining classroom + budget data
#> ✔ Classroom-Budget join: 30/30 matched (100%)
#> ℹ Step 2/4: Aggregating students to classroom level
#> ℹ Aggregating student data to classroom level
#> ✔ Aggregated 160 students into 30 classroom-year groups
#> ℹ Step 3/4: Building classroom-level master
#> ℹ Step 4/4: Building student-level master
#> ℹ Joining student + classroom data
#> ✔ Student-Classroom join: 30/30 classroom codes matched (100%)
#> ℹ   Result: 160 students x 181 columns
#> ✔ Master dataset created:
#> ℹ   Classroom-level: 30 rows x 112 cols
#> ℹ   Student-level: 160 rows x 211 cols
master
#> <alprek_linkage_master>
#>   Years: 2021-2022, 2022-2023 
#>   Classroom-level: 30 rows x 112 cols
#>   Student-level: 160 rows x 211 cols
#>   Budget match: 100 %
#>   Classroom match: 100 %
```

The master object contains two levels:

- **`classroom_level`**: One row per classroom-year with budget,
  classroom characteristics, and aggregated student demographics
- **`student_level`**: One row per student-year with all student data
  plus classroom and budget attributes

``` r
cat("Classroom-level:", nrow(master$classroom_level), "rows x",
    ncol(master$classroom_level), "cols\n")
#> Classroom-level: 30 rows x 112 cols
cat("Student-level:", nrow(master$student_level), "rows x",
    ncol(master$student_level), "cols\n")
#> Student-level: 160 rows x 211 cols
```

## Classroom-Level Analysis

The classroom-level master includes per-child budget and student
aggregates:

``` r
cl <- master$classroom_level

# Per-child budget (where available)
if ("per_child_budget" %in% names(cl)) {
  summary(cl$per_child_budget)
}
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    8973   13972   18181   19320   22040   48778

# Student aggregates
agg_cols <- c("n_children", "pct_male", "pct_poverty", "mean_age")
agg_available <- intersect(agg_cols, names(cl))
if (length(agg_available) > 0) {
  head(cl[, c("school_year", "classroom_code", agg_available)])
}
#> # A tibble: 6 × 6
#>   school_year classroom_code n_children pct_male pct_poverty mean_age
#>   <fct>       <chr>               <int>    <dbl>       <dbl>    <dbl>
#> 1 2021-2022   003U46939.01            5     60         100       3   
#> 2 2021-2022   005U71147.02            5     40          40       3.2 
#> 3 2021-2022   018H74409.02            5     40         100       4   
#> 4 2021-2022   020H10102.02            3     66.7       100       2.67
#> 5 2021-2022   024C84742.02            7     28.6        57.1     2.57
#> 6 2021-2022   025O43712.02            4     50          75       2.5
```

## Student-Level Analysis

The student-level master adds classroom attributes to each student:

``` r
sl <- master$student_level

# Student now has classroom-level attributes
if ("total_grant" %in% names(sl)) {
  cat("Grant amount range: $",
      format(min(sl$total_grant, na.rm = TRUE), big.mark = ","),
      " - $",
      format(max(sl$total_grant, na.rm = TRUE), big.mark = ","),
      "\n")
}
#> Grant amount range: $ 85,545  - $ 153,377

# Cross-module analysis: GOLD scores by delivery type
if (all(c("gold_literacy_gain_raw", "delivery_type") %in% names(sl))) {
  gain_by_dt <- tapply(sl$gold_literacy_gain_raw, sl$delivery_type,
                        mean, na.rm = TRUE)
  gain_by_dt <- gain_by_dt[!is.na(gain_by_dt)]
  if (length(gain_by_dt) > 0) {
    cat("\nMean GOLD Literacy Gain by Delivery Type:\n")
    print(round(gain_by_dt, 1))
  }
}
#> 
#> Mean GOLD Literacy Gain by Delivery Type:
#>          Public School     Private Child Care             Head Start 
#>                   12.7                    9.9                   10.7 
#> Community Organization    University Operated         Private School 
#>                   11.8                   14.5                   10.3
```

## Join Diagnostics

The master object includes diagnostic information about each join:

``` r
diag <- master$diagnostics
cat("Classroom-Budget match rate:",
    round(diag$classroom_budget$match_rate * 100, 1), "%\n")
#> Classroom-Budget match rate: 100 %
cat("Student-Classroom match rate:",
    round(diag$student_classroom$match_rate * 100, 1), "%\n")
#> Student-Classroom match rate: 100 %
```

## Transform Enrichment

The student panel includes derived variables from
[`student_transform()`](https://joonho112.github.io/ALprekDB/reference/student_transform.md):

``` r
if ("chronic_absence" %in% names(sl)) {
  cat("Chronic absence rate:",
      round(mean(sl$chronic_absence, na.rm = TRUE) * 100, 1), "%\n")
}
#> Chronic absence rate: 3.1 %

if ("risk_index" %in% names(sl)) {
  cat("\nRisk index distribution:\n")
  print(table(sl$risk_index))
}
#> 
#> Risk index distribution:
#> 
#>  0  1  2  3  4 
#> 12 70 55 22  1
```

## Student Aggregation

[`linkage_aggregate_students()`](https://joonho112.github.io/ALprekDB/reference/linkage_aggregate_students.md)
creates classroom-level summaries from student data:

``` r
agg <- linkage_aggregate_students(student)
#> ℹ Aggregating student data to classroom level
#> ✔ Aggregated 160 students into 30 classroom-year groups
head(agg[, c("school_year", "classroom_code", "n_children")])
#> # A tibble: 6 × 3
#>   school_year classroom_code n_children
#>   <fct>       <chr>               <int>
#> 1 2021-2022   003U46939.01            5
#> 2 2021-2022   005U71147.02            5
#> 3 2021-2022   018H74409.02            5
#> 4 2021-2022   020H10102.02            3
#> 5 2021-2022   024C84742.02            7
#> 6 2021-2022   025O43712.02            4
```

## DuckDB Integration

``` r
# Store processed data in DuckDB for efficient querying
db_path <- tempfile(fileext = ".duckdb")
conn <- db_init(db_path)
#> ✔ Created new ALprekDB database: /tmp/Rtmp1OFFKt/file260c10b54bc8.duckdb

# Write all panels
db_write_panel(conn, budget)
#> ✔ Wrote 30 rows x 38 cols to 'budget_panel'
db_write_panel(conn, classroom)
#> ✔ Wrote 30 rows x 44 cols to 'classroom_panel'
db_write_panel(conn, student)
#> ✔ Wrote 160 rows x 145 cols to 'student_panel'

# Write master
db_write_master(conn, master)
#> ✔ Wrote 30 rows x 112 cols to 'master_classroom'
#> ✔ Wrote 160 rows x 211 cols to 'master_student'

# List tables
db_list_tables(conn)
#> [1] "budget_panel"     "classroom_panel"  "master_classroom" "master_student"  
#> [5] "student_panel"
```

### SQL Queries

``` r
# Budget trends by year
db_query(conn, "
  SELECT school_year, COUNT(*) as n,
         ROUND(AVG(grand_total), 0) as mean_budget
  FROM budget_panel
  GROUP BY school_year
  ORDER BY school_year
")
#> # A tibble: 2 × 3
#>   school_year     n mean_budget
#>   <chr>       <dbl>       <dbl>
#> 1 2021-2022      15       93443
#> 2 2022-2023      15       90839
```

``` r
# Student demographics by year
db_query(conn, "
  SELECT school_year, COUNT(*) as n,
         ROUND(AVG(CASE WHEN gender='Male' THEN 1.0 ELSE 0.0 END)*100, 1) as pct_male,
         ROUND(AVG(poverty_dum)*100, 1) as pct_poverty
  FROM student_panel
  GROUP BY school_year
  ORDER BY school_year
")
#> # A tibble: 2 × 4
#>   school_year     n pct_male pct_poverty
#>   <chr>       <dbl>    <dbl>       <dbl>
#> 1 2021-2022      80     56.3        73.8
#> 2 2022-2023      80     43.8        66.3
```

``` r
# Table metadata
db_table_info(conn, "student_panel")[1:10, ]
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

``` r
db_close(conn)
unlink(db_path)
```

## Export

Export master datasets to multiple formats:

``` r
# CSV
linkage_export_csv(master, "output/linkage/")

# RDS (preserves S3 classes and factor levels)
linkage_export_rds(master, "output/linkage/linkage_master.rds")

# Stata
linkage_export_stata(master, "output/linkage/")
```

## Next Steps

- **Architecture**: See
  [`vignette("reference-architecture")`](https://joonho112.github.io/ALprekDB/articles/reference-architecture.md)
  for the full S3 class hierarchy and data dictionary
- **Panel Construction**: See
  [`vignette("panel-construction")`](https://joonho112.github.io/ALprekDB/articles/panel-construction.md)
  for details on individual module pipelines
