# Building Multi-Year Panel Data

## Overview

ALprekDB processes 4 years of ADECE Pre-K administrative data (2021-22
through 2024-25) into longitudinal panel datasets. This vignette
demonstrates the multi-year processing pipeline using synthetic data.

``` r
library(ALprekDB)
```

## Creating Synthetic Panel Data

``` r
# Generate 3-year panel data for all modules
budget <- alprek_synthetic_budget(n_classrooms = 15, n_years = 3, seed = 42)
classroom <- alprek_synthetic_classroom(n_classrooms = 15, n_years = 3, seed = 42)
student <- alprek_synthetic_student(n_students = 60, n_classrooms = 15,
                                     n_years = 3, seed = 42)
```

## Budget Panel

### Structure

The budget panel contains per-classroom funding data across years:

``` r
budget
#> <alprek_budget_panel>
#>   Years: 2021-2022, 2022-2023, 2023-2024 
#>   Total rows: 45 
#>      2021-2022 : 15 classrooms ( legacy )
#>      2022-2023 : 15 classrooms ( legacy )
#>      2023-2024 : 15 classrooms ( legacy )
str(budget$data[, 1:10], give.attr = FALSE)
#> tibble [45 × 10] (S3: tbl_df/tbl/data.frame)
#>  $ school_year         : Factor w/ 3 levels "2021-2022","2022-2023",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ year                : int [1:45] 2021 2021 2021 2021 2021 2021 2021 2021 2021 2021 ...
#>  $ classroom_code      : chr [1:45] "003U46939.01" "005U71147.02" "018H74409.02" "020H10102.02" ...
#>  $ classroom_name      : chr [1:45] "Classroom 11" "Classroom 15" "Classroom 4" "Classroom 9" ...
#>  $ county_code         : chr [1:45] "003" "005" "018" "020" ...
#>  $ delivery_type       : Factor w/ 7 levels "Public School",..: 6 6 3 3 2 4 7 4 4 6 ...
#>  $ delivery_type_binary: Factor w/ 2 levels "Public","Non-Public": 2 2 2 2 2 2 2 2 2 2 ...
#>  $ delivery_type_3way  : Factor w/ 3 levels "Public","Private",..: 2 2 3 3 2 2 2 2 2 2 ...
#>  $ program_code        : chr [1:45] "46939" "71147" "74409" "10102" ...
#>  $ class_num           : chr [1:45] "01" "02" "02" "02" ...
```

Each row represents one classroom-year with 38 columns spanning OSR and
Other funding sources.

### Year-over-Year Trends

``` r
budget_stats <- budget_summary_stats(budget)
budget_stats[, c("school_year", "n", "grand_total_mean",
                  "grand_total_median", "share_osr_mean")]
#> # A tibble: 3 × 5
#>   school_year     n grand_total_mean grand_total_median share_osr_mean
#>   <fct>       <int>            <dbl>              <dbl>          <dbl>
#> 1 2021-2022      15           93443.              93742          0.970
#> 2 2022-2023      15           90839               90812          0.970
#> 3 2023-2024      15           93425.              94581          0.969
```

### Tracking Classrooms Across Years

``` r
tracking <- budget_track_classrooms(budget)
head(tracking)
#> # A tibble: 6 × 8
#>   classroom_code delivery_type   county_code `2021-2022` `2022-2023` `2023-2024`
#>   <chr>          <fct>           <chr>       <lgl>       <lgl>       <lgl>      
#> 1 003U46939.01   University Ope… 003         TRUE        TRUE        TRUE       
#> 2 005U71147.02   University Ope… 005         TRUE        TRUE        TRUE       
#> 3 018H74409.02   Head Start      018         TRUE        TRUE        TRUE       
#> 4 020H10102.02   Head Start      020         TRUE        TRUE        TRUE       
#> 5 024C84742.02   Private Child … 024         TRUE        TRUE        TRUE       
#> 6 025O43712.02   Community Orga… 025         TRUE        TRUE        TRUE       
#> # ℹ 2 more variables: n_years_present <dbl>, all_years <lgl>
```

## Classroom Panel

### Structure

The classroom panel includes classroom characteristics, teacher
demographics, and geographic coordinates:

``` r
classroom
#> <alprek_classroom_panel>
#>   Years: 2021-2022, 2022-2023, 2023-2024 
#>   Total observations: 45 
#>   Columns: 44 
#>      2021-2022 : 15 classrooms ( legacy )
#>      2022-2023 : 15 classrooms ( legacy )
#>      2023-2024 : 15 classrooms ( legacy )
```

Key column groups:

- **Identifiers**: `classroom_code`, `school_year`, `region`,
  `county_code`
- **Delivery**: `delivery_type` (7 categories)
- **Geography**: `latitude`, `longitude`
- **Teacher Demographics**: `lead_tch_race`, `lead_tch_gender`,
  credentials
- **Funding**: `total_grant`, `enhancement_grant`

### Format Detection

ADECE changed the Excel format in 2024-25. ALprekDB auto-detects the
format:

``` r
# Legacy format (2021-2024): ~100 columns
# New format (2024-2025): ~125 columns with additional staff DOBs, seat counts
raw <- classroom_read("FCPK_Classroom_24-25.xlsx")
raw$meta$format  # "new"
```

### Imputation

The classroom panel applies forward-fill imputation for geographic
coordinates and `year_first_funded` within site groups. The imputation
log records every change:

``` r
classroom$imputation_log
#> # A tibble: 0 × 5
#> # ℹ 5 variables: classroom_code <chr>, school_year <chr>, variable <chr>,
#> #   imputed_value <chr>, method <chr>
```

## Student Panel

### Structure

The student panel is the largest dataset with 145 columns per
student-year:

``` r
student
#> <alprek_student_panel>
#>   Years: 2021-2022, 2022-2023, 2023-2024 
#>   Total observations: 180 
#>   Unique students: 180 
#>   Columns: 145 
#>      2021-2022 : 60 students ( legacy , 145 cols)
#>      2022-2023 : 60 students ( legacy , 145 cols)
#>      2023-2024 : 60 students ( legacy , 145 cols)
```

### Demographic Distribution

``` r
# Gender distribution
table(student$data$gender)
#> 
#>   Male Female 
#>     96     84

# Race distribution
table(student$data$race)
#> 
#>           White           Black Latino/Hispanic           Asian           Mixed 
#>              67              59              21               6              14 
#>           Other         Unknown 
#>               2              11

# Poverty rate by year
tapply(student$data$poverty_dum, student$data$school_year, mean, na.rm = TRUE)
#> 2021-2022 2022-2023 2023-2024 
#> 0.5666667 0.5500000 0.7000000
```

### Assessment Scores (GOLD)

GOLD assessments cover 6 developmental domains, each measured in fall
and spring:

``` r
gold_cols <- grep("^gold_literacy", names(student$data), value = TRUE)
gold_cols
#>  [1] "gold_literacy_fall_raw"        "gold_literacy_fall_scale"     
#>  [3] "gold_literacy_fall_whe"        "gold_literacy_fall_nn"        
#>  [5] "gold_literacy_fall_kready"     "gold_literacy_spring_raw"     
#>  [7] "gold_literacy_spring_scale"    "gold_literacy_spring_whe"     
#>  [9] "gold_literacy_spring_nn"       "gold_literacy_spring_kready"  
#> [11] "gold_literacy_gain_raw"        "gold_literacy_gain_scale"     
#> [13] "gold_literacy_kready_improved"
```

``` r
# Literacy fall vs spring raw scores
summary(student$data$gold_literacy_fall_raw)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>   15.00   26.00   38.50   39.55   54.00   65.00      12
summary(student$data$gold_literacy_spring_raw)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>    21.0    38.0    50.0    50.6    65.0    82.0      12
```

### Derived Variables

The student panel includes derived variables from
[`student_transform()`](https://joonho112.github.io/ALprekDB/reference/student_transform.md):

``` r
# GOLD gain scores
summary(student$data$gold_literacy_gain_raw)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>    3.00    7.00   11.00   11.04   15.25   20.00      12

# Chronic absence rate
mean(student$data$chronic_absence, na.rm = TRUE)
#> [1] 0.01666667

# Service density
summary(student$data$n_services)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   0.000   2.000   3.000   2.628   3.000   5.000

# Risk index
table(student$data$risk_index)
#> 
#>  0  1  2  3 
#> 29 70 61 20
```

## Real Data Pipeline

With actual ADECE files, the multi-year processing uses the `config` +
`process_years` pattern:

``` r
# Budget: 4-year pipeline
budget_configs <- list(
  budget_config("2021-2022", "Budget_21-22.xlsx"),
  budget_config("2022-2023", "Budget_22-23.xlsx"),
  budget_config("2023-2024", "Budget_23-24.xlsx"),
  budget_config("2024-2025", "Budget_24-25.xlsx")
)
budget_result <- budget_process_years(budget_configs)
budget_panel <- budget_result$panel  # 5,867 rows x 53 cols

# Classroom: 4-year pipeline
classroom_configs <- list(
  classroom_config("2021-2022", "Classroom_21-22.xlsx"),
  classroom_config("2022-2023", "Classroom_22-23.xlsx"),
  classroom_config("2023-2024", "Classroom_23-24.xlsx"),
  classroom_config("2024-2025", "Classroom_24-25.xlsx")
)
classroom_result <- classroom_process_years(classroom_configs)
classroom_panel <- classroom_result$panel  # 5,888 rows x 125 cols

# Student: 4-year pipeline + transform
student_configs <- list(
  student_config("2021-2022", "Student_21-22.xlsx"),
  student_config("2022-2023", "Student_22-23.xlsx"),
  student_config("2023-2024", "Student_23-24.xlsx"),
  student_config("2024-2025", "Student_24-25.xlsx")
)
student_result <- student_process_years(student_configs)
student_panel <- student_result$panel  # 92,507 rows x 261 cols

# Apply transform enrichment (27 derived variables)
enriched_panel <- student_transform(student_panel)
# 92,507 rows x 288 cols
```

## Validation Framework

Each module has a validation step with multiple checks:

| Module    | \# Checks | Key Checks                               |
|-----------|-----------|------------------------------------------|
| Budget    | 7         | Reconciliation, totals, share bounds     |
| Classroom | 10        | Coordinates, credentials, delivery types |
| Student   | 12        | Demographics, assessments, attendance    |
| Linkage   | 8         | Match rates, orphans, key uniqueness     |

``` r
# Validation returns check-level results
validation <- student_validate(clean_obj, strict = FALSE)
validation$checks  # tibble of check results
validation$n_errors
validation$n_warnings
```

## Next Steps

- **Linkage**: See
  [`vignette("linkage-and-analysis")`](https://joonho112.github.io/ALprekDB/articles/linkage-and-analysis.md)
  for joining these three panels into master datasets
- **Architecture**: See
  [`vignette("reference-architecture")`](https://joonho112.github.io/ALprekDB/articles/reference-architecture.md)
  for S3 class details
