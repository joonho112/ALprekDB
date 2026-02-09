# Getting Started with ALprekDB

## Overview

ALprekDB is a modular R package for processing, cleaning, validating,
and managing Alabama First Class Pre-K (FCPK) administrative data
received from ADECE (Alabama Department of Early Childhood Education).

The package provides a complete pipeline from raw Excel files to
analysis-ready panel datasets:

    Excel files  -->  Read  -->  Clean  -->  Validate  -->  Panel  -->  Linkage  -->  Database
    (Budget)          S3         S3          S3             S3          S3            DuckDB
    (Classroom)       objects    objects     objects        objects     objects
    (Student)

### Package Modules

| Module        | Purpose                                                              |
|---------------|----------------------------------------------------------------------|
| **Budget**    | Per-classroom funding (OSR and Other sources, 11 categories)         |
| **Classroom** | Classroom characteristics, teacher demographics, geography           |
| **Student**   | Student demographics, assessments (GOLD, PPVT, eDECA, ASQ), services |
| **Linkage**   | Cross-module joins to create master datasets                         |
| **Transform** | Derived variables (gain scores, chronic absence, risk index)         |
| **Database**  | DuckDB persistent storage with SQL query support                     |

## Installation

``` r
# From GitHub:
# install.packages("remotes")
remotes::install_github("joonho112/ALprekDB")
```

``` r
library(ALprekDB)
```

## Classroom Codes

Every record in ADECE data is identified by a **classroom code** with
the format `CCCDNNNNN.NN`:

- `CCC` = County code (001-067)
- `D` = Delivery type code (P=Public, C=Community, H=Head Start, etc.)
- `NNNNN` = Program number
- `NN` = Class number within the program

Use
[`parse_classroom_codes()`](https://joonho112.github.io/ALprekDB/reference/parse_classroom_codes.md)
to extract these components:

``` r
codes <- c("001P12345.01", "067H54321.02", "033C98765.01")
parsed <- parse_classroom_codes(codes)
parsed
#> # A tibble: 3 × 5
#>   county_code delivery_type_code program_code class_num delivery_type     
#>   <chr>       <chr>              <chr>        <chr>     <chr>             
#> 1 001         P                  12345        01        Public School     
#> 2 067         H                  54321        02        Head Start        
#> 3 033         C                  98765        01        Private Child Care
```

## Codebook System

ALprekDB uses data-driven CSV codebooks stored in `inst/extdata/` for
column mappings and value standardization. This makes the cleaning rules
transparent and easily auditable.

``` r
# Budget category mappings
budget_categories <- alprek_category_groups()
head(budget_categories, 5)
#> # A tibble: 5 × 3
#>   category_detail       category_group        notes                             
#>   <chr>                 <chr>                 <chr>                             
#> 1 Lead Teacher Salary   lead_teacher_salary   ""                                
#> 2 Lead Teacher Benefits lead_teacher_benefits ""                                
#> 3 Aux Teacher Salary    aux_teacher_salary    ""                                
#> 4 Aux Teacher Benefits  aux_teacher_benefits  ""                                
#> 5 Payroll Taxes         payroll_taxes         "Legacy only. Allocated to benefi…

# Delivery type code mappings
delivery_types <- alprek_delivery_types()
delivery_types
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

# Race mapping (classroom module)
race_map <- alprek_race_mapping()
race_map
#> # A tibble: 16 × 3
#>    raw_value                                 standardized    factor_order
#>    <chr>                                     <chr>                  <int>
#>  1 White                                     White                      1
#>  2 Black or African American                 Black                      2
#>  3 Black/African American                    Black                      2
#>  4 Latino                                    Latino/Hispanic            3
#>  5 Hispanic                                  Latino/Hispanic            3
#>  6 Asian                                     Asian                      4
#>  7 Filipino                                  Asian                      4
#>  8 Mixed Heritage                            Mixed                      5
#>  9 Mixed heritage                            Mixed                      5
#> 10 Alaska Native                             Other                      6
#> 11 American Indian                           Other                      6
#> 12 Native Hawaiian or Other Pacific Islander Other                      6
#> 13 Native Hawaiian or other Pacific Islander Other                      6
#> 14 Unknown                                   Unknown                    7
#> 15 Decline to answer                         Unknown                    7
#> 16 No Response                               Unknown                    7
```

## Quick Start with Synthetic Data

For demonstrations, ALprekDB includes synthetic data generators that
produce realistic datasets without requiring access to confidential
ADECE files:

``` r
# Generate synthetic panels (all share classroom codes for linkage)
budget <- alprek_synthetic_budget(n_classrooms = 15, n_years = 2, seed = 42)
classroom <- alprek_synthetic_classroom(n_classrooms = 15, n_years = 2, seed = 42)
student <- alprek_synthetic_student(n_students = 60, n_classrooms = 15,
                                     n_years = 2, seed = 42)
```

``` r
budget
#> <alprek_budget_panel>
#>   Years: 2021-2022, 2022-2023 
#>   Total rows: 30 
#>      2021-2022 : 15 classrooms ( legacy )
#>      2022-2023 : 15 classrooms ( legacy )
head(budget$data[, c("school_year", "classroom_code", "delivery_type",
                      "grand_total", "share_osr")])
#> # A tibble: 6 × 5
#>   school_year classroom_code delivery_type          grand_total share_osr
#>   <fct>       <chr>          <fct>                        <dbl>     <dbl>
#> 1 2021-2022   003U46939.01   University Operated          87613     0.976
#> 2 2021-2022   005U71147.02   University Operated          94099     0.968
#> 3 2021-2022   018H74409.02   Head Start                   88412     0.967
#> 4 2021-2022   020H10102.02   Head Start                   82646     0.943
#> 5 2021-2022   024C84742.02   Private Child Care          110229     0.968
#> 6 2021-2022   025O43712.02   Community Organization       88962     0.963
```

``` r
student
#> <alprek_student_panel>
#>   Years: 2021-2022, 2022-2023 
#>   Total observations: 120 
#>   Unique students: 120 
#>   Columns: 145 
#>      2021-2022 : 60 students ( legacy , 145 cols)
#>      2022-2023 : 60 students ( legacy , 145 cols)
```

## Real Data Processing Pipeline

With actual ADECE Excel files, the processing workflow follows these
steps:

``` r
# 1. Configure
config <- budget_config(
  school_year = "2023-2024",
  path = "path/to/FCPK_Budget_23-24.xlsx"
)

# 2. Process (read + clean + validate)
result <- budget_process(config)

# 3. Inspect
result$raw       # alprek_budget_raw
result$clean     # alprek_budget_master
result$validation  # alprek_budget_validation

# 4. Multi-year panel
configs <- list(
  budget_config("2021-2022", "Budget_21-22.xlsx"),
  budget_config("2022-2023", "Budget_22-23.xlsx"),
  budget_config("2023-2024", "Budget_23-24.xlsx"),
  budget_config("2024-2025", "Budget_24-25.xlsx")
)
panel_result <- budget_process_years(configs)
panel <- panel_result$panel  # alprek_budget_panel
```

## Exploring the Panel

``` r
# Budget summary by year
budget_summary <- budget_summary_stats(budget)
budget_summary[, c("school_year", "n", "grand_total_mean",
                    "share_osr_mean")]
#> # A tibble: 2 × 4
#>   school_year     n grand_total_mean share_osr_mean
#>   <fct>       <int>            <dbl>          <dbl>
#> 1 2021-2022      15           93443.          0.970
#> 2 2022-2023      15           90839           0.970
```

## Validation

Every module includes a validation step that checks data quality:

``` r
# After cleaning
validation <- budget_validate(cleaned_obj, strict = FALSE)
print(validation)
# Shows: checks passed, warnings, errors
```

## Export

Export panel data to multiple formats:

``` r
budget_export_csv(panel, "output/budget_panel.csv")
budget_export_rds(panel, "output/budget_panel.rds")
budget_export_stata(panel, "output/budget_panel.dta")
```

## Next Steps

- **Panel Construction**: See
  [`vignette("panel-construction")`](https://joonho112.github.io/ALprekDB/articles/panel-construction.md)
  for multi-year processing workflows
- **Linkage & Analysis**: See
  [`vignette("linkage-and-analysis")`](https://joonho112.github.io/ALprekDB/articles/linkage-and-analysis.md)
  for cross-module linkage and DuckDB integration
- **Architecture**: See
  [`vignette("reference-architecture")`](https://joonho112.github.io/ALprekDB/articles/reference-architecture.md)
  for S3 class hierarchy and data dictionary
