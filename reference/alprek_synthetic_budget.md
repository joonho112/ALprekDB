# Generate Synthetic Budget Panel Data

Creates a synthetic `alprek_budget_panel` object with realistic budget
amounts across OSR and Other funding sources.

## Usage

``` r
alprek_synthetic_budget(n_classrooms = 20, n_years = 2, seed = 42)
```

## Arguments

- n_classrooms:

  Integer. Number of classrooms per year.

- n_years:

  Integer. Number of school years (1-4).

- seed:

  Integer. Random seed for reproducibility.

## Value

An `alprek_budget_panel` S3 object.

## Examples

``` r
budget <- alprek_synthetic_budget(n_classrooms = 10, n_years = 2)
budget
#> <alprek_budget_panel>
#>   Years: 2021-2022, 2022-2023 
#>   Total rows: 20 
#>      2021-2022 : 10 classrooms ( legacy )
#>      2022-2023 : 10 classrooms ( legacy )
head(budget$data)
#> # A tibble: 6 × 38
#>   school_year  year classroom_code classroom_name county_code delivery_type     
#>   <fct>       <int> <chr>          <chr>          <chr>       <fct>             
#> 1 2021-2022    2021 018P12453.02   Classroom 4    018         Public School     
#> 2 2021-2022    2021 020S54276.01   Classroom 9    020         Private School    
#> 3 2021-2022    2021 024O78087.02   Classroom 7    024         Community Organiz…
#> 4 2021-2022    2021 025H76016.01   Classroom 3    025         Head Start        
#> 5 2021-2022    2021 026F93315.01   Classroom 10   026         Faith-Based Organ…
#> 6 2021-2022    2021 037F92646.02   Classroom 8    037         Faith-Based Organ…
#> # ℹ 32 more variables: delivery_type_binary <fct>, delivery_type_3way <fct>,
#> #   program_code <chr>, class_num <chr>, delivery_type_code <chr>,
#> #   osr_lead_teacher_salary <dbl>, osr_lead_teacher_benefits <dbl>,
#> #   osr_aux_teacher_salary <dbl>, osr_aux_teacher_benefits <dbl>,
#> #   osr_instructional_support <dbl>, osr_operations_maintenance <dbl>,
#> #   osr_equipment <dbl>, osr_administrative <dbl>,
#> #   other_lead_teacher_salary <dbl>, other_lead_teacher_benefits <dbl>, …
```
