# Generate Synthetic Student Panel Data

Creates a synthetic `alprek_student_panel` object with student
demographics, GOLD assessment scores, attendance, service indicators,
and derived variables (gain scores, chronic absence, risk index).

## Usage

``` r
alprek_synthetic_student(
  n_students = 100,
  n_classrooms = 20,
  n_years = 2,
  seed = 42
)
```

## Arguments

- n_students:

  Integer. Number of students per year.

- n_classrooms:

  Integer. Number of classrooms (must match other generators for
  linkage). Defaults to 20.

- n_years:

  Integer. Number of school years (1-4).

- seed:

  Integer. Random seed for reproducibility.

## Value

An `alprek_student_panel` S3 object.

## Examples

``` r
student <- alprek_synthetic_student(n_students = 50, n_years = 2)
student
#> <alprek_student_panel>
#>   Years: 2021-2022, 2022-2023 
#>   Total observations: 100 
#>   Unique students: 100 
#>   Columns: 145 
#>      2021-2022 : 50 students ( legacy , 145 cols)
#>      2022-2023 : 50 students ( legacy , 145 cols)
head(student$data[, 1:10])
#> # A tibble: 6 × 10
#>   school_year  year classroom_code classroom_name adece_id region_num site_code 
#>   <fct>       <int> <chr>          <chr>          <chr>         <int> <chr>     
#> 1 2021-2022    2021 948O945429.02  Classroom 7    S000101           7 948O945429
#> 2 2021-2022    2021 902C980851.01  Classroom 16   S000201           4 902C980851
#> 3 2021-2022    2021 988O943740.03  Classroom 18   S000301           6 988O943740
#> 4 2021-2022    2021 999C966052.03  Classroom 11   S000401          11 999C966052
#> 5 2021-2022    2021 964S926115.02  Classroom 2    S000501           2 964S926115
#> 6 2021-2022    2021 917C983315.01  Classroom 6    S000601          10 917C983315
#> # ℹ 3 more variables: site_name <chr>, program_code <chr>, program_name <chr>
```
