# Generate Synthetic Classroom Panel Data

Creates a synthetic `alprek_classroom_panel` object with classroom
characteristics, teacher demographics, and geographic data.

## Usage

``` r
alprek_synthetic_classroom(n_classrooms = 20, n_years = 2, seed = 42)
```

## Arguments

- n_classrooms:

  Integer. Number of classrooms per year.

- n_years:

  Integer. Number of school years (1-4).

- seed:

  Integer. Random seed for reproducibility.

## Value

An `alprek_classroom_panel` S3 object.

## Examples

``` r
classroom <- alprek_synthetic_classroom(n_classrooms = 10, n_years = 2)
classroom
#> <alprek_classroom_panel>
#>   Years: 2021-2022, 2022-2023 
#>   Total observations: 20 
#>   Columns: 44 
#>      2021-2022 : 10 classrooms ( legacy )
#>      2022-2023 : 10 classrooms ( legacy )
head(classroom$data)
#> # A tibble: 6 × 44
#>   classroom_code classroom_name school_year  year region county_code county_name
#>   <chr>          <chr>          <fct>       <int>  <int> <chr>       <chr>      
#> 1 018P12453.02   Classroom 4    2021-2022    2021      8 018         Tuscaloosa 
#> 2 020S54276.01   Classroom 9    2021-2022    2021      9 020         Montgomery 
#> 3 024O78087.02   Classroom 7    2021-2022    2021      1 024         Madison    
#> 4 025H76016.01   Classroom 3    2021-2022    2021      4 025         Baldwin    
#> 5 026F93315.01   Classroom 10   2021-2022    2021      2 026         Mobile     
#> 6 037F92646.02   Classroom 8    2021-2022    2021      8 037         Baldwin    
#> # ℹ 37 more variables: delivery_type <fct>, program_code <chr>,
#> #   site_name <chr>, site_code <chr>, class_num <chr>, title_i <chr>,
#> #   title_i_numeric <dbl>, total_grant <dbl>, enhancement_grant <dbl>,
#> #   latitude <dbl>, longitude <dbl>, year_first_funded <int>, class_type <chr>,
#> #   senate_dist <int>, house_dist <int>, seat_count <int>, lead_tch_name <chr>,
#> #   lead_tch_degree_raw <chr>, lead_tch_degree_level <fct>,
#> #   lead_tch_degree_area <chr>, lead_tch_degree_area_simple <chr>, …
```
