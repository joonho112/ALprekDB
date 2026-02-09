# Load Student Delivery Type Mapping

Returns the mapping from raw delivery type text values to standardized
categories for student data. Handles mixed capitalization in 2024-25
data (e.g., "community organization operated" vs "Community
Organization") and variant spellings (e.g., "private childcare" vs
"private child care").

## Usage

``` r
alprek_student_delivery_mapping()
```

## Value

A tibble with columns: `raw_value`, `standardized`.

## Details

Standardized delivery types (7 categories): Public School, Community
Organization, Private Child Care, Faith-Based Organization, Head Start,
University Operated, Private School.

## Examples

``` r
alprek_student_delivery_mapping()
#> # A tibble: 16 × 2
#>    raw_value                       standardized            
#>    <chr>                           <chr>                   
#>  1 public school                   Public School           
#>  2 Public School                   Public School           
#>  3 community organization operated Community Organization  
#>  4 Community Organization          Community Organization  
#>  5 private child care              Private Child Care      
#>  6 private childcare               Private Child Care      
#>  7 Private Child Care              Private Child Care      
#>  8 faith based organization        Faith-Based Organization
#>  9 Faith-Based Organization        Faith-Based Organization
#> 10 Faith Based Organization        Faith-Based Organization
#> 11 Head Start                      Head Start              
#> 12 university operated             University Operated     
#> 13 university                      University Operated     
#> 14 University Operated             University Operated     
#> 15 private school                  Private School          
#> 16 Private School                  Private School          
```
