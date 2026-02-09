# Load Budget Category Groups Codebook

Returns the mapping from detailed budget line item names to standardized
category groups. This codebook is used during the cleaning step to
aggregate line items into the 8 standard budget categories.

## Usage

``` r
alprek_category_groups()
```

## Value

A tibble with columns: `category_detail`, `category_group`, `notes`.

## Examples

``` r
alprek_category_groups()
#> # A tibble: 38 × 3
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
#> # ℹ 28 more rows
```
