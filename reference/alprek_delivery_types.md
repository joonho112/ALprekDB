# Load Delivery Type Codes

Returns the mapping from single-letter delivery type codes to full
names.

## Usage

``` r
alprek_delivery_types()
```

## Value

A tibble with columns: `code`, `name`, `name_short`.

## Examples

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
