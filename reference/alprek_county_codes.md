# Load Alabama County Codes

Returns the mapping from 3-digit county codes used in classroom codes to
county names and FIPS codes.

## Usage

``` r
alprek_county_codes()
```

## Value

A tibble with columns: `county_code`, `county_name`, `fips_code`.

## Examples

``` r
alprek_county_codes()
#> # A tibble: 67 × 3
#>    county_code county_name fips_code
#>    <chr>       <chr>       <chr>    
#>  1 001         Autauga     01001    
#>  2 003         Baldwin     01003    
#>  3 005         Barbour     01005    
#>  4 007         Bibb        01007    
#>  5 009         Blount      01009    
#>  6 011         Bullock     01011    
#>  7 013         Butler      01013    
#>  8 015         Calhoun     01015    
#>  9 017         Chambers    01017    
#> 10 019         Cherokee    01019    
#> # ℹ 57 more rows
```
