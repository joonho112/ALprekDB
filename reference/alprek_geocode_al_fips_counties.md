# Load Alabama County FIPS Reference Table

Returns the canonical 67-row Alabama county FIPS table (state FIPS 01).
Used by
[`geocode_validate()`](https://joonho112.github.io/ALprekDB/reference/geocode_validate.md)
to confirm that Melissa-returned `FIPS` and `COUNTYNAME` values fall
within Alabama and that the FIPS\<-\>name pairing is consistent.

## Usage

``` r
alprek_geocode_al_fips_counties()
```

## Value

A tibble with 67 rows and columns: `fips_full`, `fips_state`,
`fips_county`, `county_name`, `county_name_canonical_lower`, `state`.

## Examples

``` r
alprek_geocode_al_fips_counties()
#> # A tibble: 67 × 6
#>    fips_full fips_state fips_county county_name county_name_canonical_lo…¹ state
#>    <chr>     <chr>      <chr>       <chr>       <chr>                      <chr>
#>  1 01001     01         001         Autauga     autauga                    AL   
#>  2 01003     01         003         Baldwin     baldwin                    AL   
#>  3 01005     01         005         Barbour     barbour                    AL   
#>  4 01007     01         007         Bibb        bibb                       AL   
#>  5 01009     01         009         Blount      blount                     AL   
#>  6 01011     01         011         Bullock     bullock                    AL   
#>  7 01013     01         013         Butler      butler                     AL   
#>  8 01015     01         015         Calhoun     calhoun                    AL   
#>  9 01017     01         017         Chambers    chambers                   AL   
#> 10 01019     01         019         Cherokee    cherokee                   AL   
#> # ℹ 57 more rows
#> # ℹ abbreviated name: ¹​county_name_canonical_lower
```
