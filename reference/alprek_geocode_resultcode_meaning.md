# Load Melissa RESULTCODE Meaning Table

Returns the 8-row mapping from Melissa Geocoder `RESULTCODE` values
(`GS01`-`GS08`) to human-readable labels, precision tier, expected
accuracy in meters, whether the code is acceptable for the master table,
and v0.8.0 observed counts.

## Usage

``` r
alprek_geocode_resultcode_meaning()
```

## Value

A tibble with 8 rows and columns: `code`, `label`, `precision_tier`,
`expected_accuracy_m`, `acceptable_for_master`,
`observed_in_v080_input`, `observed_n_in_v080`, `paired_status_in_v080`,
`source`, `retrieved_at`.

## Examples

``` r
alprek_geocode_resultcode_meaning()
#> # A tibble: 8 × 10
#>   code  label           precision_tier expected_accuracy_m acceptable_for_master
#>   <chr> <chr>           <chr>                        <dbl> <lgl>                
#> 1 GS01  Geocoded to St… zip4                           242 TRUE                 
#> 2 GS02  Geocoded to Ne… area                          1500 FALSE                
#> 3 GS03  Geocoded to Co… zip5                          4001 FALSE                
#> 4 GS04  Geocoded to St… area                        100000 FALSE                
#> 5 GS05  Geocoded to Ro… rooftop                         90 TRUE                 
#> 6 GS06  Geocoded to In… parcel                         128 TRUE                 
#> 7 GS07  Records Found … unknown                         NA FALSE                
#> 8 GS08  UNDOCUMENTED (… none                            NA FALSE                
#> # ℹ 5 more variables: observed_in_v080_input <lgl>, observed_n_in_v080 <int>,
#> #   paired_status_in_v080 <chr>, source <chr>, retrieved_at <date>
```
