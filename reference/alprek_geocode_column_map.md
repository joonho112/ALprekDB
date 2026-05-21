# Load Geocode Melissa Column Map (v1)

Returns the 29-row column mapping for the Melissa v1 geocoded delivery
contract. Used internally by
[`geocode_read()`](https://joonho112.github.io/ALprekDB/reference/geocode_read.md)
and
[`geocode_detect_format()`](https://joonho112.github.io/ALprekDB/reference/geocode_detect_format.md)
to verify that incoming xlsx files match the expected schema (column
names, dtypes, source groups, required vs. optional flags, and observed
v0.8.0 baseline counts).

## Usage

``` r
alprek_geocode_column_map()
```

## Value

A tibble with 29 rows and columns: `raw_col`, `std_col`, `dtype`,
`source_group`, `is_required`, `observed_n_na`, `observed_n_distinct`,
`notes`.

## Examples

``` r
alprek_geocode_column_map()
#> # A tibble: 29 × 8
#>    raw_col         std_col         dtype  source_group is_required observed_n_na
#>    <chr>           <chr>           <chr>  <chr>        <lgl>               <int>
#>  1 row_id          row_id          chara… id           TRUE                    0
#>  2 school_year     school_year     chara… id           TRUE                    0
#>  3 site_name       site_name       chara… id           TRUE                    0
#>  4 site_code       site_code       chara… id           FALSE                 108
#>  5 site_street     site_street     chara… adece        TRUE                    0
#>  6 site_city       site_city       chara… adece        TRUE                    0
#>  7 site_state      site_state      chara… adece        TRUE                    0
#>  8 site_zip        site_zip        numer… adece        FALSE                   1
#>  9 geocode_address geocode_address chara… id           TRUE                    0
#> 10 latitude        latitude        numer… adece        FALSE                 184
#> # ℹ 19 more rows
#> # ℹ 2 more variables: observed_n_distinct <int>, notes <chr>
```
