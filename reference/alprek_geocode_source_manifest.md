# Load Geocode Source Manifest

Returns the 1-row canonical source manifest mapping the Melissa v1
geocoded delivery file to its vendor metadata (sheet, vendor version,
delivery date, cycle year, expected column count, example path).

## Usage

``` r
alprek_geocode_source_manifest()
```

## Value

A tibble with 1 row and columns: `kind`, `filename_pattern`, `sheet`,
`vendor`, `version`, `delivery_date`, `cycle_year`, `n_cols_expected`,
`example_path`, `notes`.

## Examples

``` r
alprek_geocode_source_manifest()
#> # A tibble: 1 × 10
#>   kind       filename_pattern      sheet vendor version delivery_date cycle_year
#>   <chr>      <chr>                 <chr> <chr>  <chr>   <date>        <chr>     
#> 1 melissa_v1 *_geocoding_master_*… Shee… Melis… 2026-03 2026-03-04    2026-2027 
#> # ℹ 3 more variables: n_cols_expected <int>, example_path <chr>, notes <chr>
```
