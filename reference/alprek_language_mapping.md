# Load Classroom Language Mapping

Returns the mapping for cleaning the "fluent language other than
English" field. Handles null variants ("N/A", "no", "None", "English"),
erroneous data entry values, and case normalization of language names.

## Usage

``` r
alprek_language_mapping()
```

## Value

A tibble with columns: `raw_value`, `standardized`, `is_null`.

## Details

Values with `is_null = TRUE` are mapped to `NA` during cleaning (they
indicate no second language rather than a specific language). Values
with `is_null = FALSE` are mapped to their `standardized` form.

## Examples

``` r
alprek_language_mapping()
#> # A tibble: 39 × 3
#>    raw_value standardized is_null
#>    <chr>     <chr>        <lgl>  
#>  1 N/A       NA           TRUE   
#>  2 n/a       NA           TRUE   
#>  3 na        NA           TRUE   
#>  4 NA        NA           TRUE   
#>  5 no        NA           TRUE   
#>  6 No        NA           TRUE   
#>  7 NO        NA           TRUE   
#>  8 None      NA           TRUE   
#>  9 none      NA           TRUE   
#> 10 NONE      NA           TRUE   
#> # ℹ 29 more rows
```
