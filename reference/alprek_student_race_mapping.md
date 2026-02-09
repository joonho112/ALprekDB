# Load Student Race/Ethnicity Mapping

Returns the mapping from raw race/ethnicity text values to standardized
categories for student data. Maps diverse raw entries across all 4 years
(e.g., "Black or African American" in 2021-22 vs "Black/African
American" in 2022-25) to a consistent set of 7 categories.

## Usage

``` r
alprek_student_race_mapping()
```

## Value

A tibble with columns: `raw_value`, `standardized`, `factor_order`.

## Details

Standardized categories and their factor order:

1.  White

2.  Black

3.  Latino/Hispanic

4.  Asian

5.  Mixed

6.  Other

7.  Unknown

Note: Student race values differ from classroom teacher race values
(e.g., "Hispanic" appears in 2021-22 student data but not in classroom
data).

## Examples

``` r
alprek_student_race_mapping()
#> # A tibble: 15 × 3
#>    raw_value                                 standardized    factor_order
#>    <chr>                                     <chr>                  <int>
#>  1 White                                     White                      1
#>  2 Black or African American                 Black                      2
#>  3 Black/African American                    Black                      2
#>  4 Hispanic                                  Latino/Hispanic            3
#>  5 Latino/Hispanic                           Latino/Hispanic            3
#>  6 Asian                                     Asian                      4
#>  7 Filipino                                  Asian                      4
#>  8 Mixed Heritage                            Mixed                      5
#>  9 Mixed heritage                            Mixed                      5
#> 10 Alaska Native                             Other                      6
#> 11 American Indian                           Other                      6
#> 12 Native Hawaiian or Other Pacific Islander Other                      6
#> 13 Native Hawaiian or other Pacific Islander Other                      6
#> 14 Unknown                                   Unknown                    7
#> 15 Decline to answer                         Unknown                    7
```
