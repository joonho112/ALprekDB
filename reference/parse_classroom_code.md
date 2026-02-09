# Parse a Single Classroom Code

Parses an Alabama Pre-K classroom code in "XXXAYYYYY.ZZ" format into its
component parts.

## Usage

``` r
parse_classroom_code(code)
```

## Arguments

- code:

  Character. A single classroom code string.

## Value

A named list with elements: `county_code`, `delivery_type_code`,
`program_code`, `class_num`. Returns NAs for invalid codes.

## Examples

``` r
parse_classroom_code("823P012601.01")
#> $county_code
#> [1] "823"
#> 
#> $delivery_type_code
#> [1] "P"
#> 
#> $program_code
#> [1] "012601"
#> 
#> $class_num
#> [1] "01"
#> 
# list(county_code = "823", delivery_type_code = "P",
#      program_code = "012601", class_num = "01")
```
