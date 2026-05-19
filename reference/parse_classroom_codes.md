# Parse Multiple Classroom Codes

Vectorized version of
[`parse_classroom_code()`](https://joonho112.github.io/ALprekDB/reference/parse_classroom_code.md).
Parses a vector of classroom codes and returns a tibble with parsed
components plus the delivery type name.

## Usage

``` r
parse_classroom_codes(codes)
```

## Arguments

- codes:

  Character vector of classroom codes.

## Value

A tibble with columns: `county_code`, `delivery_type_code`,
`delivery_type`, `program_code`, `class_num`.

## Examples

``` r
parse_classroom_codes(c("901P900001.01", "956C900789.02"))
#> # A tibble: 2 × 5
#>   county_code delivery_type_code program_code class_num delivery_type     
#>   <chr>       <chr>              <chr>        <chr>     <chr>             
#> 1 901         P                  900001       01        Public School     
#> 2 956         C                  900789       02        Private Child Care
```
