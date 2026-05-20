# Load Applications Status Codes

Returns the mapping from "Process Name" values in ADECE application
files to standardized kind labels and cycle years.

## Usage

``` r
alprek_applications_status_codes()
```

## Value

A tibble with columns: `process_name`, `kind_inferred`, `cycle_year`,
`notes`.

## Examples

``` r
alprek_applications_status_codes()
#> # A tibble: 6 × 4
#>   process_name                                    kind_inferred cycle_year notes
#>   <chr>                                           <chr>         <chr>      <chr>
#> 1 2026 - 2027 First Class Pre-K Classroom Renewal renewal       2026-2027  Rene…
#> 2 2026 - 2027 First Class Pre-K New Classroom Ap… new_app       2026-2027  Appl…
#> 3 2026-27 First Class Pre-K New Classroom         new_app       2026-2027  Obse…
#> 4 2026 - 2027 First Class Pre-K New Classroom Ap… new_app       2026-2027  Obse…
#> 5 2026 - 2027 First Class Pre-K Classroom Non-Re… non_renewal   2026-2027  Site…
#> 6 Show the Debugger Trace Report                  noise         NA         Junk…
```
