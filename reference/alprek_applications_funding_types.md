# Load Applications Funding Types

Returns the mapping from ADECE funding type labels to standardized
funding categories.

## Usage

``` r
alprek_applications_funding_types()
```

## Value

A tibble with columns: `funding_type`, `funding_category`, `notes`.

## Examples

``` r
alprek_applications_funding_types()
#> # A tibble: 6 × 3
#>   funding_type                           funding_category notes                 
#>   <chr>                                  <chr>            <chr>                 
#> 1 Classroom Funding                      classroom        Standard per-classroo…
#> 2 Supplemental Funding                   supplemental     Additional funding be…
#> 3 Reduced Capacity Funding               reduced          Funding adjusted for …
#> 4 New Classroom Funding                  classroom        Observed cycle-1 labe…
#> 5 Reduced Enrollment                     reduced          Observed cycle-1 abbr…
#> 6 Classroom Funding;Supplemental Funding combined         Observed combined ADE…
```
