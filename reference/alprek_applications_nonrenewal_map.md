# Load Applications Non-Renewals Column Map

Returns the positional column mapping for ADECE non-renewal data
(cycle-1, 2026-2027). Note: the source sheet has no header row, so
column positions are mapped explicitly (col_1, col_2, ...).

## Usage

``` r
alprek_applications_nonrenewal_map(cycle = "cycle1")
```

## Arguments

- cycle:

  Character. Default `"cycle1"`.

## Value

A tibble with columns: `raw_column`, `standard_name`, `type`, `notes`.

## Examples

``` r
alprek_applications_nonrenewal_map()
#> # A tibble: 7 × 4
#>   raw_column standard_name        type        notes                             
#>   <chr>      <chr>                <chr>       <chr>                             
#> 1 col_1      region               categorical Positional: Region 1-9 (sheet has…
#> 2 col_2      county               categorical Positional: county name           
#> 3 col_3      organization_name    id          Positional: organization          
#> 4 col_4      project_name         id          Positional: classroom name        
#> 5 col_5      prior_funding_amount numeric     Positional: prior cycle funding a…
#> 6 col_6      prior_funding_type   categorical Positional: prior cycle funding t…
#> 7 col_7      notes                text        Positional: ADECE annotations (of…
```
