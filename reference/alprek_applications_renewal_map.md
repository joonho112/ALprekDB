# Load Applications Renewals Column Map

Returns the column mapping for ADECE renewal classroom applications data
(cycle-1, 2026-2027). Used by
[`applications_read_renewals()`](https://joonho112.github.io/ALprekDB/reference/applications_read_renewals.md).

## Usage

``` r
alprek_applications_renewal_map(cycle = "cycle1")
```

## Arguments

- cycle:

  Character. Default `"cycle1"`.

## Value

A tibble with columns: `raw_column`, `standard_name`, `type`, `notes`.

## Examples

``` r
alprek_applications_renewal_map()
#> Warning: One or more parsing issues, call `problems()` on your data frame for details,
#> e.g.:
#>   dat <- vroom(...)
#>   problems(dat)
#> # A tibble: 15 × 4
#>    raw_column                  standard_name         type        notes          
#>    <chr>                       <chr>                 <chr>       <chr>          
#>  1 Process Name                process_name          id          Cycle-1 added;…
#>  2 Region of Classroom         region                categorical Region 1-9     
#>  3 County of Classroom         county                categorical Alabama county…
#>  4 26/27 Organization Name     organization_name     id          Cycle year pre…
#>  5 26/27 Project Name          project_name          id          Classroom name…
#>  6 26/27 Funding               funding_type          categorical Funding catego…
#>  7 Type of Program             program_type          categorical Public School …
#>  8 25/26 Project Name          project_name_prior    id          Prior cycle cl…
#>  9 25/26 Funding Type          funding_type_prior    categorical Prior cycle fu…
#> 10 25/26 Award                 award_prior           numeric     Prior cycle aw…
#> 11 Total 26/27 Funding Request total_funding_request numeric     Current cycle …
#> 12 26-27 **DRAFT** Base Award  draft_base_award      numeric     ADECE draft ba…
#> 13 Tier-Adjustment             tier_adjustment       numeric     Adjustment bas…
#> 14 26/27**DRAFT** AWARD        draft_award           numeric     Final draft aw…
#> 15 NOTES                       notes                 text        ADECE annotati…
```
