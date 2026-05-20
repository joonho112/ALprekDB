# Load Applications New Classroom Column Map

Returns the column mapping for ADECE new classroom applications data
(cycle-1, 2026-2027). Used by
[`applications_read_new()`](https://joonho112.github.io/ALprekDB/reference/applications_read_new.md).

## Usage

``` r
alprek_applications_new_map(cycle = "cycle1")
```

## Arguments

- cycle:

  Character. Default `"cycle1"`.

## Value

A tibble with columns: `raw_column`, `standard_name`, `type`, `notes`.

## Examples

``` r
alprek_applications_new_map()
#> # A tibble: 11 × 4
#>    raw_column            standard_name         type        notes                
#>    <chr>                 <chr>                 <chr>       <chr>                
#>  1 Process Name          process_name          id          Cycle-1 added; categ…
#>  2 Region of Classroom   region                categorical Region 1-9           
#>  3 County of Classroom   county                categorical Alabama county name  
#>  4 Organization Name     organization_name     id          No cycle-year prefix…
#>  5 Project Name          project_name          id          Proposed classroom n…
#>  6 Funding               funding_type          categorical Funding category     
#>  7 Type of Program       program_type          categorical Public/Private/Head …
#>  8 Total Funding Request total_funding_request numeric     Requested funding in…
#>  9 26-27 Awards          award_other           numeric     Other-source award a…
#> 10 New Classroom Award   new_classroom_award   numeric     New classroom award …
#> 11 TOTAL 26-27 Awards    total_award           numeric     Total awards for new…
```
