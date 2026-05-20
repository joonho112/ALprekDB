# Load Applications Capacity Column Map

Returns the column mapping for ADECE site capacity data (cycle-1,
2026-2027). Used by
[`applications_read_capacity()`](https://joonho112.github.io/ALprekDB/reference/applications_read_capacity.md).

## Usage

``` r
alprek_applications_capacity_map(cycle = "cycle1")
```

## Arguments

- cycle:

  Character. Default `"cycle1"`.

## Value

A tibble with columns: `raw_column`, `standard_name`, `type`, `notes`.

## Examples

``` r
alprek_applications_capacity_map()
#> # A tibble: 7 × 4
#>   raw_column                                           standard_name type  notes
#>   <chr>                                                <chr>         <chr> <chr>
#> 1 Site Code                                            site_code     id    ADEC…
#> 2 Site Name                                            site_name     id    Site…
#> 3 # of Classrooms At Site                              n_classrooms  inte… Coun…
#> 4 Current Site Enrollment                              enrollment    inte… Curr…
#> 5 # Children Site Can Serve                            capacity      inte… Maxi…
#> 6 # Children Waitlisted, Pre-Registered, or Pending a… waitlist      inte… Chil…
#> 7 # Spaces Available at Site (and Children are waitin… spaces_avail… inte… Spac…
```
