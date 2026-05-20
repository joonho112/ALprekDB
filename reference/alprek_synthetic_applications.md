# Generate Synthetic Applications Data

Creates synthetic ADECE classroom applications data covering the four
canonical input kinds (renewals, new applications, non-renewals,
capacity). Output mirrors cycle-1 (2026-2027) standardized schema using
fake `9xx`-prefix classroom codes and `9xxxxx` program codes so that
examples cannot be confused with confidential source records.

Designed for vignette, tests, and demonstrations. Shares classroom/site
codes with
[`alprek_synthetic_budget()`](https://joonho112.github.io/ALprekDB/reference/alprek_synthetic_budget.md)
/
[`alprek_synthetic_classroom()`](https://joonho112.github.io/ALprekDB/reference/alprek_synthetic_classroom.md)
/
[`alprek_synthetic_student()`](https://joonho112.github.io/ALprekDB/reference/alprek_synthetic_student.md)
when called with the same `seed`.

## Usage

``` r
alprek_synthetic_applications(
  n_renewals = 100L,
  n_new = 30L,
  n_non_renewals = 5L,
  n_capacity_sites = 120L,
  cycle_year = "2026-2027",
  seed = 42L
)
```

## Arguments

- n_renewals:

  Integer. Number of renewal classrooms. Default `100`.

- n_new:

  Integer. Number of new classroom applications. Default `30`.

- n_non_renewals:

  Integer. Number of non-renewals. Default `5`.

- n_capacity_sites:

  Integer. Number of site capacity rows. Default `120` (covers both
  renewals and new).

- cycle_year:

  Character. Cycle year label, e.g., `"2026-2027"`. Default
  `"2026-2027"`.

- seed:

  Integer. Random seed for reproducibility. Default `42L`.

## Value

A list with four tibbles in standardized cycle-1 schema:

- `renewals` (n_renewals rows): process_name, region, county,
  organization_name, project_name, funding_type, program_type,
  project_name_prior, funding_type_prior, award_prior,
  total_funding_request, draft_base_award, tier_adjustment, draft_award,
  notes

- `new_apps` (n_new rows): process_name, region, county,
  organization_name, project_name, funding_type, program_type,
  total_funding_request, award_other, new_classroom_award, total_award

- `non_renewals` (n_non_renewals rows): region, county,
  organization_name, project_name, prior_funding_amount,
  prior_funding_type, notes

- `capacity` (n_capacity_sites rows): site_code, site_name,
  n_classrooms, enrollment, capacity, waitlist,
  spaces_available_with_waitlist

## Examples

``` r
apps <- alprek_synthetic_applications(n_renewals = 20, n_new = 5, seed = 42)
head(apps$renewals)
#> # A tibble: 6 × 15
#>   process_name         region county organization_name project_name funding_type
#>   <chr>                <chr>  <chr>  <chr>             <chr>        <chr>       
#> 1 2026 - 2027 First C… Regio… Shelby Synthetic Academy Classroom H… Classroom F…
#> 2 2026 - 2027 First C… Regio… Jeffe… Synthetic Academy Classroom E… Supplementa…
#> 3 2026 - 2027 First C… Regio… Macon  Synthetic Learni… Classroom A… Classroom F…
#> 4 2026 - 2027 First C… Regio… Sumter Synthetic Head S… Classroom Q… Classroom F…
#> 5 2026 - 2027 First C… Regio… Mobile Synthetic Daycar… Classroom G… Supplementa…
#> 6 2026 - 2027 First C… Regio… Perry  Synthetic Christ… Classroom D… Supplementa…
#> # ℹ 9 more variables: program_type <chr>, project_name_prior <chr>,
#> #   funding_type_prior <chr>, award_prior <dbl>, total_funding_request <dbl>,
#> #   draft_base_award <dbl>, tier_adjustment <dbl>, draft_award <dbl>,
#> #   notes <chr>
head(apps$new_apps)
#> # A tibble: 5 × 11
#>   process_name         region county organization_name project_name funding_type
#>   <chr>                <chr>  <chr>  <chr>             <chr>        <chr>       
#> 1 2026 - 2027 First C… Regio… Madis… Synthetic New Ac… New Classro… Supplementa…
#> 2 2026 - 2027 First C… Regio… Lee    Synthetic New Pu… New Classro… Classroom F…
#> 3 2026 - 2027 First C… Regio… Lownd… Synthetic New Pu… New Classro… Classroom F…
#> 4 2026 - 2027 First C… Regio… Greene Synthetic New Pr… New Classro… Classroom F…
#> 5 2026 - 2027 First C… Regio… Lee    Synthetic New He… New Classro… Classroom F…
#> # ℹ 5 more variables: program_type <chr>, total_funding_request <dbl>,
#> #   award_other <dbl>, new_classroom_award <dbl>, total_award <dbl>
```
