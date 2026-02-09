# Join Classroom and Budget Data

Performs a left join of classroom panel data with budget panel data,
using `school_year` and `classroom_code` as join keys. Budget-only
columns are appended to the classroom data (shared columns use classroom
as the authoritative source).

## Usage

``` r
linkage_classroom_budget(classroom, budget)
```

## Arguments

- classroom:

  An `alprek_classroom_panel` object.

- budget:

  An `alprek_budget_panel` object.

## Value

An `alprek_linkage_classroom` S3 object with elements:

- `data`: tibble of joined classroom + budget data.

- `diagnostics`: list with join statistics.

- `meta`: list with metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- linkage_classroom_budget(classroom_panel, budget_panel)
cb$data
cb$diagnostics
} # }
```
