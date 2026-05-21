# Load Melissa STATUSCODE Meaning Table

Returns the 4-row mapping from the derived `STATUSCODE` field (no
Melissa public documentation found; observed 1:1 pairing with
`RESULTCODE` in the v0.8.0 input) to human-readable labels and v0.8.0
observed counts.

## Usage

``` r
alprek_geocode_statuscode_meaning()
```

## Value

A tibble with 4 rows and columns: `code`, `label`, `is_success`,
`paired_resultcode_in_v080`, `observed_n_in_v080`, `source`,
`retrieved_at`.

## Examples

``` r
alprek_geocode_statuscode_meaning()
#> # A tibble: 4 × 7
#>   code  label        is_success paired_resultcode_in…¹ observed_n_in_v080 source
#>   <chr> <chr>        <lgl>      <chr>                               <int> <chr> 
#> 1 9     Paired with… TRUE       GS01                                   28 v0.8.…
#> 2 5     Paired with… TRUE       GS03                                  253 v0.8.…
#> 3 A     Paired with… TRUE       GS06                                  311 v0.8.…
#> 4 B     Paired with… TRUE       GS05                                 2804 v0.8.…
#> # ℹ abbreviated name: ¹​paired_resultcode_in_v080
#> # ℹ 1 more variable: retrieved_at <date>
```
