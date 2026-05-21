# Export Geocode Master / Panel / Reconciled Data to Stata (.dta)

Writes the `$data` slot to Stata format via
[`haven::write_dta()`](https://haven.tidyverse.org/reference/read_dta.html).
Factor columns are coerced to their character labels first (Stata's
`.dta` format encodes factor labels but loses `ordered` semantics).
Requires the `haven` package (`Suggests`).

## Usage

``` r
geocode_export_stata(x, path = NULL, version = 14, ...)
```

## Arguments

- x:

  An `alprek_geocode_master`, `alprek_geocode_panel`, or
  `alprek_geocode_reconciled` object.

- path:

  Character. Output path. If `NULL`, auto-generates
  `output/geocode/geocode_<run_id>.dta`.

- version:

  Integer. Stata file version (default `14`).

- ...:

  Forwarded to
  [`haven::write_dta()`](https://haven.tidyverse.org/reference/read_dta.html).

## Value

Invisible character path.
