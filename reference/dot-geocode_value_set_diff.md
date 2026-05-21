# Per-enum value-set diff (added / removed / in_both levels)

Per-enum value-set diff (added / removed / in_both levels)

## Usage

``` r
.geocode_value_set_diff(df_old, df_new, cols)
```

## Arguments

- df_old, df_new:

  Tibbles.

- cols:

  Character vector of enum columns to inspect; missing columns are
  silently skipped.

## Value

A tibble with columns: `column`, `value`, `status` (one of `"in_both"`,
`"added"`, `"removed"`), `n_old`, `n_new`. Always long-format (one row
per `(column, value)`).
