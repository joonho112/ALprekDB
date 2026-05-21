# Per-row, per-column change ledger

Per-row, per-column change ledger

## Usage

``` r
.geocode_row_changes(df_old, df_new, join_key = "row_id", change_cols)
```

## Arguments

- df_old, df_new:

  Tibbles with `row_id` and the comparison columns.

- join_key:

  Character. Default `"row_id"`.

- change_cols:

  Character vector. Columns to inspect. Missing columns are silently
  skipped.

## Value

A tibble with columns: `row_id`, `column`, `old_value`, `new_value`. One
row per (row_id, column) that differs (NA-equal pairs are NOT flagged as
changes; NA ≠ value IS flagged).
