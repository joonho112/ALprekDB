# Per-column schema diff with possible-rename pairing (Jaro-Winkler)

Per-column schema diff with possible-rename pairing (Jaro-Winkler)

## Usage

``` r
.geocode_schema_diff(
  old_cols,
  new_cols,
  old_dtypes,
  new_dtypes,
  jw_threshold = 0.85
)
```

## Arguments

- old_cols, new_cols:

  Character vectors of column names.

- old_dtypes, new_dtypes:

  Named character vectors of dtype labels; names must match
  `old_cols`/`new_cols`.

- jw_threshold:

  Numeric in `[0, 1]`. Default `0.85`. Greedy 1-to-1 rename pairs
  require similarity at least this large.

## Value

A tibble with columns: `column_old`, `column_new`, `dtype_old`,
`dtype_new`, `status`, `jw_sim`, `note`. Statuses: `"in_both"`,
`"dtype_changed"`, `"added"`, `"removed"`, `"possible_rename"`.
