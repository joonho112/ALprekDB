# Extract school-year root (strip the `_new` suffix when present)

Extract school-year root (strip the `_new` suffix when present)

## Usage

``` r
.geocode_school_year_root(x)
```

## Arguments

- x:

  Character vector.

## Value

Character vector. `"2025-2026_new"` → `"2025-2026"`; plain year strings
pass through unchanged.
