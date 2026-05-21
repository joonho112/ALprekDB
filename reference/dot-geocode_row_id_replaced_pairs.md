# Detect resolved row_id pairs (`_new` placeholder → assigned site_code)

Joins removed (only_old) rows whose school_year ends in `_new` against
added (only_new) rows whose school_year is the same root WITHOUT `_new`,
on the natural keys `(school_year_root, site_name, geocode_address)`.
Each successful join is reported as one row in the returned tibble. The
matching semantics match Step 3.5 of the protocol (`row_id_replaced`
pairs).

## Usage

``` r
.geocode_row_id_replaced_pairs(removed_rows, added_rows)
```

## Arguments

- removed_rows, added_rows:

  Tibbles with at least the columns `row_id`, `school_year`,
  `site_name`, `geocode_address`, `site_code` (the latter may be `NA` in
  `_new` rows).

## Value

A tibble with columns: `old_row_id`, `new_row_id`, `school_year_old`,
`school_year_new`, `site_name`, `geocode_address`, `assigned_site_code`.
