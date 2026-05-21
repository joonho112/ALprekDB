# Compute Melissa-vs-classroom county agreement on a classroom_level tibble.

Returns NULL when no usable comparison exists. Otherwise returns a list
with `rate`, `n_match`, `n_mismatch`, `n_na`, `source` (string tag for
diagnostics).

## Usage

``` r
.linkage_geocode_county_agreement(cl)
```

## Details

Comparison sources in priority order:

1.  A precomputed boolean `geocode_county_check_match` (best case).

2.  Both a Melissa-side county string column AND a classroom-side county
    string column. We recompute agreement case-insensitively.
    Melissa-side candidates: `melissa_county_name`,
    `geocode_county_name`, `COUNTYNAME`. Classroom-side candidates:
    `county_name`, `county`.

3.  Unprefixed `county_check_match` (survives the join in some setups).
