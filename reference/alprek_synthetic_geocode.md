# Generate Synthetic Geocoded Master Data

Creates a synthetic geocoded master tibble matching the 29-column
Melissa v1 delivery contract. Mirrors the empirical distributions
observed in the v0.8.0 audit (school_year, RESULTCODE, ADECE-coord
missing patterns, AL geography) but uses fake site_codes (`999P`-prefix)
and synthetic anchors so that examples cannot be confused with
confidential ADECE source records.

Designed for vignette, tests, and demonstrations. Returns a flat tibble
(not an S3 panel object) so callers can hand it directly into
[`geocode_clean()`](https://joonho112.github.io/ALprekDB/reference/geocode_clean.md),
[`geocode_validate()`](https://joonho112.github.io/ALprekDB/reference/geocode_validate.md),
and
[`geocode_reconcile()`](https://joonho112.github.io/ALprekDB/reference/geocode_reconcile.md).

When `edge_case` is non-NULL (one of `"G01"`-`"G18"`), returns a
package-internal 5-row fixture with one row deliberately mutated to
trigger that edge case. Useful for golden tests of
[`geocode_validate()`](https://joonho112.github.io/ALprekDB/reference/geocode_validate.md)
/
[`geocode_reconcile()`](https://joonho112.github.io/ALprekDB/reference/geocode_reconcile.md)
behavior.

## Usage

``` r
alprek_synthetic_geocode(
  n_sites = 50L,
  n_years = 3L,
  share_missing_adece = 0.1,
  share_missing_site_code = 0.03,
  share_high_resultcode_agreement = 0.7,
  edge_case = NULL,
  cycle_year_anchor = 2024L,
  seed = 20260520L
)
```

## Arguments

- n_sites:

  Integer. Number of distinct sites (renewal pattern). Default `50L`.
  Sites are stable across years (same `row_id`, `site_code`, `site_name`
  repeated).

- n_years:

  Integer. Number of school years to span. Default `3L`. Most-recent
  year is `cycle_year_anchor` and the panel extends backward. Total rows
  = `n_sites * n_years` (plus a small "\_new" cohort if `n_years >= 4`).

- share_missing_adece:

  Numeric between 0 and 1, inclusive. Share of rows where the ADECE
  `latitude`/`longitude` is `NA` (and `has_latlon` is `FALSE`). Default
  `0.10` (deliberately higher than the v0.8.0 empirical ~5.4% so the
  synthetic signal is unambiguous in tests).

- share_missing_site_code:

  Numeric between 0 and 1, inclusive. Share of rows where `site_code` is
  `NA` (the `_new` cohort pattern). Default `0.03`.

- share_high_resultcode_agreement:

  Numeric between 0 and 1, inclusive. Share of rows assigned
  `RESULTCODE == "GS05"` (rooftop, high agreement quality). The
  remainder split across `c("GS06", "GS03", "GS01")` at the v0.8.0
  empirical ratios. Default `0.7`.

- edge_case:

  Character or NULL. If non-NULL, must be one of `"G01"`..`"G18"`; the
  function returns the package-internal fixture's `$data` tibble.
  Default `NULL` (generate full synthetic panel).

- cycle_year_anchor:

  Integer. Anchor year (Y for the `"YYYY-YYYY+1"` school_year of the
  most recent observation). The synthetic panel extends backward by
  `n_years - 1` years from this anchor. Default `2024L` (so anchor
  school_year is `"2024-2025"`).

- seed:

  Integer. Random seed for reproducibility. Default `20260520L`.

## Value

A tibble with 29 columns matching the Melissa v1 column map (see
[`alprek_geocode_column_map()`](https://joonho112.github.io/ALprekDB/reference/alprek_geocode_column_map.md)):

- 5 id cols: row_id, school_year, site_name, site_code, geocode_address

- 7 adece cols: site_street, site_city, site_state, site_zip, latitude,
  longitude, has_latlon

- 6 melissa_norm cols: md_street, md_city, md_state, GEOZIP, PLUS4, DPB

- 11 melissa_out cols: LAT, LNG, CT, CENSUSBLOC, FIPS, COUNTYNAME,
  PLACENAME, PLACECODE, RESULTCODE, STATUSCODE, ERRORCODE

Notable dtype contracts (v0.8.0):

- `LAT` and `LNG` are CHARACTER (Melissa source contract; coerced to
  numeric only in
  [`geocode_clean()`](https://joonho112.github.io/ALprekDB/reference/geocode_clean.md)).

- `ERRORCODE` is LOGICAL and 100% `NA` (v0.8.0 contract).

- `latitude` / `longitude` are NUMERIC (ADECE source).

## Examples

``` r
# Default invocation: 50 sites x 3 years ~ 150 rows
g <- alprek_synthetic_geocode()
nrow(g)
#> [1] 150
ncol(g)
#> [1] 29

# Smaller panel for tests
g_small <- alprek_synthetic_geocode(n_sites = 10, n_years = 2,
                                      seed = 42)
nrow(g_small)
#> [1] 20

# Edge-case mini-fixture (G05: drift; ADECE-Melissa distance 1-10km)
g_g05 <- alprek_synthetic_geocode(edge_case = "G05")
nrow(g_g05)  # 5 rows; row 1 has the drift mutation
#> [1] 5
```
