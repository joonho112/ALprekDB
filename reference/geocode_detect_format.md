# Detect Melissa Geocode File Format

Inspects the column names of a Melissa-returned geocoded delivery and
decides whether it matches the v1 contract
(`format = "melissa_v1_2026"`) or is unrecognized
(`format = "unknown"`). Reports a confidence score in `[0, 1]` and the
set of input columns that are not in the v1 contract.

Used by
[`geocode_read()`](https://joonho112.github.io/ALprekDB/reference/geocode_read.md)
(as an upstream gate) and by
[`geocode_compare_deliveries()`](https://joonho112.github.io/ALprekDB/reference/geocode_compare_deliveries.md)
(to label each delivery's format before diffing). Mirrors the role of
[`applications_detect_format()`](https://joonho112.github.io/ALprekDB/reference/applications_detect_format.md)
and
[`budget_detect_format()`](https://joonho112.github.io/ALprekDB/reference/budget_detect_format.md)
in their respective modules.

Detection logic:

- Marker columns (must all be present): `row_id`, `LAT`, `LNG`,
  `RESULTCODE`. Absence of any marker → `format = "unknown"`,
  `confidence = 0`.

- Exact match against the 29 v1 columns (case-sensitive) →
  `confidence = 1`.

- Markers present but some v1 columns missing or extra → `confidence`
  interpolated by Jaccard similarity: `|input ∩ v1| / |input ∪ v1|`.

- Threshold: `confidence >= 0.5` AND all markers present →
  `format = "melissa_v1_2026"`. Otherwise `"unknown"`.

## Usage

``` r
geocode_detect_format(x, sheet = "Sheet1")
```

## Arguments

- x:

  One of:

  - an `alprek_geocode_raw` object (output of
    [`geocode_read()`](https://joonho112.github.io/ALprekDB/reference/geocode_read.md))
    — column names are read from `x$meta$col_names`,

  - a character vector of column names, OR

  - a single character file path to an xlsx file — the first sheet's
    header row is read and used as the column names.

- sheet:

  Character. Sheet name to read when `x` is a path. Default `"Sheet1"`
  (the v1 contract).

## Value

A `list` with class `"alprek_geocode_format_detection"` and fields:

- `format` — character scalar, one of `"melissa_v1_2026"`, `"unknown"`.

- `confidence` — numeric in `[0, 1]`. `1` = exact match; `0` = marker
  columns absent; otherwise Jaccard similarity between input cols and v1
  cols.

- `unknown_columns` — character vector of columns present in the input
  that are NOT in the v1 contract (`character(0)` when none).

- `missing_v1_columns` — character vector of v1 contract columns absent
  from the input (`character(0)` when complete).

- `markers_found` — character vector of marker columns observed (subset
  of `c("row_id", "LAT", "LNG", "RESULTCODE")`).

- `n_input_cols` — integer count of input columns.

## See also

[`geocode_read()`](https://joonho112.github.io/ALprekDB/reference/geocode_read.md),
[`geocode_compare_deliveries()`](https://joonho112.github.io/ALprekDB/reference/geocode_compare_deliveries.md),
[`alprek_geocode_column_map()`](https://joonho112.github.io/ALprekDB/reference/alprek_geocode_column_map.md).

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- geocode_read(path, cycle_year = "2026-2027")
geocode_detect_format(raw)
# $format = "melissa_v1_2026", $confidence = 1

# From a character vector of column names
v1 <- alprek_geocode_column_map()$raw_col
geocode_detect_format(v1)

# From a file path (reads header row of Sheet1)
geocode_detect_format("ORIGINAL-DATA/2026-03-04_geocoding_master_Final.xlsx")
} # }
```
