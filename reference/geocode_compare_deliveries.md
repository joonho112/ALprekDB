# Compare Two Melissa Geocode Deliveries

Compares two Melissa-returned geocoded xlsx deliveries end-to-end:
schema (column set, dtypes, possible renames), enum value sets,
row-level changes (additions, removals, value updates), and
`_new`-placeholder → assigned-site_code resolution pairs. Returns a
structured `alprek_geocode_delivery_diff` object that the caller can
inspect manually or hand to the printer for a one-paragraph summary.

The verdict (`"compatible"`, `"compatible_with_additions"`,
`"breaking"`) follows Step 1.4 of the format-diff protocol: schema
breaking changes, duplicate `row_id`s in either file, or a sheet rename
always escalate to `"breaking"`; otherwise the delivery is
`"compatible"` if neither value sets nor any rows changed, else
`"compatible_with_additions"`.

## Usage

``` r
geocode_compare_deliveries(
  path_old,
  path_new,
  sheet = "Sheet1",
  rename_jw_threshold = 0.85,
  join_key = "row_id",
  enum_cols = c("school_year", "RESULTCODE", "STATUSCODE", "COUNTYNAME", "FIPS",
    "PLACENAME"),
  change_cols = c("site_code", "latitude", "longitude", "LAT", "LNG", "geocode_address",
    "site_street", "COUNTYNAME", "RESULTCODE"),
  verbose = FALSE
)
```

## Arguments

- path_old, path_new:

  Character. Paths to the two delivery xlsx files. Both must exist.

- sheet:

  Character. Sheet name to read in both files. Default `"Sheet1"`. A
  requested sheet that is absent from either file triggers
  `verdict = "breaking"`.

- rename_jw_threshold:

  Numeric in `[0, 1]`. Default `0.85`. Greedy 1-to-1 column-rename
  pairing requires Jaro-Winkler similarity at least this large. Higher =
  stricter pairing.

- join_key:

  Character. Column to join on for row-level comparison. Default
  `"row_id"`.

- enum_cols:

  Character vector. Columns whose distinct value sets to inspect.
  Default
  `c("school_year", "RESULTCODE", "STATUSCODE", "COUNTYNAME", "FIPS", "PLACENAME")`.

- change_cols:

  Character vector. Columns whose row-level values to diff. Default
  `c("site_code", "latitude", "longitude", "LAT", "LNG", "geocode_address", "site_street", "COUNTYNAME", "RESULTCODE")`.

- verbose:

  Logical. Print progress messages? Default `FALSE`.

## Value

An `alprek_geocode_delivery_diff` S3 list with elements:

- `$meta` — list of input paths, SHA-256 hashes, sheet names, dimensions
  for both deliveries, computed timestamp.

- `$schema_diff` — tibble (one row per column) with statuses
  `"in_both"`, `"dtype_changed"`, `"possible_rename"`, `"added"`,
  `"removed"`.

- `$value_set_diff` — tibble (one row per `(column, value)`) with
  statuses `"in_both"`, `"added"`, `"removed"`.

- `$rows_only_old`, `$rows_only_new` — tibbles of `row_id`s in only one
  delivery, augmented with `likely_replaced_by` / `likely_replaces` when
  a `_new` ↔ resolved pair was found.

- `$rows_changed` — tibble of per-row, per-column changes (NA-on-both =
  unchanged; NA-vs-value = changed).

- `$row_id_replaced_pairs` — tibble of `_new` → assigned-site_code
  row_id resolutions detected via
  `(school_year_root, site_name, geocode_address)`.

- `$summary` — tibble of `(metric, value)` headline counters.

- `$verdict` — one of `"compatible"`, `"compatible_with_additions"`,
  `"breaking"`.

- `$verdict_reasons` — character vector of reason strings driving the
  verdict.

## See also

[`geocode_detect_format()`](https://joonho112.github.io/ALprekDB/reference/geocode_detect_format.md),
[`geocode_read()`](https://joonho112.github.io/ALprekDB/reference/geocode_read.md),
[`alprek_geocode_column_map()`](https://joonho112.github.io/ALprekDB/reference/alprek_geocode_column_map.md).

## Examples

``` r
if (FALSE) { # \dontrun{
diff <- geocode_compare_deliveries(
  path_old = "ORIGINAL-DATA/2026-03-04_geocoding_master_Final.xlsx",
  path_new = "ORIGINAL-DATA/2026-09-15_geocoding_master_Final.xlsx"
)
diff
diff$verdict
diff$row_id_replaced_pairs
} # }
```
