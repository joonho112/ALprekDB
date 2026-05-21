# Clean Melissa-Returned Geocoded Master Data

Standardizes column names, dtypes, and value formatting of an
`alprek_geocode_raw` object so downstream geocoding steps
([`geocode_validate()`](https://joonho112.github.io/ALprekDB/reference/geocode_validate.md),
[`geocode_reconcile()`](https://joonho112.github.io/ALprekDB/reference/geocode_reconcile.md))
can operate against a stable contract. The 11-step pipeline (in order)
is:

1.  Apply column map (rename if needed per
    [`alprek_geocode_column_map()`](https://joonho112.github.io/ALprekDB/reference/alprek_geocode_column_map.md);
    for v1 most names are unchanged).

2.  Coerce Melissa `LAT` / `LNG` from character to numeric. This is the
    key transformation. Coercion failures are logged.

3.  Defensively coerce ADECE `latitude` / `longitude` to numeric.

4.  Coerce `ERRORCODE` from logical to character. (`readxl` parses
    all-NA columns as logical — standardize to character so future
    deliveries that populate ERRORCODE compose with the same schema.)

5.  Keep ZIP-family fields character. `site_zip` is parsed as numeric by
    `readxl` from a numeric column; convert to character to preserve
    leading zeros if any. `GEOZIP` / `PLUS4` / `DPB` are already
    character but trimmed defensively.

6.  Standardize `school_year`: trim, validate against canonical set
    `{2021-2022, 2022-2023, 2023-2024, 2024-2025, 2025-2026_new}`.
    Unknown values are logged with severity `WARN`.

7.  Title-case `COUNTYNAME` (Melissa returns ALL-CAPS). Cross-validate
    against
    [`alprek_geocode_al_fips_counties()`](https://joonho112.github.io/ALprekDB/reference/alprek_geocode_al_fips_counties.md);
    unrecognized AL county names are logged with severity `WARN`.

8.  Trim whitespace on all character columns.

9.  Defensively coerce `has_latlon` to logical.

10. Attach `data_source_map` attribute that labels each column by
    provenance group: id/adece -\> "ADECE", melissa_norm/melissa_out -\>
    "Melissa-".

11. Drop lock-file artifact rows if any `~$*.xlsx` rows leaked into the
    data (defensive).

`raw_row_index` and `lineage_id` are preserved unchanged.

## Usage

``` r
geocode_clean(raw, config = NULL)
```

## Arguments

- raw:

  An `alprek_geocode_raw` object from
  [`geocode_read()`](https://joonho112.github.io/ALprekDB/reference/geocode_read.md).

- config:

  Optional `alprek_geocode_config` (from
  [`geocode_config()`](https://joonho112.github.io/ALprekDB/reference/geocode_config.md)).
  If `NULL`, a minimal default is constructed from `raw$meta`.

## Value

An `alprek_geocode_clean` S3 object (list) with elements:

- `data`: tibble of cleaned data; preserves source columns plus
  `raw_row_index` and `lineage_id`. `data_source_map` attribute names
  each column by provenance group.

- `cleaning_log`: tibble with columns `rule`, `n_affected`, `details`,
  `severity` (one of `INFO`/`WARN`/`ERROR`).

- `meta`: list inheriting key provenance from `raw$meta` (`file_sha256`,
  `git_sha`, `source`, `cycle_year`, `receipt_date`, `path`, `sheet`,
  `file_basename`) plus `geocoding_source = "melissa_v1_2026"`,
  row-index mirrors, `n_rows`, and `cleaned_at`.

## See also

[`geocode_read()`](https://joonho112.github.io/ALprekDB/reference/geocode_read.md),
[`alprek_geocode_column_map()`](https://joonho112.github.io/ALprekDB/reference/alprek_geocode_column_map.md),
[`alprek_geocode_al_fips_counties()`](https://joonho112.github.io/ALprekDB/reference/alprek_geocode_al_fips_counties.md).

## Examples

``` r
if (FALSE) { # \dontrun{
raw   <- geocode_read(path = "...", cycle_year = "2026-2027")
clean <- geocode_clean(raw)
clean
} # }
```
