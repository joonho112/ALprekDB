# Clean ADECE Applications Data

Standardizes column names and types of an `alprek_applications_raw`
object using cycle-specific column mappings from
`inst/extdata/mappings/applications_column_map_<kind>_<cycle>.csv`.
Filters out known noise rows (e.g., "Show the Debugger Trace Report"),
drops capacity-sheet aggregate rows that have no `site_code`, and
preserves per-row `raw_row_index`/`lineage_id` plus `data_source` for
provenance.

No geocoding, address parsing, or spatial work happens here — those are
handled by downstream packages.

## Usage

``` r
applications_clean(raw, cycle = NULL, remove_noise_rows = TRUE)
```

## Arguments

- raw:

  An `alprek_applications_raw` object (from `applications_read_*()`).

- cycle:

  Character. Cycle schema label. Default auto-detected via
  [`applications_detect_format()`](https://joonho112.github.io/ALprekDB/reference/applications_detect_format.md).

- remove_noise_rows:

  Logical. Drop rows whose `process_name` maps to
  `kind_inferred == "noise"` in `applications_status_codes.csv`? Default
  `TRUE`.

## Value

An `alprek_applications_clean` S3 object with elements:

- `data`: tibble of cleaned data with standardized column names and row
  lineage fields

- `cleaning_log`: tibble of changes applied (variable, rule, n_rows),
  including parse failures and aggregate-row drops

- `meta`: list inheriting from raw + cycle, n_rows_in, n_rows_out,
  n_rows_dropped, cleaned_at

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- applications_read_renewals(path, cycle_year = "2026-2027")
clean <- applications_clean(raw)
clean
} # }
```
