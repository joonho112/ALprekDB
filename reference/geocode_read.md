# Read Melissa-Returned Geocoded Master File

Reads the Melissa-returned geocoded xlsx file (v1 contract, 29 columns)
and captures provenance (file SHA-256, git SHA, cycle year, receipt
date, sheet, raw row index, row lineage ID). Does **not** clean,
normalize, or coerce — that is
[`geocode_clean()`](https://joonho112.github.io/ALprekDB/reference/geocode_clean.md)'s
job. In particular, `LAT` / `LNG` are preserved as character per the
Melissa source contract (they are coerced to numeric only by
[`geocode_clean()`](https://joonho112.github.io/ALprekDB/reference/geocode_clean.md)).

Mirrors the read+provenance pattern established by
[`applications_read_renewals()`](https://joonho112.github.io/ALprekDB/reference/applications_read_renewals.md)
et al., except that the geocode module is the **first consumer of
row-level geocode lineage** introduced for v0.8.0. `lineage_id` is
stored directly in `$data` as a stable row key and mirrored in `$meta`
for compatibility.

## Usage

``` r
geocode_read(
  path,
  sheet = "Sheet1",
  cycle_year,
  receipt_date = Sys.Date(),
  source = "melissa",
  verbose = TRUE
)
```

## Arguments

- path:

  Character. Path to the Melissa-returned geocoded xlsx file (e.g.,
  `"ORIGINAL-DATA/2026-03-04_Pre-K Geocoding Melissa/2026-03-04_geocoding_master_Final.xlsx"`).
  Required. Existence is checked at call time with an informative error.

- sheet:

  Character. Worksheet name within the xlsx file. Default `"Sheet1"`
  (the v1 Melissa contract).

- cycle_year:

  Character. Cycle year label in `"YYYY-YYYY"` format (e.g.,
  `"2026-2027"`). Required.

- receipt_date:

  Date or character. Date the geocoded file was received from Melissa
  (e.g., `"2026-03-04"` or `as.Date("2026-03-04")`). Default
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

- source:

  Character. Geocoding source label, used in provenance tracking.
  Default `"melissa"` (the only supported vendor in v0.8.0). Reserved
  for future multi-vendor support.

- verbose:

  Logical. Print progress messages? Default `TRUE`.

## Value

An `alprek_geocode_raw` S3 object (list) with elements:

- `data`: tibble of raw Melissa data (29 columns as-is from the xlsx,
  plus `raw_row_index` and `lineage_id` columns for stable row
  tracking). LAT/LNG remain character per Melissa source contract.

- `meta`: list with `path`, `sheet`, `cycle_year`, `receipt_date`,
  `source`, `file_sha256`, `file_basename`, `git_sha`, `n_rows`,
  `n_cols`, `col_names`, `read_at`, `lineage_id` (compatibility mirror
  of `$data$lineage_id`), `raw_row_index` (`1:nrow`).

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- geocode_read(
  path = file.path("ORIGINAL-DATA",
                   "2026-03-04_Pre-K Geocoding Melissa",
                   "2026-03-04_geocoding_master_Final.xlsx"),
  cycle_year = "2026-2027",
  receipt_date = "2026-03-04"
)
raw
} # }
```
