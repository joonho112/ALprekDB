# Read ADECE Renewal Classroom Applications

Reads the renewal classroom applications sheet from an ADECE master xlsx
file (cycle-1 layout). Captures provenance (file hash, git SHA, cycle
year, receipt date, sheet, raw row index, row lineage ID) but does NOT
clean or standardize columns — that is
[`applications_clean()`](https://joonho112.github.io/ALprekDB/reference/applications_clean.md)'s
job.

## Usage

``` r
applications_read_renewals(
  path,
  sheet = "26-27 requests_TW",
  cycle_year,
  receipt_date = Sys.Date()
)
```

## Arguments

- path:

  Character. Path to the ADECE master xlsx file.

- sheet:

  Character. Sheet name. Default `"26-27 requests_TW"` (cycle-1
  convention). For cycle-0 separate-file layout, the sheet name may be
  `NULL` (first sheet).

- cycle_year:

  Character. Cycle year label (e.g., `"2026-2027"`). Required.

- receipt_date:

  Date or character. Date file received from ADECE. Default
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

## Value

An `alprek_applications_raw` S3 object (list) with elements:

- `data`: tibble of raw data with column names as-is plus
  `raw_row_index` and stable `lineage_id`

- `meta`: list with kind = "renewals", path, sheet, cycle_year,
  receipt_date, file_sha256, n_rows, n_cols, col_names, git_sha, read_at

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- applications_read_renewals(
  path = "Copy of 2026-27 Classroom Applications_tw04202026 (003).xlsx",
  cycle_year = "2026-2027"
)
raw
} # }
```
