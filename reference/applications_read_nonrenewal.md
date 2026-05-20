# Read ADECE Non-Renewal Classrooms

Reads the Non-Renew sheet from an ADECE master xlsx file. IMPORTANT:
This sheet has **no header row** in cycle-1; data starts at row 1. The
read function sets `col_names = FALSE` and assigns positional column
names (`col_1` .. `col_7`). Use
[`applications_clean()`](https://joonho112.github.io/ALprekDB/reference/applications_clean.md)
to rename via the `applications_column_map_nonrenewals_*` codebook.

## Usage

``` r
applications_read_nonrenewal(
  path,
  sheet = "Non-Renew",
  cycle_year,
  receipt_date = Sys.Date()
)
```

## Arguments

- path:

  Character. Path to the ADECE master xlsx file.

- sheet:

  Character. Sheet name. Default `"Non-Renew"`.

- cycle_year:

  Character. Cycle year label (e.g., `"2026-2027"`). Required.

- receipt_date:

  Date or character. Date file received from ADECE. Default
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

## Value

An `alprek_applications_raw` S3 object with `kind = "non_renewals"`.
