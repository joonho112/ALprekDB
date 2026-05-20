# Read ADECE Site Capacity Report

Read ADECE Site Capacity Report

## Usage

``` r
applications_read_capacity(
  path,
  sheet = "rptSite_ClassroomsWithAvailabil",
  cycle_year,
  receipt_date = Sys.Date()
)
```

## Arguments

- path:

  Character. Path to the ADECE master xlsx file.

- sheet:

  Character. Sheet name. Default `"rptSite_ClassroomsWithAvailabil"`
  (sheet name truncated at 31 chars by Excel).

- cycle_year:

  Character. Cycle year label (e.g., `"2026-2027"`). Required.

- receipt_date:

  Date or character. Date file received from ADECE. Default
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

## Value

An `alprek_applications_raw` S3 object with `kind = "capacity"`.
