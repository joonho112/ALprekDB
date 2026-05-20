# Read ADECE New Classroom Applications

Read ADECE New Classroom Applications

## Usage

``` r
applications_read_new(
  path,
  sheet = "26-27 new",
  cycle_year,
  receipt_date = Sys.Date()
)
```

## Arguments

- path:

  Character. Path to the ADECE master xlsx file.

- sheet:

  Character. Sheet name. Default `"26-27 new"`.

- cycle_year:

  Character. Cycle year label (e.g., `"2026-2027"`). Required.

- receipt_date:

  Date or character. Date file received from ADECE. Default
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

## Value

An `alprek_applications_raw` S3 object with `kind = "new_apps"`.
