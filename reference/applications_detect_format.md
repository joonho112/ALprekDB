# Detect ADECE Applications Data Format

Inspects raw column names to determine which cycle's schema a particular
sheet conforms to. Returns one of `"cycle1"` (2026-2027 combined-file
layout), `"cycle0"` (2025-2026 separate-file layout), or `"unknown"`.
Used by
[`applications_clean()`](https://joonho112.github.io/ALprekDB/reference/applications_clean.md)
to pick the correct column mapping.

## Usage

``` r
applications_detect_format(x, kind = NULL)
```

## Arguments

- x:

  Either an `alprek_applications_raw` object (the recommended input) or
  a character vector of raw column names.

- kind:

  Character. The data kind being inspected — affects which marker
  columns are checked. One of `"renewals"`, `"new_apps"`,
  `"non_renewals"`, `"capacity"`. If `x` is an
  `alprek_applications_raw`, this is inferred from `x$meta$kind`.

## Value

A character scalar: `"cycle1"`, `"cycle0"`, or `"unknown"`.

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- applications_read_renewals(path, cycle_year = "2026-2027")
applications_detect_format(raw)        # likely "cycle1"
applications_detect_format(c("Classroom Code", "Site Code", "Region of Classroom"),
                             kind = "renewals")  # likely "cycle0"
} # }
```
