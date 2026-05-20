# Export Applications Data to Excel

Writes both grains (applications + capacity, if present) as separate
sheets, plus an optional `Summary` sheet. Requires `openxlsx`.

## Usage

``` r
applications_export_excel(x, path = NULL, include_summary = TRUE)
```

## Arguments

- x:

  An `alprek_applications_master` or `alprek_applications_panel`.

- path:

  Character. Output path. If `NULL`, auto-generates.

- include_summary:

  Logical. Add a per-cycle / per-bucket summary sheet? Default `TRUE`.

## Value

Invisible file path.
