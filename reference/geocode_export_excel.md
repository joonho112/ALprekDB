# Export Geocode Master / Panel / Reconciled Data to Excel

Writes the `$data` slot to a single `Geocode` worksheet in an `.xlsx`
file. When `include_summary = TRUE` adds a second sheet `Summary`
carrying the `coord_model_status` distribution, the `lat_source`
distribution, and the count of rows flagged `needs_followup_geocoding`.
Requires the `openxlsx` package (`Suggests`).

## Usage

``` r
geocode_export_excel(x, path = NULL, include_summary = FALSE, ...)
```

## Arguments

- x:

  An `alprek_geocode_master`, `alprek_geocode_panel`, or
  `alprek_geocode_reconciled` object.

- path:

  Character. Output path. If `NULL`, auto-generates
  `output/geocode/geocode_<run_id>.xlsx`.

- include_summary:

  Logical. Add a summary sheet? Default `FALSE`.

- ...:

  Forwarded to
  [`openxlsx::saveWorkbook()`](https://rdrr.io/pkg/openxlsx/man/saveWorkbook.html).

## Value

Invisible character path.
