# Export Classroom Data to Excel

Exports processed classroom data to Excel format. Requires the
`openxlsx` package.

## Usage

``` r
classroom_export_excel(x, path, include_summary = TRUE)
```

## Arguments

- x:

  An `alprek_classroom_clean` or `alprek_classroom_panel` object.

- path:

  Character. Output file path.

- include_summary:

  Logical. Add a summary statistics sheet? Default `TRUE`.

## Value

Invisible file path.
