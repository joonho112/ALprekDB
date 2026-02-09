# Export Linkage Data to Excel

Exports linked data to Excel format with optional summary. Requires the
`openxlsx` package.

## Usage

``` r
linkage_export_excel(x, path, include_summary = TRUE)
```

## Arguments

- x:

  An `alprek_linkage_classroom`, `alprek_linkage_student`, or
  `alprek_linkage_master` object.

- path:

  Character. Output file path.

- include_summary:

  Logical. Add a summary statistics sheet? Default `TRUE`.

## Value

Invisible file path.
