# Export Budget Data to Excel

Exports processed budget data to Excel format. Requires the `openxlsx`
package.

## Usage

``` r
budget_export_excel(x, path, include_summary = TRUE)
```

## Arguments

- x:

  An `alprek_budget_master` or `alprek_budget_panel` object.

- path:

  Character. Output file path.

- include_summary:

  Logical. Add a summary statistics sheet? Default `TRUE`.

## Value

Invisible file path.
