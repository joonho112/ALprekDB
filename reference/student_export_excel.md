# Export Student Data to Excel

Exports processed student data to Excel format. Requires the `openxlsx`
package.

## Usage

``` r
student_export_excel(x, path, include_summary = TRUE)
```

## Arguments

- x:

  An `alprek_student_clean` or `alprek_student_panel` object.

- path:

  Character. Output file path.

- include_summary:

  Logical. Add a summary statistics sheet? Default `TRUE`.

## Value

Invisible file path.
