# Export Student Data to Stata (.dta)

Exports processed student data to Stata format. Requires the `haven`
package.

## Usage

``` r
student_export_stata(x, path, version = 14)
```

## Arguments

- x:

  An `alprek_student_clean` or `alprek_student_panel` object.

- path:

  Character. Output file path.

- version:

  Integer. Stata file version (default 14).

## Value

Invisible file path.
