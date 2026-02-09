# Export Classroom Data to Stata (.dta)

Exports processed classroom data to Stata format. Requires the `haven`
package.

## Usage

``` r
classroom_export_stata(x, path, version = 14)
```

## Arguments

- x:

  An `alprek_classroom_clean` or `alprek_classroom_panel` object.

- path:

  Character. Output file path.

- version:

  Integer. Stata file version (default 14).

## Value

Invisible file path.
