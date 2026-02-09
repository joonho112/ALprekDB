# Export Budget Data to Stata (.dta)

Exports processed budget data to Stata format. Requires the `haven`
package.

## Usage

``` r
budget_export_stata(x, path, version = 14)
```

## Arguments

- x:

  An `alprek_budget_master` or `alprek_budget_panel` object.

- path:

  Character. Output file path.

- version:

  Integer. Stata file version (default 14).

## Value

Invisible file path.
