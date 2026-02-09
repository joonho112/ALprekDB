# Export Budget Data to RDS

Exports processed budget data to R's native serialized format.

## Usage

``` r
budget_export_rds(x, path, compress = TRUE)
```

## Arguments

- x:

  An `alprek_budget_master` or `alprek_budget_panel` object.

- path:

  Character. Output file path.

- compress:

  Logical. Use compression? Default `TRUE`.

## Value

Invisible file path.
