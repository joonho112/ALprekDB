# Export Budget Data to CSV

Exports processed budget data to CSV format. Supports both wide
(default) and long format output.

## Usage

``` r
budget_export_csv(x, path = NULL, format = c("wide", "long"))
```

## Arguments

- x:

  An `alprek_budget_master` or `alprek_budget_panel` object.

- path:

  Character. Output file path. If `NULL`, auto-generates a name.

- format:

  Character. Output format: `"wide"` (default) or `"long"`.

## Value

Invisible file path of the written file.
