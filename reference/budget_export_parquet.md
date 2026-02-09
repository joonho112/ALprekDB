# Export Budget Data to Parquet

Exports processed budget data to Parquet format for efficient storage
and fast reading. Requires the `arrow` package.

## Usage

``` r
budget_export_parquet(x, path, compression = "snappy")
```

## Arguments

- x:

  An `alprek_budget_master` or `alprek_budget_panel` object.

- path:

  Character. Output file path.

- compression:

  Character. Compression algorithm. Default is `"snappy"`.

## Value

Invisible file path.
