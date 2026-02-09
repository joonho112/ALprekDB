# Export Classroom Data to Parquet

Exports processed classroom data to Parquet format. Requires the `arrow`
package.

## Usage

``` r
classroom_export_parquet(x, path, compression = "snappy")
```

## Arguments

- x:

  An `alprek_classroom_clean` or `alprek_classroom_panel` object.

- path:

  Character. Output file path.

- compression:

  Character. Compression algorithm. Default `"snappy"`.

## Value

Invisible file path.
