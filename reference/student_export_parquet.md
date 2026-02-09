# Export Student Data to Parquet

Exports processed student data to Parquet format. Requires the `arrow`
package.

## Usage

``` r
student_export_parquet(x, path, compression = "snappy")
```

## Arguments

- x:

  An `alprek_student_clean` or `alprek_student_panel` object.

- path:

  Character. Output file path.

- compression:

  Character. Compression algorithm. Default `"snappy"`.

## Value

Invisible file path.
