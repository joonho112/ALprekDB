# Export Linkage Data to Parquet

Exports linked data to Parquet format. Requires the `arrow` package. For
master objects, exports both levels.

## Usage

``` r
linkage_export_parquet(x, path, compression = "snappy")
```

## Arguments

- x:

  An `alprek_linkage_classroom`, `alprek_linkage_student`, or
  `alprek_linkage_master` object.

- path:

  Character. Output file path. For master objects, `_classroom` and
  `_student` suffixes are added.

- compression:

  Character. Compression algorithm. Default `"snappy"`.

## Value

Invisible file path(s).
