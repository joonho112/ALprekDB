# Export Geocode Master / Panel / Reconciled Data to Parquet

Writes the `$data` slot to Apache Parquet. Requires the `arrow` package
(`Suggests`). Same row preservation contract as
[`geocode_export_csv()`](https://joonho112.github.io/ALprekDB/reference/geocode_export_csv.md).

## Usage

``` r
geocode_export_parquet(x, path = NULL, compression = "snappy", ...)
```

## Arguments

- x:

  An `alprek_geocode_master`, `alprek_geocode_panel`, or
  `alprek_geocode_reconciled` object.

- path:

  Character. Output path. If `NULL`, auto-generates
  `output/geocode/geocode_<run_id>.parquet`.

- compression:

  Character. Compression algorithm. Default `"snappy"`.

- ...:

  Forwarded to
  [`arrow::write_parquet()`](https://arrow.apache.org/docs/r/reference/write_parquet.html).

## Value

Invisible character path.
