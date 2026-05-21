# Read a delivery xlsx with light provenance for compare_deliveries()

Internal helper for
[`geocode_compare_deliveries()`](https://joonho112.github.io/ALprekDB/reference/geocode_compare_deliveries.md).
Returns a small list with `$data`, `$sha256`, `$path`, `$sheet`,
`$col_names`, `$dtypes`, `$n_rows`, `$n_cols`. Reads with the same
defaults as
[`geocode_read()`](https://joonho112.github.io/ALprekDB/reference/geocode_read.md)
so dtype detection is consistent.

## Usage

``` r
.geocode_read_for_compare(path, sheet = "Sheet1")
```

## Arguments

- path:

  Character path to xlsx.

- sheet:

  Character sheet name (default `"Sheet1"`).

## Value

A list (see description).
