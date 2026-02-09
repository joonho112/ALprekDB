# Export Linkage Data to RDS

Exports linked data to R's native serialized format.

## Usage

``` r
linkage_export_rds(x, path, compress = TRUE)
```

## Arguments

- x:

  An `alprek_linkage_classroom`, `alprek_linkage_student`, or
  `alprek_linkage_master` object.

- path:

  Character. Output file path.

- compress:

  Logical. Use compression? Default `TRUE`.

## Value

Invisible file path.
