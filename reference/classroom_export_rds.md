# Export Classroom Data to RDS

Exports processed classroom data to R's native serialized format.

## Usage

``` r
classroom_export_rds(x, path, compress = TRUE)
```

## Arguments

- x:

  An `alprek_classroom_clean` or `alprek_classroom_panel` object.

- path:

  Character. Output file path.

- compress:

  Logical. Use compression? Default `TRUE`.

## Value

Invisible file path.
