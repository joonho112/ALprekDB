# Export Student Data to RDS

Exports processed student data to R's native serialized format.

## Usage

``` r
student_export_rds(x, path, compress = TRUE)
```

## Arguments

- x:

  An `alprek_student_clean` or `alprek_student_panel` object.

- path:

  Character. Output file path.

- compress:

  Logical. Use compression? Default `TRUE`.

## Value

Invisible file path.
