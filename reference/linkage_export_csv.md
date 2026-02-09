# Export Linkage Data to CSV

Exports linked data to CSV format. For master objects, exports both
classroom-level and student-level files.

## Usage

``` r
linkage_export_csv(x, path)
```

## Arguments

- x:

  An `alprek_linkage_classroom`, `alprek_linkage_student`, or
  `alprek_linkage_master` object.

- path:

  Character. Output file path. For master objects, this is the base path
  — `_classroom` and `_student` suffixes are added automatically.

## Value

Invisible file path(s) of the written file(s).
