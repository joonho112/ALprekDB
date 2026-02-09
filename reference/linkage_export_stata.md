# Export Linkage Data to Stata (.dta)

Exports linked data to Stata format. Requires the `haven` package. For
master objects, exports both levels.

## Usage

``` r
linkage_export_stata(x, path, version = 14)
```

## Arguments

- x:

  An `alprek_linkage_classroom`, `alprek_linkage_student`, or
  `alprek_linkage_master` object.

- path:

  Character. Output file path. For master objects, `_classroom` and
  `_student` suffixes are added.

- version:

  Integer. Stata file version (default 14).

## Value

Invisible file path(s).
