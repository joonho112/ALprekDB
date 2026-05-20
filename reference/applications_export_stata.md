# Export Applications Data to Stata (.dta)

Writes the chosen grain to Stata format. Stata's column-name limit (32
chars) and reserved-keyword constraints may cause column renames; the
caller is responsible for naming if necessary. Requires `haven`.

## Usage

``` r
applications_export_stata(
  x,
  path = NULL,
  version = 14,
  grain = c("apps", "capacity")
)
```

## Arguments

- x:

  An `alprek_applications_master` or `alprek_applications_panel`.

- path:

  Character. Output path. If `NULL`, auto-generates.

- version:

  Integer. Stata file version (default `14`).

- grain:

  Character. `"apps"` (default) or `"capacity"`.

## Value

Invisible file path.
