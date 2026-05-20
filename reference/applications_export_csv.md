# Export Applications Data to CSV

Exports an `alprek_applications_master` or `alprek_applications_panel`
to CSV. Because the master/panel has two grains (applications +
capacity), use the `grain` argument to choose which one to write.
Mirrors
[`budget_export_csv()`](https://joonho112.github.io/ALprekDB/reference/budget_export_csv.md).

## Usage

``` r
applications_export_csv(x, path = NULL, grain = c("apps", "capacity"))
```

## Arguments

- x:

  An `alprek_applications_master` or `alprek_applications_panel`.

- path:

  Character. Output path. If `NULL`, auto-generates.

- grain:

  Character. `"apps"` (default) or `"capacity"`.

## Value

Invisible file path of the written file.
