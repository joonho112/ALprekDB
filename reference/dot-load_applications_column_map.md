# Load Applications Column Mapping

Loads the column name mapping for a specific applications source kind
and cycle. Used internally during the read step.

## Usage

``` r
.load_applications_column_map(kind, cycle = "cycle1")
```

## Arguments

- kind:

  Character. One of `"renewals"`, `"new"`, `"nonrenewals"`,
  `"capacity"`.

- cycle:

  Character. Currently `"cycle1"` only (cycle-1 = 2026-2027). Future
  cycles will add `"cycle2"`, etc.

## Value

A tibble with columns: `raw_column`, `standard_name`, `type`, `notes`.
