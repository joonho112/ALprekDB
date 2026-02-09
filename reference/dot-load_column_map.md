# Load Budget Column Mapping

Loads the column name mapping for a specific budget file format. Used
internally during the read step.

## Usage

``` r
.load_column_map(format)
```

## Arguments

- format:

  Character. Either `"legacy"` or `"new"`.

## Value

A tibble with columns: `raw_column`, `standard_name`, `type`, `notes`.
