# Load Classroom Column Mapping

Loads the column name mapping for a specific classroom file format. Used
internally during the read step to rename raw Excel columns to
standardized names.

## Usage

``` r
.load_classroom_column_map(format)
```

## Arguments

- format:

  Character. Either `"legacy"` (2021-2024, 100 columns) or `"new"`
  (2024-2025, 125 columns).

## Value

A tibble with columns: `raw_column`, `standard_name`, `type`, `notes`.
