# Clean a single teacher role's data

Clean a single teacher role's data

## Usage

``` r
.clean_teacher_role(
  df,
  prefix,
  degree_patterns,
  race_mapping,
  language_mapping
)
```

## Value

List with `data` (updated df) and `unmatched` (tibble of unmatched
degree values).
