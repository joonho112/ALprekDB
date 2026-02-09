# Track Classroom Presence Across Years

Creates a summary showing which classrooms are present in which years,
useful for identifying new, continuing, and exiting classrooms.

## Usage

``` r
budget_track_classrooms(panel)
```

## Arguments

- panel:

  An `alprek_budget_panel` object.

## Value

A tibble with one row per classroom and logical columns for each year.
