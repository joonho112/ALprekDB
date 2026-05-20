# Track Classroom Presence Across Application Cycles

For each unique classroom (by `matched_classroom_code` for bucket A/B/C,
by composite key for D), reports which cycles it applied in. Mirrors
[`budget_track_classrooms()`](https://joonho112.github.io/ALprekDB/reference/budget_track_classrooms.md).

## Usage

``` r
applications_track_classrooms(panel)
```

## Arguments

- panel:

  An `alprek_applications_panel` object.

## Value

A tibble with one row per classroom + logical columns per cycle +
`n_cycles_present`, `all_cycles`, `first_cycle`, `last_cycle`.
