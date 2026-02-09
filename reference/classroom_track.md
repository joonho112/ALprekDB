# Track Classroom Presence Across Years

Creates a binary matrix showing which classrooms are present in which
school years.

## Usage

``` r
classroom_track(panel)
```

## Arguments

- panel:

  An `alprek_classroom_panel` object.

## Value

A tibble with classroom_code and one logical column per school year.

## Examples

``` r
if (FALSE) { # \dontrun{
panel <- classroom_bind_years(clean_list = list(c1, c2, c3))
track <- classroom_track(panel)
# Count classrooms present in all years
sum(rowSums(track[, -1]) == ncol(track) - 1)
} # }
```
