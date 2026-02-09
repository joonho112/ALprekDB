# Track Student Presence Across Years

Creates a binary matrix showing which students are present in which
school years (based on ADECE ID).

## Usage

``` r
student_track(panel)
```

## Arguments

- panel:

  An `alprek_student_panel` object.

## Value

A tibble with adece_id and one logical column per school year.

## Examples

``` r
if (FALSE) { # \dontrun{
panel <- student_bind_years(clean_list = list(c1, c2, c3, c4))
track <- student_track(panel)
# Count students present in all years
sum(rowSums(track[, -1]) == length(panel$years))
} # }
```
