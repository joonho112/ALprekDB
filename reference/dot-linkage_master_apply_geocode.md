# Apply the geocode-classroom linkage to an in-progress classroom_level tibble.

Calls
[`linkage_geocode_classroom()`](https://joonho112.github.io/ALprekDB/reference/linkage_geocode_classroom.md)
on the geocode panel + the original classroom panel to obtain the
prefixed `geocode_*` columns, then attaches the slim per-(site_code,
school_year) lookup to `classroom_level` via a left-join on
`(classroom_code, school_year)`. We join on classroom_code rather than
re-running the site-level join here so that we preserve
classroom_level's exact row order.

## Usage

``` r
.linkage_master_apply_geocode(classroom_level, classroom_panel, geocode_panel)
```

## Details

Returns the augmented tibble with an attached `.geocode_linkage_meta`
attribute carrying the diagnostics + meta from the underlying call.
