# Create Master Linked Dataset

Creates a fully linked master dataset at two levels:

1.  **Classroom-level**: classroom + budget + student aggregates +
    derived vars. Optionally augmented with reconciled-geocode columns
    (from
    [`linkage_geocode_classroom()`](https://joonho112.github.io/ALprekDB/reference/linkage_geocode_classroom.md))
    and per-cycle applications context (from
    [`linkage_applications_classroom()`](https://joonho112.github.io/ALprekDB/reference/linkage_applications_classroom.md)).

2.  **Student-level**: student + classroom + budget columns.

**Backward compatibility (v0.7.0).** Calling with only the three
required panels produces output identical to v0.7.0: no `geocode_*`
columns and no application-context columns. Passing `geocode` and/or
`applications` activates the optional join branches without changing the
row count of `classroom_level` or `student_level`.

**Geocode join (v0.8.0 critical-path).** When `geocode` is a
`alprek_geocode_panel`, the classroom-level master inherits the 12
prefixed `geocode_*` columns produced by
[`linkage_geocode_classroom()`](https://joonho112.github.io/ALprekDB/reference/linkage_geocode_classroom.md):
the 10 authoritative reconcile columns plus `geocode_run_id` and
`geocode_lineage_id`. The ADECE `latitude` / `longitude` columns from
the classroom panel are left untouched (Decision §11.4: escape-hatch /
inspection).

**Applications join (v0.8.0).** When `applications` is a
`alprek_applications_master`,
[`linkage_applications_classroom()`](https://joonho112.github.io/ALprekDB/reference/linkage_applications_classroom.md)
is invoked and its `classroom_level` rows are merged onto
`classroom_level` for the applications' `cycle_year`. Rows outside that
cycle year retain the master's pre-application schema with
application-context columns left NA / FALSE.

**Order of operations.** budget -\> student aggregates -\>
classroom-level master (with derived per-child/per-seat budget) -\>
optional geocode join -\> optional applications join. The geocode join
runs first so that bucket-D applications (no `site_code` yet) can be
later wired against reconciled coordinates via the applications path.

## Usage

``` r
linkage_create_master(
  budget,
  classroom,
  student,
  geocode = NULL,
  applications = NULL
)
```

## Arguments

- budget:

  An `alprek_budget_panel` object.

- classroom:

  An `alprek_classroom_panel` object.

- student:

  An `alprek_student_panel` object.

- geocode:

  Optional. An `alprek_geocode_panel` object (typically from
  [`geocode_bind_years()`](https://joonho112.github.io/ALprekDB/reference/geocode_bind_years.md)).
  When supplied, the classroom-level master receives the 12 prefixed
  `geocode_*` columns described above. Default `NULL` (v0.7.0 behavior).

- applications:

  Optional. An `alprek_applications_master` object (typically from
  [`applications_transform()`](https://joonho112.github.io/ALprekDB/reference/applications_transform.md)).
  When supplied, the classroom-level master receives per-cycle
  application context for the applications' `cycle_year`. Default
  `NULL`.

## Value

An `alprek_linkage_master` S3 object (list) with elements:

- `classroom_level`: tibble with 1 row per classroom-year.

- `student_level`: tibble with 1 row per student-year.

- `diagnostics`: list of all join diagnostics. When `geocode` is
  supplied, includes `geocode_coverage` (from
  [`linkage_coverage_geocode()`](https://joonho112.github.io/ALprekDB/reference/linkage_coverage_geocode.md))
  and `geocode_linkage` (the diagnostic tibble from
  [`linkage_geocode_classroom()`](https://joonho112.github.io/ALprekDB/reference/linkage_geocode_classroom.md)).
  When `applications` is supplied, includes `applications_linkage` (the
  diagnostic tibble from
  [`linkage_applications_classroom()`](https://joonho112.github.io/ALprekDB/reference/linkage_applications_classroom.md)).

- `meta`: list with metadata, including `geocode` and `applications` run
  identifiers when those branches fired.

## See also

[`linkage_geocode_classroom()`](https://joonho112.github.io/ALprekDB/reference/linkage_geocode_classroom.md),
[`linkage_applications_classroom()`](https://joonho112.github.io/ALprekDB/reference/linkage_applications_classroom.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# v0.7.0 (3-arg) usage -- unchanged
master <- linkage_create_master(budget_panel, classroom_panel, student_panel)

# v0.8.0 with geocode panel
panel_g <- geocode_bind_years(geocode_master)
master  <- linkage_create_master(
  budget_panel, classroom_panel, student_panel,
  geocode = panel_g
)

# Full v0.8.0 with geocode + applications
master <- linkage_create_master(
  budget_panel, classroom_panel, student_panel,
  geocode      = panel_g,
  applications = app_master
)
} # }
```
