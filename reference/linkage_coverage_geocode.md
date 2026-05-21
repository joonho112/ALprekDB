# Summarize Geocode Coverage on a Classroom-Level Master

Builds a small coverage tibble that quantifies how many classroom-year
rows received an authoritative reconciled coordinate from the geocode
panel, how many still need follow-up, and how many are model-ready for
downstream Bayesian SAE consumers.

This helper is called by
[`linkage_create_master()`](https://joonho112.github.io/ALprekDB/reference/linkage_create_master.md)
when a geocode panel is supplied, and is also useful as a standalone
diagnostic on any tibble that has the prefixed `geocode_*` columns
introduced by
[`linkage_geocode_classroom()`](https://joonho112.github.io/ALprekDB/reference/linkage_geocode_classroom.md).

Recognized columns (all optional; the helper degrades gracefully):

- `geocode_lat_final` – presence -\> "has coord"

- `geocode_needs_followup_geocoding` – TRUE -\> "needs followup"

- `geocode_coord_model_status` – "model_ready" -\> "model ready"

- `geocode_lat_source` – factor levels rolled up

## Usage

``` r
linkage_coverage_geocode(classroom_level)
```

## Arguments

- classroom_level:

  A tibble (typically `master$classroom_level`) carrying the prefixed
  `geocode_*` columns. May also be any data frame; missing columns are
  treated as fully NA.

## Value

A list with named scalar metrics:

- `n_classroom_total` – total rows in `classroom_level`

- `n_classroom_with_coord` – rows with non-NA `geocode_lat_final`

- `n_needing_followup` – rows with
  `geocode_needs_followup_geocoding == TRUE`

- `n_model_ready` – rows with
  `geocode_coord_model_status == "model_ready"`

- `pct_with_coord` – 100 \* (n_with_coord / n_total)

- `pct_followup` – 100 \* (n_needing_followup / n_total)

- `pct_model_ready` – 100 \* (n_model_ready / n_total)

- `by_lat_source` – a `tibble` with `lat_source`, `n`, `pct` (or NULL if
  column absent)

- `by_coord_model_status` – a `tibble` with `coord_model_status`, `n`,
  `pct` (or NULL if column absent)
