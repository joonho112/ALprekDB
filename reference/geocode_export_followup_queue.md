# Export the Geocode Follow-Up Queue as a Production-Ready CSV

Writes the analyst-facing follow-up queue (sites needing re-geocoding)
to a CSV at a predictable path. This is the user-facing surface for Goal
\#3 of v0.8.0: handing operations a named list of sites to
re-investigate after every Melissa delivery.

The function is a thin wrapper around
[`geocode_followup_queue()`](https://joonho112.github.io/ALprekDB/reference/geocode_followup_queue.md)
from `R/geocode-reconcile.R` that:

- Accepts either an `alprek_geocode_reconciled` (preferred) or an
  `alprek_geocode_panel` (multi-run; the panel is reduced to its `$data`
  and a synthetic reconciled-like shape is used to look up follow-up
  flags directly).

- Auto-generates the output path as
  `output/geocode/sites_needing_geocoding_<cycle_year>.csv` unless
  `path` is supplied.

- Prepends a clearly visible internal-use comment header to the CSV when
  `internal_use = TRUE` (the default), per the package privacy contract.
  The queue carries full site addresses and is NOT a public deliverable.

- Returns the in-memory tibble (invisibly) so callers can both write to
  disk and inspect the queue in the same expression.

The exported CSV's columns match the Step 4.4 queue exactly (and start
with `lineage_id` for traceability):
`lineage_id, row_id, school_year, site_code, site_name, site_street, site_city, site_state, site_zip, lat_source, coord_agreement_band, distance_adece_melissa_m, melissa_result_code, lat_precision, followup_reason, suggested_action`.

Per the follow-up action and privacy contract, the in-memory return
value also carries the attributes
`privacy_level = "internal_address_followup"` and
`contains_address_fields = TRUE`. When `internal_use = TRUE` the return
value additionally has `attr(., "internal_use") <- TRUE`.

## Usage

``` r
geocode_export_followup_queue(
  x,
  path = NULL,
  cycle_year = NULL,
  include_disputed = TRUE,
  internal_use = TRUE
)
```

## Arguments

- x:

  An `alprek_geocode_reconciled` or `alprek_geocode_panel`.

- path:

  Character. Output path. If `NULL`, auto-generates
  `output/geocode/sites_needing_geocoding_<cycle_year>.csv`.
  `<cycle_year>` is taken from `cycle_year` (if supplied) or from
  `x$meta$cycle_year`; falls back to `"unknown"` if neither resolves.

- cycle_year:

  Character or `NULL`. Override the auto-path's cycle token (and the
  comment header). Default `NULL`.

- include_disputed:

  Logical. Forwarded to
  [`geocode_followup_queue()`](https://joonho112.github.io/ALprekDB/reference/geocode_followup_queue.md).
  When `TRUE` (default), rows with `lat_source == "disputed_melissa"`
  are retained in the queue.

- internal_use:

  Logical. When `TRUE` (default), prepend an
  `# INTERNAL USE -- DO NOT REDISTRIBUTE` header to the CSV and set
  `attr(out, "internal_use") <- TRUE` on the returned tibble.

## Value

Invisibly returns the in-memory follow-up queue tibble (with privacy
attributes attached). The CSV at `path` is the primary side effect.

## See also

[`geocode_followup_queue()`](https://joonho112.github.io/ALprekDB/reference/geocode_followup_queue.md),
[`geocode_reconcile()`](https://joonho112.github.io/ALprekDB/reference/geocode_reconcile.md),
[`geocode_export_csv()`](https://joonho112.github.io/ALprekDB/reference/geocode_export_csv.md).

## Examples

``` r
if (FALSE) { # \dontrun{
raw   <- geocode_read(path = "...", cycle_year = "2026-2027")
rec   <- geocode_reconcile(geocode_clean(raw))
fq    <- geocode_export_followup_queue(rec)
nrow(fq)
attr(fq, "privacy_level")
} # }
```
