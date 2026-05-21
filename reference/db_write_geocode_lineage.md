# Write a geocode lineage row to DuckDB

Records one lineage row per write into `geocode_lineage`. The input `x`
may be an `alprek_geocode_clean`, `alprek_geocode_reconciled`,
`alprek_geocode_master`, or `alprek_geocode_panel`. For panels, one row
per `geocode_run_id` in `panel$binding_log` is appended.

Lineage columns:

- `geocode_run_id` (character)

- `source` (character; e.g., `"melissa"`)

- `cycle_year` (character)

- `snapshot_date` (character / ISO date)

- `file_sha256` (character)

- `git_sha` (character)

- `n_rows` (integer)

- `n_followup` (integer; counts when known)

- `distance_threshold_rules` (character)

- `flat_threshold_m` (integer)

- `written_at` (character timestamp)

## Usage

``` r
db_write_geocode_lineage(conn, x, run_id = NULL)
```

## Arguments

- conn:

  A DBI connection.

- x:

  One of `alprek_geocode_clean`, `alprek_geocode_reconciled`,
  `alprek_geocode_master`, or `alprek_geocode_panel`.

- run_id:

  Optional character — override the derived `geocode_run_id` (only
  honored when `x` is non-panel).

## Value

Invisible character `"geocode_lineage"`.
