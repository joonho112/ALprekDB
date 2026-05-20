# Combine Multiple Cycles into an Applications Panel

Stacks two or more `alprek_applications_master` objects (one per cycle)
into a longitudinal `alprek_applications_panel`. Mirrors
[`budget_bind_years()`](https://joonho112.github.io/ALprekDB/reference/budget_bind_years.md).
Preserves the **two-grain** structure of the applications module:
per-application rows go to `$data`, per-site capacity rows go to
`$capacity_data`.

Each cycle's `cycle_year` (from `master$meta$cycle_year`) is asserted to
be present and unique across the inputs; the panel rows are sorted by
`cycle_year` then by `application_id` (or `site_code` for capacity).

## Usage

``` r
applications_bind_years(..., master_list = NULL)
```

## Arguments

- ...:

  `alprek_applications_master` objects to combine.

- master_list:

  Optional list of `alprek_applications_master` objects. Alternative to
  `...` for programmatic use.

## Value

An `alprek_applications_panel` S3 list with elements:

- `data`: applications-grain long panel (one row per application-cycle)

- `capacity_data`: capacity-grain long panel or NULL if no inputs had
  capacity

- `cycle_years`: sorted vector of distinct cycle_year values

- `n_cycles`: number of cycles in the panel

- `by_cycle`: per-cycle summary list (`cycle_year`, `n_apps`,
  `n_capacity`, `n_buckets`)

- `meta`: `binded_at` timestamp + `tier_bands` (from first master)

## Examples

``` r
if (FALSE) { # \dontrun{
mst_2526 <- applications_transform(rec_2526, cap_2526)
mst_2627 <- applications_transform(rec_2627, cap_2627)
panel <- applications_bind_years(mst_2526, mst_2627)
panel
} # }
```
