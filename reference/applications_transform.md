# Transform Reconciled Applications Into Master Object

Adds simple, data-layer derived variables to a reconciled applications
object and (optionally) to a cleaned capacity object. Mirrors the
[`budget_transform()`](https://joonho112.github.io/ALprekDB/reference/budget_transform.md)
/
[`student_transform()`](https://joonho112.github.io/ALprekDB/reference/student_transform.md)
API.

**In-scope derivations** (this function):

- Applications grain: `is_renewal`, `is_new`, `is_oversubscribed_app`
  (renewal had over-enrollment last cycle, if known),
  `applied_this_cycle` (always `TRUE` per row; the column gains meaning
  after
  [`applications_bind_years()`](https://joonho112.github.io/ALprekDB/reference/applications_bind_years.md)
  or a join to `classroom_panel`), `cycle_year` (carried from meta),
  `tier_prev_dollars`, `tier_prev_rank` (1-6 inferred from observed
  cycle-0 thresholds), `tier_prev_band`
  (`"high"`/`"medium"`/`"low"`/NA).

- Capacity grain (only if `capacity_clean` is provided):
  `capacity_utilization = enrollment / capacity` (NA-safe),
  `waitlist_ratio = waitlist / capacity` (NA-safe), `is_oversubscribed`
  (waitlist \> 0 OR enrollment \> capacity).

**Out-of-scope** (downstream packages - NOT computed here): geocoded
coordinates, ACS-weighted indicators, isochrone-derived features,
posterior tier from Bayesian SAE.

## Usage

``` r
applications_transform(
  reconciled,
  capacity_clean = NULL,
  tier_bands = c(0, 2550, 3570, 4590, 5610)
)
```

## Arguments

- reconciled:

  An `alprek_applications_reconciled` object.

- capacity_clean:

  Optional `alprek_applications_clean` with `meta$kind == "capacity"`.
  If supplied, capacity-grain derivations are added in `$capacity_data`.

- tier_bands:

  Numeric vector of breakpoints (default
  `c(0, 2550, 3570, 4590, 5610)`) - observed cycle-0 carry-forward
  dollar amounts per tier. Used to infer `tier_prev_rank`.

## Value

An `alprek_applications_master` S3 list with:

- `data`: applications-grain tibble (reconciled + 7 derived cols)

- `capacity_data`: capacity-grain tibble or NULL

- `derived_log`: tibble (variable, formula, n_non_na, n_na, note)

- `meta`: list inheriting from reconciled + `transformed_at`,
  `tier_bands`, `has_capacity`.

## Examples

``` r
if (FALSE) { # \dontrun{
rec <- applications_reconcile(ren_clean, new_clean, panel)
mst <- applications_transform(rec, capacity_clean = cap_clean)
mst
} # }
```
