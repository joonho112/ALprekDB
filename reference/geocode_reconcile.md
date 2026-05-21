# Reconcile ADECE and Melissa Geocoded Coordinates (Step 4.3)

Applies the LOCKED 15-cell decision matrix from Step 4.2 to a cleaned
Melissa-returned geocoded dataset. For each site row, picks an
authoritative (lat, lng) pair, records the precision tier, computes the
ADECE\<-\>Melissa distance (when both sources are present), assigns a
coordinate-agreement band, and flags rows that need analyst followup.
Every decision is logged in `reconciliation_log` with the matrix cell ID
so downstream consumers (Step 4.4 followup queue, Step 4.5 sanity tests)
can audit any single-row decision without re-running the reconciler.

Decision-matrix scope (Step 4.2 LOCKED):

- **D1-D9** – both sources present; outcome driven by RESULTCODE
  (`GS01`/`GS05`/`GS06`), per-tier threshold, and gross-outlier check
  (\>=10 km).

- **D10** – both sources present, RESULTCODE == `GS03`, any distance:
  `disputed_melissa` (centroid is unreliable; flag for followup).

- **D11** – ADECE only (Melissa unexpectedly missing): use ADECE, flag.

- **D12** – Melissa only (`GS01`/`GS05`): use Melissa, OK (Melissa is
  the authoritative geocode source for these rows).

- **D13** – Melissa only (`GS06`, interpolated rooftop): use Melissa,
  flag for analyst review.

- **D14** – Melissa only (`GS03`, ZIP centroid): use Melissa, flag.

- **D15** – both missing: no coordinate; flag with `both_missing`.

The matrix can be inverted at the priority level via
`config$authoritative_priority == "adece_first"`, in which case D1-D9
resolve to ADECE-anchored decisions (still flagged when distances exceed
the per-tier threshold).

Tier thresholds come from `config$tiered_thresholds` (default
`list(GS01 = 50, GS05 = 250, GS06 = 500, GS03 = Inf)`). The reconciler
never hardcodes RESULTCODE -\> precision_tier; it looks them up from
[`alprek_geocode_resultcode_meaning()`](https://joonho112.github.io/ALprekDB/reference/alprek_geocode_resultcode_meaning.md)
so future codebook updates flow through without code changes.
`acceptable_for_master` in that same codebook is also enforced:
unacceptable or unknown RESULTCODE values are retained for analyst
review but flagged and marked `not_model_ready`.

## Usage

``` r
geocode_reconcile(clean, config = NULL)
```

## Arguments

- clean:

  An `alprek_geocode_clean` object from
  [`geocode_clean()`](https://joonho112.github.io/ALprekDB/reference/geocode_clean.md).
  Must contain `latitude`, `longitude` (ADECE), `LAT`, `LNG` (Melissa),
  `RESULTCODE`, and an identifier column (`row_id` preferred,
  `raw_row_index` as fallback).

- config:

  Optional `alprek_geocode_config` (from
  [`geocode_config()`](https://joonho112.github.io/ALprekDB/reference/geocode_config.md)).
  When `NULL`, the reconciler uses the default LOCKED matrix:
  `authoritative_priority = "melissa_first"`,
  `distance_threshold_rules = "by_resultcode"`,
  `tiered_thresholds = list(GS01 = 50, GS05 = 250, GS06 = 500, GS03 = Inf)`.

## Value

An `alprek_geocode_reconciled` S3 list with elements:

- `data`: tibble (1 row per input row) with all original columns plus 10
  new authoritative columns:

  - `lat_final`, `lng_final` (numeric or NA)

  - `lat_source` (factor: `{melissa, adece, disputed_melissa, none}`)

  - `lat_precision` (ordered factor:
    `{none, unknown, centroid, zip5, zip4, area, parcel, rooftop}`,
    increasing precision)

  - `distance_adece_melissa_m` (numeric or NA)

  - `coord_agreement_band` (factor:
    `{exact, tight, loose, drift, gross, one_source_only, none}`)

  - `needs_followup_geocoding` (logical)

  - `followup_reason` (factor with controlled vocabulary)

  - `coord_model_status` (ordered factor:
    `{missing, not_model_ready, provisional_followup, model_ready}`)

  - `geocode_provenance` (compact character string)

- `reconciliation_log`: tibble (1 row per input row) of per-row audit
  records (`row_id`, `matrix_cell`, `decision_source`, etc.).

- `summary`: tibble of decision-cell counts.

- `meta`: list (`reconciled_at`, `authoritative_priority`,
  `distance_threshold_rules`, `tiered_thresholds`, `n_rows`,
  `n_needs_followup`, `n_disputed`, `n_lat_source_*`, `git_sha`,
  inherited from `clean$meta`).

## Decision matrix (15 cells)

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Cell | adece | melissa | RESULTCODE | dist vs tier | outcome |
| D1 | Y | Y | GS01 | \<=50m | melissa, zip4, no followup |
| D2 | Y | Y | GS01 | 50m..10km | melissa, zip4, flag (disagreement_above_threshold) |
| D3 | Y | Y | GS01 | \>=10km | disputed_melissa, zip4, flag (disagreement_gross) |
| D4 | Y | Y | GS05 | \<=250m | melissa, rooftop, no followup |
| D5 | Y | Y | GS05 | 250m..10km | melissa, rooftop, flag |
| D6 | Y | Y | GS05 | \>=10km | disputed_melissa, rooftop, flag (gross) |
| D7 | Y | Y | GS06 | \<=500m | melissa, parcel, no followup |
| D8 | Y | Y | GS06 | 500m..10km | melissa, parcel, flag |
| D9 | Y | Y | GS06 | \>=10km | disputed_melissa, parcel, flag (gross) |
| D10 | Y | Y | GS03 | any | disputed_melissa, zip5, flag (gs03_always) |
| D11 | Y | N | – | – | adece, unknown, flag (melissa_unexpectedly_missing) |
| D12 | N | Y | GS01/GS05 | – | melissa, zip4/rooftop, no followup |
| D13 | N | Y | GS06 | – | melissa, parcel, flag (melissa_only_interpolated) |
| D14 | N | Y | GS03 | – | melissa, zip5, flag (melissa_only_gs03) |
| D15 | N | N | – | – | none, none, flag (both_missing) |

## Band boundaries

`exact` (\<10 m), `tight` (10-100 m), `loose` (100 m-1 km), `drift`
(1-10 km), `gross` (\>=10 km), `one_source_only` (only ADECE or only
Melissa present), `none` (neither present).

## See also

[`geocode_clean()`](https://joonho112.github.io/ALprekDB/reference/geocode_clean.md),
[`geocode_config()`](https://joonho112.github.io/ALprekDB/reference/geocode_config.md),
[`alprek_geocode_resultcode_meaning()`](https://joonho112.github.io/ALprekDB/reference/alprek_geocode_resultcode_meaning.md),
[`alprek_haversine_m()`](https://joonho112.github.io/ALprekDB/reference/alprek_haversine_m.md).

## Examples

``` r
if (FALSE) { # \dontrun{
raw   <- geocode_read(path = "...", cycle_year = "2026-2027",
                      receipt_date = "2026-03-04")
clean <- geocode_clean(raw)
rec   <- geocode_reconcile(clean)
print(rec)
rec$summary
head(rec$data[, c("row_id", "lat_final", "lng_final",
                   "lat_source", "lat_precision",
                   "coord_agreement_band",
                   "needs_followup_geocoding")])
} # }
```
