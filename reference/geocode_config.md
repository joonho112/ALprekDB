# Create a Geocoding Processing Configuration

Creates a typed configuration object that controls the geocoding module
pipeline (read -\> clean -\> validate -\> reconcile -\> followup -\>
export). The object is consumed by `geocode_process()` and the
lower-level step functions
([`geocode_read()`](https://joonho112.github.io/ALprekDB/reference/geocode_read.md),
[`geocode_clean()`](https://joonho112.github.io/ALprekDB/reference/geocode_clean.md),
[`geocode_validate()`](https://joonho112.github.io/ALprekDB/reference/geocode_validate.md),
[`geocode_reconcile()`](https://joonho112.github.io/ALprekDB/reference/geocode_reconcile.md),
etc.).

## Usage

``` r
geocode_config(
  path,
  sheet = "Sheet1",
  vendor = "melissa",
  cycle_year,
  delivery_date,
  seed = 20260520L,
  verbose = TRUE,
  authoritative_priority = c("melissa_first", "adece_first"),
  distance_threshold_rules = c("by_resultcode", "flat_100m", "flat_250m"),
  flat_threshold_m = 250L,
  tiered_thresholds = list(GS01 = 50, GS05 = 250, GS06 = 500, GS03 = Inf),
  acceptable_resultcodes = c("GS01", "GS05", "GS06"),
  al_lat_bounds = c(30, 36),
  al_lng_bounds = c(-89, -84)
)
```

## Arguments

- path:

  Character. Path to the Melissa-returned geocoded xlsx file (e.g.,
  `"ORIGINAL-DATA/2026-03-04_Pre-K Geocoding Melissa/2026-03-04_geocoding_master_Final.xlsx"`).
  Required at call site. Existence is **not** checked at constructor
  time —
  [`geocode_read()`](https://joonho112.github.io/ALprekDB/reference/geocode_read.md)
  is responsible for the file-system check so the config object can be
  assembled in dry-run / test contexts.

- sheet:

  Character. Worksheet name within the xlsx file. Default `"Sheet1"`
  (the v1 Melissa contract).

- vendor:

  Character. Geocoding vendor identifier. Default `"melissa"`. Reserved
  for future multi-vendor support.

- cycle_year:

  Character. Cycle year in `"YYYY-YYYY"` format (e.g., `"2026-2027"`).
  Required.

- delivery_date:

  Date or character. Date the geocoded file was delivered by the vendor
  (e.g., `"2026-03-04"` or `as.Date("2026-03-04")`). Required. Character
  input is coerced to `Date` via
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html); an unparsable
  string throws.

- seed:

  Integer. Random seed for reproducibility (deterministic tiebreaks,
  sampling for diagnostics). Default `20260520L`.

- verbose:

  Logical. Print progress messages? Default `TRUE`.

- authoritative_priority:

  Character. Which source wins when both ADECE and Melissa have valid
  coordinates but they disagree. One of
  `c("melissa_first", "adece_first")`. Default `"melissa_first"`.

- distance_threshold_rules:

  Character. How
  [`geocode_reconcile()`](https://joonho112.github.io/ALprekDB/reference/geocode_reconcile.md)
  decides which (ADECE, Melissa) lat/long pairs require manual followup.
  One of `c("by_resultcode", "flat_100m", "flat_250m")`. Default
  `"by_resultcode"` (per-RESULTCODE tiered thresholds — see
  `tiered_thresholds`).

- flat_threshold_m:

  Integer. Flat distance threshold in meters, used only when
  `distance_threshold_rules` starts with `"flat_"`. Default `250L`.

- tiered_thresholds:

  Named list. Per-RESULTCODE thresholds in meters used when
  `distance_threshold_rules == "by_resultcode"`. Names must be
  RESULTCODE strings (e.g., `"GS01"`); values may be `Inf` (always flag
  for review). Default
  `list(GS01 = 50, GS05 = 250, GS06 = 500, GS03 = Inf)`.

- acceptable_resultcodes:

  Character vector. Melissa RESULTCODE values expected to be
  master-acceptable under the current codebook. This is retained as
  configuration metadata and a print surface; the reconciler enforces
  `acceptable_for_master` from
  [`alprek_geocode_resultcode_meaning()`](https://joonho112.github.io/ALprekDB/reference/alprek_geocode_resultcode_meaning.md)
  so the CSV remains the source of truth. Default
  `c("GS01", "GS05", "GS06")`, matching
  `melissa_resultcode_codes.csv$acceptable_for_master == TRUE`.

- al_lat_bounds:

  Numeric length-2. Alabama latitude bounding box `c(min, max)` for the
  in-state sanity check. Default `c(30, 36)`.

- al_lng_bounds:

  Numeric length-2. Alabama longitude bounding box `c(min, max)`.
  Default `c(-89, -84)`.

## Value

An `alprek_geocode_config` S3 object — a named list with all parameters
above, ready for consumption by the geocode pipeline.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- geocode_config(
  path = file.path("ORIGINAL-DATA",
                   "2026-03-04_Pre-K Geocoding Melissa",
                   "2026-03-04_geocoding_master_Final.xlsx"),
  cycle_year = "2026-2027",
  delivery_date = "2026-03-04"
)
print(cfg)
} # }
```
