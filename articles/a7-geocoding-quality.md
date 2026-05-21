# A7: Geocoding Quality

## 1. Overview

The **geocode** module (added in v0.8.0) reconciles two coordinate
sources for each Alabama First Class Pre-K site:

- **Melissa.com** — commercial geocoding vendor (delivered as a
  29-column Excel file each release cycle). Melissa returns site
  latitude / longitude plus a `RESULTCODE` describing the precision tier
  of every lookup.
- **ADECE** — administrative coordinates that ADECE staff maintain in
  their internal classroom data (carried through the
  `alprek_classroom_panel`’s `latitude` / `longitude` columns).

Both sources have known weaknesses. Melissa can fall back to a ZIP
centroid when an address is unparseable. ADECE coordinates can drift if
a site moves or if the original entry was a Google-Maps-on-iPhone tap
rather than a careful rooftop pin. The geocode module’s job is to **pick
one authoritative coordinate per site-year**, **explain why**, and
**flag the rows that need analyst follow-up**.

This vignette walks through that pipeline end-to-end on **synthetic
data**
([`alprek_synthetic_geocode()`](https://joonho112.github.io/ALprekDB/reference/alprek_synthetic_geocode.md)),
so every chunk runs without an ADECE/Melissa file. Real-data calls look
identical — just point
[`geocode_read()`](https://joonho112.github.io/ALprekDB/reference/geocode_read.md)
at the actual Melissa `.xlsx`.

``` r

library(ALprekDB)
```

The module follows the same shape as every other ALprekDB module: read
-\> detect -\> clean -\> validate -\> reconcile -\> transform -\> panel
-\> export -\> linkage -\> DuckDB persistence.

## 2. Scope (v0.8.0)

**In-scope** (this module):

- Single Melissa snapshot (the `2026-03-04` v1 delivery)
- 7 codebooks: `geocode_column_map_melissa_v1`,
  `melissa_resultcode_codes`, `melissa_statuscode_codes`,
  `melissa_errorcode_codes`, `geocode_al_fips_counties`,
  `geocode_source_manifest`, `geocode_edge_cases`
- 15-cell decision matrix for picking an authoritative coordinate
- Per-tier distance thresholds (50 m / 250 m / 500 m / Inf) from
  observed Melissa precision medians
- 10 authoritative reconcile columns (incl. `coord_model_status`)
- Multi-format export (CSV / Parquet / Excel / RDS / Stata)
- Follow-up queue CSV exporter
- Linkage to `classroom_panel` and `applications_master`
- DuckDB persistence (4 new tables)

**Out-of-scope** (intentional; mirrors the v0.7.0 stance retraction in
NEWS):

- ACS area-weighted aggregation -\> future `ALprekACS`
- OSRM isochrone / drive-time -\> future package
- Bayesian small-area estimation -\> future `ALprekSAE`
- Live vendor API calls — permanent out-of-scope
- Multi-source consensus (ArcGIS / Google fallback) — future release

The acceptability of any Melissa `RESULTCODE` for SAE modeling is
governed by the **codebook**
(`inst/extdata/codebooks/melissa_resultcode_codes.csv`,
`acceptable_for_master` column), not by Melissa’s vendor documentation.
The decision matrix and reconciler read that codebook through
[`alprek_geocode_resultcode_meaning()`](https://joonho112.github.io/ALprekDB/reference/alprek_geocode_resultcode_meaning.md)
— there is **no hardcoded vendor semantics in R/**. To change a tier’s
acceptability, edit the CSV and the package picks it up.

## 3. Read the Melissa file

[`geocode_read()`](https://joonho112.github.io/ALprekDB/reference/geocode_read.md)
reads the Melissa-returned xlsx (default sheet: `Sheet1`), stamps
`raw_row_index` and a row-stable `lineage_id`, and captures full
provenance (file SHA-256, git SHA, cycle year, receipt date) in `$meta`.

``` r

raw <- geocode_read(
  path         = file.path("ORIGINAL-DATA",
                            "2026-03-04_Pre-K Geocoding Melissa",
                            "2026-03-04_geocoding_master_Final.xlsx"),
  cycle_year   = "2026-2027",
  receipt_date = "2026-03-04"
)
raw
#> <alprek_geocode_raw>
#>   path:          .../2026-03-04_geocoding_master_Final.xlsx
#>   cycle_year:    2026-2027
#>   receipt_date:  2026-03-04
#>   n_rows: 3396  n_cols: 29
```

For the rest of this vignette we use synthetic data of identical shape:

``` r

g_synth <- alprek_synthetic_geocode(
  n_sites = 30L,
  n_years = 3L,
  seed    = 42L
)
dim(g_synth)
#> [1] 90 29
names(g_synth)[1:12]
#>  [1] "row_id"          "school_year"     "site_name"       "site_code"      
#>  [5] "geocode_address" "site_street"     "site_city"       "site_state"     
#>  [9] "site_zip"        "latitude"        "longitude"       "has_latlon"
```

[`alprek_synthetic_geocode()`](https://joonho112.github.io/ALprekDB/reference/alprek_synthetic_geocode.md)
returns a flat 29-column tibble that matches the Melissa v1 contract
exactly. To pass it through the rest of the pipeline we wrap it in an
`alprek_geocode_raw` shell (this mirrors
[`geocode_read()`](https://joonho112.github.io/ALprekDB/reference/geocode_read.md)’s
output and is the same approach the test suite uses):

``` r

fake_geocode_raw <- function(df,
                              cycle_year   = "2026-2027",
                              receipt_date = "2026-03-04") {
  # Mirror the dtype contract that readxl produces on a real v1 file.
  df$LAT        <- as.character(df$LAT)
  df$LNG        <- as.character(df$LNG)
  df$ERRORCODE  <- as.logical(df$ERRORCODE)
  df$site_zip   <- as.numeric(df$site_zip)
  df$COUNTYNAME <- toupper(df$COUNTYNAME)
  df$raw_row_index <- seq_len(nrow(df))
  df$lineage_id    <- sprintf("synth_%06d", df$raw_row_index)
  meta <- list(
    path          = "/tmp/fake-melissa.xlsx",
    sheet         = "Sheet1",
    cycle_year    = cycle_year,
    receipt_date  = receipt_date,
    source        = "melissa",
    file_sha256   = paste(rep("a", 64L), collapse = ""),
    file_basename = "synthetic-melissa.xlsx",
    git_sha       = "synthetic",
    n_rows        = nrow(df),
    n_cols        = ncol(df) - 2L,
    col_names     = setdiff(names(df), c("raw_row_index", "lineage_id")),
    read_at       = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    lineage_id    = df$lineage_id,
    raw_row_index = df$raw_row_index
  )
  structure(list(data = tibble::as_tibble(df), meta = meta),
            class = "alprek_geocode_raw")
}

raw <- fake_geocode_raw(g_synth)
raw
#> <alprek_geocode_raw>
#>   Source:       melissa 
#>   File:         synthetic-melissa.xlsx 
#>   Sheet:        Sheet1 
#>   Cycle year:   2026-2027 
#>   Receipt:      2026-03-04 
#>   SHA-256:      aaaaaaaaaaaaaaaa ...
#>   Rows x Cols: 90 x 29
#>   Read at:      2026-05-21 18:50:11 UTC
```

## 4. Detect, clean, validate

### Detect

[`geocode_detect_format()`](https://joonho112.github.io/ALprekDB/reference/geocode_detect_format.md)
examines marker columns (`RESULTCODE`, `STATUSCODE`, the 29-column
contract) to classify the input as `"v1"` (the 2026-03-04 contract),
`"unknown_with_overlap"`, or `"unknown"`. This is the seam future
Melissa deliveries plug into.

``` r

fmt <- geocode_detect_format(raw)
fmt$format
#> [1] "melissa_v1_2026"
fmt$confidence
#> [1] 1
```

### Clean

[`geocode_clean()`](https://joonho112.github.io/ALprekDB/reference/geocode_clean.md)
runs an 11-step pipeline: applies the column map, coerces `LAT` / `LNG`
from character to numeric (the **key** transformation), coerces
`ERRORCODE` to character, preserves ZIP-family fields, validates
`school_year`, title-cases `COUNTYNAME`, trims whitespace, and attaches
a `data_source_map` attribute (ADECE vs. Melissa-2026-03) on every
column. `lineage_id` is preserved unchanged.

``` r

clean <- geocode_clean(raw)
clean
#> <alprek_geocode_clean>
#>   Source:            melissa 
#>   Geocoding source:  melissa_v1_2026 
#>   File:              synthetic-melissa.xlsx 
#>   Cycle year:        2026-2027 
#>   Receipt date:      2026-03-04 
#>   SHA-256:          aaaaaaaaaaaaaaaa...
#>   Rows: in=90 out=90 dropped=0
#>   Cols:              31 
#>   Cleaning log:     8 rule(s)
#>                     severity:  INFO=8 
#>   Cleaned at:        2026-05-21 18:50:11 UTC
```

The `$cleaning_log` records every rule applied, with `INFO` / `WARN` /
`ERROR` severity:

``` r

clean$cleaning_log[, c("rule", "n_affected", "severity")]
#> # A tibble: 8 × 3
#>   rule                          n_affected severity
#>   <chr>                              <int> <chr>   
#> 1 apply_column_map                       0 INFO    
#> 2 coerce_LAT_to_numeric                 90 INFO    
#> 3 coerce_LNG_to_numeric                 90 INFO    
#> 4 coerce_ERRORCODE_to_character         90 INFO    
#> 5 site_zip_to_character                 90 INFO    
#> 6 COUNTYNAME_title_case                 90 INFO    
#> 7 trim_whitespace                        0 INFO    
#> 8 attach_data_source_map                31 INFO
```

### Validate

[`geocode_validate()`](https://joonho112.github.io/ALprekDB/reference/geocode_validate.md)
runs 15 checks against the codebook contracts. Real-data v1 (3,396 rows)
typically returns `passed = TRUE` with a handful of WARN-level rows for
known-quirky records.

``` r

v <- geocode_validate(clean)
v
#> <alprek_geocode_validation>
#>   Overall: PASSED
#>   Errors: 0 | Warnings: 0 | Info: 1
#> 
#>   Checks:
#>     [+] required_columns -- All 29 expected columns present -- All 29 expected columns present
#>     [+] row_id_unique -- row_id is unique across all rows -- 90 unique row_id values
#>     [+] row_id_format -- row_id matches {YYYY-YYYY}_{site_code} or *_new_NNNN -- all row_id values match canonical pattern
#>     [+] school_year_canonical -- school_year is in the canonical 5-level set -- all values in {2021-2022, 2022-2023, 2023-2024, 2024-2025, 2025-2026_new}
#>     [+] site_code_missingness_in_new_only -- site_code NA only when school_year ends in '_new' -- 3 _new row(s) with NA site_code (allowed by contract)
#>     [+] melissa_lat_lng_present -- Melissa LAT and LNG are 100% non-NA -- all rows have non-NA Melissa LAT/LNG
#>     [+] has_latlon_consistency -- has_latlon == !is.na(latitude) -- has_latlon agrees with !is.na(latitude) on all rows
#>     [+] melissa_coord_in_al_bounds -- Melissa coords in AL bounds [30,36] x [-89,-84] -- all 90 non-NA Melissa coord(s) within AL bounds
#>     [+] adece_coord_in_al_bounds -- ADECE coords in AL bounds [30,36] x [-89,-84] -- all 81 non-NA ADECE coord(s) within AL bounds
#>     [+] resultcode_canonical -- RESULTCODE is in the documented Melissa set {GS01..GS08} -- all RESULTCODE values in {GS01, GS02, GS03, GS04, GS05, GS06, GS07, GS08}
#>     [+] statuscode_canonical -- STATUSCODE in observed codebook set -- all STATUSCODE values in {9, 5, A, B}
#>     [+] resultcode_statuscode_consistency -- RESULTCODE <-> STATUSCODE follows codebook 1:1 pairing -- all known-RESULTCODE rows follow GS01<->9, GS03<->5, GS06<->A, GS05<->B
#>     [+] errorcode_all_na_in_v080 -- ERRORCODE is 100% NA (v0.8.0 contract) -- ERRORCODE is 100% NA (matches v0.8.0 contract)
#>     [+] provenance_complete -- meta carries file_sha256, cycle_year, receipt_date, git_sha -- meta has all provenance keys (file_sha256, cycle_year, receipt_date, git_sha)
#>     [+] lineage_id_complete -- lineage_id exists, is non-blank, and is unique -- lineage_id present, non-blank, and unique for every row
#>     [i] summary_coverage -- Summary coverage and follow-up queue estimate -- RESULTCODE coverage: GS03=11.1%, GS05=77.8%, GS06=11.1%; follow-up queue estimate: 26 row(s) (~28.9%); PLACENAME missingness: 0 row(s) (~0.0%)
```

## 5. The decision matrix

The reconciler picks an authoritative `(lat, lng)` pair per row using a
**15-cell decision matrix**. Each cell is keyed on three inputs:

1.  Is ADECE present? (`latitude` / `longitude` non-NA)
2.  Is Melissa present? (`LAT` / `LNG` non-NA after numeric coercion)
3.  Melissa `RESULTCODE` (precision tier; see codebook below)

The four Melissa codes observed in v1 are:

| `RESULTCODE` | Melissa label | Precision tier in `lat_precision` | Per-tier threshold |
|----|----|----|----|
| `GS01` | Street level (ZIP+4 / Plus4) | `zip4` | 50 m |
| `GS05` | Rooftop within property boundaries | `rooftop` | 250 m |
| `GS06` | Interpolated rooftop | `parcel` | 500 m |
| `GS03` | Community / ZIP centroid | `zip5` | Inf (always flag) |

These labels are vendor-accurate per Melissa’s *Result Code Details*
documentation. The first cycle of this module (pre-Step 2.1) used
inverted labels; the v0.8.0 codebook is the corrected reference.

### The 15 cells (Step 4.2 LOCKED)

| Cell | ADECE | Melissa | `RESULTCODE` | Distance band | Outcome |
|----|----|----|----|----|----|
| D1 | Y | Y | `GS01` | \<=50 m | melissa, zip4, no follow-up |
| D2 | Y | Y | `GS01` | 50 m - 10 km | melissa, zip4, flag (`disagreement_above_threshold`) |
| D3 | Y | Y | `GS01` | \>=10 km | `disputed_melissa`, flag (`disagreement_gross`) |
| D4 | Y | Y | `GS05` | \<=250 m | melissa, rooftop, no follow-up |
| D5 | Y | Y | `GS05` | 250 m - 10 km | melissa, rooftop, flag |
| D6 | Y | Y | `GS05` | \>=10 km | `disputed_melissa`, flag (gross) |
| D7 | Y | Y | `GS06` | \<=500 m | melissa, parcel, no follow-up |
| D8 | Y | Y | `GS06` | 500 m - 10 km | melissa, parcel, flag |
| D9 | Y | Y | `GS06` | \>=10 km | `disputed_melissa`, flag (gross) |
| D10 | Y | Y | `GS03` | any | `disputed_melissa`, flag (`resultcode_gs03_always_flag`) |
| D11 | Y | N | – | – | adece, flag (`melissa_unexpectedly_missing`) |
| D12 | N | Y | `GS01`/`GS05` | – | melissa, no flag |
| D13 | N | Y | `GS06` | – | melissa, flag (`melissa_only_interpolated`) |
| D14 | N | Y | `GS03` | – | melissa, flag (`melissa_only_gs03`) |
| D15 | N | N | – | – | none, flag (`both_missing`) |

`GS03` (ZIP centroid) is **always** flagged because a ZIP centroid is
not a site coordinate even when it happens to land near the ADECE
coordinate. Empirically, `GS03` rows in the v1 delivery have a median
ADECE-Melissa disagreement of ~4 km; treating them as model-ready would
inject ZIP-grain noise into a site-grain estimator.

You can swap the per-tier scheme for a flat threshold:

``` r

cfg_flat <- geocode_config(
  path                     = "...",
  cycle_year               = "2026-2027",
  delivery_date            = "2026-03-04",
  distance_threshold_rules = "flat_250m"
)
```

`by_resultcode` is the default (and what we use for the rest of this
vignette).

## 6. Reconcile

[`geocode_reconcile()`](https://joonho112.github.io/ALprekDB/reference/geocode_reconcile.md)
walks every row through the matrix, computes the ADECE-Melissa haversine
distance when both are present, and emits 10 new authoritative columns.

``` r

rec <- geocode_reconcile(clean)
rec
#> <alprek_geocode_reconciled>
#>   Authoritative priority:  melissa_first
#>   Distance rule:           by_resultcode
#>   Tiered thresholds (m):
#>     GS01  = 50
#>     GS05  = 250
#>     GS06  = 500
#>     GS03  = Inf (always flag)
#>   Rows:                    90
#>     lat_source = melissa:          81
#>     lat_source = adece:            0
#>     lat_source = disputed_melissa: 9
#>     lat_source = none:             0
#>   needs_followup_geocoding: 12 (13.3%)
#>   Reconciled at:           2026-05-21 18:50:12 UTC
#> 
#>   Decision-cell counts:
#>     D1   n=    0 Both present, GS01, within 50m: melissa, zip4
#>     D2   n=    0 Both present, GS01, 50m-10km: melissa, zip4, flagged
#>     D3   n=    0 Both present, GS01, >=10km: disputed_melissa, zip4, gross
#>     D4   n=   64 Both present, GS05 within 250m: melissa, rooftop; future unacceptable codes route here flagged
#>     D5   n=    0 Both present, GS05, 250m-10km: melissa, rooftop, flagged
#>     D6   n=    0 Both present, GS05, >=10km: disputed_melissa, rooftop, gross
#>     D7   n=    8 Both present, GS06, within 500m: melissa, parcel
#>     D8   n=    0 Both present, GS06, 500m-10km: melissa, parcel, flagged
#>     D9   n=    0 Both present, GS06, >=10km: disputed_melissa, parcel, gross
#>     D10  n=    9 Both present, GS03 (ZIP centroid): disputed_melissa, zip5
#>     D11  n=    0 ADECE only (Melissa unexpectedly missing)
#>     D12  n=    6 Melissa only (GS01/GS05): melissa, no follow-up; future unacceptable codes route here flagged
#>     D13  n=    2 Melissa only (GS06 interpolated rooftop): flagged
#>     D14  n=    1 Melissa only (GS03 ZIP centroid): flagged
#>     D15  n=    0 Both missing: no coordinate, flagged
```

The 10 authoritative columns added to `$data`:

| Column | Type | Meaning |
|----|----|----|
| `lat_final` / `lng_final` | numeric | The chosen coordinate (NA where matrix cell == D15) |
| `lat_source` | factor | `{melissa, adece, disputed_melissa, none}` |
| `lat_precision` | ordered factor | `{none, unknown, centroid, zip5, zip4, area, parcel, rooftop}` |
| `distance_adece_melissa_m` | numeric | Haversine distance, NA when only one source |
| `coord_agreement_band` | factor | `{exact, tight, loose, drift, gross, one_source_only, none}` |
| `needs_followup_geocoding` | logical | The analyst-queue flag |
| `followup_reason` | factor | Controlled vocabulary (see section 13) |
| `coord_model_status` | ordered factor | `{missing, not_model_ready, provisional_followup, model_ready}` |
| `geocode_provenance` | character | Compact audit string per row |

The `coord_model_status` column is the **SAE-readiness gate** finalized
for v0.8.0. Only `model_ready` rows should feed an unqualified SAE
estimator; `provisional_followup` and `not_model_ready` rows remain
visible in every export so downstream consumers can choose to downweight
or exclude them — they are **never silently dropped**.

``` r

rec$data[, c("row_id", "lat_source", "lat_precision",
              "distance_adece_melissa_m", "coord_agreement_band",
              "needs_followup_geocoding", "followup_reason",
              "coord_model_status")] |>
  head(8)
#> # A tibble: 8 × 8
#>   row_id    lat_source lat_precision distance_adece_melis…¹ coord_agreement_band
#>   <chr>     <fct>      <ord>                          <dbl> <fct>               
#> 1 2022-202… melissa    rooftop                         63.5 tight               
#> 2 2022-202… melissa    parcel                         107.  loose               
#> 3 2022-202… melissa    rooftop                         44.9 tight               
#> 4 2022-202… melissa    rooftop                        124.  loose               
#> 5 2022-202… melissa    rooftop                         34.9 tight               
#> 6 2022-202… melissa    rooftop                         49.9 tight               
#> 7 2022-202… melissa    rooftop                         12.5 tight               
#> 8 2022-202… melissa    parcel                          91.6 tight               
#> # ℹ abbreviated name: ¹​distance_adece_melissa_m
#> # ℹ 3 more variables: needs_followup_geocoding <lgl>, followup_reason <fct>,
#> #   coord_model_status <ord>
```

The `$reconciliation_log` records the matrix cell, threshold used, and a
textual note for every input row — the per-row audit chain analysts need
when reviewing a decision:

``` r

rec$reconciliation_log[, c("row_id", "matrix_cell", "lat_source",
                            "distance_adece_melissa_m",
                            "threshold_name", "followup_reason")] |>
  head(5)
#> # A tibble: 5 × 6
#>   row_id            matrix_cell lat_source distance_adece_melis…¹ threshold_name
#>   <chr>             <chr>       <chr>                       <dbl> <chr>         
#> 1 2022-2023_999P00… D4          melissa                      63.5 tiered:GS05   
#> 2 2022-2023_999P00… D7          melissa                     107.  tiered:GS06   
#> 3 2022-2023_999P00… D4          melissa                      44.9 tiered:GS05   
#> 4 2022-2023_999P00… D4          melissa                     124.  tiered:GS05   
#> 5 2022-2023_999P00… D4          melissa                      34.9 tiered:GS05   
#> # ℹ abbreviated name: ¹​distance_adece_melissa_m
#> # ℹ 1 more variable: followup_reason <chr>
```

`$summary` gives a per-cell count for the run:

``` r

rec$summary
#> # A tibble: 15 × 3
#>    matrix_cell     n description                                                
#>    <chr>       <int> <chr>                                                      
#>  1 D1              0 Both present, GS01, within 50m: melissa, zip4              
#>  2 D2              0 Both present, GS01, 50m-10km: melissa, zip4, flagged       
#>  3 D3              0 Both present, GS01, >=10km: disputed_melissa, zip4, gross  
#>  4 D4             64 Both present, GS05 within 250m: melissa, rooftop; future u…
#>  5 D5              0 Both present, GS05, 250m-10km: melissa, rooftop, flagged   
#>  6 D6              0 Both present, GS05, >=10km: disputed_melissa, rooftop, gro…
#>  7 D7              8 Both present, GS06, within 500m: melissa, parcel           
#>  8 D8              0 Both present, GS06, 500m-10km: melissa, parcel, flagged    
#>  9 D9              0 Both present, GS06, >=10km: disputed_melissa, parcel, gross
#> 10 D10             9 Both present, GS03 (ZIP centroid): disputed_melissa, zip5  
#> 11 D11             0 ADECE only (Melissa unexpectedly missing)                  
#> 12 D12             6 Melissa only (GS01/GS05): melissa, no follow-up; future un…
#> 13 D13             2 Melissa only (GS06 interpolated rooftop): flagged          
#> 14 D14             1 Melissa only (GS03 ZIP centroid): flagged                  
#> 15 D15             0 Both missing: no coordinate, flagged
```

## 7. Transform: 5 derived variables

[`geocode_transform()`](https://joonho112.github.io/ALprekDB/reference/geocode_transform.md)
adds **data-layer** derived variables (no geocoding API calls, no ACS,
no Bayesian — those are out-of-scope). The result is an
`alprek_geocode_master`.

``` r

mst <- geocode_transform(rec)
mst
#> <alprek_geocode_master>
#>   geocode_run_id:   melissa_v1_2026-03
#>   Vendor / cycle:   melissa / 2026-2027
#>   Delivery date:    2026-03-04
#>   Rows:             90 (46 cols)
#>     in_alabama:     90 TRUE / 0 FALSE / 0 NA
#>     precision_tier: rooftop=70, parcel=10, zip4=0, zip5=10, centroid=0, area=0, unknown=0, none=0
#>     coord_model_status: missing=0, not_model_ready=10, provisional_followup=2, model_ready=78
#>   Transform log:    5 rule(s)
#>   Transformed at:   2026-05-21 18:50:12 UTC
```

Derived variables:

| Variable | Type | Notes |
|----|----|----|
| `precision_tier` | ordered factor | Aliased from `lat_precision`, levels reversed so [`sort()`](https://rdrr.io/r/base/sort.html) puts rooftop rows first |
| `in_alabama` | logical NA-able | `lat_final` in \[30, 36\] AND `lng_final` in \[-89, -84\] |
| `county_check_match` | logical NA-able | Melissa `COUNTYNAME` vs. an ADECE county sidecar when supplied; otherwise `NA` |
| `coord_age_years` | integer NA-able | `cycle_year_first - school_year_first` |
| `geocode_run_id` | character | Panel-stable identifier, e.g., `"melissa_v1_2026-03"` |

``` r

mst$transform_log[, c("rule", "n_affected", "severity")]
#> # A tibble: 5 × 3
#>   rule               n_affected severity
#>   <chr>                   <int> <chr>   
#> 1 precision_tier             90 INFO    
#> 2 in_alabama                 90 INFO    
#> 3 county_check_match          0 INFO    
#> 4 coord_age_years            90 INFO    
#> 5 geocode_run_id             90 INFO
```

The `$data` slot preserves `lineage_id` (Step 3.1) and
`coord_model_status` (Step 4.3) untouched. Exporters in section 9
respect that contract: rows with `coord_model_status != "model_ready"`
are **visible** in every output, not silently filtered.

## 8. Multi-cycle panel

[`geocode_bind_years()`](https://joonho112.github.io/ALprekDB/reference/geocode_bind_years.md)
stacks multiple `alprek_geocode_master` snapshots into an
`alprek_geocode_panel`. **Important**: a single Melissa delivery is
already a 5-year long panel (school_year ∈
`{2021-2022, ..., 2025-2026_new}`); the within-delivery long shape is
materialized upstream.
[`geocode_bind_years()`](https://joonho112.github.io/ALprekDB/reference/geocode_bind_years.md)
is for binding **multiple Melissa runs across release cycles** (e.g., a
future v0.9.0 delivery on top of the current v0.8.0 delivery). For
v0.8.0 with a single run, the call collapses to a degenerate identity
that returns a 1-run panel whose `$data` equals the input’s `$data`.

``` r

panel <- geocode_bind_years(mst)
panel
#> <alprek_geocode_panel>
#>   n_runs:        1
#>   run_ids:       melissa_v1_2026-03
#>   snapshot_dates: 2026-03-04 
#>   n_rows_total:  90
#>   rows per run:  melissa_v1_2026-03=90
#>   bound_at:      2026-05-21 18:50:12 UTC
```

The binding log records one row per run with file SHA-256 and snapshot
date:

``` r

panel$binding_log
#> # A tibble: 1 × 7
#>   geocode_run_id     snapshot_date file_sha256 n_rows n_columns severity details
#>   <chr>              <date>        <chr>        <int>     <int> <chr>    <chr>  
#> 1 melissa_v1_2026-03 2026-03-04    aaaaaaaaaa…     90        46 INFO     NA
```

The unique key in the bound panel is `(row_id, geocode_run_id)` — the
same `row_id` is allowed across runs (a renewal site re-geocoded each
cycle).

## 9. Export (5 formats + the follow-up queue)

Standard data exports preserve every row (including `not_model_ready`
and `provisional_followup` rows):

``` r

geocode_export_csv(mst)
#> -> "output/geocode/geocode_melissa_v1_2026-03.csv"
geocode_export_parquet(mst)
geocode_export_excel(mst,   include_summary = TRUE)
geocode_export_rds(mst)
geocode_export_stata(mst)
```

The Excel exporter optionally writes a `Summary` sheet with the
`coord_model_status` / `lat_source` distributions and the count of rows
flagged `needs_followup_geocoding`.

## 10. Linkage with classroom + applications

The geocode module hooks into the existing linkage layer at two points.

### `linkage_geocode_classroom()`

Joins an `alprek_geocode_panel` onto an `alprek_classroom_panel` so
every classroom-year row inherits the per-site authoritative
coordinates. Geocoding happens at the **site** grain; the classroom
panel is at classroom-year grain, so multiple classrooms at the same
site share one geocode row. The function preserves classroom row order
and never inflates.

``` r

panel_c <- classroom_bind_years(c2122, c2223, c2324, c2425)
lk      <- linkage_geocode_classroom(panel, panel_c)
lk
#> <alprek_geocode_linkage_classroom>
#>   Classroom rows:       2410
#>   Geocode rows (in):    3396
#>   Matched (classroom):  2287 (94.9%)
#>   Unmatched classroom:  123
#>   Unmatched geocode:    1232
```

The 12 attached columns are prefixed `geocode_*` (e.g.,
`geocode_lat_final`, `geocode_coord_model_status`, `geocode_lineage_id`,
`geocode_run_id`) so they cannot collide with the classroom panel’s
ADECE `latitude` / `longitude` columns — those are kept verbatim as an
escape hatch for inspection.

### `linkage_geocode_applications()`

Joins the geocode panel onto an `alprek_applications_master`. For
renewals + bucket-C new applications, joins on
`(matched_site_code, school_year)`. For bucket-D “truly new”
applications (no `site_code` yet), joins on `row_id` — the Melissa
file’s `2025-2026_new_NNNN` row_ids correspond directly to bucket-D
applications.

``` r

lk_apps <- linkage_geocode_applications(panel, app_master)
lk_apps$diagnostics
```

## 11. `linkage_create_master()` — the Goal \#2 deliverable

The master extension (v0.8.0) gives
[`linkage_create_master()`](https://joonho112.github.io/ALprekDB/reference/linkage_create_master.md)
two new optional arguments: `geocode =` and `applications =`. The
signature is **backward compatible** — a 3-arg call produces v0.7.0
output.

``` r

# v0.7.0 (3-arg) — unchanged
master <- linkage_create_master(budget_panel, classroom_panel,
                                  student_panel)

# v0.8.0 with reconciled geocode
master <- linkage_create_master(
  budget_panel, classroom_panel, student_panel,
  geocode = panel
)

# Full v0.8.0 with geocode + applications
master <- linkage_create_master(
  budget_panel, classroom_panel, student_panel,
  geocode      = panel,
  applications = app_master
)
master$classroom_level |>
  dplyr::select(school_year, classroom_code, site_code,
                  geocode_lat_final, geocode_lat_source,
                  geocode_coord_model_status,
                  geocode_needs_followup_geocoding) |>
  head(5)
```

`master$classroom_level` gains 12 prefixed `geocode_*` columns: the 10
authoritative/model-readiness columns plus `geocode_run_id` and
`geocode_lineage_id`. The ADECE `latitude` / `longitude` columns remain
untouched. `master$diagnostics` includes `geocode_coverage` (rolled-up
percentage of model-ready coords) and `geocode_linkage` (the underlying
diagnostic tibble).

## 11.5 Analysis-time merge patterns

By design, the module-level production panels at `output/budget/`,
`output/classroom/`, `output/student/`, and `output/applications/` do
**not** carry the geocode columns. ALprekDB keeps cleaning and
validation strictly module-local: each panel is the canonical output of
one cleaning pipeline. Joining geocode to a panel is the analyst’s
choice, exercised at analysis time using the package’s `linkage_*()`
functions. The benefit is straight separation of concerns: a budget
cleaning bug never has to wait on a geocoding rebuild, and analysts who
need a different cycle of Melissa data can swap it in freely.

Four common patterns follow. Each is 2-4 lines of code.

**Pattern A — Classroom + geocode.** Direct join on
`(site_code, school_year)`. Adds 12 prefixed `geocode_*` columns to the
classroom panel.

``` r

classroom <- readRDS("output/classroom/classroom_panel_2021-2026.rds")
# panel_geo is the alprek_geocode_panel built earlier in this vignette
linked <- linkage_geocode_classroom(panel_geo, classroom)
linked$data  # 7,409 rows + 12 geocode_* cols + ADECE latitude/longitude preserved
```

**Pattern B — Applications + geocode.** Two-phase join:
`matched_site_code` for renewals (buckets A/B/C) and `row_id` for
bucket-D “truly new” applications whose `_new_NNNN` row_ids correspond
directly to Melissa rows.

``` r

apps   <- readRDS("output/applications/applications_master_2026-2027.rds")
linked <- linkage_geocode_applications(panel_geo, apps)
linked$data  # 1,617 rows + 12 geocode_* cols, with bucket-D rows joined on row_id
```

**Pattern C — Budget / student panels.** Budget has only
`classroom_code` (no `site_code`), so
[`linkage_create_master()`](https://joonho112.github.io/ALprekDB/reference/linkage_create_master.md)
handles the indirect resolution. Student is at student-year grain; the
geocode value is propagated via the classroom join inside the master
integrator.

``` r

budget    <- readRDS("output/budget/budget_panel_2021-2025.rds")
student   <- readRDS("output/student/student_panel_2021-2026.rds")
m <- linkage_create_master(budget, classroom, student, geocode = panel_geo)
m$classroom_level  # budget cols + classroom cols + student aggregates + 12 geocode_* cols
m$student_level    # 116,689 student-year rows with classroom-derived geocode_* cols
```

**Pattern D — Full integrated master.** This is the same call that
produced `output/master/master_classroom_level_2026-2027_v0.8.0.csv` in
the v0.8.0 release packet; rerun it locally to inspect alongside the
other panels.

``` r

apps <- readRDS("output/applications/applications_master_2026-2027.rds")
m <- linkage_create_master(budget, classroom, student,
                           geocode      = panel_geo,
                           applications = apps)
m$classroom_level  # 7,409 rows × 246 cols: budget + classroom + student-agg + 12 geocode_* + 19 app_*
```

The geocode panel itself can be loaded from the v0.8.0 release artifact
or rebuilt from a Melissa xlsx; both flows are equivalent.

``` r

# Option 1: Reload from release artifact
rec <- readRDS("output/geocode/geocode_reconciled_2026-2027.rds")
master_geo <- geocode_transform(rec, config = geocode_config(
  path = rec$meta$path, cycle_year = rec$meta$cycle_year,
  delivery_date = rec$meta$receipt_date, verbose = FALSE))
panel_geo <- geocode_bind_years(master_geo)

# Option 2: Rebuild from xlsx (Sections 3-8 of this vignette)
# raw <- geocode_read(...); clean <- geocode_clean(raw); ...
```

## 12. DuckDB persistence

For long-term storage and SQL access, the geocode module writes 4 tables
into the shared DuckDB file alongside budget / classroom / student /
applications:

- `geocode_clean` — per `geocode_run_id`, cleaned standardized cols
- `geocode_reconciled` — per run, full reconciled rows + 10 auth cols +
  `lineage_id`
- `geocode_panel` — cross-run, long-format multi-snapshot view
- `geocode_lineage` — one row per write, capturing run-level meta (file
  SHA-256, n_rows, n_followup, threshold rules, timestamps)

``` r

conn <- db_init("output/alprekdb.duckdb")

# Per-run write
db_write_geocode_clean(conn, clean)
db_write_geocode_reconciled(conn, rec)
db_write_geocode_panel(conn, panel, overwrite = TRUE)

# Round-trip
rec_back   <- db_read_geocode_reconciled(conn)
panel_back <- db_read_geocode_panel(conn)
lineage_df <- db_read_geocode_lineage(conn)

DBI::dbDisconnect(conn, shutdown = TRUE)
```

Ordered-factor levels for `lat_precision`, `coord_model_status`, and
`precision_tier` round-trip through the shared column-type registry.

## 13. The follow-up queue (Goal \#3)

[`geocode_export_followup_queue()`](https://joonho112.github.io/ALprekDB/reference/geocode_export_followup_queue.md)
is the user-facing surface for the analyst-driven re-geocoding workflow.
It surfaces every row flagged `needs_followup_geocoding = TRUE`,
annotates each with a controlled `suggested_action`, and writes
`output/geocode/sites_needing_geocoding_<cycle>.csv`.

``` r

fq <- geocode_followup_queue(rec)
nrow(fq)
#> [1] 12
fq[, c("row_id", "school_year", "lat_source",
        "distance_adece_melissa_m", "melissa_result_code",
        "followup_reason", "suggested_action")] |>
  head(5)
#> # A tibble: 5 × 7
#>   row_id       school_year lat_source distance_adece_melis…¹ melissa_result_code
#>   <chr>        <chr>       <chr>                       <dbl> <chr>              
#> 1 2025-2026_n… 2025-2026_… melissa                      NA   GS06               
#> 2 2024-2025_9… 2024-2025   disputed_…                   38.8 GS03               
#> 3 2024-2025_9… 2024-2025   disputed_…                   38.4 GS03               
#> 4 2023-2024_9… 2023-2024   disputed_…                   78.6 GS03               
#> 5 2023-2024_9… 2023-2024   disputed_…                   73.8 GS03               
#> # ℹ abbreviated name: ¹​distance_adece_melissa_m
#> # ℹ 2 more variables: followup_reason <chr>, suggested_action <chr>
```

The disk-bound exporter behaves the same and adds an internal-use header
on the CSV:

``` r

fq <- geocode_export_followup_queue(
  rec,
  cycle_year       = "2026-2027",
  internal_use     = TRUE,   # default — adds "DO NOT REDISTRIBUTE" header
  include_disputed = TRUE
)
attr(fq, "output_path")
#> "output/geocode/sites_needing_geocoding_2026-2027.csv"
attr(fq, "privacy_level")
#> "internal_address_followup"
```

### Interpreting `followup_reason` -\> `suggested_action`

The controlled vocabulary mapping is **deterministic** so a downstream
caller can group / filter without re-reading the decision matrix:

| `followup_reason` | `suggested_action` | What to do |
|----|----|----|
| `disagreement_above_threshold` | `manual_source_adjudication` | Both sources present, distance \> tier threshold but \< 10 km. Inspect both addresses; usually one is the program site, the other a billing or mailing address. |
| `disagreement_gross` | `verify_adece_address_and_request_recheck` | Distance \>= 10 km. Usually an ADECE address-field error. Check ADECE first, then re-request the Melissa run if needed. |
| `resultcode_not_acceptable_for_master` | `request_higher_precision_geocode` | Vendor RESULTCODE is in the codebook but `acceptable_for_master = FALSE` (e.g., `GS02`/`GS04` if future Melissa runs return them). |
| `both_missing` | `obtain_coord` | Neither ADECE nor Melissa produced a coordinate. Obtain coordinate by any means. |
| `melissa_unexpectedly_missing` | `request_melissa_geocode` | ADECE has a coord, Melissa returned blank. Re-submit to Melissa. |
| `melissa_only_interpolated` | `request_higher_precision_geocode` | Melissa returned `GS06` interpolated rooftop and ADECE is absent. Re-geocode at higher precision. |
| `melissa_only_gs03` | `request_higher_precision_geocode` | Melissa returned `GS03` ZIP centroid and ADECE is absent. Re-geocode. |
| `resultcode_gs03_always_flag` | `manual_review_gs03` | `GS03` ZIP centroid with ADECE present. Decide whether ADECE is reliable enough to override the centroid. |
| (other / NA) | `manual_review` | Catch-all for unmapped reasons. |

Sort order in the exported queue: `school_year` descending, then
`distance_adece_melissa_m` descending (largest disagreements first;
one-source-only rows sort to the end).

## 14. Caveats

### Privacy: `site_street` is a full address

The follow-up queue contains full site addresses (`site_street`,
`site_city`, `site_state`, `site_zip`) because the analyst needs them to
verify / re-geocode. By default the in-memory tibble carries:

- `attr(fq, "privacy_level") = "internal_address_followup"`
- `attr(fq, "contains_address_fields") = TRUE`
- `attr(fq, "internal_use") = TRUE` (when `internal_use = TRUE`)

The on-disk CSV is prefixed with a comment header
`# INTERNAL USE -- DO NOT REDISTRIBUTE`. **Do not commit the follow-up
queue to a public repository or share it outside the data team.** The
`output/` and `ORIGINAL-DATA/` directories are excluded from the
package’s public mirror by design.

### GS03 is always flagged

Even when a `GS03` row has a Melissa coordinate that lands near the
ADECE coordinate, the matrix flags it. A ZIP centroid is not a site
coordinate; the apparent agreement is artifactual. If ADECE is present,
you can use ADECE as the provisional final coordinate — that’s what
`authoritative_priority = "adece_first"` does — but the row still needs
analyst attention.

### Coord age (`coord_age_years`)

ADECE coordinates can be years old.
`coord_age_years = cycle_year_first - school_year_first` is the rough
freshness gauge; large positive values mean the panel row is being
modeled against an older ADECE coordinate. Sites that move (or that grow
into a new classroom at a new street) can break the assumption that an
old coordinate still describes the program. This is informational only;
no automatic correction is applied.

### Centroid vs. rooftop precision

`GS01` (ZIP+4 street level, 50 m threshold) is the **tightest**
threshold in v0.8.0 — much tighter than `GS05` (rooftop, 250 m) and
`GS06` (interpolated rooftop, 500 m). This counterintuitive ordering
comes from the v0.8.0 empirical medians: the small `GS01` sample
clustered tightly, while `GS06`’s interpolation produces a wider
acceptable disagreement window. If a future delivery shifts those
distributions, edit
`inst/extdata/codebooks/melissa_resultcode_codes.csv` and the per-tier
list in
[`geocode_config()`](https://joonho112.github.io/ALprekDB/reference/geocode_config.md).

### `STATUSCODE` is a derived field

Melissa publishes `RESULTCODE` in its public documentation; the
`STATUSCODE` column is internal to this delivery and not documented
externally. We treat the codebook’s 1:1 mapping (`GS01<->9`, `GS03<->5`,
`GS05<->B`, `GS06<->A`) as **authoritative for this v1 contract** and
re-derive validation against it. Future deliveries that change the
mapping should update
`inst/extdata/codebooks/melissa_statuscode_codes.csv` first.

### Acceptability is codebook-governed

`acceptable_for_master` in `melissa_resultcode_codes.csv` is the
**single source of truth** for whether a Melissa RESULTCODE is
SAE-ready. The reconciler reads it through
[`alprek_geocode_resultcode_meaning()`](https://joonho112.github.io/ALprekDB/reference/alprek_geocode_resultcode_meaning.md).
To re-include `GS02`/`GS04` / exclude `GS06` for a future modeling
cycle, edit the CSV — no R changes required.

## What this module does NOT do

| Concern | Where it lives |
|----|----|
| Live geocoding API calls | Permanently out-of-scope |
| Multi-source consensus (ArcGIS, Google fallback) | Future release |
| ACS area-weighted aggregation | Future `ALprekACS` package |
| OSRM isochrone / drive-time | Future package |
| Bayesian SAE of economic-need tiers | Future `ALprekSAE` package |

This split is intentional: the data-contract layer (this module) must be
stable, audited, and codebook-driven before spatial / statistical layers
consume `lat_final` as truth.

## See also

- [`vignette("a1-getting-started", package = "ALprekDB")`](https://joonho112.github.io/ALprekDB/articles/a1-getting-started.md)
  — package overview
- [`vignette("a3-linkage-analysis", package = "ALprekDB")`](https://joonho112.github.io/ALprekDB/articles/a3-linkage-analysis.md)
  — broader linkage patterns this module integrates with
- [`vignette("a4-duckdb-sql", package = "ALprekDB")`](https://joonho112.github.io/ALprekDB/articles/a4-duckdb-sql.md)
  — DuckDB schema + SQL
- [`vignette("a6-applications-intake", package = "ALprekDB")`](https://joonho112.github.io/ALprekDB/articles/a6-applications-intake.md)
  — sibling module this module mirrors in pattern
- [`vignette("m2-validation-framework", package = "ALprekDB")`](https://joonho112.github.io/ALprekDB/articles/m2-validation-framework.md)
  — how
  [`geocode_validate()`](https://joonho112.github.io/ALprekDB/reference/geocode_validate.md)
  plugs into the package-wide framework
- [`vignette("m4-privacy-provenance", package = "ALprekDB")`](https://joonho112.github.io/ALprekDB/articles/m4-privacy-provenance.md)
  — full treatment of the privacy guardrails the follow-up queue
  inherits
