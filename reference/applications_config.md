# Create an Applications Processing Configuration

Creates a typed configuration object that controls the applications
module pipeline (read -\> clean -\> reconcile -\> validate -\> transform
-\> panel -\> export).

## Usage

``` r
applications_config(
  cycle_year,
  master_path = NULL,
  renewals_path = NULL,
  new_apps_path = NULL,
  consolidated_path = NULL,
  prior_deliverable_path = NULL,
  output_dir = NULL,
  fuzzy_threshold = 0.85,
  cycle = c("cycle1", "cycle0"),
  seed = 20260519L,
  remove_noise_rows = TRUE,
  verbose = TRUE
)
```

## Arguments

- cycle_year:

  Character. Cycle year in `"YYYY-YYYY"` format (e.g., `"2026-2027"`).
  Required.

- master_path:

  Character. Path to the cycle-1 master xlsx file containing all
  renewal/new/non-renewal/capacity sheets. Required for cycle-1
  (combined-file layout).

- renewals_path:

  Character. Path to cycle-0 renewals xlsx file (separate-file layout).
  Used when cycle-0 is being re-processed. Default `NULL`.

- new_apps_path:

  Character. Path to cycle-0 new applications xlsx file. Default `NULL`.

- consolidated_path:

  Character. Path to "Classroom requests" or equivalent supplementary
  xlsx file. Optional. Default `NULL`.

- prior_deliverable_path:

  Character. Path to prior cycle final deliverable xlsx (e.g., 2025-2026
  full application list_Added_Economic Needs.xlsx) — used as reference /
  YoY comparison. Default `NULL`.

- output_dir:

  Character. Output directory. Default `NULL` (auto:
  `output/applications/<cycle_year>`).

- fuzzy_threshold:

  Numeric in (0, 1). Jaro-Winkler similarity threshold for fuzzy
  classroom-name matching in
  [`applications_reconcile()`](https://joonho112.github.io/ALprekDB/reference/applications_reconcile.md).
  Default `0.85`.

- cycle:

  Character. Cycle schema label ("cycle1" / "cycle0"). Used by
  [`applications_detect_format()`](https://joonho112.github.io/ALprekDB/reference/applications_detect_format.md)
  and codebook loaders. Default `"cycle1"`.

- seed:

  Integer. Random seed for reproducibility (used by fuzzy match
  deterministic tiebreaks etc.). Default `20260519L`.

- remove_noise_rows:

  Logical. Drop noise rows (e.g., "Show the Debugger Trace Report") at
  clean step? Default `TRUE`.

- verbose:

  Logical. Print progress messages? Default `TRUE`.

## Value

An `alprek_applications_config` S3 object.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- applications_config(
  cycle_year = "2026-2027",
  master_path = file.path("ORIGINAL-DATA", "applications_2026_2027.xlsx")
)
result <- applications_process(cfg)
} # }
```
