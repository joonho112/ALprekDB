# Derive Advanced Analysis Variables for Student Data

Computes derived variables for research analysis, including GOLD
assessment gain scores, K-readiness transition indicators, chronic
absence flags, service density indices, and eDECA pre-post gains.

## Usage

``` r
student_transform(
  clean_obj,
  add_gold_gains = TRUE,
  add_kready_transitions = TRUE,
  add_chronic_absence = TRUE,
  add_service_density = TRUE,
  add_edeca_gains = TRUE,
  chronic_absence_threshold = 18,
  school_days = 180
)
```

## Arguments

- clean_obj:

  An `alprek_student_clean` or `alprek_student_panel` object.

- add_gold_gains:

  Logical. Compute GOLD fall-to-spring gain scores (6 domains x raw +
  scale = 12 columns)? Default `TRUE`.

- add_kready_transitions:

  Logical. Compute K-readiness transition indicators (6 domains)?
  Default `TRUE`.

- add_chronic_absence:

  Logical. Compute chronic absence flag and percentage? Default `TRUE`.

- add_service_density:

  Logical. Compute service count and risk index? Default `TRUE`.

- add_edeca_gains:

  Logical. Compute eDECA pre-post T-score gains (5 constructs)? Default
  `TRUE`.

- chronic_absence_threshold:

  Numeric. Days absent threshold for chronic absence flag. Default `18`
  (~10% of 180 school days).

- school_days:

  Numeric. Total school days for absence percentage calculation. Default
  `180`.

## Value

The same S3 class as the input object, with additional derived columns
in `$data`. A `$transform_log` element is added to track which variables
were created.

## Details

This function is designed to be called **after**
[`student_clean()`](https://joonho112.github.io/ALprekDB/reference/student_clean.md)
and optionally **before**
[`student_bind_years()`](https://joonho112.github.io/ALprekDB/reference/student_bind_years.md).
It does not modify the S3 class, so downstream functions (export,
validate, linkage) continue to work without modification.

The function is idempotent: running it twice produces the same result
(existing derived columns are overwritten).

Source columns that do not exist in the data are silently skipped with
an informational message.

## Derived Variables

- GOLD Gains (12 cols):

  `gold_[domain]_gain_raw` and `gold_[domain]_gain_scale` for 6 domains:
  literacy, math, se, physical, cognitive, language. Computed as
  spring - fall.

- K-Readiness Transitions (6 cols):

  `gold_[domain]_kready_improved`: 1 if fall = Emerging and spring =
  Accomplished, else 0.

- Chronic Absence (2 cols):

  `chronic_absence` (0/1 flag) and `chronic_absence_pct`
  (days_absent_total / school_days \* 100).

- Service Density (2 cols):

  `n_services` (count of binary service indicators) and `risk_index`
  (count of risk indicators).

- eDECA Gains (5 cols):

  `edeca_[construct]_gain`: post T-score minus pre T-score for
  initiative, self_reg, attachment, tpf, behavior.

## Examples

``` r
if (FALSE) { # \dontrun{
clean <- student_clean(student_read("data/FCPK Student Details 23-24.xlsx"))
enriched <- student_transform(clean)
# Or with panel:
panel <- student_process_years(configs)$panel
enriched_panel <- student_transform(panel)
} # }
```
