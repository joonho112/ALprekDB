# Reconcile ADECE Applications Against Prior Classroom Panel

Assigns each cycle-1 application row (renewals + new) into one of four
buckets and records every match decision (exact, fuzzy automatic, no
match) plus the top-3 fuzzy candidates considered. Solves Gap \#1 from
the 2026-05 ad-hoc cycle (automatic match decisions weren't logged).

Buckets:

- **A** - renewal row, exact key match against `prior_classroom_panel`
  (organization, prior project name, county). Carries the matched
  `classroom_code`.

- **B** - renewal row, no exact match, fuzzy (Jaro-Winkler) similarity
  `>= fuzzy_threshold`. Carries best-match `classroom_code`, flagged for
  analyst review.

- **C** - new-application row whose fuzzy similarity to a prior
  classroom `>= fuzzy_threshold` (probably an additional classroom at an
  existing program). Carries best-match `classroom_code`, flagged.

- **D** - no candidate `>= fuzzy_threshold`. Treated as truly new;
  downstream geocoding + isochrone packages take it from here.

No geocoding, ACS integration, or Bayesian modelling here - those live
in separate packages.

## Usage

``` r
applications_reconcile(
  renewals_clean,
  new_apps_clean,
  prior_classroom_panel = NULL,
  prior_school_year = NULL,
  fuzzy_threshold = 0.85,
  seed = 20260519L,
  allow_degraded = FALSE
)
```

## Arguments

- renewals_clean:

  An `alprek_applications_clean` object whose `$meta$kind` is
  `"renewals"`.

- new_apps_clean:

  An `alprek_applications_clean` object whose `$meta$kind` is
  `"new_apps"`.

- prior_classroom_panel:

  Optional `alprek_classroom_panel` produced by `classroom_panel()`.
  Required by default. When `NULL` and `allow_degraded = TRUE`, no fuzzy
  work is performed and all rows receive `bucket = "unknown"` so
  production workflows cannot confuse missing panel data with exact
  reconciliation.

- prior_school_year:

  Optional character (e.g., `"2024-2025"`). The school year in
  `prior_classroom_panel` to match against. When `NULL`, defaults to
  `max(prior_classroom_panel$years)`.

- fuzzy_threshold:

  Numeric in `[0, 1]`. Similarity at or above which a fuzzy candidate is
  auto-accepted. Default `0.85`.

- seed:

  Integer used for deterministic tie-breaking and recorded in the audit
  log. Default `20260519L`.

- allow_degraded:

  Logical. If `TRUE`, allow `prior_classroom_panel = NULL` for synthetic
  demos and return `bucket = "unknown"`. Default `FALSE` so production
  workflows cannot mistake missing reconciliation for an exact match.

## Value

An `alprek_applications_reconciled` S3 list with elements:

- `reconciled`: tibble with one row per input row, augmented with
  `application_id`, `source_sheet`, `bucket`, `matched_classroom_code`,
  `matched_site_code`, `match_method`, `match_score`.

- `reconciliation_log`: long tibble. One row per chosen decision plus up
  to three runner-up `fuzzy_candidate` rows per non-exact decision.
  Columns:
  `application_id, source_sheet, name_raw, name_matched, match_method, score, threshold_used, decision_source, decision_timestamp, decision_seed, candidate_classroom_code, candidate_site_code, candidate_rank, score_margin, note`.

- `summary`: tibble of bucket counts.

- `meta`: list (`fuzzy_threshold`, `seed`, `reconciled_at`,
  `prior_school_year`, `n_in_renewals`, `n_in_new_apps`, `n_a`, `n_b`,
  `n_c`, `n_d`, `n_unknown`, `git_sha`).

## Examples

``` r
if (FALSE) { # \dontrun{
r <- applications_read_renewals(path, cycle_year = "2026-2027")
n <- applications_read_new(path, cycle_year = "2026-2027")
rc <- applications_clean(r); nc <- applications_clean(n)
rec <- applications_reconcile(rc, nc, prior_classroom_panel = panel)
rec
rec$summary
} # }
```
