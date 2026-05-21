# Build a Follow-Up Queue From a Reconciled Geocode Object (Step 4.4)

Surfaces the subset of reconciled site-rows that need analyst follow-up
geocoding, sorted for triage and annotated with a suggested action. This
is the consumer-facing helper that turns the per-row
`needs_followup_geocoding` flag (from
[`geocode_reconcile()`](https://joonho112.github.io/ALprekDB/reference/geocode_reconcile.md))
into a short, actionable queue.

Each row in the output represents one site/year that the reconciler
flagged for follow-up. The `suggested_action` column is a controlled
vocabulary derived deterministically from `followup_reason`, so the
downstream caller can group/filter the queue without re-reading the
decision matrix.

Sort order (descending priority):

1.  `school_year` descending (latest year first).

2.  `distance_adece_melissa_m` descending (largest disagreement first);
    rows with `NA` distance (one-source-only or both-missing) sort to
    the end.

`suggested_action` mapping (controlled vocabulary):

|  |  |
|----|----|
| `followup_reason` | `suggested_action` |
| `disagreement_above_threshold` | `manual_source_adjudication` |
| `disagreement_gross` | `verify_adece_address_and_request_recheck` |
| `resultcode_not_acceptable_for_master` | `request_higher_precision_geocode` |
| `both_missing` | `obtain_coord` |
| `melissa_unexpectedly_missing` | `request_melissa_geocode` |
| `melissa_only_interpolated` | `request_higher_precision_geocode` |
| `melissa_only_gs03` | `request_higher_precision_geocode` |
| `resultcode_gs03_always_flag` | `manual_review_gs03` |
| other / `NA` | `manual_review` |

## Usage

``` r
geocode_followup_queue(reconciled, include_disputed = TRUE)
```

## Arguments

- reconciled:

  An `alprek_geocode_reconciled` object from
  [`geocode_reconcile()`](https://joonho112.github.io/ALprekDB/reference/geocode_reconcile.md).

- include_disputed:

  Logical. When `TRUE` (default), rows whose
  `lat_source == "disputed_melissa"` (matrix cells D3 / D6 / D9 / D10)
  are included in the queue. Set to `FALSE` to exclude them (e.g., when
  the analyst handles disputed rows in a separate workflow).

## Value

A tibble (not S3) with one row per site needing follow-up, ordered per
the sort rules above. Columns, in order:

- `lineage_id` (character; stable row-level lineage key)

- `row_id` (character)

- `school_year` (character)

- `site_code` (character)

- `site_name` (character)

- `site_street`, `site_city`, `site_state`, `site_zip` (character)

- `lat_source` (character; coerced from the reconciled factor)

- `coord_agreement_band` (character)

- `distance_adece_melissa_m` (numeric; NA where not computable)

- `melissa_result_code` (character; renamed from `RESULTCODE`)

- `lat_precision` (character)

- `followup_reason` (character)

- `suggested_action` (character; one of the controlled vocab values)

Returns a 0-row tibble with the same schema when no rows need follow-up
(or when all flagged rows are disputed and `include_disputed = FALSE`).
All returned queues, including 0-row outputs, carry attributes
`privacy_level = "internal_address_followup"` and
`contains_address_fields = TRUE`.

## See also

[`geocode_reconcile()`](https://joonho112.github.io/ALprekDB/reference/geocode_reconcile.md)
for the upstream decision matrix that produces
`needs_followup_geocoding` and `followup_reason`.

## Examples

``` r
if (FALSE) { # \dontrun{
raw   <- geocode_read(path = "...", cycle_year = "2026-2027",
                      receipt_date = "2026-03-04")
clean <- geocode_clean(raw)
rec   <- geocode_reconcile(clean)
fq    <- geocode_followup_queue(rec)
table(fq$suggested_action)
# Exclude disputed Melissa rows (handled separately):
fq_no_disputed <- geocode_followup_queue(rec, include_disputed = FALSE)
} # }
```
