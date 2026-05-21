# Export Geocode Master / Panel / Reconciled Data to CSV

Writes the `$data` slot of a geocode S3 object to CSV. Mirrors
[`applications_export_csv()`](https://joonho112.github.io/ALprekDB/reference/applications_export_csv.md).
Auto-generates `path` if `NULL`.

**Phase 5 contract.** All Phase 5 exports preserve `lineage_id` (Step
3.1 stable row lineage) and `coord_model_status` (Step 4.3 ordered
factor:
`{missing, not_model_ready, provisional_followup, model_ready}`). Rows
with `coord_model_status != "model_ready"` remain visible in the export;
downstream SAE consumers MUST distinguish provisional from model-ready
coordinates. This exporter never silently filters rows.

## Usage

``` r
geocode_export_csv(x, path = NULL, ...)
```

## Arguments

- x:

  An `alprek_geocode_master`, `alprek_geocode_panel`, or
  `alprek_geocode_reconciled` object.

- path:

  Character. Output path. If `NULL`, auto-generates
  `output/geocode/geocode_<run_id>.csv` (master/reconciled) or
  `output/geocode/geocode_panel_<run_ids>.csv` (panel).

- ...:

  Additional arguments forwarded to
  [`utils::write.csv()`](https://rdrr.io/r/utils/write.table.html).

## Value

Invisible character path of the written file.

## See also

[`geocode_export_parquet()`](https://joonho112.github.io/ALprekDB/reference/geocode_export_parquet.md),
[`geocode_export_excel()`](https://joonho112.github.io/ALprekDB/reference/geocode_export_excel.md),
[`geocode_export_rds()`](https://joonho112.github.io/ALprekDB/reference/geocode_export_rds.md),
[`geocode_export_stata()`](https://joonho112.github.io/ALprekDB/reference/geocode_export_stata.md),
[`geocode_export_followup_queue()`](https://joonho112.github.io/ALprekDB/reference/geocode_export_followup_queue.md).

## Examples

``` r
if (FALSE) { # \dontrun{
mst <- geocode_transform(geocode_reconcile(geocode_clean(geocode_read("..."))))
geocode_export_csv(mst)
geocode_export_csv(mst, "output/custom.csv")
} # }
```
