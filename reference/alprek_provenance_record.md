# Construct a Provenance Record

Bundles the standard provenance fields (file hash, receipt date, cycle
year, geocoding source, git SHA, timestamp) into a single tibble row
suitable for appending to `applications_lineage` and other lineage
tables.

## Usage

``` r
alprek_provenance_record(
  file_path,
  cycle_year,
  receipt_date = Sys.Date(),
  sheet = NA_character_,
  geocoding_source = NA_character_,
  repo_path = "."
)
```

## Arguments

- file_path:

  Character. Source file path. Hashed via
  [`alprek_file_hash()`](https://joonho112.github.io/ALprekDB/reference/alprek_file_hash.md).

- cycle_year:

  Character. Cycle year label (e.g., `"2026-2027"`).

- receipt_date:

  Date or character. Date the source file was received. Default
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

- sheet:

  Character. Sheet name within the xlsx (optional).

- geocoding_source:

  Character. Geocoding service used (only relevant downstream; default
  `NA_character_` for the applications module).

- repo_path:

  Character. Path to git repository. Default `"."`.

## Value

A tibble row with provenance fields.

## Examples

``` r
if (FALSE) { # \dontrun{
alprek_provenance_record(
  file_path = "Copy of 2026-27 ... (003).xlsx",
  cycle_year = "2026-2027",
  receipt_date = "2026-05-19",
  sheet = "26-27 requests_TW"
)
} # }
```
