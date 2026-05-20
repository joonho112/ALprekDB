# M3 - Codebooks and Mappings

## Overview

ALprekDB keeps many data-cleaning decisions outside R code. Public CSV
files in `inst/extdata/` define column mappings and codebooks that are
read by the processing functions at runtime.

This design has three practical benefits:

- codebooks are readable by analysts and program staff who do not write
  R;
- mapping changes are versioned as data-cleaning decisions;
- public documentation can describe processing rules without sharing raw
  ADECE records.

``` r

library(ALprekDB)
```

## Installed CSV Inventory

The package ships 22 public CSV files in v0.7.0: 12 codebooks and 10
column mappings. Eight of those files support the new Applications
module.

``` r

ext_dir <- system.file("extdata", package = "ALprekDB", mustWork = TRUE)
ext_files <- list.files(
  ext_dir,
  pattern = "[.]csv$",
  recursive = TRUE,
  full.names = TRUE
)
ext_relative_files <- sub("^.*[/]extdata[/]", "", ext_files)

csv_inventory <- data.frame(
  file = ext_relative_files,
  role = ifelse(grepl("^mappings/", ext_relative_files), "mapping", "codebook"),
  rows = unname(vapply(
    ext_files,
    function(path) nrow(utils::read.csv(path, check.names = FALSE)),
    integer(1)
  )),
  fields = unname(vapply(
    ext_files,
    function(path) length(names(utils::read.csv(path, nrows = 0, check.names = FALSE))),
    integer(1)
  )),
  row.names = NULL
)

csv_inventory[order(csv_inventory$role, csv_inventory$file), ]
#>                                                       file     role rows fields
#> 1                    codebooks/applications_edge_cases.csv codebook   17      7
#> 2                 codebooks/applications_funding_types.csv codebook    6      3
#> 3               codebooks/applications_source_manifest.csv codebook   13      6
#> 4                  codebooks/applications_status_codes.csv codebook    6      4
#> 5                     codebooks/budget_category_groups.csv codebook   38      3
#> 6                  codebooks/classroom_degree_patterns.csv codebook   27      6
#> 7                 codebooks/classroom_language_mapping.csv codebook   39      3
#> 8                     codebooks/classroom_race_mapping.csv codebook   16      3
#> 9                               codebooks/county_codes.csv codebook   67      3
#> 10                       codebooks/delivery_type_codes.csv codebook    7      3
#> 11             codebooks/student_delivery_type_mapping.csv codebook   16      2
#> 12                      codebooks/student_race_mapping.csv codebook   15      3
#> 13    mappings/applications_column_map_capacity_cycle1.csv  mapping    7      4
#> 14         mappings/applications_column_map_new_cycle1.csv  mapping   11      4
#> 15 mappings/applications_column_map_nonrenewals_cycle1.csv  mapping    7      4
#> 16    mappings/applications_column_map_renewals_cycle1.csv  mapping   15      4
#> 17                   mappings/budget_column_map_legacy.csv  mapping    6      4
#> 18                      mappings/budget_column_map_new.csv  mapping   12      4
#> 19                mappings/classroom_column_map_legacy.csv  mapping  100      4
#> 20                   mappings/classroom_column_map_new.csv  mapping  125      4
#> 21                  mappings/student_column_map_legacy.csv  mapping  202      4
#> 22                     mappings/student_column_map_new.csv  mapping  270      4
```

``` r

aggregate(
  cbind(files = rep(1L, nrow(csv_inventory)), rows = csv_inventory$rows),
  by = list(role = csv_inventory$role),
  FUN = sum
)
#>       role files rows
#> 1 codebook    12  267
#> 2  mapping    10  755
```

These files are part of the package API in a broad sense: changing them
changes how raw fields become standardized analytic columns.

## Two Kinds of CSV Files

Column mappings have a common four-column schema.

``` r

mapping_files <- csv_inventory[csv_inventory$role == "mapping", "file"]

data.frame(
  file = mapping_files,
  schema = vapply(
    file.path(ext_dir, mapping_files),
    function(path) {
      paste(names(utils::read.csv(path, nrows = 0, check.names = FALSE)), collapse = ", ")
    },
    character(1)
  ),
  row.names = NULL
)
#>                                                       file
#> 1     mappings/applications_column_map_capacity_cycle1.csv
#> 2          mappings/applications_column_map_new_cycle1.csv
#> 3  mappings/applications_column_map_nonrenewals_cycle1.csv
#> 4     mappings/applications_column_map_renewals_cycle1.csv
#> 5                    mappings/budget_column_map_legacy.csv
#> 6                       mappings/budget_column_map_new.csv
#> 7                 mappings/classroom_column_map_legacy.csv
#> 8                    mappings/classroom_column_map_new.csv
#> 9                   mappings/student_column_map_legacy.csv
#> 10                     mappings/student_column_map_new.csv
#>                                    schema
#> 1  raw_column, standard_name, type, notes
#> 2  raw_column, standard_name, type, notes
#> 3  raw_column, standard_name, type, notes
#> 4  raw_column, standard_name, type, notes
#> 5  raw_column, standard_name, type, notes
#> 6  raw_column, standard_name, type, notes
#> 7  raw_column, standard_name, type, notes
#> 8  raw_column, standard_name, type, notes
#> 9  raw_column, standard_name, type, notes
#> 10 raw_column, standard_name, type, notes
```

| Mapping field | Meaning |
|----|----|
| `raw_column` | Column label expected in a source file format. |
| `standard_name` | Package-standard variable name used after cleaning. |
| `type` | Domain tag used for cleaning, privacy filtering, and tests. |
| `notes` | Human-readable explanation or implementation note. |

Codebooks have domain-specific schemas because they serve different
cleaning tasks.

``` r

helpers <- list(
  delivery_types = alprek_delivery_types(),
  county_codes = alprek_county_codes(),
  degree_patterns = alprek_degree_patterns(),
  budget_categories = alprek_category_groups(),
  classroom_race = alprek_race_mapping(),
  classroom_language = alprek_language_mapping(),
  student_race = alprek_student_race_mapping(),
  student_delivery = alprek_student_delivery_mapping(),
  applications_status = alprek_applications_status_codes(),
  applications_funding = alprek_applications_funding_types(),
  applications_manifest = alprek_applications_source_manifest(),
  applications_edge_cases = utils::read.csv(
    file.path(ext_dir, "codebooks", "applications_edge_cases.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
)

data.frame(
  helper = names(helpers),
  rows = vapply(helpers, nrow, integer(1)),
  schema = vapply(helpers, function(x) paste(names(x), collapse = ", "), character(1)),
  row.names = NULL
)
#>                     helper rows
#> 1           delivery_types    7
#> 2             county_codes   67
#> 3          degree_patterns   27
#> 4        budget_categories   38
#> 5           classroom_race   16
#> 6       classroom_language   39
#> 7             student_race   15
#> 8         student_delivery   16
#> 9      applications_status    6
#> 10    applications_funding    6
#> 11   applications_manifest   13
#> 12 applications_edge_cases   17
#>                                                                                schema
#> 1                                                              code, name, name_short
#> 2                                                 county_code, county_name, fips_code
#> 3                          pattern_type, regex, result, priority, teacher_role, notes
#> 4                                              category_detail, category_group, notes
#> 5                                               raw_value, standardized, factor_order
#> 6                                                    raw_value, standardized, is_null
#> 7                                               raw_value, standardized, factor_order
#> 8                                                             raw_value, standardized
#> 9                                      process_name, kind_inferred, cycle_year, notes
#> 10                                              funding_type, funding_category, notes
#> 11          kind, filename_pattern, sheet, cycle_year, canonical_status, known_issues
#> 12 case_id, label, description, detection_rule, policy, severity, validate_check_name
```

## Codebooks as Boundary Objects

A codebook can be read in a spreadsheet, reviewed in a pull request, and
loaded by the package. The delivery-type codebook is a simple example.

``` r

alprek_delivery_types()
#> # A tibble: 7 × 3
#>   code  name                     name_short    
#>   <chr> <chr>                    <chr>         
#> 1 P     Public School            Public        
#> 2 C     Private Child Care       Private CC    
#> 3 H     Head Start               Head Start    
#> 4 O     Community Organization   Community     
#> 5 F     Faith-Based Organization Faith-Based   
#> 6 U     University Operated      University    
#> 7 S     Private School           Private School
```

The same seven provider types are used by classroom codes, classroom
panels, student panels, linkage diagnostics, and synthetic data.

The degree-pattern codebook is more procedural. It defines
regular-expression patterns and priorities for parsing free-text
credential fields into standard degree levels and areas. The codebook is
still data, but it drives executable cleaning logic.

``` r

aggregate(
  priority ~ pattern_type,
  data = alprek_degree_patterns(),
  FUN = length
)
#>                pattern_type priority
#> 1               degree_area        9
#> 2 degree_area_consolidation       11
#> 3              degree_level        7
```

## How Mappings Are Used

The module cleaning functions load mappings after format detection.

``` text
read source columns
  -> detect format
  -> load matching column map
  -> rename raw columns to standard names
  -> apply codebooks and derived variables
  -> validate standardized output
```

For example:

- budget cleaning uses `budget_category_groups.csv` to group detailed
  budget line items;
- classroom cleaning uses classroom mappings, degree patterns, race
  mappings, language mappings, and delivery-type codes;
- student cleaning uses student mappings, student race mappings, and
  student delivery-type mappings;
- applications cleaning uses four cycle-1 column maps plus status-code,
  funding-type, source-manifest, and edge-case codebooks.

The public mappings describe how raw field names are standardized; they
should not contain row-level examples from real source data.

## Format Detection

Column mappings are selected by detected format. Detection rules look at
column names, marker fields, and source-file width rather than row-level
values.

``` r

budget_legacy <- data.frame(
  "Classroom Code" = "",
  "Classroom Name" = "",
  "Example From OSR Funds" = 0,
  check.names = FALSE
)

budget_new <- data.frame(
  "Classroom Code" = "",
  "Classroom Name" = "",
  "OSR" = 0,
  "Proration" = 0,
  check.names = FALSE
)

classroom_legacy <- as.data.frame(matrix(NA, nrow = 1, ncol = 100))
names(classroom_legacy) <- paste0("col", seq_len(ncol(classroom_legacy)))

classroom_new <- as.data.frame(matrix(NA, nrow = 1, ncol = 121))
names(classroom_new) <- paste0("col", seq_len(ncol(classroom_new)))

student_legacy <- as.data.frame(matrix(NA, nrow = 1, ncol = 202))
names(student_legacy) <- paste0("col", seq_len(ncol(student_legacy)))

student_new <- as.data.frame(matrix(NA, nrow = 1, ncol = 250))
names(student_new) <- paste0("col", seq_len(ncol(student_new)))

data.frame(
  module = c("budget", "budget", "classroom", "classroom", "student", "student"),
  scenario = c("legacy marker", "new marker", "legacy width", "new width", "legacy width", "new width"),
  detected = c(
    budget_detect_format(budget_legacy),
    budget_detect_format(budget_new),
    classroom_detect_format(classroom_legacy),
    classroom_detect_format(classroom_new),
    student_detect_format(student_legacy),
    student_detect_format(student_new)
  )
)
#>      module      scenario detected
#> 1    budget legacy marker   legacy
#> 2    budget    new marker      new
#> 3 classroom  legacy width   legacy
#> 4 classroom     new width      new
#> 5   student  legacy width   legacy
#> 6   student     new width      new
```

Format detection is deliberately conservative. If a source export is too
narrow or contains incompatible markers, the package stops rather than
guessing.

## Applications Codebooks and Mappings

The v0.7.0 Applications module adds four public codebooks and four
cycle-1 column maps:

``` r

csv_inventory[
  grepl("^codebooks/applications_|^mappings/applications_", csv_inventory$file),
  c("file", "role", "rows", "fields")
]
#>                                                       file     role rows fields
#> 1                    codebooks/applications_edge_cases.csv codebook   17      7
#> 2                 codebooks/applications_funding_types.csv codebook    6      3
#> 3               codebooks/applications_source_manifest.csv codebook   13      6
#> 4                  codebooks/applications_status_codes.csv codebook    6      4
#> 13    mappings/applications_column_map_capacity_cycle1.csv  mapping    7      4
#> 14         mappings/applications_column_map_new_cycle1.csv  mapping   11      4
#> 15 mappings/applications_column_map_nonrenewals_cycle1.csv  mapping    7      4
#> 16    mappings/applications_column_map_renewals_cycle1.csv  mapping   15      4
```

The non-renewal source sheet in cycle-1 is headerless, so
`applications_column_map_nonrenewals_cycle1.csv` maps positional
`col_1`, `col_2`, … fields. Future cycles should add new cycle-specific
mapping files rather than editing old maps in place.

## Mapping Contracts

The tests enforce these contracts:

- all 22 expected CSV files are present;
- mapping files use `raw_column`, `standard_name`, `type`, and `notes`;
- mapping keys are nonblank and unique within each file;
- `standard_name` values are unique and use snake case;
- codebook keys are unique;
- canonical domains such as delivery types and race categories are
  covered;
- degree-pattern regexes compile;
- privacy-related mapping types remain explicit in new-format maps.

``` r

all_mappings <- lapply(file.path(ext_dir, mapping_files), function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
})
names(all_mappings) <- mapping_files

data.frame(
  file = names(all_mappings),
  unique_raw_column = vapply(all_mappings, function(x) !any(duplicated(x$raw_column)), logical(1)),
  unique_standard_name = vapply(all_mappings, function(x) !any(duplicated(x$standard_name)), logical(1)),
  snake_case_standard_name = vapply(
    all_mappings,
    function(x) all(grepl("^[a-z][a-z0-9_]*$", x$standard_name)),
    logical(1)
  ),
  row.names = NULL
)
#>                                                       file unique_raw_column
#> 1     mappings/applications_column_map_capacity_cycle1.csv              TRUE
#> 2          mappings/applications_column_map_new_cycle1.csv              TRUE
#> 3  mappings/applications_column_map_nonrenewals_cycle1.csv              TRUE
#> 4     mappings/applications_column_map_renewals_cycle1.csv              TRUE
#> 5                    mappings/budget_column_map_legacy.csv              TRUE
#> 6                       mappings/budget_column_map_new.csv              TRUE
#> 7                 mappings/classroom_column_map_legacy.csv              TRUE
#> 8                    mappings/classroom_column_map_new.csv              TRUE
#> 9                   mappings/student_column_map_legacy.csv              TRUE
#> 10                     mappings/student_column_map_new.csv              TRUE
#>    unique_standard_name snake_case_standard_name
#> 1                  TRUE                     TRUE
#> 2                  TRUE                     TRUE
#> 3                  TRUE                     TRUE
#> 4                 FALSE                     TRUE
#> 5                  TRUE                     TRUE
#> 6                  TRUE                     TRUE
#> 7                  TRUE                     TRUE
#> 8                  TRUE                     TRUE
#> 9                  TRUE                     TRUE
#> 10                 TRUE                     TRUE
```

## Extension Workflow

When a source format changes, the safest update path is:

1.  Add or update a mapping CSV with the same schema.
2.  Add or update a detection rule that selects the mapping only for the
    intended format.
3.  Keep raw data and row-level examples out of `inst/extdata/`.
4.  Add schema tests for the mapping file and its required standardized
    names.
5.  Add cleaning tests that exercise the new mapping on synthetic or
    minimal fixture data.
6.  Run the affected module tests and render the relevant vignettes.

This keeps format drift visible as a versioned package change rather
than a private one-off script edit.

## What Public Codebooks Should Not Contain

Do not put these in public package codebooks or mappings:

- raw ADECE files or row extracts;
- real classroom codes from confidential files;
- student identifiers;
- names, dates of birth, or staff contact fields;
- local absolute paths;
- private output filenames;
- examples copied from a private worksheet.

Public codebooks can describe categories, standardized names, and
format-level field mappings. They should not document individual
records.

## Hidden Contract Checks

## Reading Map

| Need                                                    | Where to go |
|---------------------------------------------------------|-------------|
| Why externalized codebooks are part of the architecture | M1          |
| How validation checks enforce codebook contracts        | M2          |
| How to build panels with current mappings               | A2          |
| How to keep private source files out of public docs     | M4          |
