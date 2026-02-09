# ALprekDB: Alabama Pre-K Administrative Data Processing

A modular toolkit for processing, cleaning, validating, and managing
Alabama First Class Pre-K (FCPK) administrative records. This package
standardizes annual data processing workflows for budget, classroom, and
student data files received from ADECE.

## Budget Module

The budget module provides a complete pipeline:

- [`budget_read()`](https://joonho112.github.io/ALprekDB/reference/budget_read.md):
  Import Excel files with auto format detection

- [`budget_clean()`](https://joonho112.github.io/ALprekDB/reference/budget_clean.md):
  Transform to long-format intermediate

- [`budget_validate()`](https://joonho112.github.io/ALprekDB/reference/budget_validate.md):
  Data quality checks with reconciliation

- [`budget_transform()`](https://joonho112.github.io/ALprekDB/reference/budget_transform.md):
  Wide master with derived variables

- [`budget_bind_years()`](https://joonho112.github.io/ALprekDB/reference/budget_bind_years.md):
  Multi-year panel construction

- [`budget_export_csv()`](https://joonho112.github.io/ALprekDB/reference/budget_export_csv.md),
  [`budget_export_parquet()`](https://joonho112.github.io/ALprekDB/reference/budget_export_parquet.md),
  [`budget_export_excel()`](https://joonho112.github.io/ALprekDB/reference/budget_export_excel.md),
  [`budget_export_rds()`](https://joonho112.github.io/ALprekDB/reference/budget_export_rds.md):
  Multiple export formats

## Convenience Functions

- [`budget_process()`](https://joonho112.github.io/ALprekDB/reference/budget_process.md):
  Full pipeline for one year

- [`budget_process_years()`](https://joonho112.github.io/ALprekDB/reference/budget_process_years.md):
  Full pipeline for multiple years

- [`budget_config()`](https://joonho112.github.io/ALprekDB/reference/budget_config.md):
  Create processing configuration

## Classroom Module

The classroom module provides a complete pipeline:

- [`classroom_read()`](https://joonho112.github.io/ALprekDB/reference/classroom_read.md):
  Import Classroom Details Excel files with format detection

- [`classroom_clean()`](https://joonho112.github.io/ALprekDB/reference/classroom_clean.md):
  Standardize variables, classify degrees, normalize demographics

- [`classroom_validate()`](https://joonho112.github.io/ALprekDB/reference/classroom_validate.md):
  Data quality checks (10 checks, advisory)

- [`classroom_bind_years()`](https://joonho112.github.io/ALprekDB/reference/classroom_bind_years.md):
  Multi-year panel with forward-fill imputation

- [`classroom_export_csv()`](https://joonho112.github.io/ALprekDB/reference/classroom_export_csv.md),
  [`classroom_export_excel()`](https://joonho112.github.io/ALprekDB/reference/classroom_export_excel.md),
  [`classroom_export_rds()`](https://joonho112.github.io/ALprekDB/reference/classroom_export_rds.md),
  [`classroom_export_stata()`](https://joonho112.github.io/ALprekDB/reference/classroom_export_stata.md),
  [`classroom_export_parquet()`](https://joonho112.github.io/ALprekDB/reference/classroom_export_parquet.md):
  Multiple export formats

- [`classroom_process()`](https://joonho112.github.io/ALprekDB/reference/classroom_process.md):
  Full pipeline for one year

- [`classroom_process_years()`](https://joonho112.github.io/ALprekDB/reference/classroom_process_years.md):
  Full pipeline for multiple years

- [`classroom_config()`](https://joonho112.github.io/ALprekDB/reference/classroom_config.md):
  Create processing configuration

## Student Module

The student module processes FCPK Student/Child Detail files:

- [`student_read()`](https://joonho112.github.io/ALprekDB/reference/student_read.md):
  Import Student Details Excel files with format detection

- [`student_clean()`](https://joonho112.github.io/ALprekDB/reference/student_clean.md):
  Standardize demographics, parse income, clean assessments

- [`student_validate()`](https://joonho112.github.io/ALprekDB/reference/student_validate.md):
  Data quality checks (12 checks, advisory)

- [`student_bind_years()`](https://joonho112.github.io/ALprekDB/reference/student_bind_years.md):
  Multi-year panel construction (no imputation)

- [`student_export_csv()`](https://joonho112.github.io/ALprekDB/reference/student_export_csv.md),
  [`student_export_excel()`](https://joonho112.github.io/ALprekDB/reference/student_export_excel.md),
  [`student_export_rds()`](https://joonho112.github.io/ALprekDB/reference/student_export_rds.md),
  [`student_export_stata()`](https://joonho112.github.io/ALprekDB/reference/student_export_stata.md),
  [`student_export_parquet()`](https://joonho112.github.io/ALprekDB/reference/student_export_parquet.md):
  Multiple export formats

- [`student_process()`](https://joonho112.github.io/ALprekDB/reference/student_process.md):
  Full pipeline for one year

- [`student_process_years()`](https://joonho112.github.io/ALprekDB/reference/student_process_years.md):
  Full pipeline for multiple years

- [`student_config()`](https://joonho112.github.io/ALprekDB/reference/student_config.md):
  Create processing configuration

- [`student_track()`](https://joonho112.github.io/ALprekDB/reference/student_track.md):
  Track student presence across years

- [`student_summary_stats()`](https://joonho112.github.io/ALprekDB/reference/student_summary_stats.md):
  Summary statistics

## Student Transform

Derive advanced analysis variables from cleaned student data:

- [`student_transform()`](https://joonho112.github.io/ALprekDB/reference/student_transform.md):
  Compute GOLD gain scores, K-readiness transitions, chronic absence
  flags, service density indices, and eDECA pre-post gains

## Linkage Module

The linkage module connects budget, classroom, and student data:

- [`linkage_classroom_budget()`](https://joonho112.github.io/ALprekDB/reference/linkage_classroom_budget.md):
  Join classroom + budget panels

- [`linkage_student_classroom()`](https://joonho112.github.io/ALprekDB/reference/linkage_student_classroom.md):
  Join student + classroom panels

- [`linkage_aggregate_students()`](https://joonho112.github.io/ALprekDB/reference/linkage_aggregate_students.md):
  Aggregate student data to classroom level

- [`linkage_create_master()`](https://joonho112.github.io/ALprekDB/reference/linkage_create_master.md):
  Create fully linked master dataset (2 levels)

- [`linkage_validate()`](https://joonho112.github.io/ALprekDB/reference/linkage_validate.md):
  Data quality checks on joined data (8 checks)

- [`linkage_summary_stats()`](https://joonho112.github.io/ALprekDB/reference/linkage_summary_stats.md):
  Summary statistics for linked data

- [`linkage_export_csv()`](https://joonho112.github.io/ALprekDB/reference/linkage_export_csv.md),
  [`linkage_export_excel()`](https://joonho112.github.io/ALprekDB/reference/linkage_export_excel.md),
  [`linkage_export_rds()`](https://joonho112.github.io/ALprekDB/reference/linkage_export_rds.md),
  [`linkage_export_stata()`](https://joonho112.github.io/ALprekDB/reference/linkage_export_stata.md),
  [`linkage_export_parquet()`](https://joonho112.github.io/ALprekDB/reference/linkage_export_parquet.md):
  Multiple export formats

## Database Module

DuckDB-based persistent storage for processed panel data:

- [`db_init()`](https://joonho112.github.io/ALprekDB/reference/db_init.md):
  Create or open a DuckDB database

- [`db_close()`](https://joonho112.github.io/ALprekDB/reference/db_close.md):
  Close database connection

- [`db_write_panel()`](https://joonho112.github.io/ALprekDB/reference/db_write_panel.md):
  Write panel data to database

- [`db_write_master()`](https://joonho112.github.io/ALprekDB/reference/db_write_master.md):
  Write linked master dataset to database

- [`db_write_year()`](https://joonho112.github.io/ALprekDB/reference/db_write_year.md):
  Append single year incrementally

- [`db_read_panel()`](https://joonho112.github.io/ALprekDB/reference/db_read_panel.md):
  Read panel data with type reconstruction

- [`db_read_master()`](https://joonho112.github.io/ALprekDB/reference/db_read_master.md):
  Read master dataset from database

- [`db_list_tables()`](https://joonho112.github.io/ALprekDB/reference/db_list_tables.md):
  List user tables

- [`db_table_info()`](https://joonho112.github.io/ALprekDB/reference/db_table_info.md):
  Column metadata with R type information

- [`db_query()`](https://joonho112.github.io/ALprekDB/reference/db_query.md):
  Execute arbitrary SQL queries

## Synthetic Data

Generate realistic synthetic datasets for examples and vignettes:

- [`alprek_synthetic_budget()`](https://joonho112.github.io/ALprekDB/reference/alprek_synthetic_budget.md):
  Synthetic budget panel data

- [`alprek_synthetic_classroom()`](https://joonho112.github.io/ALprekDB/reference/alprek_synthetic_classroom.md):
  Synthetic classroom panel data

- [`alprek_synthetic_student()`](https://joonho112.github.io/ALprekDB/reference/alprek_synthetic_student.md):
  Synthetic student panel data (with derived variables)

All three generators share classroom codes (via seed) enabling
cross-module linkage with
[`linkage_create_master()`](https://joonho112.github.io/ALprekDB/reference/linkage_create_master.md).

## Codebooks

- [`alprek_category_groups()`](https://joonho112.github.io/ALprekDB/reference/alprek_category_groups.md):
  Budget category mappings

- [`alprek_delivery_types()`](https://joonho112.github.io/ALprekDB/reference/alprek_delivery_types.md):
  Delivery type code mappings

- [`alprek_county_codes()`](https://joonho112.github.io/ALprekDB/reference/alprek_county_codes.md):
  Alabama county code mappings

- [`alprek_degree_patterns()`](https://joonho112.github.io/ALprekDB/reference/alprek_degree_patterns.md):
  Teacher degree/credential classification patterns

- [`alprek_race_mapping()`](https://joonho112.github.io/ALprekDB/reference/alprek_race_mapping.md):
  Race/ethnicity standardization mapping

- [`alprek_language_mapping()`](https://joonho112.github.io/ALprekDB/reference/alprek_language_mapping.md):
  Fluent language field cleaning mapping

- [`alprek_student_race_mapping()`](https://joonho112.github.io/ALprekDB/reference/alprek_student_race_mapping.md):
  Student race/ethnicity standardization

- [`alprek_student_delivery_mapping()`](https://joonho112.github.io/ALprekDB/reference/alprek_student_delivery_mapping.md):
  Student delivery type standardization

## Utilities

- [`parse_classroom_code()`](https://joonho112.github.io/ALprekDB/reference/parse_classroom_code.md):
  Parse classroom code components

- [`parse_classroom_codes()`](https://joonho112.github.io/ALprekDB/reference/parse_classroom_codes.md):
  Vectorized classroom code parser

- [`alprek_infer_school_year()`](https://joonho112.github.io/ALprekDB/reference/alprek_infer_school_year.md):
  Infer school year from filename

## See also

Useful links:

- <https://joonho112.github.io/ALprekDB/>

- <https://github.com/joonho112/ALprekDB>

- Report bugs at <https://github.com/joonho112/ALprekDB/issues>

## Author

**Maintainer**: JoonHo Lee <jlee296@ua.edu>
([ORCID](https://orcid.org/0009-0006-4019-8703))
