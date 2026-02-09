#' ALprekDB: Alabama Pre-K Administrative Data Processing
#'
#' @description A modular toolkit for processing, cleaning, validating, and
#' managing Alabama First Class Pre-K (FCPK) administrative records. This
#' package standardizes annual data processing workflows for budget, classroom,
#' and student data files received from ADECE.
#'
#' @section Budget Module:
#' The budget module provides a complete pipeline:
#' - [budget_read()]: Import Excel files with auto format detection
#' - [budget_clean()]: Transform to long-format intermediate
#' - [budget_validate()]: Data quality checks with reconciliation
#' - [budget_transform()]: Wide master with derived variables
#' - [budget_bind_years()]: Multi-year panel construction
#' - [budget_export_csv()], [budget_export_parquet()],
#'   [budget_export_excel()], [budget_export_rds()]: Multiple export formats
#'
#' @section Convenience Functions:
#' - [budget_process()]: Full pipeline for one year
#' - [budget_process_years()]: Full pipeline for multiple years
#' - [budget_config()]: Create processing configuration
#'
#' @section Classroom Module:
#' The classroom module provides a complete pipeline:
#' - [classroom_read()]: Import Classroom Details Excel files with format detection
#' - [classroom_clean()]: Standardize variables, classify degrees, normalize demographics
#' - [classroom_validate()]: Data quality checks (10 checks, advisory)
#' - [classroom_bind_years()]: Multi-year panel with forward-fill imputation
#' - [classroom_export_csv()], [classroom_export_excel()],
#'   [classroom_export_rds()], [classroom_export_stata()],
#'   [classroom_export_parquet()]: Multiple export formats
#' - [classroom_process()]: Full pipeline for one year
#' - [classroom_process_years()]: Full pipeline for multiple years
#' - [classroom_config()]: Create processing configuration
#'
#' @section Student Module:
#' The student module processes FCPK Student/Child Detail files:
#' - [student_read()]: Import Student Details Excel files with format detection
#' - [student_clean()]: Standardize demographics, parse income, clean assessments
#' - [student_validate()]: Data quality checks (12 checks, advisory)
#' - [student_bind_years()]: Multi-year panel construction (no imputation)
#' - [student_export_csv()], [student_export_excel()],
#'   [student_export_rds()], [student_export_stata()],
#'   [student_export_parquet()]: Multiple export formats
#' - [student_process()]: Full pipeline for one year
#' - [student_process_years()]: Full pipeline for multiple years
#' - [student_config()]: Create processing configuration
#' - [student_track()]: Track student presence across years
#' - [student_summary_stats()]: Summary statistics
#'
#' @section Student Transform:
#' Derive advanced analysis variables from cleaned student data:
#' - [student_transform()]: Compute GOLD gain scores, K-readiness transitions,
#'   chronic absence flags, service density indices, and eDECA pre-post gains
#'
#' @section Linkage Module:
#' The linkage module connects budget, classroom, and student data:
#' - [linkage_classroom_budget()]: Join classroom + budget panels
#' - [linkage_student_classroom()]: Join student + classroom panels
#' - [linkage_aggregate_students()]: Aggregate student data to classroom level
#' - [linkage_create_master()]: Create fully linked master dataset (2 levels)
#' - [linkage_validate()]: Data quality checks on joined data (8 checks)
#' - [linkage_summary_stats()]: Summary statistics for linked data
#' - [linkage_export_csv()], [linkage_export_excel()],
#'   [linkage_export_rds()], [linkage_export_stata()],
#'   [linkage_export_parquet()]: Multiple export formats
#'
#' @section Database Module:
#' DuckDB-based persistent storage for processed panel data:
#' - [db_init()]: Create or open a DuckDB database
#' - [db_close()]: Close database connection
#' - [db_write_panel()]: Write panel data to database
#' - [db_write_master()]: Write linked master dataset to database
#' - [db_write_year()]: Append single year incrementally
#' - [db_read_panel()]: Read panel data with type reconstruction
#' - [db_read_master()]: Read master dataset from database
#' - [db_list_tables()]: List user tables
#' - [db_table_info()]: Column metadata with R type information
#' - [db_query()]: Execute arbitrary SQL queries
#'
#' @section Synthetic Data:
#' Generate realistic synthetic datasets for examples and vignettes:
#' - [alprek_synthetic_budget()]: Synthetic budget panel data
#' - [alprek_synthetic_classroom()]: Synthetic classroom panel data
#' - [alprek_synthetic_student()]: Synthetic student panel data (with derived variables)
#'
#' All three generators share classroom codes (via seed) enabling cross-module
#' linkage with [linkage_create_master()].
#'
#' @section Codebooks:
#' - [alprek_category_groups()]: Budget category mappings
#' - [alprek_delivery_types()]: Delivery type code mappings
#' - [alprek_county_codes()]: Alabama county code mappings
#' - [alprek_degree_patterns()]: Teacher degree/credential classification patterns
#' - [alprek_race_mapping()]: Race/ethnicity standardization mapping
#' - [alprek_language_mapping()]: Fluent language field cleaning mapping
#' - [alprek_student_race_mapping()]: Student race/ethnicity standardization
#' - [alprek_student_delivery_mapping()]: Student delivery type standardization
#'
#' @section Utilities:
#' - [parse_classroom_code()]: Parse classroom code components
#' - [parse_classroom_codes()]: Vectorized classroom code parser
#' - [alprek_infer_school_year()]: Infer school year from filename
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom rlang .data
#' @importFrom dplyr across all_of any_of
#' @importFrom stats setNames rnorm runif
#' @importFrom utils head
NULL

# Suppress R CMD check NOTEs for common tidy eval patterns
utils::globalVariables(c(".", ".data"))
