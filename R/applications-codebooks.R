#' Load Applications Column Mapping
#'
#' @description Loads the column name mapping for a specific applications
#'   source kind and cycle. Used internally during the read step.
#'
#' @param kind Character. One of `"renewals"`, `"new"`, `"nonrenewals"`,
#'   `"capacity"`.
#' @param cycle Character. Currently `"cycle1"` only (cycle-1 = 2026-2027).
#'   Future cycles will add `"cycle2"`, etc.
#' @return A tibble with columns: `raw_column`, `standard_name`, `type`, `notes`.
#' @keywords internal
.load_applications_column_map <- function(kind, cycle = "cycle1") {
  kind <- match.arg(kind, c("renewals", "new", "nonrenewals", "capacity"))
  fname <- paste0("applications_column_map_", kind, "_", cycle, ".csv")
  path <- system.file("extdata", "mappings", fname,
                       package = "ALprekDB", mustWork = TRUE)
  suppressMessages(readr::read_csv(path, show_col_types = FALSE,
                                     progress = FALSE))
}


#' Load Applications Renewals Column Map
#'
#' @description Returns the column mapping for ADECE renewal classroom
#'   applications data (cycle-1, 2026-2027). Used by `applications_read_renewals()`.
#'
#' @param cycle Character. Default `"cycle1"`.
#' @return A tibble with columns: `raw_column`, `standard_name`, `type`, `notes`.
#'
#' @examples
#' alprek_applications_renewal_map()
#'
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#' @export
alprek_applications_renewal_map <- function(cycle = "cycle1") {
  .load_applications_column_map("renewals", cycle = cycle)
}


#' Load Applications New Classroom Column Map
#'
#' @description Returns the column mapping for ADECE new classroom applications
#'   data (cycle-1, 2026-2027). Used by `applications_read_new()`.
#'
#' @param cycle Character. Default `"cycle1"`.
#' @return A tibble with columns: `raw_column`, `standard_name`, `type`, `notes`.
#'
#' @examples
#' alprek_applications_new_map()
#'
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#' @export
alprek_applications_new_map <- function(cycle = "cycle1") {
  .load_applications_column_map("new", cycle = cycle)
}


#' Load Applications Non-Renewals Column Map
#'
#' @description Returns the positional column mapping for ADECE non-renewal
#'   data (cycle-1, 2026-2027). Note: the source sheet has no header row,
#'   so column positions are mapped explicitly (col_1, col_2, ...).
#'
#' @param cycle Character. Default `"cycle1"`.
#' @return A tibble with columns: `raw_column`, `standard_name`, `type`, `notes`.
#'
#' @examples
#' alprek_applications_nonrenewal_map()
#'
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#' @export
alprek_applications_nonrenewal_map <- function(cycle = "cycle1") {
  .load_applications_column_map("nonrenewals", cycle = cycle)
}


#' Load Applications Capacity Column Map
#'
#' @description Returns the column mapping for ADECE site capacity data
#'   (cycle-1, 2026-2027). Used by `applications_read_capacity()`.
#'
#' @param cycle Character. Default `"cycle1"`.
#' @return A tibble with columns: `raw_column`, `standard_name`, `type`, `notes`.
#'
#' @examples
#' alprek_applications_capacity_map()
#'
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#' @export
alprek_applications_capacity_map <- function(cycle = "cycle1") {
  .load_applications_column_map("capacity", cycle = cycle)
}


#' Load Applications Status Codes
#'
#' @description Returns the mapping from "Process Name" values in ADECE
#'   application files to standardized kind labels and cycle years.
#'
#' @return A tibble with columns: `process_name`, `kind_inferred`,
#'   `cycle_year`, `notes`.
#'
#' @examples
#' alprek_applications_status_codes()
#'
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#' @export
alprek_applications_status_codes <- function() {
  path <- system.file("extdata", "codebooks",
                       "applications_status_codes.csv",
                       package = "ALprekDB", mustWork = TRUE)
  suppressMessages(readr::read_csv(path, show_col_types = FALSE,
                                     progress = FALSE))
}


#' Load Applications Funding Types
#'
#' @description Returns the mapping from ADECE funding type labels to
#'   standardized funding categories.
#'
#' @return A tibble with columns: `funding_type`, `funding_category`, `notes`.
#'
#' @examples
#' alprek_applications_funding_types()
#'
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#' @export
alprek_applications_funding_types <- function() {
  path <- system.file("extdata", "codebooks",
                       "applications_funding_types.csv",
                       package = "ALprekDB", mustWork = TRUE)
  suppressMessages(readr::read_csv(path, show_col_types = FALSE,
                                     progress = FALSE))
}


#' Load Applications Source Manifest
#'
#' @description Returns the canonical source manifest mapping ADECE input
#'   files to data kinds, sheets, cycle years, and canonical status.
#'
#' @return A tibble with columns: `kind`, `filename_pattern`, `sheet`,
#'   `cycle_year`, `canonical_status`, `known_issues`.
#'
#' @examples
#' alprek_applications_source_manifest()
#'
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#' @export
alprek_applications_source_manifest <- function() {
  path <- system.file("extdata", "codebooks",
                       "applications_source_manifest.csv",
                       package = "ALprekDB", mustWork = TRUE)
  suppressMessages(readr::read_csv(path, show_col_types = FALSE,
                                     progress = FALSE))
}
