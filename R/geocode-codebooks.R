#' Load Geocode Melissa Column Map (v1)
#'
#' @description Returns the 29-row column mapping for the Melissa v1
#'   geocoded delivery contract. Used internally by `geocode_read()` and
#'   `geocode_detect_format()` to verify that incoming xlsx files match
#'   the expected schema (column names, dtypes, source groups, required
#'   vs. optional flags, and observed v0.8.0 baseline counts).
#'
#' @return A tibble with 29 rows and columns: `raw_col`, `std_col`,
#'   `dtype`, `source_group`, `is_required`, `observed_n_na`,
#'   `observed_n_distinct`, `notes`.
#'
#' @examples
#' alprek_geocode_column_map()
#'
#' @export
alprek_geocode_column_map <- function() {
  path <- system.file("extdata", "codebooks",
                       "geocode_column_map_melissa_v1.csv",
                       package = "ALprekDB", mustWork = TRUE)
  suppressMessages(readr::read_csv(
    path,
    col_types = readr::cols(
      raw_col              = readr::col_character(),
      std_col              = readr::col_character(),
      dtype                = readr::col_character(),
      source_group         = readr::col_character(),
      is_required          = readr::col_logical(),
      observed_n_na        = readr::col_integer(),
      observed_n_distinct  = readr::col_integer(),
      notes                = readr::col_character()
    ),
    progress = FALSE
  ))
}


#' Load Melissa RESULTCODE Meaning Table
#'
#' @description Returns the 8-row mapping from Melissa Geocoder
#'   `RESULTCODE` values (`GS01`-`GS08`) to human-readable labels,
#'   precision tier, expected accuracy in meters, whether the code is
#'   acceptable for the master table, and v0.8.0 observed counts.
#'
#' @return A tibble with 8 rows and columns: `code`, `label`,
#'   `precision_tier`, `expected_accuracy_m`, `acceptable_for_master`,
#'   `observed_in_v080_input`, `observed_n_in_v080`,
#'   `paired_status_in_v080`, `source`, `retrieved_at`.
#'
#' @examples
#' alprek_geocode_resultcode_meaning()
#'
#' @export
alprek_geocode_resultcode_meaning <- function() {
  path <- system.file("extdata", "codebooks",
                       "melissa_resultcode_codes.csv",
                       package = "ALprekDB", mustWork = TRUE)
  suppressMessages(readr::read_csv(
    path,
    col_types = readr::cols(
      code                    = readr::col_character(),
      label                   = readr::col_character(),
      precision_tier          = readr::col_character(),
      expected_accuracy_m     = readr::col_double(),
      acceptable_for_master   = readr::col_logical(),
      observed_in_v080_input  = readr::col_logical(),
      observed_n_in_v080      = readr::col_integer(),
      paired_status_in_v080   = readr::col_character(),
      source                  = readr::col_character(),
      retrieved_at            = readr::col_date()
    ),
    progress = FALSE
  ))
}


#' Load Melissa STATUSCODE Meaning Table
#'
#' @description Returns the 4-row mapping from the derived
#'   `STATUSCODE` field (no Melissa public documentation found; observed
#'   1:1 pairing with `RESULTCODE` in the v0.8.0 input) to human-readable
#'   labels and v0.8.0 observed counts.
#'
#' @return A tibble with 4 rows and columns: `code`, `label`,
#'   `is_success`, `paired_resultcode_in_v080`, `observed_n_in_v080`,
#'   `source`, `retrieved_at`.
#'
#' @examples
#' alprek_geocode_statuscode_meaning()
#'
#' @export
alprek_geocode_statuscode_meaning <- function() {
  path <- system.file("extdata", "codebooks",
                       "melissa_statuscode_codes.csv",
                       package = "ALprekDB", mustWork = TRUE)
  suppressMessages(readr::read_csv(
    path,
    col_types = readr::cols(
      code                       = readr::col_character(),
      label                      = readr::col_character(),
      is_success                 = readr::col_logical(),
      paired_resultcode_in_v080  = readr::col_character(),
      observed_n_in_v080         = readr::col_integer(),
      source                     = readr::col_character(),
      retrieved_at               = readr::col_date()
    ),
    progress = FALSE
  ))
}


#' Load Melissa ERRORCODE Meaning Table
#'
#' @description Returns the 17-row reference table of Melissa GE-/AE-
#'   family error codes (severity, meaning, public source). All 17 codes
#'   were unobserved (100% NA) in the v0.8.0 input but are catalogued
#'   here so future deliveries that surface ERRORCODE values can be
#'   decoded without further research.
#'
#' @return A tibble with 17 rows and columns: `code`, `label`, `severity`,
#'   `meaning`, `observed_in_v080_input`, `observed_n_in_v080`, `source`,
#'   `retrieved_at`.
#'
#' @examples
#' alprek_geocode_errorcode_meaning()
#'
#' @export
alprek_geocode_errorcode_meaning <- function() {
  path <- system.file("extdata", "codebooks",
                       "melissa_errorcode_codes.csv",
                       package = "ALprekDB", mustWork = TRUE)
  suppressMessages(readr::read_csv(
    path,
    col_types = readr::cols(
      code                    = readr::col_character(),
      label                   = readr::col_character(),
      severity                = readr::col_character(),
      meaning                 = readr::col_character(),
      observed_in_v080_input  = readr::col_logical(),
      observed_n_in_v080      = readr::col_integer(),
      source                  = readr::col_character(),
      retrieved_at            = readr::col_date()
    ),
    progress = FALSE
  ))
}


#' Load Alabama County FIPS Reference Table
#'
#' @description Returns the canonical 67-row Alabama county FIPS table
#'   (state FIPS 01). Used by `geocode_validate()` to confirm that
#'   Melissa-returned `FIPS` and `COUNTYNAME` values fall within Alabama
#'   and that the FIPS<->name pairing is consistent.
#'
#' @return A tibble with 67 rows and columns: `fips_full`, `fips_state`,
#'   `fips_county`, `county_name`, `county_name_canonical_lower`, `state`.
#'
#' @examples
#' alprek_geocode_al_fips_counties()
#'
#' @export
alprek_geocode_al_fips_counties <- function() {
  path <- system.file("extdata", "codebooks",
                       "geocode_al_fips_counties.csv",
                       package = "ALprekDB", mustWork = TRUE)
  suppressMessages(readr::read_csv(
    path,
    col_types = readr::cols(
      fips_full                    = readr::col_character(),
      fips_state                   = readr::col_character(),
      fips_county                  = readr::col_character(),
      county_name                  = readr::col_character(),
      county_name_canonical_lower  = readr::col_character(),
      state                        = readr::col_character()
    ),
    progress = FALSE
  ))
}


#' Load Geocode Source Manifest
#'
#' @description Returns the 1-row canonical source manifest mapping the
#'   Melissa v1 geocoded delivery file to its vendor metadata (sheet,
#'   vendor version, delivery date, cycle year, expected column count,
#'   example path).
#'
#' @return A tibble with 1 row and columns: `kind`, `filename_pattern`,
#'   `sheet`, `vendor`, `version`, `delivery_date`, `cycle_year`,
#'   `n_cols_expected`, `example_path`, `notes`.
#'
#' @examples
#' alprek_geocode_source_manifest()
#'
#' @export
alprek_geocode_source_manifest <- function() {
  path <- system.file("extdata", "codebooks",
                       "geocode_source_manifest.csv",
                       package = "ALprekDB", mustWork = TRUE)
  suppressMessages(readr::read_csv(
    path,
    col_types = readr::cols(
      kind             = readr::col_character(),
      filename_pattern = readr::col_character(),
      sheet            = readr::col_character(),
      vendor           = readr::col_character(),
      version          = readr::col_character(),
      delivery_date    = readr::col_date(),
      cycle_year       = readr::col_character(),
      n_cols_expected  = readr::col_integer(),
      example_path     = readr::col_character(),
      notes            = readr::col_character()
    ),
    progress = FALSE
  ))
}
