#' Load Budget Category Groups Codebook
#'
#' @description Returns the mapping from detailed budget line item names to
#'   standardized category groups. This codebook is used during the cleaning
#'   step to aggregate line items into the 8 standard budget categories.
#'
#' @return A tibble with columns: `category_detail`, `category_group`, `notes`.
#'
#' @examples
#' alprek_category_groups()
#'
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#' @export
alprek_category_groups <- function() {
  path <- system.file(
    "extdata", "codebooks", "budget_category_groups.csv",
    package = "ALprekDB", mustWork = TRUE
  )
  tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE))
}


#' Load Delivery Type Codes
#'
#' @description Returns the mapping from single-letter delivery type codes
#'   to full names.
#'
#' @return A tibble with columns: `code`, `name`, `name_short`.
#'
#' @examples
#' alprek_delivery_types()
#'
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#' @export
alprek_delivery_types <- function() {
  path <- system.file(
    "extdata", "codebooks", "delivery_type_codes.csv",
    package = "ALprekDB", mustWork = TRUE
  )
  tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE))
}


#' Load Alabama County Codes
#'
#' @description Returns the mapping from 3-digit county codes used in classroom
#'   codes to county names and FIPS codes.
#'
#' @return A tibble with columns: `county_code`, `county_name`, `fips_code`.
#'
#' @examples
#' alprek_county_codes()
#'
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#' @export
alprek_county_codes <- function() {
  path <- system.file(
    "extdata", "codebooks", "county_codes.csv",
    package = "ALprekDB", mustWork = TRUE
  )
  tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character"))
}


#' Load Budget Column Mapping
#'
#' @description Loads the column name mapping for a specific budget file format.
#'   Used internally during the read step.
#'
#' @param format Character. Either `"legacy"` or `"new"`.
#' @return A tibble with columns: `raw_column`, `standard_name`, `type`, `notes`.
#' @keywords internal
.load_column_map <- function(format) {
  fname <- paste0("budget_column_map_", format, ".csv")
  path <- system.file("extdata", "mappings", fname, package = "ALprekDB", mustWork = TRUE)
  tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE))
}
