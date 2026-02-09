#' Load Student Race/Ethnicity Mapping
#'
#' @description Returns the mapping from raw race/ethnicity text values to
#'   standardized categories for student data. Maps diverse raw entries across
#'   all 4 years (e.g., "Black or African American" in 2021-22 vs
#'   "Black/African American" in 2022-25) to a consistent set of 7 categories.
#'
#' @return A tibble with columns: `raw_value`, `standardized`, `factor_order`.
#'
#' @details Standardized categories and their factor order:
#'   \enumerate{
#'     \item White
#'     \item Black
#'     \item Latino/Hispanic
#'     \item Asian
#'     \item Mixed
#'     \item Other
#'     \item Unknown
#'   }
#'   Note: Student race values differ from classroom teacher race values
#'   (e.g., "Hispanic" appears in 2021-22 student data but not in classroom data).
#'
#' @examples
#' alprek_student_race_mapping()
#'
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#' @export
alprek_student_race_mapping <- function() {
  path <- system.file(
    "extdata", "codebooks", "student_race_mapping.csv",
    package = "ALprekDB", mustWork = TRUE
  )

tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE))
}


#' Load Student Delivery Type Mapping
#'
#' @description Returns the mapping from raw delivery type text values to
#'   standardized categories for student data. Handles mixed capitalization
#'   in 2024-25 data (e.g., "community organization operated" vs
#'   "Community Organization") and variant spellings (e.g., "private childcare"
#'   vs "private child care").
#'
#' @return A tibble with columns: `raw_value`, `standardized`.
#'
#' @details Standardized delivery types (7 categories):
#'   Public School, Community Organization, Private Child Care,
#'   Faith-Based Organization, Head Start, University Operated, Private School.
#'
#' @examples
#' alprek_student_delivery_mapping()
#'
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#' @export
alprek_student_delivery_mapping <- function() {
  path <- system.file(
    "extdata", "codebooks", "student_delivery_type_mapping.csv",
    package = "ALprekDB", mustWork = TRUE
  )
  tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE))
}


#' Load Student Column Mapping
#'
#' @description Loads the column name mapping for a specific student file
#'   format. Used internally during the read step to rename raw Excel columns
#'   to standardized names.
#'
#' @param format Character. Either `"legacy"` (2021-2024, 202 columns) or
#'   `"new"` (2024-2025, 270 columns).
#' @return A tibble with columns: `raw_column`, `standard_name`, `type`, `notes`.
#' @keywords internal
.load_student_column_map <- function(format) {
  format <- match.arg(format, c("legacy", "new"))
  fname <- paste0("student_column_map_", format, ".csv")
  path <- system.file("extdata", "mappings", fname,
                       package = "ALprekDB", mustWork = TRUE)
  tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE))
}
