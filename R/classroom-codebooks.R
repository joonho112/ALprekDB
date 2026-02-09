#' Load Degree Classification Patterns
#'
#' @description Returns the regex-based classification patterns used to parse
#'   free-text teacher degree/credential fields into standardized categories.
#'   The codebook contains three pattern types:
#'   \itemize{
#'     \item \strong{degree_level}: Maps raw text to ordered education levels
#'       (Waiver, Doctoral, Ed.S., Master's, Bachelor's, Associate, Coursework)
#'     \item \strong{degree_area}: Maps raw text to subject area categories
#'       (ECE, Elementary Education, Child Development, etc.)
#'     \item \strong{degree_area_consolidation}: Reduces 9 area categories to 6
#'       for cross-year comparability
#'   }
#'
#' @return A tibble with columns: `pattern_type`, `regex`, `result`, `priority`,
#'   `teacher_role`, `notes`.
#'
#' @details Patterns are applied by priority (highest first). Waiver detection
#'   (priority 100) overrides all other degree classifications. The `teacher_role`
#'   column is currently `"all"` for all patterns but allows future role-specific
#'   patterns.
#'
#' @examples
#' alprek_degree_patterns()
#'
#' # View degree level patterns only
#' dp <- alprek_degree_patterns()
#' dp[dp$pattern_type == "degree_level", ]
#'
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#' @export
alprek_degree_patterns <- function() {
  path <- system.file(
    "extdata", "codebooks", "classroom_degree_patterns.csv",
    package = "ALprekDB", mustWork = TRUE
  )
  tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE))
}


#' Load Race/Ethnicity Mapping
#'
#' @description Returns the mapping from raw race/ethnicity text values to
#'   standardized categories. Maps diverse raw entries (e.g., "Black or African
#'   American", "Black/African American") to a consistent set of 7 categories.
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
#'
#' @examples
#' alprek_race_mapping()
#'
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#' @export
alprek_race_mapping <- function() {
  path <- system.file(
    "extdata", "codebooks", "classroom_race_mapping.csv",
    package = "ALprekDB", mustWork = TRUE
  )
  tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE))
}


#' Load Classroom Language Mapping
#'
#' @description Returns the mapping for cleaning the "fluent language other than
#'   English" field. Handles null variants ("N/A", "no", "None", "English"),
#'   erroneous data entry values, and case normalization of language names.
#'
#' @return A tibble with columns: `raw_value`, `standardized`, `is_null`.
#'
#' @details Values with `is_null = TRUE` are mapped to `NA` during cleaning
#'   (they indicate no second language rather than a specific language).
#'   Values with `is_null = FALSE` are mapped to their `standardized` form.
#'
#' @examples
#' alprek_language_mapping()
#'
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#' @export
alprek_language_mapping <- function() {
  path <- system.file(
    "extdata", "codebooks", "classroom_language_mapping.csv",
    package = "ALprekDB", mustWork = TRUE
  )
  tibble::as_tibble(utils::read.csv(
    path, stringsAsFactors = FALSE, na.strings = ""
  ))
}


#' Load Classroom Column Mapping
#'
#' @description Loads the column name mapping for a specific classroom file
#'   format. Used internally during the read step to rename raw Excel columns
#'   to standardized names.
#'
#' @param format Character. Either `"legacy"` (2021-2024, 100 columns) or
#'   `"new"` (2024-2025, 125 columns).
#' @return A tibble with columns: `raw_column`, `standard_name`, `type`, `notes`.
#' @keywords internal
.load_classroom_column_map <- function(format) {
  format <- match.arg(format, c("legacy", "new"))
  fname <- paste0("classroom_column_map_", format, ".csv")
  path <- system.file("extdata", "mappings", fname,
                       package = "ALprekDB", mustWork = TRUE)
  tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE))
}
