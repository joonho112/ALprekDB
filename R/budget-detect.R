#' Detect Budget File Format
#'
#' @description Determines whether a budget data frame uses the legacy
#'   (2021-2024) or new (2024-2025+) format based on column name patterns.
#'
#' @param df A data frame (raw, from Excel import).
#' @return Character string: `"legacy"` or `"new"`.
#'
#' @details
#' Detection logic:
#' - **Legacy**: Any column name ending in `"From OSR Funds"`.
#' - **New**: Any column name containing `"OSR"` AND any column containing
#'   `"Proration"`.
#'
#' @examples
#' \dontrun{
#' df <- readxl::read_excel("rptClassBudgets 2021-2022.xlsx")
#' budget_detect_format(df)
#' # "legacy"
#' }
#'
#' @export
budget_detect_format <- function(df) {
  col_names <- names(df)
  has_core_cols <- .detect_budget_core_columns(col_names)
  has_legacy <- .detect_legacy_markers(col_names)
  has_new <- .detect_new_markers(col_names)

  if (!has_core_cols && (has_legacy || has_new)) {
    stop(
      "Cannot detect budget format. Budget marker columns were found, but ",
      "required classroom-budget identifiers are missing: Classroom Code, ",
      "Classroom Name.",
      call. = FALSE
    )
  }

  if (has_legacy && has_new) {
    stop(
      "Cannot detect budget format. Found both legacy and new budget markers; ",
      "please verify this is a canonical processed classroom budget export.",
      call. = FALSE
    )
  }

  if (has_legacy) {
    return("legacy")
  }

  if (has_new) {
    return("new")
  }

  stop(
    "Cannot detect budget format. Expected either:\n",
    "  - Legacy: columns ending in 'From OSR Funds'\n",
    "  - New: columns containing 'OSR' and 'Proration'\n",
    "Found columns: ", paste(head(col_names, 10), collapse = ", "),
    if (length(col_names) > 10) paste0(", ... (", length(col_names), " total)"),
    call. = FALSE
  )
}


#' Detect legacy format markers
#' @param col_names Character vector of column names.
#' @return Logical.
#' @keywords internal
.detect_legacy_markers <- function(col_names) {
  any(grepl("From OSR Funds$", col_names))
}


#' Detect new format markers
#' @param col_names Character vector of column names.
#' @return Logical.
#' @keywords internal
.detect_new_markers <- function(col_names) {
  has_osr <- any(grepl("\\bOSR\\b", col_names))
  has_proration <- any(grepl("Proration", col_names, ignore.case = TRUE))
  has_osr && has_proration
}


#' Detect core processed classroom-budget identifier columns
#' @param col_names Character vector of column names.
#' @return Logical.
#' @keywords internal
.detect_budget_core_columns <- function(col_names) {
  all(c("Classroom Code", "Classroom Name") %in% col_names)
}
