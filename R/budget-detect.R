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

  if (.detect_legacy_markers(col_names)) {
    return("legacy")
  }

  if (.detect_new_markers(col_names)) {
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
