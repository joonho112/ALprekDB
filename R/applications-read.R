#' Read ADECE Renewal Classroom Applications
#'
#' @description Reads the renewal classroom applications sheet from an ADECE
#'   master xlsx file (cycle-1 layout). Captures provenance (file hash,
#'   git SHA, cycle year, receipt date, sheet, raw row index, row lineage ID)
#'   but does NOT clean or standardize columns — that is [applications_clean()]'s
#'   job.
#'
#' @param path Character. Path to the ADECE master xlsx file.
#' @param sheet Character. Sheet name. Default `"26-27 requests_TW"`
#'   (cycle-1 convention). For cycle-0 separate-file layout, the sheet name
#'   may be `NULL` (first sheet).
#' @param cycle_year Character. Cycle year label (e.g., `"2026-2027"`).
#'   Required.
#' @param receipt_date Date or character. Date file received from ADECE.
#'   Default `Sys.Date()`.
#'
#' @return An `alprek_applications_raw` S3 object (list) with elements:
#'   - `data`: tibble of raw data with column names as-is plus `raw_row_index`
#'     and stable `lineage_id`
#'   - `meta`: list with kind = "renewals", path, sheet, cycle_year,
#'     receipt_date, file_sha256, n_rows, n_cols, col_names, git_sha, read_at
#'
#' @examples
#' \dontrun{
#' raw <- applications_read_renewals(
#'   path = "Copy of 2026-27 Classroom Applications_tw04202026 (003).xlsx",
#'   cycle_year = "2026-2027"
#' )
#' raw
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom tibble as_tibble
#' @export
applications_read_renewals <- function(path,
                                         sheet = "26-27 requests_TW",
                                         cycle_year,
                                         receipt_date = Sys.Date()) {
  .applications_read_one(path = path, sheet = sheet, kind = "renewals",
                          cycle_year = cycle_year,
                          receipt_date = receipt_date,
                          col_names = TRUE)
}


#' Read ADECE New Classroom Applications
#'
#' @inheritParams applications_read_renewals
#' @param sheet Character. Sheet name. Default `"26-27 new"`.
#' @return An `alprek_applications_raw` S3 object with `kind = "new_apps"`.
#' @export
applications_read_new <- function(path,
                                    sheet = "26-27 new",
                                    cycle_year,
                                    receipt_date = Sys.Date()) {
  .applications_read_one(path = path, sheet = sheet, kind = "new_apps",
                          cycle_year = cycle_year,
                          receipt_date = receipt_date,
                          col_names = TRUE)
}


#' Read ADECE Non-Renewal Classrooms
#'
#' @description Reads the Non-Renew sheet from an ADECE master xlsx file.
#'   IMPORTANT: This sheet has **no header row** in cycle-1; data starts at
#'   row 1. The read function sets `col_names = FALSE` and assigns
#'   positional column names (`col_1` .. `col_7`). Use [applications_clean()]
#'   to rename via the `applications_column_map_nonrenewals_*` codebook.
#'
#' @inheritParams applications_read_renewals
#' @param sheet Character. Sheet name. Default `"Non-Renew"`.
#' @return An `alprek_applications_raw` S3 object with `kind = "non_renewals"`.
#' @export
applications_read_nonrenewal <- function(path,
                                           sheet = "Non-Renew",
                                           cycle_year,
                                           receipt_date = Sys.Date()) {
  .applications_read_one(path = path, sheet = sheet, kind = "non_renewals",
                          cycle_year = cycle_year,
                          receipt_date = receipt_date,
                          col_names = FALSE)
}


#' Read ADECE Site Capacity Report
#'
#' @inheritParams applications_read_renewals
#' @param sheet Character. Sheet name. Default
#'   `"rptSite_ClassroomsWithAvailabil"` (sheet name truncated at 31 chars
#'   by Excel).
#' @return An `alprek_applications_raw` S3 object with `kind = "capacity"`.
#' @export
applications_read_capacity <- function(path,
                                         sheet = "rptSite_ClassroomsWithAvailabil",
                                         cycle_year,
                                         receipt_date = Sys.Date()) {
  .applications_read_one(path = path, sheet = sheet, kind = "capacity",
                          cycle_year = cycle_year,
                          receipt_date = receipt_date,
                          col_names = TRUE)
}


# Internal: shared read implementation
.applications_read_one <- function(path, sheet, kind, cycle_year,
                                     receipt_date, col_names) {

  if (missing(path) || is.null(path) || !nzchar(path)) {
    stop("path is required.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }
  if (missing(cycle_year) || is.null(cycle_year) ||
      !grepl("^\\d{4}-\\d{4}$", cycle_year)) {
    stop("cycle_year is required in 'YYYY-YYYY' format.", call. = FALSE)
  }

  # Validate sheet exists
  available_sheets <- readxl::excel_sheets(path)
  if (!is.null(sheet) && !sheet %in% available_sheets) {
    stop(sprintf(
      "Sheet '%s' not found in %s. Available sheets: %s",
      sheet, basename(path),
      paste(sprintf("'%s'", available_sheets), collapse = ", ")
    ), call. = FALSE)
  }

  # Read
  df_raw <- suppressMessages(
    readxl::read_excel(path, sheet = sheet, guess_max = 10000,
                        col_names = col_names, .name_repair = "minimal")
  )

  # For nonrenewal (positional): assign col_1, col_2, ...
  if (isFALSE(col_names)) {
    names(df_raw) <- sprintf("col_%d", seq_along(df_raw))
  }

  source_col_names <- names(df_raw)
  file_sha256 <- alprek_file_hash(path)
  df_raw$raw_row_index <- seq_len(nrow(df_raw))
  df_raw$lineage_id <- .alprek_lineage_id(file_sha256, sheet,
                                           df_raw$raw_row_index,
                                           cycle_year)

  meta <- list(
    kind = kind,
    path = path,
    sheet = sheet,
    cycle_year = cycle_year,
    receipt_date = if (inherits(receipt_date, "Date"))
                      format(receipt_date, "%Y-%m-%d") else
                      as.character(receipt_date),
    file_sha256 = file_sha256,
    file_basename = basename(path),
    n_rows = nrow(df_raw),
    n_cols = length(source_col_names),
    col_names = source_col_names,
    git_sha = alprek_git_sha(),
    read_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )

  structure(list(data = tibble::as_tibble(df_raw), meta = meta),
            class = "alprek_applications_raw")
}


#' Print method for alprek_applications_raw
#' @param x An alprek_applications_raw object.
#' @param ... Ignored.
#' @export
print.alprek_applications_raw <- function(x, ...) {
  cat("<alprek_applications_raw>\n")
  cat("  Kind:        ", x$meta$kind, "\n")
  cat("  File:        ", x$meta$file_basename, "\n")
  cat("  Sheet:       ", x$meta$sheet, "\n")
  cat("  Cycle year:  ", x$meta$cycle_year, "\n")
  cat("  Receipt:     ", x$meta$receipt_date, "\n")
  cat("  SHA-256:     ", substr(x$meta$file_sha256, 1, 16), "...\n")
  cat("  Rows x Cols: ", x$meta$n_rows, " x ", x$meta$n_cols, "\n", sep = "")
  cat("  Read at:     ", x$meta$read_at, "\n")
  invisible(x)
}
