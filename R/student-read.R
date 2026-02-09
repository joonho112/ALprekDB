#' Read Pre-K Student/Child Detail Data
#'
#' @description Reads an FCPK Student/Child Details Excel file, normalizes column
#'   names, drops footer rows, detects format (legacy vs new), and returns an
#'   `alprek_student_raw` S3 object.
#'
#' @param path Character. Path to the Excel file.
#' @param school_year Character. School year in `"YYYY-YYYY"` format. If `NULL`,
#'   attempts to infer from the filename.
#' @param sheet Character or numeric. Sheet name or number. Defaults to
#'   `"rptChildren_Excel"` which is the standard sheet name for student detail files.
#' @param remove_footer Logical. If `TRUE` (default), removes summary/footer
#'   rows (rows where ADECE ID is NA).
#'
#' @return An `alprek_student_raw` S3 object (list) with elements:
#'   - `data`: tibble of raw student data with whitespace-squeezed column names.
#'   - `meta`: list of metadata (path, school_year, year, format, etc.).
#'
#' @examples
#' \dontrun{
#' raw <- student_read("FCPK Student Details 21-22.xlsx")
#' raw <- student_read("24-25 FCPK Child Details.xlsx", school_year = "2024-2025")
#' }
#'
#' @importFrom readxl read_excel excel_sheets
#' @importFrom dplyr filter
#' @importFrom stringr str_squish
#' @export
student_read <- function(path,
                         school_year = NULL,
                         sheet = "rptChildren_Excel",
                         remove_footer = TRUE) {

  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }

  # Verify sheet exists
  sheets <- readxl::excel_sheets(path)
  if (is.character(sheet) && !(sheet %in% sheets)) {
    msg_warn("Sheet '{sheet}' not found. Available: {paste(sheets, collapse = ', ')}. Using first sheet.")
    sheet <- sheets[1]
  }

  msg_info("Reading sheet: '{sheet}' from {basename(path)}")

  # Read raw
  df_raw <- readxl::read_excel(path, sheet = sheet, guess_max = 10000)
  n_rows_raw <- nrow(df_raw)
  n_cols_raw <- ncol(df_raw)

  # Clean column names (squish whitespace, but keep original names)
  names(df_raw) <- alprek_clean_colnames(names(df_raw))

  # Detect format
  format_type <- student_detect_format(df_raw)
  msg_info("Detected format: {format_type} ({n_cols_raw} columns)")

  # Infer school year if not provided
  if (is.null(school_year)) {
    school_year <- alprek_infer_school_year(path)
    if (is.na(school_year)) {
      stop("Could not detect school_year from filename. Please provide it explicitly.",
           call. = FALSE)
    }
    msg_info("Inferred school year: {school_year}")
  }

  year <- alprek_school_year_to_start(school_year)

  # Remove footer rows
  n_rows_clean <- n_rows_raw
  if (remove_footer) {
    id_col <- .find_student_id_col(names(df_raw))
    if (!is.null(id_col)) {
      df_raw <- df_raw[!is.na(df_raw[[id_col]]), , drop = FALSE]
      n_rows_clean <- nrow(df_raw)
      if (n_rows_raw != n_rows_clean) {
        msg_info("Removed {n_rows_raw - n_rows_clean} footer/summary row(s)")
      }
    }
  }

  # Build S3 object
  result <- structure(
    list(
      data = df_raw,
      meta = list(
        path = path,
        school_year = school_year,
        year = year,
        format = format_type,
        sheet = sheet,
        n_rows_raw = n_rows_raw,
        n_rows_clean = n_rows_clean,
        n_cols = n_cols_raw,
        col_names = names(df_raw),
        read_at = Sys.time()
      )
    ),
    class = "alprek_student_raw"
  )

  msg_success("Read {n_rows_clean} students for {school_year} ({format_type} format, {n_cols_raw} cols)")
  result
}


#' Print method for alprek_student_raw
#' @param x An alprek_student_raw object.
#' @param ... Ignored.
#' @export
print.alprek_student_raw <- function(x, ...) {
  cat("<alprek_student_raw>\n")
  cat("  School year:", x$meta$school_year, "\n")
  cat("  Format:", x$meta$format, "\n")
  cat("  Students:", x$meta$n_rows_clean, "\n")
  cat("  Columns:", x$meta$n_cols, "\n")
  cat("  Source:", basename(x$meta$path), "\n")
  invisible(x)
}


#' Detect Student File Format
#'
#' @description Determines whether a student data frame uses the legacy
#'   (2021-2024, 202 columns) or new (2024-2025, 270 columns) format.
#'
#' @param df A data frame (raw, from Excel import with cleaned column names).
#' @return Character string: `"legacy"` or `"new"`.
#'
#' @details
#' Detection logic:
#' - **New**: Contains "Child First Name" or "Modified Schedule" or "Student ID"
#'   (without "State") or ncol >= 250.
#' - **Legacy**: ncol <= 210 and no new-format marker columns present.
#'
#' @examples
#' \dontrun{
#' df <- readxl::read_excel("FCPK Student Details 21-22.xlsx")
#' student_detect_format(df)
#' # "legacy"
#' }
#'
#' @export
student_detect_format <- function(df) {
  col_names <- names(df)
  nc <- ncol(df)

  # New format markers (2024-2025+)
  has_child_name <- "Child First Name" %in% col_names
  has_modified <- "Modified Schedule" %in% col_names
  # "Student ID" is new; "State Student ID" is legacy
  has_student_id <- ("Student ID" %in% col_names) && !("State Student ID" %in% col_names)

  if (has_child_name || has_modified || has_student_id || nc >= 250) {
    return("new")
  }

  if (nc <= 210) {
    return("legacy")
  }

  stop(
    "Cannot detect student format. Found ", nc, " columns.\n",
    "Expected legacy (~202 cols) or new (~270 cols).\n",
    "First 10 columns: ", paste(head(col_names, 10), collapse = ", "),
    call. = FALSE
  )
}


# ---- Internal helpers ----

#' Find the student ID column in raw data
#' @param col_names Character vector of column names.
#' @return Column name or NULL.
#' @keywords internal
.find_student_id_col <- function(col_names) {
  # Try ADECE ID first (present in all formats)
  if ("ADECE ID" %in% col_names) return("ADECE ID")
  # Try fuzzy match
  idx <- grep("adece.*id", col_names, ignore.case = TRUE)
  if (length(idx) > 0) return(col_names[idx[1]])
  NULL
}
