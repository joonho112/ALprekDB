#' Read Pre-K Classroom Detail Data
#'
#' @description Reads an FCPK Classroom Details Excel file, normalizes column
#'   names, drops footer rows, detects format (legacy vs new), and returns an
#'   `alprek_classroom_raw` S3 object.
#'
#' @param path Character. Path to the Excel file.
#' @param school_year Character. School year in `"YYYY-YYYY"` format. If `NULL`,
#'   attempts to infer from the filename.
#' @param sheet Character or numeric. Sheet name or number. Defaults to
#'   `"rptRIF"` which is the standard sheet name for classroom detail files.
#' @param remove_footer Logical. If `TRUE` (default), removes summary/footer
#'   rows (rows where Classroom Code is NA).
#'
#' @return An `alprek_classroom_raw` S3 object (list) with elements:
#'   - `data`: tibble of raw classroom data with original column names
#'     (whitespace-squeezed but not renamed).
#'   - `meta`: list of metadata (path, school_year, year, format, etc.).
#'
#' @examples
#' \dontrun{
#' raw <- classroom_read("FCPK Classroom Details 21-22.xlsx")
#' raw <- classroom_read("24-25 Classroom Details.xlsx", school_year = "2024-2025")
#' }
#'
#' @importFrom readxl read_excel excel_sheets
#' @importFrom dplyr filter
#' @importFrom stringr str_squish
#' @export
classroom_read <- function(path,
                           school_year = NULL,
                           sheet = "rptRIF",
                           remove_footer = TRUE) {

  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }

  # Verify sheet exists

  sheets <- readxl::excel_sheets(path)
  if (is.character(sheet) && !(sheet %in% sheets)) {
    # Fall back to first sheet with a warning
    msg_warn("Sheet '{sheet}' not found. Available: {paste(sheets, collapse = ', ')}. Using first sheet.")
    sheet <- sheets[1]
  }

  msg_info("Reading sheet: '{sheet}' from {basename(path)}")

  # Read raw — all as text to avoid type coercion issues
  df_raw <- readxl::read_excel(path, sheet = sheet, guess_max = 10000)
  n_rows_raw <- nrow(df_raw)
  n_cols_raw <- ncol(df_raw)

  # Clean column names (squish whitespace, but keep original names)
  names(df_raw) <- alprek_clean_colnames(names(df_raw))

  # Detect format
  format_type <- classroom_detect_format(df_raw)
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
    # Find the classroom code column (first column in all formats)
    code_col <- .find_classroom_code_col(names(df_raw))
    if (!is.null(code_col)) {
      df_raw <- df_raw[!is.na(df_raw[[code_col]]), , drop = FALSE]
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
    class = "alprek_classroom_raw"
  )

  msg_success("Read {n_rows_clean} classrooms for {school_year} ({format_type} format, {n_cols_raw} cols)")
  result
}


#' Print method for alprek_classroom_raw
#' @param x An alprek_classroom_raw object.
#' @param ... Ignored.
#' @export
print.alprek_classroom_raw <- function(x, ...) {
  cat("<alprek_classroom_raw>\n")
  cat("  School year:", x$meta$school_year, "\n")
  cat("  Format:", x$meta$format, "\n")
  cat("  Classrooms:", x$meta$n_rows_clean, "\n")
  cat("  Columns:", x$meta$n_cols, "\n")
  cat("  Source:", basename(x$meta$path), "\n")
  invisible(x)
}


#' Detect Classroom File Format
#'
#' @description Determines whether a classroom data frame uses the legacy
#'   (2021-2024, ~100 columns) or new (2024-2025, ~125 columns) format.
#'
#' @param df A data frame (raw, from Excel import with cleaned column names).
#' @return Character string: `"legacy"` or `"new"`.
#'
#' @details
#' Detection logic:
#' - **New**: Contains "Fund Source" or "Classroom Code Formula" or ncol >= 120.
#' - **Legacy**: ncol <= 105 OR contains "Aux. Teacher Ethnicity" (with period).
#'
#' @examples
#' \dontrun{
#' df <- readxl::read_excel("FCPK Classroom Details 21-22.xlsx", sheet = "rptRIF")
#' classroom_detect_format(df)
#' # "legacy"
#' }
#'
#' @export
classroom_detect_format <- function(df) {
  col_names <- names(df)
  nc <- ncol(df)

  # New format markers (2024-2025+)
  has_fund_source <- any(grepl("Fund Source", col_names, ignore.case = TRUE))
  has_formula <- any(grepl("Classroom Code Formula", col_names, ignore.case = TRUE))

  if (has_fund_source || has_formula || nc >= 120) {
    return("new")
  }

  # Legacy: <= 105 cols or has "Aux. Teacher Ethnicity" (period in name)
  has_aux_period <- any(grepl("Aux\\. Teacher Ethnicity", col_names))
  if (nc <= 105 || has_aux_period) {
    return("legacy")
  }

  stop(
    "Cannot detect classroom format. Found ", nc, " columns.\n",
    "Expected legacy (~100 cols) or new (~125 cols).\n",
    "First 10 columns: ", paste(head(col_names, 10), collapse = ", "),
    call. = FALSE
  )
}


# ---- Internal helpers ----

#' Find the classroom code column in raw data
#' @param col_names Character vector of column names.
#' @return Column name or NULL.
#' @keywords internal
.find_classroom_code_col <- function(col_names) {
  # Try exact match first
  candidates <- c("Classroom Code", "ClassroomCode")
  for (cand in candidates) {
    if (cand %in% col_names) return(cand)
  }
  # Try fuzzy match
  idx <- grep("classroom.*code", col_names, ignore.case = TRUE)
  if (length(idx) > 0) return(col_names[idx[1]])
  NULL
}
