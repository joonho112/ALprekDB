#' Read Pre-K Budget Data
#'
#' @description Reads a Pre-K budget Excel file, normalizes column names,
#'   drops footer rows, detects format, and returns an `alprek_budget_raw`
#'   S3 object. Does NOT reshape or aggregate -- that is [budget_clean()]'s job.
#'
#' @param path Character. Path to the Excel file.
#' @param school_year Character. School year in `"YYYY-YYYY"` format. If `NULL`,
#'   attempts to infer from the filename.
#' @param sheet Character or numeric. Sheet name or number. If `NULL`, reads
#'   the first sheet.
#' @param remove_footer Logical. If `TRUE` (default), removes summary/footer
#'   rows (e.g., rows where Classroom Code is NA or Classroom Name starts
#'   with "Count:").
#'
#' @return An `alprek_budget_raw` S3 object (list) with elements:
#'   - `data`: tibble of raw budget data with cleaned column names.
#'   - `meta`: list of metadata (path, school_year, year, format, etc.).
#'
#' @examples
#' \dontrun{
#' raw <- budget_read("rptClassBudgets 2021-2022.xlsx", school_year = "2021-2022")
#' raw <- budget_read("24-25 FCPK Budgets.xlsx") # auto-infers school year
#' }
#'
#' @importFrom readxl read_excel excel_sheets
#' @importFrom dplyr filter
#' @importFrom stringr str_detect str_squish
#' @export
budget_read <- function(path,
                        school_year = NULL,
                        sheet = NULL,
                        remove_footer = TRUE) {

  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }

  # Determine sheet
  sheets <- readxl::excel_sheets(path)
  if (is.null(sheet)) {
    sheet <- sheets[1]
  }
  msg_info("Reading sheet: '{sheet}' from {basename(path)}")

  # Read raw
  df_raw <- readxl::read_excel(path, sheet = sheet, guess_max = 10000)
  n_rows_raw <- nrow(df_raw)

  # Clean column names (squish whitespace)
  names(df_raw) <- alprek_clean_colnames(names(df_raw))

  # Detect format
  format_type <- budget_detect_format(df_raw)
  msg_info("Detected format: {format_type}")

  # Infer school year if not provided
  if (is.null(school_year)) {
    school_year <- alprek_infer_school_year(path)
    if (is.na(school_year)) {
      # Try from column names (Budget Version column)
      bv_col <- grep("^Budget Version", names(df_raw), value = TRUE)
      if (length(bv_col) > 0) {
        school_year <- .extract_year_from_bv(bv_col[1])
      }
    }
    if (is.na(school_year)) {
      stop("Could not detect school_year. Please provide it explicitly.", call. = FALSE)
    }
    msg_info("Inferred school year: {school_year}")
  }

  year <- alprek_school_year_to_start(school_year)

  # Add school_year and year columns to data
  df_raw$school_year <- school_year
  df_raw$year <- year

  # Remove footer rows
  n_rows_clean <- n_rows_raw
  if (remove_footer && "Classroom Code" %in% names(df_raw)) {
    df_raw <- df_raw |>
      dplyr::filter(!is.na(.data[["Classroom Code"]])) |>
      dplyr::filter(
        !stringr::str_detect(
          as.character(.data[["Classroom Name"]]),
          "^Count:"
        )
      )
    n_rows_clean <- nrow(df_raw)
    if (n_rows_raw != n_rows_clean) {
      msg_info("Removed {n_rows_raw - n_rows_clean} footer/summary row(s)")
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
        col_names = names(df_raw),
        read_at = Sys.time()
      )
    ),
    class = "alprek_budget_raw"
  )

  msg_success("Read {n_rows_clean} classrooms for {school_year} ({format_type} format)")
  result
}


#' Print method for alprek_budget_raw
#' @param x An alprek_budget_raw object.
#' @param ... Ignored.
#' @export
print.alprek_budget_raw <- function(x, ...) {
  cat("<alprek_budget_raw>\n")
  cat("  School year:", x$meta$school_year, "\n")
  cat("  Format:", x$meta$format, "\n")
  cat("  Classrooms:", x$meta$n_rows_clean, "\n")
  cat("  Columns:", length(x$meta$col_names), "\n")
  cat("  Source:", basename(x$meta$path), "\n")
  invisible(x)
}


# ---- Internal helpers ----

#' Extract school year from Budget Version column name
#' @param bv_col Character. Budget Version column name.
#' @return Character school year or NA.
#' @keywords internal
.extract_year_from_bv <- function(bv_col) {
  # Match "2021-2022" or "2024-2025" in column name

  m <- regmatches(bv_col, regexpr("(20\\d{2})-(20\\d{2})", bv_col))
  if (length(m) == 1 && nchar(m) > 0) return(m)
  NA_character_
}
