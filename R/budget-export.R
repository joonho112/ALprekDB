#' Export Budget Data to CSV
#'
#' @description Exports processed budget data to CSV format. Supports both
#'   wide (default) and long format output.
#'
#' @param x An `alprek_budget_master` or `alprek_budget_panel` object.
#' @param path Character. Output file path. If `NULL`, auto-generates a name.
#' @param format Character. Output format: `"wide"` (default) or `"long"`.
#'
#' @return Invisible file path of the written file.
#'
#' @importFrom utils write.csv
#' @export
budget_export_csv <- function(x, path = NULL, format = c("wide", "long")) {
  format <- match.arg(format)

  df <- .extract_data(x)
  if (format == "long") {
    df <- budget_to_long(x)
  }

  if (is.null(path)) {
    path <- .auto_path(x, "csv", format)
  }

  .ensure_dir(path)
  utils::write.csv(df, path, row.names = FALSE)
  msg_success("Exported {nrow(df)} rows to {path}")
  invisible(path)
}


#' Export Budget Data to Parquet
#'
#' @description Exports processed budget data to Parquet format for efficient
#'   storage and fast reading. Requires the `arrow` package.
#'
#' @param x An `alprek_budget_master` or `alprek_budget_panel` object.
#' @param path Character. Output file path.
#' @param compression Character. Compression algorithm. Default is `"snappy"`.
#'
#' @return Invisible file path.
#' @export
budget_export_parquet <- function(x, path, compression = "snappy") {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("The 'arrow' package is required for Parquet export. ",
         "Install with: install.packages('arrow')", call. = FALSE)
  }

  df <- .extract_data(x)
  .ensure_dir(path)
  arrow::write_parquet(df, path, compression = compression)
  msg_success("Exported {nrow(df)} rows to {path}")
  invisible(path)
}


#' Export Budget Data to Excel
#'
#' @description Exports processed budget data to Excel format. Requires the
#'   `openxlsx` package.
#'
#' @param x An `alprek_budget_master` or `alprek_budget_panel` object.
#' @param path Character. Output file path.
#' @param include_summary Logical. Add a summary statistics sheet? Default `TRUE`.
#'
#' @return Invisible file path.
#' @export
budget_export_excel <- function(x, path, include_summary = TRUE) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("The 'openxlsx' package is required for Excel export. ",
         "Install with: install.packages('openxlsx')", call. = FALSE)
  }

  df <- .extract_data(x)
  .ensure_dir(path)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Budget Data")
  openxlsx::writeData(wb, "Budget Data", df)

  if (include_summary) {
    summary_df <- budget_summary_stats(x)
    openxlsx::addWorksheet(wb, "Summary")
    openxlsx::writeData(wb, "Summary", summary_df)
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  msg_success("Exported {nrow(df)} rows to {path}")
  invisible(path)
}


#' Export Budget Data to RDS
#'
#' @description Exports processed budget data to R's native serialized format.
#'
#' @param x An `alprek_budget_master` or `alprek_budget_panel` object.
#' @param path Character. Output file path.
#' @param compress Logical. Use compression? Default `TRUE`.
#'
#' @return Invisible file path.
#' @export
budget_export_rds <- function(x, path, compress = TRUE) {
  .ensure_dir(path)
  saveRDS(x, path, compress = compress)
  msg_success("Exported to {path}")
  invisible(path)
}


#' Export Budget Data to Stata (.dta)
#'
#' @description Exports processed budget data to Stata format. Requires the
#'   `haven` package.
#'
#' @param x An `alprek_budget_master` or `alprek_budget_panel` object.
#' @param path Character. Output file path.
#' @param version Integer. Stata file version (default 14).
#'
#' @return Invisible file path.
#' @export
budget_export_stata <- function(x, path, version = 14) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("The 'haven' package is required for Stata export. ",
         "Install with: install.packages('haven')", call. = FALSE)
  }

  df <- .extract_data(x)
  .ensure_dir(path)
  haven::write_dta(df, path, version = version)
  msg_success("Exported {nrow(df)} rows to {path}")
  invisible(path)
}


# ---- Internal helpers ----

#' Extract data frame from master or panel
#' @keywords internal
.extract_data <- function(x) {
  if (inherits(x, "alprek_budget_master")) return(x$data)
  if (inherits(x, "alprek_budget_panel")) return(x$data)
  stop("Expected alprek_budget_master or alprek_budget_panel.", call. = FALSE)
}

#' Generate auto path
#' @keywords internal
.auto_path <- function(x, ext, format = "wide") {
  if (inherits(x, "alprek_budget_panel")) {
    years <- paste(x$years, collapse = "_")
    fname <- paste0("budget_panel_", years, "_", format, ".", ext)
  } else {
    sy <- x$meta$school_year
    fname <- paste0("budget_", sy, "_", format, ".", ext)
  }
  file.path("output", fname)
}

#' Ensure directory exists
#' @keywords internal
.ensure_dir <- function(path) {
  dir <- dirname(path)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}
