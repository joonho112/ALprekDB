#' Export Classroom Data to CSV
#'
#' @description Exports processed classroom data to CSV format.
#'
#' @param x An `alprek_classroom_clean` or `alprek_classroom_panel` object.
#' @param path Character. Output file path.
#'
#' @return Invisible file path of the written file.
#'
#' @importFrom utils write.csv
#' @export
classroom_export_csv <- function(x, path) {
  df <- .extract_classroom_data(x)
  .ensure_dir(path)
  utils::write.csv(df, path, row.names = FALSE)
  msg_success("Exported {nrow(df)} rows to {path}")
  invisible(path)
}


#' Export Classroom Data to Excel
#'
#' @description Exports processed classroom data to Excel format.
#'   Requires the `openxlsx` package.
#'
#' @param x An `alprek_classroom_clean` or `alprek_classroom_panel` object.
#' @param path Character. Output file path.
#' @param include_summary Logical. Add a summary statistics sheet? Default `TRUE`.
#'
#' @return Invisible file path.
#' @export
classroom_export_excel <- function(x, path, include_summary = TRUE) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("The 'openxlsx' package is required for Excel export. ",
         "Install with: install.packages('openxlsx')", call. = FALSE)
  }

  df <- .extract_classroom_data(x)
  .ensure_dir(path)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Classroom Data")
  openxlsx::writeData(wb, "Classroom Data", df)

  if (include_summary) {
    summary_df <- classroom_summary_stats(x)
    openxlsx::addWorksheet(wb, "Summary")
    openxlsx::writeData(wb, "Summary", summary_df)
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  msg_success("Exported {nrow(df)} rows to {path}")
  invisible(path)
}


#' Export Classroom Data to RDS
#'
#' @description Exports processed classroom data to R's native serialized format.
#'
#' @param x An `alprek_classroom_clean` or `alprek_classroom_panel` object.
#' @param path Character. Output file path.
#' @param compress Logical. Use compression? Default `TRUE`.
#'
#' @return Invisible file path.
#' @export
classroom_export_rds <- function(x, path, compress = TRUE) {
  .ensure_dir(path)
  saveRDS(x, path, compress = compress)
  msg_success("Exported to {path}")
  invisible(path)
}


#' Export Classroom Data to Stata (.dta)
#'
#' @description Exports processed classroom data to Stata format.
#'   Requires the `haven` package.
#'
#' @param x An `alprek_classroom_clean` or `alprek_classroom_panel` object.
#' @param path Character. Output file path.
#' @param version Integer. Stata file version (default 14).
#'
#' @return Invisible file path.
#' @export
classroom_export_stata <- function(x, path, version = 14) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("The 'haven' package is required for Stata export. ",
         "Install with: install.packages('haven')", call. = FALSE)
  }

  df <- .extract_classroom_data(x)
  # Stata requires valid variable names (no spaces, special chars)
  names(df) <- gsub("[^a-zA-Z0-9_]", "_", names(df))
  names(df) <- gsub("_+", "_", names(df))
  names(df) <- gsub("^_|_$", "", names(df))
  # Truncate to 32 characters (Stata limit)
  names(df) <- substr(names(df), 1, 32)
  .ensure_dir(path)
  haven::write_dta(df, path, version = version)
  msg_success("Exported {nrow(df)} rows to {path}")
  invisible(path)
}


#' Export Classroom Data to Parquet
#'
#' @description Exports processed classroom data to Parquet format.
#'   Requires the `arrow` package.
#'
#' @param x An `alprek_classroom_clean` or `alprek_classroom_panel` object.
#' @param path Character. Output file path.
#' @param compression Character. Compression algorithm. Default `"snappy"`.
#'
#' @return Invisible file path.
#' @export
classroom_export_parquet <- function(x, path, compression = "snappy") {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("The 'arrow' package is required for Parquet export. ",
         "Install with: install.packages('arrow')", call. = FALSE)
  }

  df <- .extract_classroom_data(x)
  .ensure_dir(path)
  arrow::write_parquet(df, path, compression = compression)
  msg_success("Exported {nrow(df)} rows to {path}")
  invisible(path)
}


# ---- Internal helpers ----

#' Extract data frame from classroom clean or panel objects
#' @keywords internal
.extract_classroom_data <- function(x) {
  if (inherits(x, "alprek_classroom_clean")) return(x$data)
  if (inherits(x, "alprek_classroom_panel")) return(x$data)
  stop("Expected alprek_classroom_clean or alprek_classroom_panel.", call. = FALSE)
}
