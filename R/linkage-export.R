#' Export Linkage Data to CSV
#'
#' @description Exports linked data to CSV format. For master objects, exports
#'   both classroom-level and student-level files.
#'
#' @param x An `alprek_linkage_classroom`, `alprek_linkage_student`, or
#'   `alprek_linkage_master` object.
#' @param path Character. Output file path. For master objects, this is the
#'   base path — `_classroom` and `_student` suffixes are added automatically.
#'
#' @return Invisible file path(s) of the written file(s).
#'
#' @importFrom utils write.csv
#' @export
linkage_export_csv <- function(x, path) {
  if (inherits(x, "alprek_linkage_master")) {
    base <- sub("\\.csv$", "", path)
    path_cl <- paste0(base, "_classroom.csv")
    path_st <- paste0(base, "_student.csv")
    .ensure_dir(path_cl)
    utils::write.csv(x$classroom_level, path_cl, row.names = FALSE)
    msg_success("Exported classroom-level: {nrow(x$classroom_level)} rows to {path_cl}")
    utils::write.csv(x$student_level, path_st, row.names = FALSE)
    msg_success("Exported student-level: {nrow(x$student_level)} rows to {path_st}")
    return(invisible(c(path_cl, path_st)))
  }

  df <- .extract_linkage_data(x)
  .ensure_dir(path)
  utils::write.csv(df, path, row.names = FALSE)
  msg_success("Exported {nrow(df)} rows to {path}")
  invisible(path)
}


#' Export Linkage Data to Excel
#'
#' @description Exports linked data to Excel format with optional summary.
#'   Requires the `openxlsx` package.
#'
#' @param x An `alprek_linkage_classroom`, `alprek_linkage_student`, or
#'   `alprek_linkage_master` object.
#' @param path Character. Output file path.
#' @param include_summary Logical. Add a summary statistics sheet? Default `TRUE`.
#'
#' @return Invisible file path.
#' @export
linkage_export_excel <- function(x, path, include_summary = TRUE) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("The 'openxlsx' package is required for Excel export. ",
         "Install with: install.packages('openxlsx')", call. = FALSE)
  }

  .ensure_dir(path)
  wb <- openxlsx::createWorkbook()

  if (inherits(x, "alprek_linkage_master")) {
    openxlsx::addWorksheet(wb, "Classroom Level")
    openxlsx::writeData(wb, "Classroom Level", x$classroom_level)
    openxlsx::addWorksheet(wb, "Student Level")
    openxlsx::writeData(wb, "Student Level", x$student_level)
    if (include_summary) {
      summary_df <- linkage_summary_stats(x)
      openxlsx::addWorksheet(wb, "Summary")
      openxlsx::writeData(wb, "Summary", summary_df)
    }
  } else {
    df <- .extract_linkage_data(x)
    openxlsx::addWorksheet(wb, "Linkage Data")
    openxlsx::writeData(wb, "Linkage Data", df)
    if (include_summary) {
      summary_df <- linkage_summary_stats(x)
      openxlsx::addWorksheet(wb, "Summary")
      openxlsx::writeData(wb, "Summary", summary_df)
    }
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  msg_success("Exported to {path}")
  invisible(path)
}


#' Export Linkage Data to RDS
#'
#' @description Exports linked data to R's native serialized format.
#'
#' @param x An `alprek_linkage_classroom`, `alprek_linkage_student`, or
#'   `alprek_linkage_master` object.
#' @param path Character. Output file path.
#' @param compress Logical. Use compression? Default `TRUE`.
#'
#' @return Invisible file path.
#' @export
linkage_export_rds <- function(x, path, compress = TRUE) {
  .ensure_dir(path)
  saveRDS(x, path, compress = compress)
  msg_success("Exported to {path}")
  invisible(path)
}


#' Export Linkage Data to Stata (.dta)
#'
#' @description Exports linked data to Stata format.
#'   Requires the `haven` package. For master objects, exports both levels.
#'
#' @param x An `alprek_linkage_classroom`, `alprek_linkage_student`, or
#'   `alprek_linkage_master` object.
#' @param path Character. Output file path. For master objects, `_classroom`
#'   and `_student` suffixes are added.
#' @param version Integer. Stata file version (default 14).
#'
#' @return Invisible file path(s).
#' @export
linkage_export_stata <- function(x, path, version = 14) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("The 'haven' package is required for Stata export. ",
         "Install with: install.packages('haven')", call. = FALSE)
  }

  .stata_clean_names <- function(df) {
    names(df) <- gsub("[^a-zA-Z0-9_]", "_", names(df))
    names(df) <- gsub("_+", "_", names(df))
    names(df) <- gsub("^_|_$", "", names(df))
    names(df) <- substr(names(df), 1, 32)
    df
  }

  if (inherits(x, "alprek_linkage_master")) {
    base <- sub("\\.dta$", "", path)
    path_cl <- paste0(base, "_classroom.dta")
    path_st <- paste0(base, "_student.dta")
    .ensure_dir(path_cl)

    cl_df <- .stata_clean_names(x$classroom_level)
    haven::write_dta(cl_df, path_cl, version = version)
    msg_success("Exported classroom-level: {nrow(cl_df)} rows to {path_cl}")

    st_df <- .stata_clean_names(x$student_level)
    haven::write_dta(st_df, path_st, version = version)
    msg_success("Exported student-level: {nrow(st_df)} rows to {path_st}")

    return(invisible(c(path_cl, path_st)))
  }

  df <- .extract_linkage_data(x)
  df <- .stata_clean_names(df)
  .ensure_dir(path)
  haven::write_dta(df, path, version = version)
  msg_success("Exported {nrow(df)} rows to {path}")
  invisible(path)
}


#' Export Linkage Data to Parquet
#'
#' @description Exports linked data to Parquet format.
#'   Requires the `arrow` package. For master objects, exports both levels.
#'
#' @param x An `alprek_linkage_classroom`, `alprek_linkage_student`, or
#'   `alprek_linkage_master` object.
#' @param path Character. Output file path. For master objects, `_classroom`
#'   and `_student` suffixes are added.
#' @param compression Character. Compression algorithm. Default `"snappy"`.
#'
#' @return Invisible file path(s).
#' @export
linkage_export_parquet <- function(x, path, compression = "snappy") {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("The 'arrow' package is required for Parquet export. ",
         "Install with: install.packages('arrow')", call. = FALSE)
  }

  if (inherits(x, "alprek_linkage_master")) {
    base <- sub("\\.parquet$", "", path)
    path_cl <- paste0(base, "_classroom.parquet")
    path_st <- paste0(base, "_student.parquet")
    .ensure_dir(path_cl)

    arrow::write_parquet(x$classroom_level, path_cl, compression = compression)
    msg_success("Exported classroom-level: {nrow(x$classroom_level)} rows to {path_cl}")

    arrow::write_parquet(x$student_level, path_st, compression = compression)
    msg_success("Exported student-level: {nrow(x$student_level)} rows to {path_st}")

    return(invisible(c(path_cl, path_st)))
  }

  df <- .extract_linkage_data(x)
  .ensure_dir(path)
  arrow::write_parquet(df, path, compression = compression)
  msg_success("Exported {nrow(df)} rows to {path}")
  invisible(path)
}


# ---- Internal helpers ----

#' Extract data frame from linkage objects
#' @keywords internal
.extract_linkage_data <- function(x) {
  if (inherits(x, "alprek_linkage_classroom")) return(x$data)
  if (inherits(x, "alprek_linkage_student")) return(x$data)
  if (inherits(x, "alprek_linkage_master")) return(x$classroom_level)
  stop("Expected an alprek_linkage object.", call. = FALSE)
}
