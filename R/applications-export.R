#' Export Applications Data to CSV
#'
#' @description Exports an `alprek_applications_master` or
#'   `alprek_applications_panel` to CSV. Because the master/panel has two
#'   grains (applications + capacity), use the `grain` argument to choose
#'   which one to write. Mirrors `budget_export_csv()`.
#'
#' @param x An `alprek_applications_master` or `alprek_applications_panel`.
#' @param path Character. Output path. If `NULL`, auto-generates.
#' @param grain Character. `"apps"` (default) or `"capacity"`.
#'
#' @return Invisible file path of the written file.
#'
#' @importFrom utils write.csv
#' @export
applications_export_csv <- function(x, path = NULL, grain = c("apps", "capacity")) {
  grain <- match.arg(grain)
  df <- .ap_extract_data(x, grain)
  if (is.null(path)) {
    path <- .ap_auto_path(x, "csv", grain)
  }
  .ap_ensure_dir(path)
  utils::write.csv(df, path, row.names = FALSE)
  invisible(path)
}


#' Export Applications Data to Parquet
#'
#' @description Requires `arrow`. Mirrors `budget_export_parquet()`.
#'
#' @param x An `alprek_applications_master` or `alprek_applications_panel`.
#' @param path Character. Output path. If `NULL`, auto-generates.
#' @param compression Character. Default `"snappy"`.
#' @param grain Character. `"apps"` (default) or `"capacity"`.
#' @return Invisible file path.
#' @export
applications_export_parquet <- function(x, path = NULL, compression = "snappy",
                                          grain = c("apps", "capacity")) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("The 'arrow' package is required for Parquet export. ",
         "Install with: install.packages('arrow')", call. = FALSE)
  }
  grain <- match.arg(grain)
  df <- .ap_extract_data(x, grain)
  if (is.null(path)) {
    path <- .ap_auto_path(x, "parquet", grain)
  }
  .ap_ensure_dir(path)
  arrow::write_parquet(df, path, compression = compression)
  invisible(path)
}


#' Export Applications Data to Excel
#'
#' @description Writes both grains (applications + capacity, if present) as
#'   separate sheets, plus an optional `Summary` sheet. Requires `openxlsx`.
#'
#' @param x An `alprek_applications_master` or `alprek_applications_panel`.
#' @param path Character. Output path. If `NULL`, auto-generates.
#' @param include_summary Logical. Add a per-cycle / per-bucket summary
#'   sheet? Default `TRUE`.
#' @return Invisible file path.
#' @export
applications_export_excel <- function(x, path = NULL, include_summary = TRUE) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("The 'openxlsx' package is required for Excel export. ",
         "Install with: install.packages('openxlsx')", call. = FALSE)
  }
  if (is.null(path)) {
    path <- .ap_auto_path(x, "xlsx", "workbook")
  }
  .ap_ensure_dir(path)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Applications")
  openxlsx::writeData(wb, "Applications", .ap_extract_data(x, "apps"))

  cap <- .ap_extract_data(x, "capacity", silent = TRUE)
  if (!is.null(cap) && nrow(cap) > 0L) {
    openxlsx::addWorksheet(wb, "Capacity")
    openxlsx::writeData(wb, "Capacity", cap)
  }

  if (isTRUE(include_summary)) {
    summary_df <- .ap_summary_stats(x)
    openxlsx::addWorksheet(wb, "Summary")
    openxlsx::writeData(wb, "Summary", summary_df)
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}


#' Export Applications Object to RDS
#'
#' @description Serializes the full S3 object (both grains + meta + log).
#'   Best for re-loading in R; round-trip identical.
#'
#' @param x An `alprek_applications_master` or `alprek_applications_panel`.
#' @param path Character. Output path. If `NULL`, auto-generates.
#' @param compress Logical. Compress? Default `TRUE`.
#' @return Invisible file path.
#' @export
applications_export_rds <- function(x, path = NULL, compress = TRUE) {
  if (is.null(path)) {
    path <- .ap_auto_path(x, "rds", "object")
  }
  .ap_ensure_dir(path)
  saveRDS(x, path, compress = compress)
  invisible(path)
}


#' Export Applications Data to Stata (.dta)
#'
#' @description Writes the chosen grain to Stata format. Stata's column-name
#'   limit (32 chars) and reserved-keyword constraints may cause column
#'   renames; the caller is responsible for naming if necessary.
#'   Requires `haven`.
#'
#' @param x An `alprek_applications_master` or `alprek_applications_panel`.
#' @param path Character. Output path. If `NULL`, auto-generates.
#' @param version Integer. Stata file version (default `14`).
#' @param grain Character. `"apps"` (default) or `"capacity"`.
#' @return Invisible file path.
#' @export
applications_export_stata <- function(x, path = NULL, version = 14,
                                        grain = c("apps", "capacity")) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("The 'haven' package is required for Stata export. ",
         "Install with: install.packages('haven')", call. = FALSE)
  }
  grain <- match.arg(grain)
  df <- .ap_extract_data(x, grain)
  if (is.null(path)) {
    path <- .ap_auto_path(x, "dta", grain)
  }
  .ap_ensure_dir(path)
  haven::write_dta(df, path, version = version)
  invisible(path)
}


# ============================================================================
# Internal helpers
# ============================================================================

#' @keywords internal
#' @noRd
.ap_extract_data <- function(x, grain = c("apps", "capacity"), silent = FALSE) {
  grain <- match.arg(grain)
  is_master <- inherits(x, "alprek_applications_master")
  is_panel  <- inherits(x, "alprek_applications_panel")
  if (!is_master && !is_panel) {
    stop("Expected alprek_applications_master or alprek_applications_panel.",
         call. = FALSE)
  }
  if (grain == "apps") return(x$data)
  out <- x$capacity_data
  if (is.null(out)) {
    if (isTRUE(silent)) return(NULL)
    stop("No capacity_data slot in input. Pass `grain = \"apps\"` instead.",
         call. = FALSE)
  }
  out
}

#' @keywords internal
#' @noRd
.ap_auto_path <- function(x, ext, grain) {
  base_dir <- "output"
  if (inherits(x, "alprek_applications_panel")) {
    yrs <- paste(x$cycle_years, collapse = "_")
    fname <- sprintf("applications_panel_%s_%s.%s", yrs, grain, ext)
  } else {
    cy <- x$meta$cycle_year %||% "unknown"
    fname <- sprintf("applications_%s_%s.%s", cy, grain, ext)
  }
  file.path(base_dir, fname)
}

#' @keywords internal
#' @noRd
.ap_ensure_dir <- function(path) {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  invisible(NULL)
}

#' @keywords internal
#' @noRd
.ap_summary_stats <- function(x) {
  d <- .ap_extract_data(x, "apps")
  cap <- .ap_extract_data(x, "capacity", silent = TRUE)

  grp_cols <- intersect(c("cycle_year", "bucket"), names(d))
  if (length(grp_cols) == 0L) {
    return(tibble::tibble(metric = "n_rows", value = nrow(d)))
  }
  apps_grp <- d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) |>
    dplyr::summarise(
      n_apps = dplyr::n(),
      mean_total_request = mean(.data$total_funding_request, na.rm = TRUE),
      .groups = "drop"
    )

  cap_grp <- if (!is.null(cap)) {
    cap_grp_cols <- intersect("cycle_year", names(cap))
    if (length(cap_grp_cols) == 0L) {
      tibble::tibble(metric = "n_capacity_sites",
                       value = if (is.null(cap)) 0L else nrow(cap))
    } else {
      cap |>
        dplyr::group_by(dplyr::across(dplyr::all_of(cap_grp_cols))) |>
        dplyr::summarise(
          n_sites = dplyr::n(),
          mean_capacity_utilization = mean(.data$capacity_utilization,
                                              na.rm = TRUE),
          n_oversubscribed = sum(.data$is_oversubscribed, na.rm = TRUE),
          .groups = "drop"
        )
    }
  } else NULL

  out <- list(
    applications_by_cycle_bucket = apps_grp
  )
  if (!is.null(cap_grp)) out$capacity_by_cycle <- cap_grp

  # Excel can only accept one tibble per sheet, so collapse to a long header
  parts <- list()
  for (nm in names(out)) {
    df <- out[[nm]]
    df$section <- nm
    df <- df[, c("section", setdiff(names(df), "section")), drop = FALSE]
    parts[[length(parts) + 1L]] <- df
  }
  dplyr::bind_rows(parts)
}
