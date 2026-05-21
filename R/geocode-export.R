#' Export Geocode Master / Panel / Reconciled Data to CSV
#'
#' @description Writes the `$data` slot of a geocode S3 object to CSV. Mirrors
#'   `applications_export_csv()`. Auto-generates `path` if `NULL`.
#'
#'   **Phase 5 contract.** All Phase 5 exports preserve `lineage_id` (Step 3.1
#'   stable row lineage) and `coord_model_status` (Step 4.3 ordered factor:
#'   `{missing, not_model_ready, provisional_followup, model_ready}`). Rows
#'   with `coord_model_status != "model_ready"` remain visible in the export;
#'   downstream SAE consumers MUST distinguish provisional from model-ready
#'   coordinates. This exporter never silently filters rows.
#'
#' @param x An `alprek_geocode_master`, `alprek_geocode_panel`, or
#'   `alprek_geocode_reconciled` object.
#' @param path Character. Output path. If `NULL`, auto-generates
#'   `output/geocode/geocode_<run_id>.csv` (master/reconciled) or
#'   `output/geocode/geocode_panel_<run_ids>.csv` (panel).
#' @param ... Additional arguments forwarded to [utils::write.csv()].
#'
#' @return Invisible character path of the written file.
#'
#' @examples
#' \dontrun{
#' mst <- geocode_transform(geocode_reconcile(geocode_clean(geocode_read("..."))))
#' geocode_export_csv(mst)
#' geocode_export_csv(mst, "output/custom.csv")
#' }
#'
#' @seealso [geocode_export_parquet()], [geocode_export_excel()],
#'   [geocode_export_rds()], [geocode_export_stata()],
#'   [geocode_export_followup_queue()].
#'
#' @importFrom utils write.csv
#' @export
geocode_export_csv <- function(x, path = NULL, ...) {
  df <- .geocode_extract_data(x)
  if (is.null(path)) {
    path <- .geocode_default_output_path(x, "csv")
  }
  .geocode_ensure_dir(path)
  utils::write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8", ...)
  invisible(path)
}


#' Export Geocode Master / Panel / Reconciled Data to Parquet
#'
#' @description Writes the `$data` slot to Apache Parquet. Requires the
#'   `arrow` package (`Suggests`). Same row preservation contract as
#'   [geocode_export_csv()].
#'
#' @param x An `alprek_geocode_master`, `alprek_geocode_panel`, or
#'   `alprek_geocode_reconciled` object.
#' @param path Character. Output path. If `NULL`, auto-generates
#'   `output/geocode/geocode_<run_id>.parquet`.
#' @param compression Character. Compression algorithm. Default `"snappy"`.
#' @param ... Forwarded to [arrow::write_parquet()].
#'
#' @return Invisible character path.
#'
#' @export
geocode_export_parquet <- function(x, path = NULL,
                                    compression = "snappy", ...) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("The 'arrow' package is required for Parquet export. ",
         "Install with: install.packages('arrow')", call. = FALSE)
  }
  df <- .geocode_extract_data(x)
  if (is.null(path)) {
    path <- .geocode_default_output_path(x, "parquet")
  }
  .geocode_ensure_dir(path)
  arrow::write_parquet(df, path, compression = compression, ...)
  invisible(path)
}


#' Export Geocode Master / Panel / Reconciled Data to Excel
#'
#' @description Writes the `$data` slot to a single `Geocode` worksheet in
#'   an `.xlsx` file. When `include_summary = TRUE` adds a second sheet
#'   `Summary` carrying the `coord_model_status` distribution, the
#'   `lat_source` distribution, and the count of rows flagged
#'   `needs_followup_geocoding`. Requires the `openxlsx` package
#'   (`Suggests`).
#'
#' @param x An `alprek_geocode_master`, `alprek_geocode_panel`, or
#'   `alprek_geocode_reconciled` object.
#' @param path Character. Output path. If `NULL`, auto-generates
#'   `output/geocode/geocode_<run_id>.xlsx`.
#' @param include_summary Logical. Add a summary sheet? Default `FALSE`.
#' @param ... Forwarded to [openxlsx::saveWorkbook()].
#'
#' @return Invisible character path.
#'
#' @export
geocode_export_excel <- function(x, path = NULL,
                                   include_summary = FALSE, ...) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("The 'openxlsx' package is required for Excel export. ",
         "Install with: install.packages('openxlsx')", call. = FALSE)
  }
  if (!is.logical(include_summary) || length(include_summary) != 1L ||
      is.na(include_summary)) {
    stop("include_summary must be a single TRUE/FALSE.", call. = FALSE)
  }
  df <- .geocode_extract_data(x)
  if (is.null(path)) {
    path <- .geocode_default_output_path(x, "xlsx")
  }
  .geocode_ensure_dir(path)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Geocode")
  openxlsx::writeData(wb, "Geocode", .geocode_excel_safe(df))

  if (isTRUE(include_summary)) {
    summary_df <- .geocode_summary_stats(x)
    openxlsx::addWorksheet(wb, "Summary")
    openxlsx::writeData(wb, "Summary", summary_df)
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE, ...)
  invisible(path)
}


#' Export Geocode Object to RDS
#'
#' @description Serializes the full S3 object (data + log + meta) using
#'   `saveRDS()`. Best for re-loading in R; round-trip identical.
#'
#' @param x An `alprek_geocode_master`, `alprek_geocode_panel`, or
#'   `alprek_geocode_reconciled` object.
#' @param path Character. Output path. If `NULL`, auto-generates
#'   `output/geocode/geocode_<run_id>.rds`.
#' @param compress Logical. Compress? Default `TRUE`.
#'
#' @return Invisible character path.
#'
#' @export
geocode_export_rds <- function(x, path = NULL, compress = TRUE) {
  .geocode_assert_supported(x)
  if (is.null(path)) {
    path <- .geocode_default_output_path(x, "rds")
  }
  .geocode_ensure_dir(path)
  saveRDS(x, path, compress = compress)
  invisible(path)
}


#' Export Geocode Master / Panel / Reconciled Data to Stata (.dta)
#'
#' @description Writes the `$data` slot to Stata format via
#'   [haven::write_dta()]. Factor columns are coerced to their character
#'   labels first (Stata's `.dta` format encodes factor labels but loses
#'   `ordered` semantics). Requires the `haven` package (`Suggests`).
#'
#' @param x An `alprek_geocode_master`, `alprek_geocode_panel`, or
#'   `alprek_geocode_reconciled` object.
#' @param path Character. Output path. If `NULL`, auto-generates
#'   `output/geocode/geocode_<run_id>.dta`.
#' @param version Integer. Stata file version (default `14`).
#' @param ... Forwarded to [haven::write_dta()].
#'
#' @return Invisible character path.
#'
#' @export
geocode_export_stata <- function(x, path = NULL, version = 14, ...) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("The 'haven' package is required for Stata export. ",
         "Install with: install.packages('haven')", call. = FALSE)
  }
  df <- .geocode_extract_data(x)
  if (is.null(path)) {
    path <- .geocode_default_output_path(x, "dta")
  }
  .geocode_ensure_dir(path)
  haven::write_dta(.geocode_stata_safe(df), path, version = version, ...)
  invisible(path)
}


# ============================================================================
# Critical follow-up queue exporter (Goal #3 deliverable)
# ============================================================================

#' Export the Geocode Follow-Up Queue as a Production-Ready CSV
#'
#' @description Writes the analyst-facing follow-up queue (sites needing
#'   re-geocoding) to a CSV at a predictable path. This is the user-facing
#'   surface for Goal #3 of v0.8.0: handing operations a named list of sites
#'   to re-investigate after every Melissa delivery.
#'
#'   The function is a thin wrapper around [geocode_followup_queue()] from
#'   `R/geocode-reconcile.R` that:
#'
#'   * Accepts either an `alprek_geocode_reconciled` (preferred) or an
#'     `alprek_geocode_panel` (multi-run; the panel is reduced to its
#'     `$data` and a synthetic reconciled-like shape is used to look up
#'     follow-up flags directly).
#'   * Auto-generates the output path as
#'     `output/geocode/sites_needing_geocoding_<cycle_year>.csv` unless
#'     `path` is supplied.
#'   * Prepends a clearly visible internal-use comment header to the CSV
#'     when `internal_use = TRUE` (the default), per the package privacy
#'     contract. The queue carries full site addresses and is NOT a public
#'     deliverable.
#'   * Returns the in-memory tibble (invisibly) so callers can both write to
#'     disk and inspect the queue in the same expression.
#'
#'   The exported CSV's columns match the Step 4.4 queue exactly (and start
#'   with `lineage_id` for traceability):
#'   `lineage_id, row_id, school_year, site_code, site_name,
#'    site_street, site_city, site_state, site_zip,
#'    lat_source, coord_agreement_band, distance_adece_melissa_m,
#'    melissa_result_code, lat_precision, followup_reason, suggested_action`.
#'
#'   Per the follow-up action and privacy contract, the in-memory return
#'   value also carries the attributes
#'   `privacy_level = "internal_address_followup"` and
#'   `contains_address_fields = TRUE`. When `internal_use = TRUE` the return
#'   value additionally has `attr(., "internal_use") <- TRUE`.
#'
#' @param x An `alprek_geocode_reconciled` or `alprek_geocode_panel`.
#' @param path Character. Output path. If `NULL`, auto-generates
#'   `output/geocode/sites_needing_geocoding_<cycle_year>.csv`.
#'   `<cycle_year>` is taken from `cycle_year` (if supplied) or from
#'   `x$meta$cycle_year`; falls back to `"unknown"` if neither resolves.
#' @param cycle_year Character or `NULL`. Override the auto-path's cycle
#'   token (and the comment header). Default `NULL`.
#' @param include_disputed Logical. Forwarded to [geocode_followup_queue()].
#'   When `TRUE` (default), rows with `lat_source == "disputed_melissa"`
#'   are retained in the queue.
#' @param internal_use Logical. When `TRUE` (default),
#'   prepend an `# INTERNAL USE -- DO NOT REDISTRIBUTE` header to the CSV
#'   and set `attr(out, "internal_use") <- TRUE` on the returned tibble.
#'
#' @return Invisibly returns the in-memory follow-up queue tibble (with
#'   privacy attributes attached). The CSV at `path` is the primary side
#'   effect.
#'
#' @examples
#' \dontrun{
#' raw   <- geocode_read(path = "...", cycle_year = "2026-2027")
#' rec   <- geocode_reconcile(geocode_clean(raw))
#' fq    <- geocode_export_followup_queue(rec)
#' nrow(fq)
#' attr(fq, "privacy_level")
#' }
#'
#' @seealso [geocode_followup_queue()], [geocode_reconcile()],
#'   [geocode_export_csv()].
#'
#' @export
geocode_export_followup_queue <- function(x,
                                            path = NULL,
                                            cycle_year = NULL,
                                            include_disputed = TRUE,
                                            internal_use = TRUE) {

  # ---- 0. Validate inputs --------------------------------------------------
  if (!is.logical(internal_use) || length(internal_use) != 1L ||
      is.na(internal_use)) {
    stop("internal_use must be a single TRUE/FALSE.", call. = FALSE)
  }
  if (!is.logical(include_disputed) || length(include_disputed) != 1L ||
      is.na(include_disputed)) {
    stop("include_disputed must be a single TRUE/FALSE.", call. = FALSE)
  }
  if (!is.null(cycle_year) &&
      (!is.character(cycle_year) || length(cycle_year) != 1L)) {
    stop("cycle_year must be NULL or a single character scalar.",
         call. = FALSE)
  }

  # ---- 1. Resolve a reconciled-like object --------------------------------
  rec <- .geocode_followup_resolve_reconciled(x)

  # ---- 2. Build the queue via the canonical helper ------------------------
  fq <- geocode_followup_queue(rec, include_disputed = include_disputed)

  # ---- 3. Resolve cycle token + auto-path ---------------------------------
  cy <- .geocode_resolve_cycle_year(x, cycle_year)
  if (is.null(path)) {
    base_dir <- file.path("output", "geocode")
    path <- file.path(base_dir, sprintf("sites_needing_geocoding_%s.csv",
                                          if (nzchar(cy)) cy else "unknown"))
  }
  .geocode_ensure_dir(path)

  # ---- 4. Write CSV with optional internal-use header ---------------------
  if (isTRUE(internal_use)) {
    header_lines <- c(
      "# INTERNAL USE -- DO NOT REDISTRIBUTE",
      sprintf("# Generated by geocode_export_followup_queue() on %s",
              format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
      sprintf("# cycle_year=%s ; n_rows=%d ; include_disputed=%s",
              cy, nrow(fq),
              if (isTRUE(include_disputed)) "TRUE" else "FALSE"),
      "# Contains full site addresses (privacy_level = internal_address_followup)"
    )
    con <- file(path, open = "w", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(header_lines, con = con)
    utils::write.csv(fq, con, row.names = FALSE)
  } else {
    utils::write.csv(fq, path, row.names = FALSE, fileEncoding = "UTF-8")
  }

  # ---- 5. Re-attach attributes + return invisibly --------------------------
  out <- fq
  attr(out, "privacy_level") <- "internal_address_followup"
  attr(out, "contains_address_fields") <- TRUE
  if (isTRUE(internal_use)) {
    attr(out, "internal_use") <- TRUE
  }
  attr(out, "output_path") <- path
  invisible(out)
}


# ============================================================================
# Auto-path helpers
# ============================================================================

#' Build an auto-generated output path for geocode exporters
#'
#' @param x The geocode object (master / panel / reconciled).
#' @param format One of `"csv"`, `"parquet"`, `"xlsx"`, `"rds"`, `"dta"`.
#'
#' @return Character file path under `output/geocode/`.
#'
#' @keywords internal
#' @noRd
.geocode_default_output_path <- function(x,
                                          format = c("csv", "parquet",
                                                     "xlsx", "rds", "dta")) {
  format <- match.arg(format)
  base_dir <- file.path("output", "geocode")
  ext <- format

  if (inherits(x, "alprek_geocode_panel")) {
    rids <- x$meta$run_ids %||% character(0)
    rids <- rids[!is.na(rids) & nzchar(rids)]
    rid_token <- if (length(rids) == 0L) "panel"
                 else paste(rids, collapse = "_")
    fname <- sprintf("geocode_panel_%s.%s", rid_token, ext)
  } else if (inherits(x, "alprek_geocode_master")) {
    rid <- as.character(x$meta$geocode_run_id %||% "unknown")
    fname <- sprintf("geocode_%s.%s", rid, ext)
  } else if (inherits(x, "alprek_geocode_reconciled")) {
    cy <- as.character(x$meta$cycle_year %||% "unknown")
    fname <- sprintf("geocode_reconciled_%s.%s", cy, ext)
  } else {
    fname <- sprintf("geocode_unknown.%s", ext)
  }
  file.path(base_dir, fname)
}


# ============================================================================
# Internal helpers
# ============================================================================

#' Pull the `$data` slot from a supported geocode object
#'
#' @keywords internal
#' @noRd
.geocode_extract_data <- function(x) {
  .geocode_assert_supported(x)
  d <- x$data
  if (is.null(d)) {
    stop("Input has no `$data` slot.", call. = FALSE)
  }
  d
}


#' Assert that `x` is one of the supported geocode S3 classes
#'
#' @keywords internal
#' @noRd
.geocode_assert_supported <- function(x) {
  ok <- inherits(x, "alprek_geocode_master") ||
        inherits(x, "alprek_geocode_panel") ||
        inherits(x, "alprek_geocode_reconciled")
  if (!ok) {
    stop("Expected an alprek_geocode_master, alprek_geocode_panel, or ",
         "alprek_geocode_reconciled object. Got class: ",
         paste(class(x), collapse = "/"), call. = FALSE)
  }
  invisible(TRUE)
}


#' Ensure the directory for a given file path exists (recursively)
#'
#' @keywords internal
#' @noRd
.geocode_ensure_dir <- function(path) {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  invisible(NULL)
}


#' Resolve a cycle-year token from either an explicit override or the
#' object's meta. Returns `"unknown"` if neither resolves.
#'
#' @keywords internal
#' @noRd
.geocode_resolve_cycle_year <- function(x, cycle_year = NULL) {
  if (!is.null(cycle_year) && is.character(cycle_year) &&
      length(cycle_year) == 1L && nzchar(cycle_year) &&
      !is.na(cycle_year)) {
    return(as.character(cycle_year))
  }
  cy <- if (inherits(x, "alprek_geocode_panel")) {
    # Multi-run panel: prefer x$meta$cycle_year if present (degenerate
    # single-run case), else collapse vendor run ids.
    x$meta$cycle_year %||%
      paste(x$meta$run_ids %||% character(0), collapse = "_")
  } else {
    x$meta$cycle_year %||% NA_character_
  }
  if (is.null(cy) || is.na(cy) || !nzchar(cy)) "unknown" else as.character(cy)
}


#' Resolve a reconciled-like object from `x` so that
#' [geocode_followup_queue()] can run.
#'
#' For `alprek_geocode_reconciled`: returned as-is.
#'
#' For `alprek_geocode_panel`: a synthetic `alprek_geocode_reconciled`
#' shell is fabricated whose `$data` is `x$data` and whose `$meta` inherits
#' `cycle_year` from the panel meta (panel meta does not carry this
#' canonically; we fall back to `x$meta$cycle_year` if available, else
#' `"unknown"`). This is sufficient because `geocode_followup_queue()`
#' reads only `$data` columns.
#'
#' For `alprek_geocode_master`: a master *is* a transformed reconciled
#' object, so we promote it the same way (synthetic shell on `$data`).
#'
#' @keywords internal
#' @noRd
.geocode_followup_resolve_reconciled <- function(x) {
  if (inherits(x, "alprek_geocode_reconciled")) {
    return(x)
  }
  if (inherits(x, "alprek_geocode_panel") ||
      inherits(x, "alprek_geocode_master")) {
    # Build a synthetic reconciled-shaped object. The queue helper only
    # reads $data fields (needs_followup_geocoding, row_id, etc.) and
    # checks inherits(..., "alprek_geocode_reconciled"); we therefore
    # construct one with the same class label and pass through.
    needs_col <- "needs_followup_geocoding"
    if (!needs_col %in% names(x$data)) {
      stop(sprintf(paste0("Input lacks required column '%s'; ",
                           "did geocode_reconcile() / geocode_transform() ",
                           "run cleanly?"), needs_col),
           call. = FALSE)
    }
    meta_cy <- x$meta$cycle_year %||% NA_character_
    return(structure(list(
      data               = x$data,
      reconciliation_log = tibble::tibble(),
      summary            = tibble::tibble(),
      meta               = list(cycle_year = meta_cy)
    ), class = "alprek_geocode_reconciled"))
  }
  stop("geocode_export_followup_queue() requires an alprek_geocode_reconciled, ",
       "alprek_geocode_panel, or alprek_geocode_master object. Got: ",
       paste(class(x), collapse = "/"), call. = FALSE)
}


#' Coerce factor / ordered factor columns to character for Excel
#'
#' `openxlsx::writeData()` writes ordered factors as their integer codes by
#' default, which is unreadable on inspection. Coerce to character labels
#' so the underlying tier / band / status values are human-readable in the
#' spreadsheet. Numeric and logical columns are left untouched.
#'
#' @keywords internal
#' @noRd
.geocode_excel_safe <- function(df) {
  for (nm in names(df)) {
    if (is.factor(df[[nm]])) {
      df[[nm]] <- as.character(df[[nm]])
    }
  }
  df
}


#' Coerce factor / ordered factor columns to character for Stata
#'
#' Stata's `.dta` format encodes labelled values via `haven::labelled()`,
#' but `haven::write_dta()` will drop `ordered` semantics regardless. To
#' keep round-trip simple and Stata-readable, we coerce factors to character
#' before write. Column-name sanitation is left to `haven::write_dta()`'s
#' built-in checks (caller can still rename if needed).
#'
#' @keywords internal
#' @noRd
.geocode_stata_safe <- function(df) {
  for (nm in names(df)) {
    if (is.factor(df[[nm]])) {
      df[[nm]] <- as.character(df[[nm]])
    }
    if (is.logical(df[[nm]])) {
      # Stata has no true logical; haven coerces to int -- keep but make
      # explicit for clarity.
      df[[nm]] <- as.integer(df[[nm]])
    }
  }
  df
}


#' Build a one-tibble summary of `coord_model_status`, `lat_source`, and
#' `n_followup` for the Excel `Summary` sheet.
#'
#' Long-format with `section`, `value`, and `n` columns.
#'
#' @keywords internal
#' @noRd
.geocode_summary_stats <- function(x) {
  d <- .geocode_extract_data(x)
  parts <- list()

  add_part <- function(section, value, n) {
    parts[[length(parts) + 1L]] <<- tibble::tibble(
      section = as.character(section),
      value   = as.character(value),
      n       = as.integer(n)
    )
  }

  # coord_model_status
  if ("coord_model_status" %in% names(d)) {
    cm <- as.character(d$coord_model_status)
    levs <- c("missing", "not_model_ready",
              "provisional_followup", "model_ready")
    cm_tab <- table(factor(cm, levels = levs))
    for (lv in levs) {
      add_part("coord_model_status", lv, as.integer(cm_tab[lv]))
    }
  }

  # lat_source
  if ("lat_source" %in% names(d)) {
    ls_chr <- as.character(d$lat_source)
    levs <- c("melissa", "adece", "disputed_melissa", "none")
    ls_tab <- table(factor(ls_chr, levels = levs))
    for (lv in levs) {
      add_part("lat_source", lv, as.integer(ls_tab[lv]))
    }
  }

  # n_followup
  if ("needs_followup_geocoding" %in% names(d)) {
    n_fu <- sum(as.logical(d$needs_followup_geocoding), na.rm = TRUE)
    add_part("n_followup", "needs_followup_geocoding", n_fu)
  }

  # Row count (always)
  add_part("n_rows", "total", nrow(d))

  if (length(parts) == 0L) {
    return(tibble::tibble(section = character(0),
                          value   = character(0),
                          n       = integer(0)))
  }
  do.call(rbind, parts)
}


# ---------------------------------------------------------------------------
# %||% fallback (self-contained; parity with R/geocode-transform.R)
# ---------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a
