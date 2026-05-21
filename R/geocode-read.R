#' Read Melissa-Returned Geocoded Master File
#'
#' @description Reads the Melissa-returned geocoded xlsx file (v1 contract,
#'   29 columns) and captures provenance (file SHA-256, git SHA, cycle
#'   year, receipt date, sheet, raw row index, row lineage ID). Does
#'   **not** clean, normalize, or coerce — that is [geocode_clean()]'s
#'   job. In particular, `LAT` / `LNG` are preserved as character per the
#'   Melissa source contract (they are coerced to numeric only by
#'   [geocode_clean()]).
#'
#'   Mirrors the read+provenance pattern established by
#'   [applications_read_renewals()] et al., except that the geocode
#'   module is the **first consumer of row-level geocode lineage** introduced
#'   for v0.8.0. `lineage_id` is stored directly in `$data` as a stable
#'   row key and mirrored in `$meta` for compatibility.
#'
#' @param path Character. Path to the Melissa-returned geocoded xlsx
#'   file (e.g.,
#'   `"ORIGINAL-DATA/2026-03-04_Pre-K Geocoding Melissa/2026-03-04_geocoding_master_Final.xlsx"`).
#'   Required. Existence is checked at call time with an informative
#'   error.
#' @param sheet Character. Worksheet name within the xlsx file. Default
#'   `"Sheet1"` (the v1 Melissa contract).
#' @param cycle_year Character. Cycle year label in `"YYYY-YYYY"`
#'   format (e.g., `"2026-2027"`). Required.
#' @param receipt_date Date or character. Date the geocoded file was
#'   received from Melissa (e.g., `"2026-03-04"` or
#'   `as.Date("2026-03-04")`). Default `Sys.Date()`.
#' @param source Character. Geocoding source label, used in provenance
#'   tracking. Default `"melissa"` (the only supported vendor in
#'   v0.8.0). Reserved for future multi-vendor support.
#' @param verbose Logical. Print progress messages? Default `TRUE`.
#'
#' @return An `alprek_geocode_raw` S3 object (list) with elements:
#'   - `data`: tibble of raw Melissa data (29 columns as-is from the
#'     xlsx, plus `raw_row_index` and `lineage_id` columns for stable
#'     row tracking). LAT/LNG remain character per Melissa source contract.
#'   - `meta`: list with `path`, `sheet`, `cycle_year`, `receipt_date`,
#'     `source`, `file_sha256`, `file_basename`, `git_sha`, `n_rows`,
#'     `n_cols`, `col_names`, `read_at`, `lineage_id` (compatibility
#'     mirror of `$data$lineage_id`), `raw_row_index` (`1:nrow`).
#'
#' @examples
#' \dontrun{
#' raw <- geocode_read(
#'   path = file.path("ORIGINAL-DATA",
#'                    "2026-03-04_Pre-K Geocoding Melissa",
#'                    "2026-03-04_geocoding_master_Final.xlsx"),
#'   cycle_year = "2026-2027",
#'   receipt_date = "2026-03-04"
#' )
#' raw
#' }
#'
#' @importFrom readxl read_excel excel_sheets
#' @importFrom tibble as_tibble
#' @export
geocode_read <- function(path,
                         sheet = "Sheet1",
                         cycle_year,
                         receipt_date = Sys.Date(),
                         source = "melissa",
                         verbose = TRUE) {

  # ---- argument validation ----
  if (missing(path) || is.null(path) || !is.character(path) ||
      length(path) != 1L || !nzchar(path)) {
    stop("path is required (single non-empty character; Melissa xlsx file).",
         call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(sprintf("File not found: %s", path), call. = FALSE)
  }
  if (!is.character(sheet) || length(sheet) != 1L || !nzchar(sheet)) {
    stop("sheet must be a single non-empty character.", call. = FALSE)
  }
  if (missing(cycle_year) || is.null(cycle_year) ||
      !is.character(cycle_year) || length(cycle_year) != 1L ||
      !grepl("^\\d{4}-\\d{4}$", cycle_year)) {
    stop("cycle_year is required in 'YYYY-YYYY' format (e.g., '2026-2027').",
         call. = FALSE)
  }
  if (!is.character(source) || length(source) != 1L || !nzchar(source)) {
    stop("source must be a single non-empty character (e.g., 'melissa').",
         call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("verbose must be a single logical (TRUE/FALSE).", call. = FALSE)
  }

  # ---- verify sheet exists ----
  available_sheets <- readxl::excel_sheets(path)
  if (!sheet %in% available_sheets) {
    stop(sprintf(
      "Sheet '%s' not found in %s. Available sheets: %s",
      sheet, basename(path),
      paste(sprintf("'%s'", available_sheets), collapse = ", ")
    ), call. = FALSE)
  }

  if (isTRUE(verbose)) {
    message(sprintf("[geocode_read] Reading '%s' (sheet: '%s')...",
                    basename(path), sheet))
  }

  # ---- read xlsx with default dtype guessing ----
  # NOTE: default guessing preserves LAT/LNG as character per Melissa
  # contract because they may contain leading zeros / mixed precision.
  df_raw <- suppressMessages(
    readxl::read_excel(path, sheet = sheet, guess_max = 10000,
                       col_names = TRUE, .name_repair = "minimal")
  )

  # ---- provenance ----
  source_col_names <- names(df_raw)
  file_sha256 <- alprek_file_hash(path)
  df_raw$raw_row_index <- seq_len(nrow(df_raw))

  lineage_id <- .alprek_lineage_id(
    file_sha256   = file_sha256,
    sheet         = sheet,
    raw_row_index = df_raw$raw_row_index,
    cycle_year    = cycle_year
  )
  df_raw$lineage_id <- lineage_id

  receipt_date_chr <- if (inherits(receipt_date, "Date")) {
    format(receipt_date, "%Y-%m-%d")
  } else {
    as.character(receipt_date)
  }

  meta <- list(
    path          = path,
    sheet         = sheet,
    cycle_year    = cycle_year,
    receipt_date  = receipt_date_chr,
    source        = source,
    file_sha256   = file_sha256,
    file_basename = basename(path),
    git_sha       = alprek_git_sha(),
    n_rows        = nrow(df_raw),
    n_cols        = length(source_col_names),
    col_names     = source_col_names,
    read_at       = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    lineage_id    = lineage_id,
    raw_row_index = df_raw$raw_row_index
  )

  if (isTRUE(verbose)) {
    message(sprintf("[geocode_read] Read %d rows x %d cols.",
                    meta$n_rows, meta$n_cols))
  }

  structure(list(data = tibble::as_tibble(df_raw), meta = meta),
            class = "alprek_geocode_raw")
}


#' Print method for alprek_geocode_raw
#'
#' @param x An `alprek_geocode_raw` object.
#' @param ... Ignored.
#' @export
print.alprek_geocode_raw <- function(x, ...) {
  cat("<alprek_geocode_raw>\n")
  cat("  Source:      ", x$meta$source, "\n")
  cat("  File:        ", x$meta$file_basename, "\n")
  cat("  Sheet:       ", x$meta$sheet, "\n")
  cat("  Cycle year:  ", x$meta$cycle_year, "\n")
  cat("  Receipt:     ", x$meta$receipt_date, "\n")
  cat("  SHA-256:     ", substr(x$meta$file_sha256, 1, 16), "...\n")
  cat("  Rows x Cols: ", x$meta$n_rows, " x ", x$meta$n_cols, "\n",
      sep = "")
  cat("  Read at:     ", x$meta$read_at, "\n")
  invisible(x)
}
