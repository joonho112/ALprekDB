#' Clean Melissa-Returned Geocoded Master Data
#'
#' @description Standardizes column names, dtypes, and value formatting of
#'   an `alprek_geocode_raw` object so downstream geocoding steps
#'   (`geocode_validate()`, `geocode_reconcile()`) can operate against a
#'   stable contract. The 11-step pipeline (in order) is:
#'
#'   1. Apply column map (rename if needed per
#'      `alprek_geocode_column_map()`; for v1 most names are unchanged).
#'   2. Coerce Melissa `LAT` / `LNG` from character to numeric. This is
#'      the key transformation. Coercion failures are logged.
#'   3. Defensively coerce ADECE `latitude` / `longitude` to numeric.
#'   4. Coerce `ERRORCODE` from logical to character. (`readxl` parses
#'      all-NA columns as logical — standardize to character so future
#'      deliveries that populate ERRORCODE compose with the same schema.)
#'   5. Keep ZIP-family fields character. `site_zip` is parsed as
#'      numeric by `readxl` from a numeric column; convert to character
#'      to preserve leading zeros if any. `GEOZIP` / `PLUS4` / `DPB` are
#'      already character but trimmed defensively.
#'   6. Standardize `school_year`: trim, validate against canonical set
#'      `{2021-2022, 2022-2023, 2023-2024, 2024-2025, 2025-2026_new}`.
#'      Unknown values are logged with severity `WARN`.
#'   7. Title-case `COUNTYNAME` (Melissa returns ALL-CAPS).
#'      Cross-validate against `alprek_geocode_al_fips_counties()`;
#'      unrecognized AL county names are logged with severity `WARN`.
#'   8. Trim whitespace on all character columns.
#'   9. Defensively coerce `has_latlon` to logical.
#'   10. Attach `data_source_map` attribute that labels each column by
#'       provenance group: id/adece -> "ADECE", melissa_norm/melissa_out
#'       -> "Melissa-<delivery>".
#'   11. Drop lock-file artifact rows if any `~$*.xlsx` rows leaked into
#'       the data (defensive).
#'
#'   `raw_row_index` and `lineage_id` are preserved unchanged.
#'
#' @param raw An `alprek_geocode_raw` object from [geocode_read()].
#' @param config Optional `alprek_geocode_config` (from
#'   [geocode_config()]). If `NULL`, a minimal default is constructed
#'   from `raw$meta`.
#'
#' @return An `alprek_geocode_clean` S3 object (list) with elements:
#'   - `data`: tibble of cleaned data; preserves source columns plus
#'     `raw_row_index` and `lineage_id`. `data_source_map` attribute names
#'     each column by provenance group.
#'   - `cleaning_log`: tibble with columns `rule`, `n_affected`,
#'     `details`, `severity` (one of `INFO`/`WARN`/`ERROR`).
#'   - `meta`: list inheriting key provenance from `raw$meta`
#'     (`file_sha256`, `git_sha`, `source`, `cycle_year`,
#'     `receipt_date`, `path`, `sheet`, `file_basename`) plus
#'     `geocoding_source = "melissa_v1_2026"`, row-index mirrors,
#'     `n_rows`, and `cleaned_at`.
#'
#' @examples
#' \dontrun{
#' raw   <- geocode_read(path = "...", cycle_year = "2026-2027")
#' clean <- geocode_clean(raw)
#' clean
#' }
#'
#' @seealso [geocode_read()], [alprek_geocode_column_map()],
#'   [alprek_geocode_al_fips_counties()].
#'
#' @importFrom tibble tibble as_tibble
#' @export
geocode_clean <- function(raw, config = NULL) {

  if (!inherits(raw, "alprek_geocode_raw")) {
    stop("raw must be an alprek_geocode_raw object (from geocode_read()).",
         call. = FALSE)
  }

  if (!is.null(config) && !inherits(config, "alprek_geocode_config")) {
    stop("config must be NULL or an alprek_geocode_config object ",
         "(from geocode_config()).", call. = FALSE)
  }

  # If no config provided, build a minimal default from raw$meta. We do not
  # call geocode_config() because it requires path existence semantics that
  # are out of scope for clean(); just stash the relevant fields locally.
  if (is.null(config)) {
    config <- list(
      vendor        = raw$meta$source %||% "melissa",
      cycle_year    = raw$meta$cycle_year,
      delivery_date = raw$meta$receipt_date
    )
  }

  data_in <- raw$data
  n_rows_in <- nrow(data_in)
  cleaning_log <- list()

  # canonical school_year set (v0.8.0 contract)
  canonical_school_years <- c("2021-2022", "2022-2023", "2023-2024",
                              "2024-2025", "2025-2026_new")

  # ------------------------------------------------------------------
  # 1. Apply column map (rename if needed)
  # ------------------------------------------------------------------
  cmap <- alprek_geocode_column_map()
  rename_needed <- cmap$raw_col != cmap$std_col
  if (any(rename_needed)) {
    pairs <- cmap[rename_needed, c("raw_col", "std_col")]
    pairs <- pairs[pairs$raw_col %in% names(data_in), , drop = FALSE]
    if (nrow(pairs) > 0L) {
      for (i in seq_len(nrow(pairs))) {
        names(data_in)[names(data_in) == pairs$raw_col[i]] <- pairs$std_col[i]
      }
      cleaning_log[[length(cleaning_log) + 1L]] <- tibble::tibble(
        rule       = "apply_column_map",
        n_affected = nrow(pairs),
        details    = sprintf("Renamed %d cols per v1 column map",
                             nrow(pairs)),
        severity   = "INFO"
      )
    }
  }
  # For v1 most names are unchanged; record a no-op INFO so the log is not
  # silent about step 1.
  if (length(cleaning_log) == 0L ||
      cleaning_log[[length(cleaning_log)]]$rule != "apply_column_map") {
    cleaning_log[[length(cleaning_log) + 1L]] <- tibble::tibble(
      rule       = "apply_column_map",
      n_affected = 0L,
      details    = "v1 column names already canonical; no renames",
      severity   = "INFO"
    )
  }

  # ------------------------------------------------------------------
  # 11 (early). Drop lock-file artifact rows if any leaked in.
  # Done BEFORE coercion to avoid spurious parse-failure counts.
  # The Excel lock-file pattern is `~$*.xlsx`. Such a row would appear
  # if the reader accidentally picked up a lock-file basename in any
  # field (defensive — we have not observed this in v0.8.0 data).
  # ------------------------------------------------------------------
  n_lockfile <- 0L
  if (nrow(data_in) > 0L) {
    char_cols <- vapply(data_in, is.character, logical(1))
    if (any(char_cols)) {
      char_mat <- vapply(
        data_in[char_cols],
        function(col) grepl("^~\\$.*\\.xlsx$", col, ignore.case = TRUE),
        logical(nrow(data_in))
      )
      if (is.matrix(char_mat)) {
        lockfile_rows <- rowSums(char_mat, na.rm = TRUE) > 0L
      } else {
        lockfile_rows <- as.logical(char_mat)
      }
      n_lockfile <- sum(lockfile_rows, na.rm = TRUE)
      if (n_lockfile > 0L) {
        data_in <- data_in[!lockfile_rows, , drop = FALSE]
        cleaning_log[[length(cleaning_log) + 1L]] <- tibble::tibble(
          rule       = "drop_lockfile_rows",
          n_affected = as.integer(n_lockfile),
          details    = sprintf(
            "Dropped %d row(s) matching Excel lock-file pattern (~$*.xlsx)",
            n_lockfile),
          severity   = "WARN"
        )
      }
    }
  }

  # ------------------------------------------------------------------
  # 2. Coerce Melissa LAT / LNG (character -> numeric). Key transform.
  # ------------------------------------------------------------------
  for (col_name in c("LAT", "LNG")) {
    if (col_name %in% names(data_in)) {
      orig <- data_in[[col_name]]
      orig_chr <- trimws(as.character(orig))
      new_val <- suppressWarnings(as.numeric(orig_chr))
      # Count failures: nonblank values that became NA during coercion.
      expected_missing <- is.na(orig) |
        tolower(orig_chr) %in% c("", "na", "n/a", "null", "-")
      n_failed <- sum(!expected_missing & is.na(new_val), na.rm = TRUE)
      data_in[[col_name]] <- new_val
      cleaning_log[[length(cleaning_log) + 1L]] <- tibble::tibble(
        rule       = sprintf("coerce_%s_to_numeric", col_name),
        n_affected = nrow(data_in),
        details    = sprintf(
          "Coerced %s from character to numeric (%d parse failure(s))",
          col_name, n_failed),
        severity   = if (n_failed > 0L) "WARN" else "INFO"
      )
    }
  }

  # ------------------------------------------------------------------
  # 3. Defensively coerce ADECE latitude / longitude to numeric.
  # ------------------------------------------------------------------
  for (col_name in c("latitude", "longitude")) {
    if (col_name %in% names(data_in)) {
      orig <- data_in[[col_name]]
      if (!is.numeric(orig)) {
        new_val <- suppressWarnings(as.numeric(as.character(orig)))
        data_in[[col_name]] <- new_val
        cleaning_log[[length(cleaning_log) + 1L]] <- tibble::tibble(
          rule       = sprintf("coerce_%s_to_numeric", col_name),
          n_affected = nrow(data_in),
          details    = sprintf(
            "Defensively coerced %s to numeric", col_name),
          severity   = "INFO"
        )
      }
    }
  }

  # ------------------------------------------------------------------
  # 4. Coerce ERRORCODE logical -> character (readxl all-NA quirk).
  # ------------------------------------------------------------------
  if ("ERRORCODE" %in% names(data_in)) {
    orig <- data_in[["ERRORCODE"]]
    if (!is.character(orig)) {
      data_in[["ERRORCODE"]] <- as.character(orig)
      cleaning_log[[length(cleaning_log) + 1L]] <- tibble::tibble(
        rule       = "coerce_ERRORCODE_to_character",
        n_affected = nrow(data_in),
        details    = sprintf(
          "Coerced ERRORCODE from %s to character (readxl all-NA quirk)",
          class(orig)[1]),
        severity   = "INFO"
      )
    }
  }

  # ------------------------------------------------------------------
  # 5. Keep ZIP fields character (preserve leading zeros).
  # ------------------------------------------------------------------
  if ("site_zip" %in% names(data_in)) {
    orig <- data_in[["site_zip"]]
    if (!is.character(orig)) {
      # Pad numeric ZIPs to 5 chars to restore any leading zero(s).
      chr <- ifelse(is.na(orig),
                    NA_character_,
                    formatC(orig, width = 5L, format = "d", flag = "0"))
      data_in[["site_zip"]] <- chr
      cleaning_log[[length(cleaning_log) + 1L]] <- tibble::tibble(
        rule       = "site_zip_to_character",
        n_affected = nrow(data_in),
        details    = "Converted site_zip to character (zero-padded to 5)",
        severity   = "INFO"
      )
    }
  }
  # GEOZIP / PLUS4 / DPB: already character. Trim defensively in step 8.
  for (col_name in c("GEOZIP", "PLUS4", "DPB")) {
    if (col_name %in% names(data_in) && !is.character(data_in[[col_name]])) {
      data_in[[col_name]] <- as.character(data_in[[col_name]])
    }
  }

  # ------------------------------------------------------------------
  # 6. Standardize school_year.
  # ------------------------------------------------------------------
  if ("school_year" %in% names(data_in)) {
    orig <- data_in[["school_year"]]
    new_val <- trimws(as.character(orig))
    data_in[["school_year"]] <- new_val
    nonblank <- !is.na(new_val) & nzchar(new_val)
    unknown <- nonblank & !(new_val %in% canonical_school_years)
    n_unknown <- sum(unknown, na.rm = TRUE)
    if (n_unknown > 0L) {
      bad_vals <- unique(new_val[unknown])
      cleaning_log[[length(cleaning_log) + 1L]] <- tibble::tibble(
        rule       = "school_year_unknown",
        n_affected = as.integer(n_unknown),
        details    = sprintf(
          "%d row(s) with school_year not in canonical set; bad values: %s",
          n_unknown,
          paste(sprintf("'%s'", bad_vals), collapse = ", ")),
        severity   = "WARN"
      )
    }
  }

  # ------------------------------------------------------------------
  # 7. Title-case COUNTYNAME + cross-validate against AL FIPS table.
  # ------------------------------------------------------------------
  if ("COUNTYNAME" %in% names(data_in)) {
    orig <- data_in[["COUNTYNAME"]]
    new_val <- trimws(as.character(orig))
    # Title case: lowercase then capitalize first letter of each word.
    new_val <- ifelse(
      is.na(new_val) | !nzchar(new_val),
      new_val,
      vapply(new_val, function(s) {
        if (is.na(s) || !nzchar(s)) return(s)
        parts <- strsplit(tolower(s), " ", fixed = TRUE)[[1]]
        parts <- ifelse(
          nchar(parts) == 0L,
          parts,
          paste0(toupper(substring(parts, 1, 1)), substring(parts, 2))
        )
        paste(parts, collapse = " ")
      }, character(1), USE.NAMES = FALSE)
    )
    n_changed <- sum(!is.na(orig) & orig != new_val, na.rm = TRUE)
    data_in[["COUNTYNAME"]] <- new_val
    cleaning_log[[length(cleaning_log) + 1L]] <- tibble::tibble(
      rule       = "COUNTYNAME_title_case",
      n_affected = as.integer(n_changed),
      details    = sprintf(
        "Title-cased COUNTYNAME (%d row(s) changed from Melissa's ALL-CAPS)",
        n_changed),
      severity   = "INFO"
    )

    # Cross-validate against AL FIPS table.
    al_counties <- tryCatch(alprek_geocode_al_fips_counties(),
                            error = function(e) NULL)
    if (!is.null(al_counties)) {
      canonical <- al_counties$county_name
      nonblank <- !is.na(new_val) & nzchar(new_val)
      bad <- nonblank & !(new_val %in% canonical)
      n_bad <- sum(bad, na.rm = TRUE)
      if (n_bad > 0L) {
        bad_vals <- unique(new_val[bad])
        cleaning_log[[length(cleaning_log) + 1L]] <- tibble::tibble(
          rule       = "COUNTYNAME_not_in_AL_FIPS",
          n_affected = as.integer(n_bad),
          details    = sprintf(
            "%d row(s) with COUNTYNAME not in AL FIPS table; bad values: %s",
            n_bad,
            paste(sprintf("'%s'", utils::head(bad_vals, 10L)),
                  collapse = ", ")),
          severity   = "WARN"
        )
      }
    }
  }

  # ------------------------------------------------------------------
  # 8. Trim whitespace on all character columns.
  # ------------------------------------------------------------------
  char_cols <- names(data_in)[vapply(data_in, is.character, logical(1))]
  n_trim_changes <- 0L
  for (col_name in char_cols) {
    orig <- data_in[[col_name]]
    new_val <- trimws(orig)
    if (!identical(orig, new_val)) {
      n_trim_changes <- n_trim_changes +
        sum(!is.na(orig) & orig != new_val, na.rm = TRUE)
      data_in[[col_name]] <- new_val
    }
  }
  cleaning_log[[length(cleaning_log) + 1L]] <- tibble::tibble(
    rule       = "trim_whitespace",
    n_affected = as.integer(n_trim_changes),
    details    = sprintf(
      "Trimmed whitespace on %d character column(s); %d value(s) changed",
      length(char_cols), n_trim_changes),
    severity   = "INFO"
  )

  # ------------------------------------------------------------------
  # 9. Defensively coerce has_latlon to logical.
  # ------------------------------------------------------------------
  if ("has_latlon" %in% names(data_in)) {
    orig <- data_in[["has_latlon"]]
    if (!is.logical(orig)) {
      new_val <- if (is.numeric(orig)) {
        as.logical(orig)
      } else if (is.character(orig)) {
        tolower(trimws(orig)) %in% c("true", "t", "1", "yes", "y")
      } else {
        as.logical(orig)
      }
      data_in[["has_latlon"]] <- new_val
      cleaning_log[[length(cleaning_log) + 1L]] <- tibble::tibble(
        rule       = "coerce_has_latlon_to_logical",
        n_affected = nrow(data_in),
        details    = sprintf(
          "Defensively coerced has_latlon from %s to logical",
          class(orig)[1]),
        severity   = "INFO"
      )
    }
  }

  # ------------------------------------------------------------------
  # 10. Attach data_source_map attribute (per source group).
  # ------------------------------------------------------------------
  vendor_label <- if (!is.null(config$vendor)) config$vendor else "melissa"
  delivery_label <- if (!is.null(config$delivery_date)) {
    format_one <- function(d) {
      if (inherits(d, "Date")) format(d, "%Y-%m-%d") else as.character(d)
    }
    format_one(config$delivery_date)
  } else if (!is.null(raw$meta$receipt_date)) {
    raw$meta$receipt_date
  } else {
    NA_character_
  }
  melissa_source_label <- if (!is.na(delivery_label) && nzchar(delivery_label)) {
    sprintf("Melissa-%s", delivery_label)
  } else {
    "Melissa"
  }

  group_to_source <- c(
    id           = "ADECE",
    adece        = "ADECE",
    melissa_norm = melissa_source_label,
    melissa_out  = melissa_source_label
  )
  data_source_map <- setNames(
    rep(NA_character_, ncol(data_in)),
    names(data_in)
  )
  for (i in seq_len(nrow(cmap))) {
    std <- cmap$std_col[i]
    grp <- cmap$source_group[i]
    if (std %in% names(data_in) && grp %in% names(group_to_source)) {
      data_source_map[std] <- unname(group_to_source[grp])
    }
  }
  # raw_row_index is added by the reader; mark provenance accordingly.
  if ("raw_row_index" %in% names(data_in)) {
    data_source_map["raw_row_index"] <- "ALprekDB-reader"
  }
  if ("lineage_id" %in% names(data_in)) {
    data_source_map["lineage_id"] <- "ALprekDB-reader"
  }
  attr(data_in, "data_source_map") <- data_source_map
  cleaning_log[[length(cleaning_log) + 1L]] <- tibble::tibble(
    rule       = "attach_data_source_map",
    n_affected = ncol(data_in),
    details    = sprintf(
      "Attached data_source_map attribute (id/adece -> 'ADECE', melissa_* -> '%s')",
      melissa_source_label),
    severity   = "INFO"
  )

  # ------------------------------------------------------------------
  # Assemble output
  # ------------------------------------------------------------------
  cleaning_log_df <- if (length(cleaning_log) > 0L) {
    do.call(rbind, cleaning_log)
  } else {
    tibble::tibble(rule = character(0), n_affected = integer(0),
                   details = character(0), severity = character(0))
  }

  meta_out <- list(
    path             = raw$meta$path,
    sheet            = raw$meta$sheet,
    source           = raw$meta$source,
    cycle_year       = raw$meta$cycle_year,
    receipt_date     = raw$meta$receipt_date,
    file_basename    = raw$meta$file_basename,
    file_sha256      = raw$meta$file_sha256,
    git_sha          = raw$meta$git_sha,
    geocoding_source = "melissa_v1_2026",
    lineage_id       = if ("lineage_id" %in% names(data_in))
                         as.character(data_in$lineage_id)
                       else raw$meta$lineage_id %||% NULL,
    raw_row_index    = if ("raw_row_index" %in% names(data_in))
                         as.integer(data_in$raw_row_index)
                       else raw$meta$raw_row_index %||% NULL,
    n_rows           = nrow(data_in),
    n_rows_in        = n_rows_in,
    n_rows_dropped   = n_rows_in - nrow(data_in),
    cleaned_at       = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )

  out_tbl <- tibble::as_tibble(data_in)
  # tibble::as_tibble may drop attributes; re-attach.
  attr(out_tbl, "data_source_map") <- data_source_map

  structure(list(
    data         = out_tbl,
    cleaning_log = cleaning_log_df,
    meta         = meta_out
  ), class = "alprek_geocode_clean")
}


#' Print method for alprek_geocode_clean
#'
#' @param x An `alprek_geocode_clean` object.
#' @param ... Ignored.
#' @export
print.alprek_geocode_clean <- function(x, ...) {
  cat("<alprek_geocode_clean>\n")
  cat("  Source:           ", x$meta$source, "\n")
  cat("  Geocoding source: ", x$meta$geocoding_source, "\n")
  cat("  File:             ", x$meta$file_basename, "\n")
  cat("  Cycle year:       ", x$meta$cycle_year, "\n")
  cat("  Receipt date:     ", x$meta$receipt_date, "\n")
  cat("  SHA-256:          ", substr(x$meta$file_sha256 %||% "NA", 1, 16),
      "...\n", sep = "")
  cat("  Rows: in=", x$meta$n_rows_in,
      " out=", x$meta$n_rows,
      " dropped=", x$meta$n_rows_dropped, "\n", sep = "")
  cat("  Cols:             ", ncol(x$data), "\n")
  cat("  Cleaning log:     ", nrow(x$cleaning_log), " rule(s)\n", sep = "")
  if (nrow(x$cleaning_log) > 0L) {
    sev_tab <- table(x$cleaning_log$severity)
    cat("                    severity: ",
        paste(sprintf("%s=%d", names(sev_tab), as.integer(sev_tab)),
              collapse = ", "), "\n")
  }
  cat("  Cleaned at:       ", x$meta$cleaned_at, "\n")
  invisible(x)
}


# %||% fallback (rlang re-exports this, but provide a local version to
# avoid an explicit @importFrom for a single operator).
`%||%` <- function(a, b) if (is.null(a)) b else a
