#' Detect Melissa Geocode File Format
#'
#' @description Inspects the column names of a Melissa-returned
#'   geocoded delivery and decides whether it matches the v1 contract
#'   (`format = "melissa_v1_2026"`) or is unrecognized
#'   (`format = "unknown"`). Reports a confidence score in `[0, 1]` and
#'   the set of input columns that are not in the v1 contract.
#'
#'   Used by `geocode_read()` (as an upstream gate) and by
#'   `geocode_compare_deliveries()` (to label each delivery's format
#'   before diffing). Mirrors the role of `applications_detect_format()`
#'   and `budget_detect_format()` in their respective modules.
#'
#'   Detection logic:
#'   * Marker columns (must all be present): `row_id`, `LAT`, `LNG`,
#'     `RESULTCODE`. Absence of any marker → `format = "unknown"`,
#'     `confidence = 0`.
#'   * Exact match against the 29 v1 columns (case-sensitive) →
#'     `confidence = 1`.
#'   * Markers present but some v1 columns missing or extra →
#'     `confidence` interpolated by Jaccard similarity:
#'     `|input ∩ v1| / |input ∪ v1|`.
#'   * Threshold: `confidence >= 0.5` AND all markers present →
#'     `format = "melissa_v1_2026"`. Otherwise `"unknown"`.
#'
#' @param x One of:
#'   * an `alprek_geocode_raw` object (output of `geocode_read()`) —
#'     column names are read from `x$meta$col_names`,
#'   * a character vector of column names, OR
#'   * a single character file path to an xlsx file — the first sheet's
#'     header row is read and used as the column names.
#' @param sheet Character. Sheet name to read when `x` is a path. Default
#'   `"Sheet1"` (the v1 contract).
#'
#' @return A `list` with class `"alprek_geocode_format_detection"` and
#'   fields:
#'   * `format` — character scalar, one of `"melissa_v1_2026"`,
#'     `"unknown"`.
#'   * `confidence` — numeric in `[0, 1]`. `1` = exact match; `0` =
#'     marker columns absent; otherwise Jaccard similarity between
#'     input cols and v1 cols.
#'   * `unknown_columns` — character vector of columns present in the
#'     input that are NOT in the v1 contract (`character(0)` when
#'     none).
#'   * `missing_v1_columns` — character vector of v1 contract columns
#'     absent from the input (`character(0)` when complete).
#'   * `markers_found` — character vector of marker columns observed
#'     (subset of `c("row_id", "LAT", "LNG", "RESULTCODE")`).
#'   * `n_input_cols` — integer count of input columns.
#'
#' @examples
#' \dontrun{
#' raw <- geocode_read(path, cycle_year = "2026-2027")
#' geocode_detect_format(raw)
#' # $format = "melissa_v1_2026", $confidence = 1
#'
#' # From a character vector of column names
#' v1 <- alprek_geocode_column_map()$raw_col
#' geocode_detect_format(v1)
#'
#' # From a file path (reads header row of Sheet1)
#' geocode_detect_format("ORIGINAL-DATA/2026-03-04_geocoding_master_Final.xlsx")
#' }
#'
#' @seealso [geocode_read()], [geocode_compare_deliveries()],
#'   [alprek_geocode_column_map()].
#'
#' @export
geocode_detect_format <- function(x, sheet = "Sheet1") {

  # ---- extract column names from x ----
  col_names <- if (inherits(x, "alprek_geocode_raw")) {
    x$meta$col_names
  } else if (is.character(x) && length(x) == 1L && file.exists(x)) {
    # x is a file path
    if (!is.character(sheet) || length(sheet) != 1L || !nzchar(sheet)) {
      stop("sheet must be a single non-empty character.", call. = FALSE)
    }
    available <- tryCatch(readxl::excel_sheets(x),
                          error = function(e) character(0))
    if (!sheet %in% available) {
      stop(sprintf(
        "Sheet '%s' not found in %s. Available sheets: %s",
        sheet, basename(x),
        paste(sprintf("'%s'", available), collapse = ", ")
      ), call. = FALSE)
    }
    hdr <- suppressMessages(readxl::read_excel(
      x, sheet = sheet, n_max = 0, col_names = TRUE,
      .name_repair = "minimal"
    ))
    nm <- colnames(hdr)
    nm[!is.na(nm) & nzchar(nm)]
  } else if (is.character(x)) {
    x
  } else {
    stop("x must be an alprek_geocode_raw object, a character vector of ",
         "column names, or a path to an xlsx file.", call. = FALSE)
  }

  if (!is.character(col_names) || length(col_names) == 0L) {
    stop("No column names could be extracted from x.", call. = FALSE)
  }

  # ---- v1 contract ----
  v1_cols <- alprek_geocode_column_map()$raw_col
  markers <- c("row_id", "LAT", "LNG", "RESULTCODE")

  # ---- compare ----
  markers_found <- intersect(markers, col_names)
  missing_v1   <- setdiff(v1_cols, col_names)
  unknown_in   <- setdiff(col_names, v1_cols)
  in_both      <- intersect(v1_cols, col_names)

  # Jaccard similarity over the union of the contracted set and the input
  union_size       <- length(union(v1_cols, col_names))
  intersection_n   <- length(in_both)
  jaccard          <- if (union_size == 0L) 0 else intersection_n / union_size

  # Decision
  if (length(markers_found) < length(markers)) {
    format     <- "unknown"
    confidence <- 0
  } else if (length(missing_v1) == 0L && length(unknown_in) == 0L) {
    # exact match
    format     <- "melissa_v1_2026"
    confidence <- 1
  } else if (jaccard >= 0.5) {
    # markers present + reasonable overlap
    format     <- "melissa_v1_2026"
    confidence <- jaccard
  } else {
    format     <- "unknown"
    confidence <- jaccard
  }

  structure(
    list(
      format             = format,
      confidence         = confidence,
      unknown_columns    = unknown_in,
      missing_v1_columns = missing_v1,
      markers_found      = markers_found,
      n_input_cols       = length(col_names)
    ),
    class = "alprek_geocode_format_detection"
  )
}


#' Print method for alprek_geocode_format_detection
#'
#' @param x An `alprek_geocode_format_detection` object.
#' @param ... Ignored.
#' @export
print.alprek_geocode_format_detection <- function(x, ...) {
  cat("<alprek_geocode_format_detection>\n")
  cat("  format:           ", x$format, "\n", sep = "")
  cat("  confidence:       ", format(round(x$confidence, 4),
                                       nsmall = 4), "\n", sep = "")
  cat("  n_input_cols:     ", x$n_input_cols, "\n", sep = "")
  cat("  markers_found:    ", length(x$markers_found),
      "/4 (",
      paste(x$markers_found, collapse = ", "), ")\n", sep = "")
  cat("  missing_v1_cols:  ", length(x$missing_v1_columns), "\n",
      sep = "")
  if (length(x$missing_v1_columns) > 0L) {
    cat("    ", paste(x$missing_v1_columns, collapse = ", "), "\n",
        sep = "")
  }
  cat("  unknown_columns:  ", length(x$unknown_columns), "\n",
      sep = "")
  if (length(x$unknown_columns) > 0L) {
    cat("    ", paste(x$unknown_columns, collapse = ", "), "\n",
        sep = "")
  }
  invisible(x)
}


# ---------------------------------------------------------------------------
# Compare-deliveries internal helpers
# ---------------------------------------------------------------------------

#' Normalize a column name for fuzzy matching
#'
#' @param x Character vector.
#' @return Lower-cased, alphanumeric-only character vector (same length).
#' @keywords internal
.geocode_norm_col <- function(x) {
  x <- tolower(trimws(as.character(x)))
  gsub("[^a-z0-9]", "", x)
}


#' Detect R dtype label for a column (character | numeric | logical | other)
#'
#' @param v A column vector.
#' @return One of `"character"`, `"numeric"`, `"logical"`, `"date"`,
#'   `"integer"`, `"factor"`, `"other"`.
#' @keywords internal
.geocode_dtype_label <- function(v) {
  if (inherits(v, "Date"))    return("date")
  if (inherits(v, "factor"))  return("factor")
  if (is.logical(v))          return("logical")
  if (is.integer(v))          return("integer")
  if (is.numeric(v))          return("numeric")
  if (is.character(v))        return("character")
  "other"
}


#' Read a delivery xlsx with light provenance for compare_deliveries()
#'
#' @description Internal helper for [geocode_compare_deliveries()].
#'   Returns a small list with `$data`, `$sha256`, `$path`, `$sheet`,
#'   `$col_names`, `$dtypes`, `$n_rows`, `$n_cols`. Reads with the same
#'   defaults as `geocode_read()` so dtype detection is consistent.
#'
#' @param path Character path to xlsx.
#' @param sheet Character sheet name (default `"Sheet1"`).
#' @return A list (see description).
#' @keywords internal
.geocode_read_for_compare <- function(path, sheet = "Sheet1") {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("path must be a single non-empty character.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(sprintf("File not found: %s", path), call. = FALSE)
  }
  available <- readxl::excel_sheets(path)
  sheet_used <- sheet
  sheet_found <- sheet %in% available
  # Read whichever sheet was requested; if absent, fall back to sheet 1
  # but record sheet_used vs requested so the caller can flag the rename.
  effective_sheet <- if (sheet_found) sheet else available[1L]
  df <- suppressMessages(readxl::read_excel(
    path, sheet = effective_sheet, guess_max = 10000,
    col_names = TRUE, .name_repair = "minimal"
  ))

  dtypes <- vapply(df, .geocode_dtype_label, character(1))

  list(
    data            = tibble::as_tibble(df),
    path            = path,
    sha256          = alprek_file_hash(path),
    sheet_requested = sheet,
    sheet_effective = effective_sheet,
    sheet_found     = sheet_found,
    available_sheets = available,
    col_names       = colnames(df),
    dtypes          = dtypes,
    n_rows          = nrow(df),
    n_cols          = ncol(df)
  )
}


#' Per-column schema diff with possible-rename pairing (Jaro-Winkler)
#'
#' @param old_cols,new_cols Character vectors of column names.
#' @param old_dtypes,new_dtypes Named character vectors of dtype labels;
#'   names must match `old_cols`/`new_cols`.
#' @param jw_threshold Numeric in `[0, 1]`. Default `0.85`. Greedy
#'   1-to-1 rename pairs require similarity at least this large.
#' @return A tibble with columns: `column_old`, `column_new`,
#'   `dtype_old`, `dtype_new`, `status`, `jw_sim`, `note`. Statuses:
#'   `"in_both"`, `"dtype_changed"`, `"added"`, `"removed"`,
#'   `"possible_rename"`.
#' @keywords internal
.geocode_schema_diff <- function(old_cols, new_cols,
                                 old_dtypes, new_dtypes,
                                 jw_threshold = 0.85) {

  old_cols <- as.character(old_cols)
  new_cols <- as.character(new_cols)
  old_norm <- .geocode_norm_col(old_cols)
  new_norm <- .geocode_norm_col(new_cols)

  # de-duplicate normalised keys (keep first occurrence; pathological
  # case for Melissa contracted columns since they're unique)
  old_map <- stats::setNames(old_cols, old_norm)
  old_map <- old_map[!duplicated(names(old_map))]
  new_map <- stats::setNames(new_cols, new_norm)
  new_map <- new_map[!duplicated(names(new_map))]

  in_both_keys      <- intersect(names(old_map), names(new_map))
  old_only_keys     <- setdiff(names(old_map), names(new_map))
  new_only_keys     <- setdiff(names(new_map), names(old_map))

  rows <- list()

  # ---- in_both / dtype_changed ----
  if (length(in_both_keys)) {
    col_old <- unname(old_map[in_both_keys])
    col_new <- unname(new_map[in_both_keys])
    dt_old  <- unname(old_dtypes[col_old])
    dt_new  <- unname(new_dtypes[col_new])
    same    <- !is.na(dt_old) & !is.na(dt_new) & dt_old == dt_new
    status  <- ifelse(same, "in_both", "dtype_changed")
    note    <- ifelse(same, "",
                      sprintf("dtype %s -> %s", dt_old, dt_new))
    rows[[length(rows) + 1L]] <- tibble::tibble(
      column_old = col_old,
      column_new = col_new,
      dtype_old  = dt_old,
      dtype_new  = dt_new,
      status     = status,
      jw_sim     = NA_real_,
      note       = note
    )
  }

  # ---- greedy 1-to-1 rename pairing on remaining old_only x new_only ----
  rename_pairs <- list()
  remaining_old <- old_only_keys
  matched_new   <- character(0)
  if (length(new_only_keys) && length(remaining_old)) {
    if (requireNamespace("stringdist", quietly = TRUE)) {
      sim_mat <- 1 - stringdist::stringdistmatrix(
        new_only_keys, remaining_old, method = "jw", p = 0.1
      )
      pair_df <- expand.grid(
        new_key = new_only_keys, old_key = remaining_old,
        stringsAsFactors = FALSE
      )
      pair_df$sim <- as.vector(sim_mat)
      pair_df <- pair_df[pair_df$sim >= jw_threshold, , drop = FALSE]
      pair_df <- pair_df[order(-pair_df$sim), , drop = FALSE]
      used_old <- character(0); used_new <- character(0)
      if (nrow(pair_df)) {
        for (i in seq_len(nrow(pair_df))) {
          nk <- pair_df$new_key[i]; ok <- pair_df$old_key[i]
          if (nk %in% used_new || ok %in% used_old) next
          rename_pairs[[length(rename_pairs) + 1L]] <- list(
            new_key = nk, old_key = ok, sim = pair_df$sim[i]
          )
          used_new <- c(used_new, nk); used_old <- c(used_old, ok)
        }
      }
      matched_new   <- used_new
      remaining_old <- setdiff(remaining_old, used_old)
    }
  }

  if (length(rename_pairs)) {
    rn_new <- vapply(rename_pairs, `[[`, character(1), "new_key")
    rn_old <- vapply(rename_pairs, `[[`, character(1), "old_key")
    rn_sim <- vapply(rename_pairs, `[[`, numeric(1), "sim")
    col_old <- unname(old_map[rn_old])
    col_new <- unname(new_map[rn_new])
    rows[[length(rows) + 1L]] <- tibble::tibble(
      column_old = col_old,
      column_new = col_new,
      dtype_old  = unname(old_dtypes[col_old]),
      dtype_new  = unname(new_dtypes[col_new]),
      status     = rep("possible_rename", length(col_old)),
      jw_sim     = round(rn_sim, 4),
      note       = sprintf("paired with old='%s', new='%s'",
                            col_old, col_new)
    )
  }

  # ---- removed (in old, not paired) ----
  if (length(remaining_old)) {
    col_old <- unname(old_map[remaining_old])
    rows[[length(rows) + 1L]] <- tibble::tibble(
      column_old = col_old,
      column_new = NA_character_,
      dtype_old  = unname(old_dtypes[col_old]),
      dtype_new  = NA_character_,
      status     = rep("removed", length(col_old)),
      jw_sim     = NA_real_,
      note       = rep("", length(col_old))
    )
  }

  # ---- added (in new, not paired) ----
  remaining_new <- setdiff(new_only_keys, matched_new)
  if (length(remaining_new)) {
    col_new <- unname(new_map[remaining_new])
    rows[[length(rows) + 1L]] <- tibble::tibble(
      column_old = NA_character_,
      column_new = col_new,
      dtype_old  = NA_character_,
      dtype_new  = unname(new_dtypes[col_new]),
      status     = rep("added", length(col_new)),
      jw_sim     = NA_real_,
      note       = rep("", length(col_new))
    )
  }

  if (!length(rows)) {
    return(tibble::tibble(
      column_old = character(0), column_new = character(0),
      dtype_old  = character(0), dtype_new  = character(0),
      status     = character(0), jw_sim     = numeric(0),
      note       = character(0)
    ))
  }

  out <- dplyr::bind_rows(rows)
  status_order <- c("in_both", "dtype_changed", "possible_rename",
                     "added", "removed")
  out$status <- factor(out$status, levels = status_order)
  out <- out[order(out$status,
                    dplyr::coalesce(out$column_new, out$column_old)),
              , drop = FALSE]
  out$status <- as.character(out$status)
  out
}


#' Per-enum value-set diff (added / removed / in_both levels)
#'
#' @param df_old,df_new Tibbles.
#' @param cols Character vector of enum columns to inspect; missing
#'   columns are silently skipped.
#' @return A tibble with columns: `column`, `value`, `status` (one of
#'   `"in_both"`, `"added"`, `"removed"`), `n_old`, `n_new`. Always
#'   long-format (one row per `(column, value)`).
#' @keywords internal
.geocode_value_set_diff <- function(df_old, df_new, cols) {

  cols <- intersect(cols, intersect(colnames(df_old), colnames(df_new)))
  if (!length(cols)) {
    return(tibble::tibble(
      column = character(0), value = character(0),
      status = character(0), n_old  = integer(0),
      n_new  = integer(0)
    ))
  }

  out_rows <- list()
  for (col in cols) {
    v_old <- as.character(df_old[[col]])
    v_new <- as.character(df_new[[col]])
    # Treat NA as a value of its own so we surface NA introductions
    v_old[is.na(v_old)] <- "<NA>"
    v_new[is.na(v_new)] <- "<NA>"
    tab_old <- table(v_old)
    tab_new <- table(v_new)
    all_vals <- union(names(tab_old), names(tab_new))
    if (!length(all_vals)) next
    n_old <- as.integer(unname(tab_old[all_vals]))
    n_new <- as.integer(unname(tab_new[all_vals]))
    n_old[is.na(n_old)] <- 0L
    n_new[is.na(n_new)] <- 0L
    status <- ifelse(
      n_old > 0L & n_new > 0L, "in_both",
      ifelse(n_new > 0L, "added", "removed")
    )
    out_rows[[length(out_rows) + 1L]] <- tibble::tibble(
      column = rep(col, length(all_vals)),
      value  = all_vals,
      status = status,
      n_old  = n_old,
      n_new  = n_new
    )
  }
  if (!length(out_rows)) {
    return(tibble::tibble(
      column = character(0), value = character(0),
      status = character(0), n_old  = integer(0),
      n_new  = integer(0)
    ))
  }
  dplyr::bind_rows(out_rows)
}


#' Extract school-year root (strip the `_new` suffix when present)
#'
#' @param x Character vector.
#' @return Character vector. `"2025-2026_new"` → `"2025-2026"`; plain
#'   year strings pass through unchanged.
#' @keywords internal
.geocode_school_year_root <- function(x) {
  sub("_new$", "", as.character(x))
}


#' Detect resolved row_id pairs (`_new` placeholder → assigned site_code)
#'
#' @description Joins removed (only_old) rows whose school_year ends in
#'   `_new` against added (only_new) rows whose school_year is the same
#'   root WITHOUT `_new`, on the natural keys
#'   `(school_year_root, site_name, geocode_address)`. Each successful
#'   join is reported as one row in the returned tibble. The matching
#'   semantics match Step 3.5 of the protocol (`row_id_replaced` pairs).
#'
#' @param removed_rows,added_rows Tibbles with at least the columns
#'   `row_id`, `school_year`, `site_name`, `geocode_address`,
#'   `site_code` (the latter may be `NA` in `_new` rows).
#' @return A tibble with columns: `old_row_id`, `new_row_id`,
#'   `school_year_old`, `school_year_new`, `site_name`,
#'   `geocode_address`, `assigned_site_code`.
#' @keywords internal
.geocode_row_id_replaced_pairs <- function(removed_rows, added_rows) {

  required <- c("row_id", "school_year", "site_name",
                 "geocode_address", "site_code")
  if (!all(required %in% colnames(removed_rows)) ||
      !all(required %in% colnames(added_rows))) {
    return(tibble::tibble(
      old_row_id        = character(0),
      new_row_id        = character(0),
      school_year_old   = character(0),
      school_year_new   = character(0),
      site_name         = character(0),
      geocode_address   = character(0),
      assigned_site_code = character(0)
    ))
  }

  rem <- removed_rows
  add <- added_rows
  rem$school_year_root <- .geocode_school_year_root(rem$school_year)
  add$school_year_root <- .geocode_school_year_root(add$school_year)

  # Filter: only consider removed rows whose original school_year
  # contained `_new` AND only added rows where the same root re-appears
  # without `_new`.
  rem_new <- rem[grepl("_new$", as.character(rem$school_year)), ,
                  drop = FALSE]
  add_resolved <- add[!grepl("_new$", as.character(add$school_year)),
                       , drop = FALSE]
  if (nrow(rem_new) == 0L || nrow(add_resolved) == 0L) {
    return(tibble::tibble(
      old_row_id        = character(0),
      new_row_id        = character(0),
      school_year_old   = character(0),
      school_year_new   = character(0),
      site_name         = character(0),
      geocode_address   = character(0),
      assigned_site_code = character(0)
    ))
  }

  paired <- merge(
    rem_new[, c("row_id", "school_year", "school_year_root",
                 "site_name", "geocode_address")],
    add_resolved[, c("row_id", "school_year", "school_year_root",
                      "site_name", "geocode_address", "site_code")],
    by = c("school_year_root", "site_name", "geocode_address"),
    suffixes = c("_old", "_new"),
    all = FALSE
  )

  if (!nrow(paired)) {
    return(tibble::tibble(
      old_row_id        = character(0),
      new_row_id        = character(0),
      school_year_old   = character(0),
      school_year_new   = character(0),
      site_name         = character(0),
      geocode_address   = character(0),
      assigned_site_code = character(0)
    ))
  }

  tibble::tibble(
    old_row_id         = as.character(paired$row_id_old),
    new_row_id         = as.character(paired$row_id_new),
    school_year_old    = as.character(paired$school_year_old),
    school_year_new    = as.character(paired$school_year_new),
    site_name          = as.character(paired$site_name),
    geocode_address    = as.character(paired$geocode_address),
    assigned_site_code = as.character(paired$site_code)
  )
}


#' Per-row, per-column change ledger
#'
#' @param df_old,df_new Tibbles with `row_id` and the comparison columns.
#' @param join_key Character. Default `"row_id"`.
#' @param change_cols Character vector. Columns to inspect. Missing
#'   columns are silently skipped.
#' @return A tibble with columns: `row_id`, `column`, `old_value`,
#'   `new_value`. One row per (row_id, column) that differs (NA-equal
#'   pairs are NOT flagged as changes; NA ≠ value IS flagged).
#' @keywords internal
.geocode_row_changes <- function(df_old, df_new,
                                 join_key   = "row_id",
                                 change_cols) {

  if (!join_key %in% colnames(df_old) ||
      !join_key %in% colnames(df_new)) {
    return(tibble::tibble(
      row_id    = character(0),
      column    = character(0),
      old_value = character(0),
      new_value = character(0)
    ))
  }

  common_ids <- intersect(df_old[[join_key]], df_new[[join_key]])
  if (!length(common_ids)) {
    return(tibble::tibble(
      row_id    = character(0),
      column    = character(0),
      old_value = character(0),
      new_value = character(0)
    ))
  }

  # de-duplicate per join_key (compare_deliveries() flags collisions
  # separately; here we just take the first row to avoid breakage)
  ord_old <- match(common_ids, df_old[[join_key]])
  ord_new <- match(common_ids, df_new[[join_key]])
  df_old_c <- df_old[ord_old, , drop = FALSE]
  df_new_c <- df_new[ord_new, , drop = FALSE]

  change_cols <- intersect(change_cols,
                            intersect(colnames(df_old_c),
                                       colnames(df_new_c)))
  if (!length(change_cols)) {
    return(tibble::tibble(
      row_id    = character(0),
      column    = character(0),
      old_value = character(0),
      new_value = character(0)
    ))
  }

  out_rows <- list()
  for (col in change_cols) {
    a <- as.character(df_old_c[[col]])
    b <- as.character(df_new_c[[col]])
    # NA-on-both = unchanged
    diff_mask <- !((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))
    if (any(diff_mask)) {
      out_rows[[length(out_rows) + 1L]] <- tibble::tibble(
        row_id    = as.character(common_ids[diff_mask]),
        column    = rep(col, sum(diff_mask)),
        old_value = a[diff_mask],
        new_value = b[diff_mask]
      )
    }
  }

  if (!length(out_rows)) {
    return(tibble::tibble(
      row_id    = character(0),
      column    = character(0),
      old_value = character(0),
      new_value = character(0)
    ))
  }
  dplyr::bind_rows(out_rows)
}


# ---------------------------------------------------------------------------
# Public API: geocode_compare_deliveries()
# ---------------------------------------------------------------------------

#' Compare Two Melissa Geocode Deliveries
#'
#' @description Compares two Melissa-returned geocoded xlsx deliveries
#'   end-to-end: schema (column set, dtypes, possible renames), enum
#'   value sets, row-level changes (additions, removals, value
#'   updates), and `_new`-placeholder → assigned-site_code resolution
#'   pairs. Returns a structured `alprek_geocode_delivery_diff` object
#'   that the caller can inspect manually or hand to the printer for a
#'   one-paragraph summary.
#'
#'   The verdict (`"compatible"`, `"compatible_with_additions"`,
#'   `"breaking"`) follows Step 1.4 of the format-diff protocol:
#'   schema breaking changes, duplicate `row_id`s in either file, or
#'   a sheet rename always escalate to `"breaking"`; otherwise the
#'   delivery is `"compatible"` if neither value sets nor any rows
#'   changed, else `"compatible_with_additions"`.
#'
#' @param path_old,path_new Character. Paths to the two delivery xlsx
#'   files. Both must exist.
#' @param sheet Character. Sheet name to read in both files. Default
#'   `"Sheet1"`. A requested sheet that is absent from either file
#'   triggers `verdict = "breaking"`.
#' @param rename_jw_threshold Numeric in `[0, 1]`. Default `0.85`.
#'   Greedy 1-to-1 column-rename pairing requires Jaro-Winkler
#'   similarity at least this large. Higher = stricter pairing.
#' @param join_key Character. Column to join on for row-level
#'   comparison. Default `"row_id"`.
#' @param enum_cols Character vector. Columns whose distinct value sets
#'   to inspect. Default
#'   `c("school_year", "RESULTCODE", "STATUSCODE",
#'       "COUNTYNAME", "FIPS", "PLACENAME")`.
#' @param change_cols Character vector. Columns whose row-level values
#'   to diff. Default
#'   `c("site_code", "latitude", "longitude",
#'       "LAT", "LNG", "geocode_address",
#'       "site_street", "COUNTYNAME", "RESULTCODE")`.
#' @param verbose Logical. Print progress messages? Default `FALSE`.
#'
#' @return An `alprek_geocode_delivery_diff` S3 list with elements:
#'   * `$meta` — list of input paths, SHA-256 hashes, sheet names,
#'     dimensions for both deliveries, computed timestamp.
#'   * `$schema_diff` — tibble (one row per column) with statuses
#'     `"in_both"`, `"dtype_changed"`, `"possible_rename"`, `"added"`,
#'     `"removed"`.
#'   * `$value_set_diff` — tibble (one row per `(column, value)`) with
#'     statuses `"in_both"`, `"added"`, `"removed"`.
#'   * `$rows_only_old`, `$rows_only_new` — tibbles of `row_id`s in
#'     only one delivery, augmented with `likely_replaced_by` /
#'     `likely_replaces` when a `_new` ↔ resolved pair was found.
#'   * `$rows_changed` — tibble of per-row, per-column changes
#'     (NA-on-both = unchanged; NA-vs-value = changed).
#'   * `$row_id_replaced_pairs` — tibble of `_new` → assigned-site_code
#'     row_id resolutions detected via
#'     `(school_year_root, site_name, geocode_address)`.
#'   * `$summary` — tibble of `(metric, value)` headline counters.
#'   * `$verdict` — one of `"compatible"`,
#'     `"compatible_with_additions"`, `"breaking"`.
#'   * `$verdict_reasons` — character vector of reason strings driving
#'     the verdict.
#'
#' @examples
#' \dontrun{
#' diff <- geocode_compare_deliveries(
#'   path_old = "ORIGINAL-DATA/2026-03-04_geocoding_master_Final.xlsx",
#'   path_new = "ORIGINAL-DATA/2026-09-15_geocoding_master_Final.xlsx"
#' )
#' diff
#' diff$verdict
#' diff$row_id_replaced_pairs
#' }
#'
#' @seealso [geocode_detect_format()], [geocode_read()],
#'   [alprek_geocode_column_map()].
#'
#' @export
geocode_compare_deliveries <- function(
    path_old, path_new,
    sheet = "Sheet1",
    rename_jw_threshold = 0.85,
    join_key = "row_id",
    enum_cols = c("school_year", "RESULTCODE", "STATUSCODE",
                  "COUNTYNAME", "FIPS", "PLACENAME"),
    change_cols = c("site_code", "latitude", "longitude",
                    "LAT", "LNG", "geocode_address",
                    "site_street", "COUNTYNAME", "RESULTCODE"),
    verbose = FALSE) {

  # ---- input validation ----
  if (missing(path_old) || missing(path_new)) {
    stop("Both path_old and path_new are required.", call. = FALSE)
  }
  stopifnot(
    is.character(sheet), length(sheet) == 1L, nzchar(sheet),
    is.numeric(rename_jw_threshold), length(rename_jw_threshold) == 1L,
    rename_jw_threshold >= 0, rename_jw_threshold <= 1,
    is.character(join_key), length(join_key) == 1L, nzchar(join_key),
    is.character(enum_cols),
    is.character(change_cols),
    is.logical(verbose), length(verbose) == 1L
  )

  if (isTRUE(verbose)) {
    message(sprintf("[geocode_compare_deliveries] reading old: %s",
                    basename(path_old)))
  }
  old <- .geocode_read_for_compare(path_old, sheet = sheet)
  if (isTRUE(verbose)) {
    message(sprintf("[geocode_compare_deliveries] reading new: %s",
                    basename(path_new)))
  }
  new <- .geocode_read_for_compare(path_new, sheet = sheet)

  # ---- schema diff ----
  if (isTRUE(verbose)) {
    message("[geocode_compare_deliveries] computing schema diff")
  }
  old_dtypes_named <- old$dtypes
  new_dtypes_named <- new$dtypes
  schema_diff <- .geocode_schema_diff(
    old_cols = old$col_names, new_cols = new$col_names,
    old_dtypes = old_dtypes_named, new_dtypes = new_dtypes_named,
    jw_threshold = rename_jw_threshold
  )

  schema_breaking_statuses <- c("added", "removed",
                                  "dtype_changed", "possible_rename")
  schema_breaking <- any(schema_diff$status %in% schema_breaking_statuses)

  # ---- value-set diff ----
  if (isTRUE(verbose)) {
    message("[geocode_compare_deliveries] computing value-set diff")
  }
  value_set_diff <- .geocode_value_set_diff(
    df_old = old$data, df_new = new$data, cols = enum_cols
  )
  n_value_set_added   <- sum(value_set_diff$status == "added")
  n_value_set_removed <- sum(value_set_diff$status == "removed")

  # ---- row-level diff ----
  if (isTRUE(verbose)) {
    message("[geocode_compare_deliveries] computing row-level diff")
  }
  row_id_col_present <- join_key %in% old$col_names &&
                         join_key %in% new$col_names
  collision_old <- FALSE
  collision_new <- FALSE
  rows_only_old <- tibble::tibble(row_id = character(0))
  rows_only_new <- tibble::tibble(row_id = character(0))
  rows_changed  <- tibble::tibble(
    row_id    = character(0), column   = character(0),
    old_value = character(0), new_value = character(0)
  )
  row_id_replaced_pairs <- tibble::tibble(
    old_row_id        = character(0),
    new_row_id        = character(0),
    school_year_old   = character(0),
    school_year_new   = character(0),
    site_name         = character(0),
    geocode_address   = character(0),
    assigned_site_code = character(0)
  )

  if (row_id_col_present) {
    ids_old <- as.character(old$data[[join_key]])
    ids_new <- as.character(new$data[[join_key]])
    collision_old <- any(duplicated(ids_old[!is.na(ids_old)]))
    collision_new <- any(duplicated(ids_new[!is.na(ids_new)]))

    only_old_ids <- setdiff(ids_old, ids_new)
    only_new_ids <- setdiff(ids_new, ids_old)

    # Capture core columns for the only_* tibbles (best-effort: include
    # whatever core identity columns are present)
    core_cols <- intersect(
      c("row_id", "school_year", "site_name", "site_code",
        "geocode_address"),
      union(colnames(old$data), colnames(new$data))
    )

    if (length(only_old_ids)) {
      mask <- ids_old %in% only_old_ids
      keep <- intersect(core_cols, colnames(old$data))
      rows_only_old <- old$data[mask, keep, drop = FALSE]
    }
    if (length(only_new_ids)) {
      mask <- ids_new %in% only_new_ids
      keep <- intersect(core_cols, colnames(new$data))
      rows_only_new <- new$data[mask, keep, drop = FALSE]
    }

    # Resolved-row_id pairs
    row_id_replaced_pairs <- .geocode_row_id_replaced_pairs(
      removed_rows = if (nrow(rows_only_old) &&
                          all(c("row_id", "school_year", "site_name",
                                  "geocode_address", "site_code") %in%
                                colnames(rows_only_old)))
                       rows_only_old else
                       tibble::as_tibble(old$data[
                         ids_old %in% only_old_ids,
                         intersect(c("row_id", "school_year",
                                       "site_name", "geocode_address",
                                       "site_code"),
                                     colnames(old$data)),
                         drop = FALSE]),
      added_rows   = if (nrow(rows_only_new) &&
                          all(c("row_id", "school_year", "site_name",
                                  "geocode_address", "site_code") %in%
                                colnames(rows_only_new)))
                       rows_only_new else
                       tibble::as_tibble(new$data[
                         ids_new %in% only_new_ids,
                         intersect(c("row_id", "school_year",
                                       "site_name", "geocode_address",
                                       "site_code"),
                                     colnames(new$data)),
                         drop = FALSE])
    )

    # Annotate only_old / only_new with replacement-link columns
    if (nrow(rows_only_old)) {
      rows_only_old <- tibble::as_tibble(rows_only_old)
      lookup_new <- stats::setNames(
        row_id_replaced_pairs$new_row_id,
        row_id_replaced_pairs$old_row_id
      )
      rows_only_old$likely_replaced_by <- unname(
        lookup_new[as.character(rows_only_old$row_id)]
      )
    }
    if (nrow(rows_only_new)) {
      rows_only_new <- tibble::as_tibble(rows_only_new)
      lookup_old <- stats::setNames(
        row_id_replaced_pairs$old_row_id,
        row_id_replaced_pairs$new_row_id
      )
      rows_only_new$likely_replaces <- unname(
        lookup_old[as.character(rows_only_new$row_id)]
      )
    }

    # Per-row changes (only on row_ids present in both)
    rows_changed <- .geocode_row_changes(
      df_old = old$data, df_new = new$data,
      join_key = join_key, change_cols = change_cols
    )
  }

  # ---- summary ----
  summary <- tibble::tibble(
    metric = c(
      "n_rows_old", "n_rows_new",
      "n_cols_old", "n_cols_new",
      "n_schema_in_both", "n_schema_added", "n_schema_removed",
      "n_schema_dtype_changed", "n_schema_possible_rename",
      "n_value_set_added", "n_value_set_removed",
      "n_rows_only_old", "n_rows_only_new",
      "n_rows_changed",
      "n_row_id_replaced_pairs"
    ),
    value = as.numeric(c(
      old$n_rows, new$n_rows,
      old$n_cols, new$n_cols,
      sum(schema_diff$status == "in_both"),
      sum(schema_diff$status == "added"),
      sum(schema_diff$status == "removed"),
      sum(schema_diff$status == "dtype_changed"),
      sum(schema_diff$status == "possible_rename"),
      n_value_set_added,
      n_value_set_removed,
      nrow(rows_only_old),
      nrow(rows_only_new),
      nrow(rows_changed),
      nrow(row_id_replaced_pairs)
    ))
  )

  # ---- verdict ----
  sheet_renamed <- isFALSE(old$sheet_found) || isFALSE(new$sheet_found)
  collision     <- collision_old || collision_new

  verdict_reasons <- character(0)
  if (schema_breaking) {
    verdict_reasons <- c(
      verdict_reasons,
      sprintf(
        "schema_diff has breaking status(es): %s",
        paste(sort(unique(
          schema_diff$status[schema_diff$status %in%
                                schema_breaking_statuses]
        )), collapse = ", ")
      )
    )
  }
  if (collision) {
    if (collision_old) verdict_reasons <- c(
      verdict_reasons, "duplicate row_id in path_old")
    if (collision_new) verdict_reasons <- c(
      verdict_reasons, "duplicate row_id in path_new")
  }
  if (sheet_renamed) {
    if (!old$sheet_found) verdict_reasons <- c(
      verdict_reasons,
      sprintf("sheet '%s' not found in path_old (effective sheet: '%s')",
              sheet, old$sheet_effective))
    if (!new$sheet_found) verdict_reasons <- c(
      verdict_reasons,
      sprintf("sheet '%s' not found in path_new (effective sheet: '%s')",
              sheet, new$sheet_effective))
  }

  if (schema_breaking || collision || sheet_renamed) {
    verdict <- "breaking"
  } else if (
    n_value_set_added == 0L &&
    nrow(rows_only_old) == 0L &&
    nrow(rows_only_new) == 0L &&
    nrow(rows_changed) == 0L
  ) {
    verdict <- "compatible"
    if (n_value_set_removed > 0L) {
      verdict_reasons <- c(
        verdict_reasons,
        "value-set has removed levels (informational only)"
      )
    }
  } else {
    verdict <- "compatible_with_additions"
    if (nrow(row_id_replaced_pairs) > 0L) {
      verdict_reasons <- c(
        verdict_reasons,
        sprintf("%d row_id replaced pairs detected (_new -> assigned)",
                 nrow(row_id_replaced_pairs)))
    }
    if (nrow(rows_only_new) > nrow(row_id_replaced_pairs)) {
      verdict_reasons <- c(
        verdict_reasons,
        sprintf("%d net-new row_ids appended",
                 nrow(rows_only_new) - nrow(row_id_replaced_pairs)))
    }
    if (nrow(rows_changed) > 0L) {
      verdict_reasons <- c(
        verdict_reasons,
        sprintf("%d per-row value updates", nrow(rows_changed)))
    }
    if (n_value_set_added > 0L) {
      verdict_reasons <- c(
        verdict_reasons,
        sprintf("%d new enum levels", n_value_set_added))
    }
  }

  meta <- list(
    path_old        = path_old,
    path_new        = path_new,
    sha256_old      = old$sha256,
    sha256_new      = new$sha256,
    sheet_requested = sheet,
    sheet_old       = old$sheet_effective,
    sheet_new       = new$sheet_effective,
    sheet_found_old = old$sheet_found,
    sheet_found_new = new$sheet_found,
    n_rows_old      = old$n_rows,
    n_rows_new      = new$n_rows,
    n_cols_old      = old$n_cols,
    n_cols_new      = new$n_cols,
    join_key        = join_key,
    enum_cols       = enum_cols,
    change_cols     = change_cols,
    rename_jw_threshold = rename_jw_threshold,
    computed_at     = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )

  structure(
    list(
      meta                  = meta,
      schema_diff           = schema_diff,
      value_set_diff        = value_set_diff,
      rows_only_old         = rows_only_old,
      rows_only_new         = rows_only_new,
      rows_changed          = rows_changed,
      row_id_replaced_pairs = row_id_replaced_pairs,
      summary               = summary,
      verdict               = verdict,
      verdict_reasons       = verdict_reasons
    ),
    class = "alprek_geocode_delivery_diff"
  )
}


#' Print method for alprek_geocode_delivery_diff
#'
#' @param x An `alprek_geocode_delivery_diff` object.
#' @param ... Ignored.
#' @export
print.alprek_geocode_delivery_diff <- function(x, ...) {
  cat("<alprek_geocode_delivery_diff>\n")
  cat("  verdict:        ", x$verdict, "\n", sep = "")
  cat("  computed_at:    ", x$meta$computed_at, "\n", sep = "")
  cat("  path_old:       ", basename(x$meta$path_old), "\n", sep = "")
  cat("  path_new:       ", basename(x$meta$path_new), "\n", sep = "")
  cat("  sheet_old:      ", x$meta$sheet_old,
      if (isFALSE(x$meta$sheet_found_old))
        sprintf(" (requested '%s' missing)", x$meta$sheet_requested)
      else "", "\n", sep = "")
  cat("  sheet_new:      ", x$meta$sheet_new,
      if (isFALSE(x$meta$sheet_found_new))
        sprintf(" (requested '%s' missing)", x$meta$sheet_requested)
      else "", "\n", sep = "")
  cat("  n_rows:         ", x$meta$n_rows_old, " -> ",
      x$meta$n_rows_new, "\n", sep = "")
  cat("  n_cols:         ", x$meta$n_cols_old, " -> ",
      x$meta$n_cols_new, "\n", sep = "")
  cat("  schema_diff:    ",
      sum(x$schema_diff$status == "in_both"), " in_both, ",
      sum(x$schema_diff$status == "added"), " added, ",
      sum(x$schema_diff$status == "removed"), " removed, ",
      sum(x$schema_diff$status == "dtype_changed"), " dtype_changed, ",
      sum(x$schema_diff$status == "possible_rename"),
      " possible_rename\n", sep = "")
  cat("  value_set_diff: ",
      sum(x$value_set_diff$status == "added"), " added, ",
      sum(x$value_set_diff$status == "removed"), " removed\n",
      sep = "")
  cat("  rows_only_old:  ", nrow(x$rows_only_old), "\n", sep = "")
  cat("  rows_only_new:  ", nrow(x$rows_only_new), "\n", sep = "")
  cat("  rows_changed:   ", nrow(x$rows_changed), "\n", sep = "")
  cat("  row_id_replaced_pairs: ", nrow(x$row_id_replaced_pairs),
      "\n", sep = "")
  if (length(x$verdict_reasons)) {
    cat("  reasons:\n")
    for (r in x$verdict_reasons) cat("    - ", r, "\n", sep = "")
  }
  invisible(x)
}
