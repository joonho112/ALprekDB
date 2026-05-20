#' Clean ADECE Applications Data
#'
#' @description Standardizes column names and types of an
#'   `alprek_applications_raw` object using cycle-specific column mappings from
#'   `inst/extdata/mappings/applications_column_map_<kind>_<cycle>.csv`. Filters
#'   out known noise rows (e.g., "Show the Debugger Trace Report"), drops
#'   capacity-sheet aggregate rows that have no `site_code`, and preserves
#'   per-row `raw_row_index`/`lineage_id` plus `data_source` for provenance.
#'
#'   No geocoding, address parsing, or spatial work happens here — those are
#'   handled by downstream packages.
#'
#' @param raw An `alprek_applications_raw` object (from `applications_read_*()`).
#' @param cycle Character. Cycle schema label. Default auto-detected via
#'   [applications_detect_format()].
#' @param remove_noise_rows Logical. Drop rows whose `process_name` maps to
#'   `kind_inferred == "noise"` in `applications_status_codes.csv`? Default
#'   `TRUE`.
#' @return An `alprek_applications_clean` S3 object with elements:
#'   - `data`: tibble of cleaned data with standardized column names and row
#'     lineage fields
#'   - `cleaning_log`: tibble of changes applied (variable, rule, n_rows),
#'     including parse failures and aggregate-row drops
#'   - `meta`: list inheriting from raw + cycle, n_rows_in, n_rows_out,
#'     n_rows_dropped, cleaned_at
#'
#' @examples
#' \dontrun{
#' raw <- applications_read_renewals(path, cycle_year = "2026-2027")
#' clean <- applications_clean(raw)
#' clean
#' }
#'
#' @importFrom dplyr filter mutate rename across all_of
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data
#' @export
applications_clean <- function(raw,
                                cycle = NULL,
                                remove_noise_rows = TRUE) {

  if (!inherits(raw, "alprek_applications_raw")) {
    stop("raw must be an alprek_applications_raw object (from applications_read_*()).",
         call. = FALSE)
  }

  kind <- raw$meta$kind
  if (is.null(cycle)) {
    cycle <- applications_detect_format(raw)
  }
  if (cycle == "unknown") {
    warning("Detected format = 'unknown' for kind='", kind,
            "'. Falling back to 'cycle1' codebook; results may be unreliable.",
            call. = FALSE)
    cycle <- "cycle1"
  }

  # ---- 1. Load column map ----
  map_kind <- switch(kind,
                      renewals = "renewals",
                      new_apps = "new",
                      non_renewals = "nonrenewals",
                      capacity = "capacity")
  cmap <- .load_applications_column_map(map_kind, cycle = cycle)

  # ---- 2. Apply rename ----
  data_in <- raw$data
  n_rows_in <- nrow(data_in)
  cleaning_log <- list()

  raw_cols_present <- intersect(cmap$raw_column, names(data_in))
  missing_raw <- setdiff(cmap$raw_column, raw_cols_present)
  if (length(missing_raw) > 0L) {
    cleaning_log[[length(cleaning_log) + 1L]] <- tibble::tibble(
      variable = missing_raw,
      rule = "missing_in_source",
      n_rows = 0L,
      note = sprintf("Codebook expects '%s' but not present in raw data",
                     missing_raw)
    )
  }

  rename_vec <- setNames(cmap$raw_column[match(raw_cols_present, cmap$raw_column)],
                          cmap$standard_name[match(raw_cols_present, cmap$raw_column)])
  # `rename_vec` has names = standard_name, values = raw_column
  # Build reverse for dplyr::rename which wants new = old
  rename_pairs <- setNames(rename_vec, names(rename_vec))
  data_renamed <- dplyr::rename(data_in, !!!rename_pairs)

  # Subset to mapped columns plus row-lineage fields.
  lineage_cols <- intersect(c("raw_row_index", "lineage_id"), names(data_renamed))
  data_renamed <- data_renamed[, c(names(rename_pairs), lineage_cols), drop = FALSE]

  # ---- 3. Type parsing ----
  parse_count <- 0L
  for (i in seq_len(nrow(cmap))) {
    std <- cmap$standard_name[i]
    typ <- cmap$type[i]
    if (!std %in% names(data_renamed)) next
    col <- data_renamed[[std]]
    new_col <- switch(typ,
      integer = .ap_parse_integer(col),
      numeric = .ap_parse_numeric(col),
      categorical = trimws(as.character(col)),
      id = trimws(as.character(col)),
      text = trimws(as.character(col)),
      col  # fallback
    )
    if (typ %in% c("integer", "numeric")) {
      n_failed <- .ap_parse_failure_count(col, new_col)
      if (n_failed > 0L) {
        cleaning_log[[length(cleaning_log) + 1L]] <- tibble::tibble(
          variable = std,
          rule = "parse_failure",
          n_rows = n_failed,
          note = sprintf("%d nonblank value(s) could not be parsed as %s",
                         n_failed, typ)
        )
      }
    }
    if (!identical(new_col, col)) {
      data_renamed[[std]] <- new_col
      parse_count <- parse_count + 1L
    }
  }
  cleaning_log[[length(cleaning_log) + 1L]] <- tibble::tibble(
    variable = NA_character_,
    rule = "type_parsing",
    n_rows = nrow(data_renamed),
    note = sprintf("%d columns type-coerced per codebook", parse_count)
  )

  # ---- 4. Add data_source ----
  data_source_label <- switch(kind,
    renewals = "ADECE-renewals-sheet",
    new_apps = "ADECE-new-sheet",
    non_renewals = "ADECE-nonrenewals-sheet",
    capacity = "ADECE-capacity-sheet"
  )
  data_renamed$data_source <- data_source_label

  # ---- 5. Filter noise rows ----
  n_dropped <- 0L
  if (isTRUE(remove_noise_rows) && "process_name" %in% names(data_renamed)) {
    status_codes <- alprek_applications_status_codes()
    noise_processes <- status_codes$process_name[status_codes$kind_inferred == "noise"]
    if (length(noise_processes) > 0L) {
      bad <- data_renamed$process_name %in% noise_processes
      n_dropped <- sum(bad, na.rm = TRUE)
      if (n_dropped > 0L) {
        data_renamed <- data_renamed[!bad & !is.na(data_renamed$process_name), , drop = FALSE]
        cleaning_log[[length(cleaning_log) + 1L]] <- tibble::tibble(
          variable = "process_name",
          rule = "drop_noise",
          n_rows = n_dropped,
          note = sprintf("Dropped %d noise rows (e.g., 'Show the Debugger Trace Report')",
                         n_dropped)
        )
      }
    }
  }

  # Drop Access-report aggregate rows from the capacity sheet. They have no
  # site_code and summarize statewide/region totals, so they cannot be part of
  # the site-level applications contract.
  n_dropped_capacity_aggregate <- 0L
  if (kind == "capacity" && all(c("site_code", "site_name") %in% names(data_renamed))) {
    site_code_blank <- is.na(data_renamed$site_code) |
      !nzchar(trimws(as.character(data_renamed$site_code)))
    site_name <- trimws(as.character(data_renamed$site_name))
    aggregate_label <- grepl("^(Statewide|Totals for Region)", site_name,
                             ignore.case = TRUE)
    aggregate_rows <- site_code_blank & aggregate_label
    n_dropped_capacity_aggregate <- sum(aggregate_rows, na.rm = TRUE)
    if (n_dropped_capacity_aggregate > 0L) {
      data_renamed <- data_renamed[!aggregate_rows, , drop = FALSE]
      cleaning_log[[length(cleaning_log) + 1L]] <- tibble::tibble(
        variable = "site_code",
        rule = "drop_capacity_aggregate",
        n_rows = n_dropped_capacity_aggregate,
        note = sprintf("Dropped %d statewide/region capacity aggregate row(s)",
                       n_dropped_capacity_aggregate)
      )
    }
  }

  # Drop fully-NA rows (after rename)
  n_before_naf <- nrow(data_renamed)
  content_cols <- setdiff(names(data_renamed),
                          c("raw_row_index", "lineage_id", "data_source"))
  fully_blank <- if (length(content_cols) == 0L) {
    rep(FALSE, nrow(data_renamed))
  } else {
    blank_mat <- vapply(data_renamed[content_cols], function(col) {
      is.na(col) | !nzchar(trimws(as.character(col)))
    }, logical(nrow(data_renamed)))
    rowSums(blank_mat) == length(content_cols)
  }
  data_renamed <- data_renamed[!fully_blank, , drop = FALSE]
  n_dropped_na <- n_before_naf - nrow(data_renamed)
  if (n_dropped_na > 0L) {
    cleaning_log[[length(cleaning_log) + 1L]] <- tibble::tibble(
      variable = NA_character_,
      rule = "drop_fully_na",
      n_rows = n_dropped_na,
      note = sprintf("Dropped %d fully-NA rows", n_dropped_na)
    )
  }

  # ---- 6. Assemble ----
  cleaning_log_df <- if (length(cleaning_log) > 0L)
                        do.call(rbind, cleaning_log) else
                        tibble::tibble(variable = character(0), rule = character(0),
                                        n_rows = integer(0), note = character(0))

  meta_out <- c(raw$meta, list(
    cycle = cycle,
	    n_rows_in = n_rows_in,
	    n_rows_out = nrow(data_renamed),
	    n_rows_dropped = n_dropped + n_dropped_na + n_dropped_capacity_aggregate,
	    cleaned_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
	  ))

  structure(list(
    data = tibble::as_tibble(data_renamed),
    cleaning_log = cleaning_log_df,
    meta = meta_out
  ), class = "alprek_applications_clean")
}


#' Print method for alprek_applications_clean
#' @param x An alprek_applications_clean object.
#' @param ... Ignored.
#' @export
print.alprek_applications_clean <- function(x, ...) {
  cat("<alprek_applications_clean>\n")
  cat("  Kind:        ", x$meta$kind, "\n")
  cat("  Cycle:       ", x$meta$cycle, " (", x$meta$cycle_year, ")\n", sep = "")
  cat("  Rows: in=",   x$meta$n_rows_in,
      " out=",          x$meta$n_rows_out,
      " dropped=",      x$meta$n_rows_dropped, "\n", sep = "")
  cat("  Cols:        ", ncol(x$data), "\n")
  cat("  Cleaned at:  ", x$meta$cleaned_at, "\n")
  invisible(x)
}


#' Parse applications numeric values with currency-aware cleaning
#' @keywords internal
#' @noRd
.ap_parse_numeric <- function(x) {
  .parse_budget_amount(x)
}


#' Parse applications integer values without silent truncation
#' @keywords internal
#' @noRd
.ap_parse_integer <- function(x) {
  num <- .ap_parse_numeric(x)
  non_integer <- !is.na(num) & abs(num - round(num)) > .Machine$double.eps^0.5
  out <- suppressWarnings(as.integer(round(num)))
  out[non_integer] <- NA_integer_
  out
}


#' Count nonblank source values that became NA during parsing
#' @keywords internal
#' @noRd
.ap_parse_failure_count <- function(raw, parsed) {
  raw_chr <- trimws(as.character(raw))
  expected_missing <- is.na(raw) |
    tolower(raw_chr) %in% c("", "na", "n/a", "not available", "none",
                            "null", "-", "--")
  sum(!expected_missing & is.na(parsed), na.rm = TRUE)
}
