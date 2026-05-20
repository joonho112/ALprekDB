#' Validate Cleaned or Reconciled Applications Data
#'
#' @description Comprehensive data-quality checks on a single cleaned ADECE
#'   applications object (`alprek_applications_clean`) or the merged
#'   `alprek_applications_reconciled` object. Mirrors `budget_validate()` and
#'   `classroom_validate()` API: each check is logged with a structured row
#'   (`check_name`, `check_description`, `status` one of `PASS`, `ERROR`, `WARN`, `INFO`,
#'   `n_issues`, `details`), and offending rows accumulate in `$issues`.
#'
#'   Validation is scoped to the **data contract layer**: column existence,
#'   value ranges, cross-field consistency, codebook membership, provenance.
#'   Geocoding / ACS / Bayesian-modelling checks live in downstream packages.
#'
#' @param x One of:
#'   * `alprek_applications_clean` (cleaned per-kind)
#'   * `alprek_applications_reconciled` (merged renewals + new_apps)
#'   * `alprek_applications_linkage` (classroom_panel x applications join)
#' @param strict Logical. If `TRUE`, treats warnings as overall failure.
#'   Default `FALSE`.
#' @param tolerance Numeric. Dollar tolerance for cross-field reconciliation
#'   checks. Default `1.00`.
#'
#' @return An `alprek_applications_validation` S3 list with elements:
#'   * `passed`: logical (overall result)
#'   * `n_errors`, `n_warnings`, `n_info`: integer counts
#'   * `kind`: e.g., `"renewals"`, `"new_apps"`, `"non_renewals"`, `"capacity"`,
#'     `"reconciled"`
#'   * `checks`: tibble of check results
#'   * `issues`: tibble of offending rows (one row per issue, with
#'     `issue_type` plus key context columns)
#'
#' @examples
#' \dontrun{
#' clean <- applications_clean(applications_read_renewals(path, "2026-2027"))
#' v <- applications_validate(clean)
#' print(v)
#' v$checks
#' v$issues
#' }
#'
#' @importFrom dplyr filter mutate select bind_rows n_distinct
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data
#' @export
applications_validate <- function(x, strict = FALSE, tolerance = 1.00) {

  is_clean <- inherits(x, "alprek_applications_clean")
  is_reco  <- inherits(x, "alprek_applications_reconciled")
  is_link  <- inherits(x, "alprek_applications_linkage")
  if (!is_clean && !is_reco && !is_link) {
    stop("x must be an alprek_applications_clean, ",
         "alprek_applications_reconciled, or alprek_applications_linkage object.",
         call. = FALSE)
  }
  if (!is.logical(strict) || length(strict) != 1L) {
    stop("strict must be TRUE/FALSE.", call. = FALSE)
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1L || tolerance < 0) {
    stop("tolerance must be a single non-negative number.", call. = FALSE)
  }

  checks <- list()
  issues <- tibble::tibble(issue_type = character(),
                           source_kind = character())
  acc <- new.env(parent = emptyenv())
  acc$checks <- checks
  acc$issues <- issues

  if (is_clean) {
    kind <- x$meta$kind
    df <- x$data
    meta <- x$meta
    switch(kind,
      renewals     = .ap_validate_renewals(df, meta, tolerance, acc),
      new_apps     = .ap_validate_new_apps(df, meta, tolerance, acc),
      non_renewals = .ap_validate_non_renewals(df, meta, acc),
      capacity     = .ap_validate_capacity(df, meta, acc),
      stop("Unknown kind: ", kind, call. = FALSE)
    )
  } else if (is_reco) {
    kind <- "reconciled"
    df <- x$reconciled
    meta <- x$meta
    .ap_validate_reconciled(df, meta, tolerance, acc, x$reconciliation_log)
  } else {
    # is_link
    kind <- "linkage"
    .ap_validate_linkage(x, acc)
  }

  .ap_assemble_result(acc$checks, acc$issues, kind = kind, strict = strict)
}


#' Print method for `alprek_applications_validation`
#' @param x An `alprek_applications_validation` object.
#' @param ... Ignored.
#' @export
print.alprek_applications_validation <- function(x, ...) {
  cat("<alprek_applications_validation>\n")
  cat("  Kind:    ", x$kind, "\n", sep = "")
  cat("  Overall: ", if (x$passed) "PASSED" else "FAILED", "\n", sep = "")
  cat("  Errors: ", x$n_errors, " | Warnings: ", x$n_warnings,
      " | Info: ", x$n_info, "\n", sep = "")
  if (nrow(x$checks) > 0L) {
    cat("\n  Checks:\n")
    for (i in seq_len(nrow(x$checks))) {
      row <- x$checks[i, ]
      icon <- switch(row$status,
                      PASS  = "+",
                      ERROR = "x",
                      WARN  = "!",
                      INFO  = "i")
      cat("    [", icon, "] ", row$check_description, sep = "")
      if (!is.na(row$details)) cat(" -- ", row$details, sep = "")
      cat("\n")
    }
  }
  invisible(x)
}


# ============================================================================
# Per-kind validators (internal)
# ============================================================================

#' @keywords internal
#' @noRd
.ap_validate_renewals <- function(df, meta, tolerance, acc) {
  required <- c("process_name", "region", "county", "organization_name",
                 "project_name", "funding_type", "program_type",
                 "project_name_prior", "funding_type_prior",
                 "award_prior", "total_funding_request", "draft_base_award",
                 "tier_adjustment", "draft_award", "data_source")
  .ap_check_required_cols(df, required, "required_columns_renewals",
                           "Required renewal columns present", acc)

  funding_cols <- intersect(c("award_prior", "total_funding_request",
                                "draft_base_award", "draft_award"),
                              names(df))
  .ap_check_no_negative_funding(df, funding_cols, acc, kind = "renewals")
  .ap_check_tier_adjustment_range(df, acc)
  .ap_check_draft_award_reconciles(df, tolerance, acc)
  .ap_check_county_in_alabama(df, acc, kind = "renewals")
  .ap_check_funding_type_in_codebook(df, acc, kind = "renewals")
  .ap_check_region_format(df, acc, kind = "renewals")
  .ap_check_process_name_in_codebook(df, acc, kind = "renewals")
  .ap_check_provenance(df, meta, acc)
  .ap_check_lineage_present(df, meta, acc, kind = "renewals")
  .ap_check_row_count_positive(df, acc, kind = "renewals")
  .ap_check_county_coverage_breadth(df, acc, kind = "renewals")
}


#' @keywords internal
#' @noRd
.ap_validate_new_apps <- function(df, meta, tolerance, acc) {
  required <- c("process_name", "region", "county", "organization_name",
                 "project_name", "funding_type", "program_type",
                 "total_funding_request", "award_other", "new_classroom_award",
                 "total_award", "data_source")
  .ap_check_required_cols(df, required, "required_columns_new_apps",
                           "Required new-application columns present", acc)

  funding_cols <- intersect(c("total_funding_request", "award_other",
                                "new_classroom_award", "total_award"),
                              names(df))
  .ap_check_no_negative_funding(df, funding_cols, acc, kind = "new_apps")
  .ap_check_total_award_reconciles(df, tolerance, acc)
  .ap_check_county_in_alabama(df, acc, kind = "new_apps")
  .ap_check_funding_type_in_codebook(df, acc, kind = "new_apps")
  .ap_check_region_format(df, acc, kind = "new_apps")
  .ap_check_process_name_in_codebook(df, acc, kind = "new_apps")
  .ap_check_provenance(df, meta, acc)
  .ap_check_lineage_present(df, meta, acc, kind = "new_apps")
  .ap_check_row_count_positive(df, acc, kind = "new_apps")
  .ap_check_county_coverage_breadth(df, acc, kind = "new_apps")
}


#' @keywords internal
#' @noRd
.ap_validate_non_renewals <- function(df, meta, acc) {
  required <- c("region", "county", "organization_name", "project_name",
                 "prior_funding_amount", "prior_funding_type", "data_source")
  .ap_check_required_cols(df, required, "required_columns_non_renewals",
                           "Required non-renewal columns present", acc)
  funding_cols <- intersect("prior_funding_amount", names(df))
  .ap_check_no_negative_funding(df, funding_cols, acc, kind = "non_renewals")
  .ap_check_county_in_alabama(df, acc, kind = "non_renewals")
  .ap_check_funding_type_in_codebook(df, acc, kind = "non_renewals",
                                       fund_col = "prior_funding_type")
  .ap_check_provenance(df, meta, acc)
  .ap_check_lineage_present(df, meta, acc, kind = "non_renewals")
  .ap_check_row_count_positive(df, acc, kind = "non_renewals")
}


#' @keywords internal
#' @noRd
.ap_validate_capacity <- function(df, meta, acc) {
  required <- c("site_code", "site_name", "n_classrooms", "enrollment",
                 "capacity", "waitlist", "spaces_available_with_waitlist",
                 "data_source")
  .ap_check_required_cols(df, required, "required_columns_capacity",
                           "Required capacity columns present", acc)
  .ap_check_capacity_positive(df, acc)
  .ap_check_capacity_site_code_present(df, acc)
  .ap_check_enrollment_le_capacity(df, acc)
  .ap_check_waitlist_non_negative(df, acc)
  .ap_check_provenance(df, meta, acc)
  .ap_check_lineage_present(df, meta, acc, kind = "capacity")
  .ap_check_row_count_positive(df, acc, kind = "capacity")
}


#' @keywords internal
#' @noRd
.ap_validate_reconciled <- function(df, meta, tolerance, acc, reconciliation_log) {
  # Reconciled merges renewals + new_apps. Run baseline data checks plus 2
  # reconciliation-specific checks.
  funding_cols <- intersect(c("award_prior", "total_funding_request",
                                "draft_base_award", "draft_award",
                                "award_other", "new_classroom_award",
                                "total_award"),
                              names(df))
  .ap_check_no_negative_funding(df, funding_cols, acc, kind = "reconciled")
  .ap_check_county_in_alabama(df, acc, kind = "reconciled")
  .ap_check_lineage_present(df, meta, acc, kind = "reconciled")
  .ap_check_application_id_unique(df, acc)
  .ap_check_row_count_positive(df, acc, kind = "reconciled")

  # Reconciled-only:
  .ap_check_every_row_has_bucket(df, acc)
  .ap_check_match_score_in_unit_interval(df, acc)
  .ap_check_reconciliation_log_coverage(df, reconciliation_log, acc)
}


# ============================================================================
# Atomic check helpers (internal)
# ============================================================================

#' @keywords internal
#' @noRd
.ap_add_check <- function(acc, name, description, status, n_issues, details) {
  acc$checks[[length(acc$checks) + 1L]] <- list(
    check_name        = name,
    check_description = description,
    status            = status,
    n_issues          = as.integer(n_issues),
    details           = if (is.null(details) || (length(details) == 1L && is.na(details)))
                          NA_character_ else as.character(details)
  )
  invisible(NULL)
}

#' @keywords internal
#' @noRd
.ap_add_issues <- function(acc, rows, issue_type, kind = NA_character_) {
  if (is.null(rows) || nrow(rows) == 0L) return(invisible(NULL))
  rows$issue_type <- issue_type
  rows$source_kind <- kind
  acc$issues <- dplyr::bind_rows(acc$issues, rows)
  invisible(NULL)
}

#' @keywords internal
#' @noRd
.ap_check_required_cols <- function(df, required, name, description, acc) {
  missing_cols <- setdiff(required, names(df))
  status <- if (length(missing_cols) == 0L) "PASS" else "ERROR"
  details <- if (length(missing_cols) > 0L)
               paste("Missing:", paste(missing_cols, collapse = ", "))
             else NA_character_
  .ap_add_check(acc, name, description, status, length(missing_cols), details)
}

#' @keywords internal
#' @noRd
.ap_check_no_negative_funding <- function(df, funding_cols, acc, kind) {
  if (length(funding_cols) == 0L) {
    .ap_add_check(acc, "no_negative_funding",
                   "No negative values in funding/award columns",
                   "PASS", 0L, NA_character_)
    return(invisible(NULL))
  }
  bad <- rep(FALSE, nrow(df))
  per_col_counts <- integer(length(funding_cols))
  names(per_col_counts) <- funding_cols
  for (j in seq_along(funding_cols)) {
    col <- df[[funding_cols[j]]]
    if (is.numeric(col)) {
      neg_j <- !is.na(col) & col < 0
      per_col_counts[j] <- sum(neg_j)
      bad <- bad | neg_j
    }
  }
  n <- sum(bad)
  status <- if (n == 0L) "PASS" else "ERROR"
  details <- if (n > 0L) {
    parts <- per_col_counts[per_col_counts > 0L]
    paste0(n, " negative value(s): ",
            paste(sprintf("%s=%d", names(parts), parts), collapse = ", "))
  } else NA_character_
  .ap_add_check(acc, "no_negative_funding",
                 "No negative values in funding/award columns",
                 status, n, details)
  if (n > 0L) {
    rows <- df[bad, intersect(c("organization_name", "project_name",
                                  "county", funding_cols), names(df)),
                drop = FALSE]
    .ap_add_issues(acc, tibble::as_tibble(rows), "negative_funding", kind)
  }
}

#' @keywords internal
#' @noRd
.ap_check_tier_adjustment_range <- function(df, acc) {
  if (!"tier_adjustment" %in% names(df)) {
    .ap_add_check(acc, "tier_adjustment_range",
                   "tier_adjustment within +/- $50,000",
                   "PASS", 0L, NA_character_)
    return(invisible(NULL))
  }
  ta <- df$tier_adjustment
  bad <- !is.na(ta) & abs(ta) > 50000
  n <- sum(bad)
  status <- if (n == 0L) "PASS" else "WARN"
  details <- if (n > 0L) sprintf("%d row(s) outside +/-$50,000", n) else NA_character_
  .ap_add_check(acc, "tier_adjustment_range",
                 "tier_adjustment within +/- $50,000",
                 status, n, details)
}

#' @keywords internal
#' @noRd
.ap_check_draft_award_reconciles <- function(df, tolerance, acc) {
  need <- c("draft_award", "draft_base_award", "tier_adjustment")
  if (!all(need %in% names(df))) {
    .ap_add_check(acc, "draft_award_reconciles",
                   "draft_award = draft_base_award + tier_adjustment",
                   "PASS", 0L, NA_character_)
    return(invisible(NULL))
  }
  da <- df$draft_award; db <- df$draft_base_award; ta <- df$tier_adjustment
  diff <- da - (db + ta)
  bad <- !is.na(diff) & abs(diff) > tolerance
  n <- sum(bad)
  status <- if (n == 0L) "PASS" else "WARN"
  details <- if (n > 0L)
               sprintf("%d row(s) mismatch beyond $%.2f", n, tolerance)
             else NA_character_
  .ap_add_check(acc, "draft_award_reconciles",
                 sprintf("draft_award = draft_base_award + tier_adjustment (within $%.2f)",
                         tolerance),
                 status, n, details)
}

#' @keywords internal
#' @noRd
.ap_check_total_award_reconciles <- function(df, tolerance, acc) {
  need <- c("total_award", "award_other", "new_classroom_award")
  if (!all(need %in% names(df))) {
    .ap_add_check(acc, "total_award_reconciles",
                   "total_award = award_other + new_classroom_award",
                   "PASS", 0L, NA_character_)
    return(invisible(NULL))
  }
  diff <- df$total_award - (df$award_other + df$new_classroom_award)
  bad <- !is.na(diff) & abs(diff) > tolerance
  n <- sum(bad)
  status <- if (n == 0L) "PASS" else "WARN"
  details <- if (n > 0L)
               sprintf("%d row(s) mismatch beyond $%.2f", n, tolerance)
             else NA_character_
  .ap_add_check(acc, "total_award_reconciles",
                 sprintf("total_award = award_other + new_classroom_award (within $%.2f)",
                         tolerance),
                 status, n, details)
}

#' @keywords internal
#' @noRd
.ap_check_county_in_alabama <- function(df, acc, kind) {
  if (!"county" %in% names(df)) {
    .ap_add_check(acc, "county_in_alabama",
                   "All county values are Alabama counties",
                   "PASS", 0L, NA_character_)
    return(invisible(NULL))
  }
  county_codes_path <- system.file("extdata", "codebooks", "county_codes.csv",
                                     package = "ALprekDB")
  if (!nzchar(county_codes_path)) {
    .ap_add_check(acc, "county_in_alabama",
                   "All county values are Alabama counties",
                   "WARN", 0L, "county_codes.csv codebook not found")
    return(invisible(NULL))
  }
  cc <- suppressMessages(readr::read_csv(county_codes_path,
                                            show_col_types = FALSE,
                                            progress = FALSE))
  valid <- tolower(trimws(cc$county_name))
  observed <- tolower(trimws(df$county))
  bad <- !is.na(observed) & nzchar(observed) & !(observed %in% valid)
  n <- sum(bad)
  status <- if (n == 0L) "PASS" else "ERROR"
  details <- if (n > 0L) {
    sprintf("%d row(s) with non-Alabama county: %s", n,
             paste(unique(df$county[bad])[seq_len(min(3L, length(unique(df$county[bad]))))],
                    collapse = ", "))
  } else NA_character_
  .ap_add_check(acc, "county_in_alabama",
                 "All county values are Alabama counties",
                 status, n, details)
  if (n > 0L) {
    rows <- df[bad, intersect(c("organization_name", "project_name", "county"),
                                names(df)), drop = FALSE]
    .ap_add_issues(acc, tibble::as_tibble(rows), "non_alabama_county", kind)
  }
}

#' @keywords internal
#' @noRd
.ap_check_funding_type_in_codebook <- function(df, acc, kind,
                                                  fund_col = "funding_type") {
  cols_to_check <- intersect(c(fund_col,
                                if (fund_col == "funding_type") "funding_type_prior" else NULL),
                                names(df))
  if (length(cols_to_check) == 0L) {
    .ap_add_check(acc, "funding_type_in_codebook",
                   "funding_type values match codebook",
                   "PASS", 0L, NA_character_)
    return(invisible(NULL))
  }
  fct <- tryCatch(alprek_applications_funding_types(),
                   error = function(e) NULL)
  if (is.null(fct)) {
    .ap_add_check(acc, "funding_type_in_codebook",
                   "funding_type values match codebook",
                   "WARN", 0L, "funding_types codebook not loadable")
    return(invisible(NULL))
  }
  valid <- tolower(trimws(fct$funding_type))
  total_bad <- 0L
  unique_bad <- character(0)
  for (col in cols_to_check) {
    observed <- tolower(trimws(df[[col]]))
    bad <- !is.na(observed) & nzchar(observed) & !(observed %in% valid)
    total_bad <- total_bad + sum(bad)
    unique_bad <- union(unique_bad, unique(df[[col]][bad]))
  }
  status <- if (total_bad == 0L) "PASS" else "WARN"
  details <- if (total_bad > 0L) {
    sprintf("%d row(s) with non-codebook funding_type: %s", total_bad,
             paste(unique_bad[seq_len(min(3L, length(unique_bad)))],
                    collapse = "; "))
  } else NA_character_
  .ap_add_check(acc, "funding_type_in_codebook",
                 sprintf("%s values match codebook",
                          paste(cols_to_check, collapse = "/")),
                 status, total_bad, details)
}

#' @keywords internal
#' @noRd
.ap_check_region_format <- function(df, acc, kind) {
  if (!"region" %in% names(df)) {
    .ap_add_check(acc, "region_format_valid",
                   "region matches 'Region 1' .. 'Region 9'",
                   "PASS", 0L, NA_character_)
    return(invisible(NULL))
  }
  observed <- trimws(df$region)
  bad <- !is.na(observed) & nzchar(observed) &
          !grepl("^Region\\s*[1-9]$", observed, ignore.case = TRUE)
  n <- sum(bad)
  status <- if (n == 0L) "PASS" else "WARN"
  details <- if (n > 0L) {
    sprintf("%d row(s) with non-canonical region: %s", n,
             paste(unique(df$region[bad])[seq_len(min(3L, length(unique(df$region[bad]))))],
                    collapse = ", "))
  } else NA_character_
  .ap_add_check(acc, "region_format_valid",
                 "region matches 'Region 1' .. 'Region 9'",
                 status, n, details)
}

#' @keywords internal
#' @noRd
.ap_check_process_name_in_codebook <- function(df, acc, kind) {
  if (!"process_name" %in% names(df)) {
    .ap_add_check(acc, "process_name_in_codebook",
                   "process_name in status_codes codebook",
                   "PASS", 0L, NA_character_)
    return(invisible(NULL))
  }
  sc <- tryCatch(alprek_applications_status_codes(),
                  error = function(e) NULL)
  if (is.null(sc)) {
    .ap_add_check(acc, "process_name_in_codebook",
                   "process_name in status_codes codebook",
                   "WARN", 0L, "status codes codebook not loadable")
    return(invisible(NULL))
  }
  valid <- tolower(trimws(sc$process_name))
  observed <- tolower(trimws(df$process_name))
  bad <- !is.na(observed) & nzchar(observed) & !(observed %in% valid)
  n <- sum(bad)
  status <- if (n == 0L) "PASS" else "WARN"
  details <- if (n > 0L) {
    sprintf("%d row(s) with non-codebook process_name", n)
  } else NA_character_
  .ap_add_check(acc, "process_name_in_codebook",
                 "process_name in status_codes codebook",
                 status, n, details)
}

#' @keywords internal
#' @noRd
.ap_check_provenance <- function(df, meta, acc) {
  has_sha <- !is.null(meta$file_sha256) && nzchar(meta$file_sha256)
  has_source <- "data_source" %in% names(df) &&
                  !any(is.na(df$data_source) | !nzchar(df$data_source))
  ok <- has_sha && has_source
  status <- if (ok) "PASS" else "ERROR"
  details <- if (!ok) {
    parts <- character(0)
    if (!has_sha) parts <- c(parts, "missing meta$file_sha256")
    if (!has_source) parts <- c(parts, "missing/NA data_source")
    paste(parts, collapse = "; ")
  } else NA_character_
  .ap_add_check(acc, "provenance_recorded",
                 "File SHA-256 + per-row data_source recorded",
                 status, as.integer(!ok), details)
}

#' @keywords internal
#' @noRd
.ap_check_lineage_present <- function(df, meta, acc, kind) {
  required <- c("raw_row_index", "lineage_id")
  missing_cols <- setdiff(required, names(df))
  has_git <- !is.null(meta$git_sha) &&
    length(meta$git_sha) > 0L &&
    !is.na(meta$git_sha[1]) &&
    nzchar(as.character(meta$git_sha[1]))

  bad <- rep(FALSE, nrow(df))
  dup <- rep(FALSE, nrow(df))
  if (length(missing_cols) == 0L) {
    raw_missing <- is.na(df$raw_row_index)
    lineage_missing <- is.na(df$lineage_id) |
      !nzchar(trimws(as.character(df$lineage_id)))
    bad <- raw_missing | lineage_missing
    dup <- !bad & duplicated(df$lineage_id)
  }

  n <- length(missing_cols) + sum(bad, na.rm = TRUE) +
    sum(dup, na.rm = TRUE) + as.integer(!has_git)
  status <- if (n == 0L) "PASS" else "ERROR"
  details <- if (n > 0L) {
    parts <- character(0)
    if (length(missing_cols) > 0L) {
      parts <- c(parts, paste("missing columns:", paste(missing_cols, collapse = ", ")))
    }
    if (sum(bad, na.rm = TRUE) > 0L) {
      parts <- c(parts, sprintf("%d row(s) missing row lineage", sum(bad, na.rm = TRUE)))
    }
    if (sum(dup, na.rm = TRUE) > 0L) {
      parts <- c(parts, sprintf("%d duplicate lineage_id value(s)", sum(dup, na.rm = TRUE)))
    }
    if (!has_git) {
      parts <- c(parts, "missing meta$git_sha")
    }
    paste(parts, collapse = "; ")
  } else NA_character_

  .ap_add_check(acc, "row_lineage_recorded",
                 "raw_row_index + lineage_id + git_sha recorded",
                 status, n, details)
  if (length(missing_cols) == 0L && any(bad | dup, na.rm = TRUE)) {
    rows <- df[bad | dup, intersect(c("raw_row_index", "lineage_id",
                                      "data_source", "application_id",
                                      "organization_name", "project_name",
                                      "site_code", "site_name"),
                                    names(df)), drop = FALSE]
    .ap_add_issues(acc, tibble::as_tibble(rows), "row_lineage_invalid", kind)
  }
}

#' @keywords internal
#' @noRd
.ap_check_row_count_positive <- function(df, acc, kind) {
  n <- nrow(df)
  status <- if (n > 0L) "PASS" else "ERROR"
  details <- if (n == 0L) "data has zero rows" else NA_character_
  .ap_add_check(acc, "row_count_positive",
                 "Cleaned data has at least one row",
                 status, as.integer(n == 0L), details)
}

#' @keywords internal
#' @noRd
.ap_check_county_coverage_breadth <- function(df, acc, kind) {
  if (!"county" %in% names(df)) {
    .ap_add_check(acc, "county_coverage_breadth",
                   "Counties represented",
                   "PASS", 0L, NA_character_)
    return(invisible(NULL))
  }
  k <- dplyr::n_distinct(df$county[!is.na(df$county)])
  status <- if (k >= 30L) "PASS" else "INFO"
  details <- if (k < 30L) sprintf("only %d distinct counties", k) else
             sprintf("%d distinct counties", k)
  .ap_add_check(acc, "county_coverage_breadth",
                 "Counties represented (>= 30 expected statewide)",
                 status, as.integer(k < 30L), details)
}

#' @keywords internal
#' @noRd
.ap_check_capacity_site_code_present <- function(df, acc) {
  if (!"site_code" %in% names(df)) {
    .ap_add_check(acc, "capacity_site_code_present",
                   "Capacity rows have non-missing site_code",
                   "ERROR", nrow(df), "site_code column missing")
    return(invisible(NULL))
  }
  site_code <- trimws(as.character(df$site_code))
  bad <- is.na(df$site_code) | !nzchar(site_code)
  n <- sum(bad, na.rm = TRUE)
  status <- if (n == 0L) "PASS" else "ERROR"
  details <- if (n > 0L) sprintf("%d row(s) missing site_code", n) else NA_character_
  .ap_add_check(acc, "capacity_site_code_present",
                 "Capacity rows have non-missing site_code",
                 status, n, details)
  if (n > 0L) {
    rows <- df[bad, intersect(c("raw_row_index", "lineage_id", "site_code",
                                "site_name", "capacity", "enrollment"),
                              names(df)), drop = FALSE]
    .ap_add_issues(acc, tibble::as_tibble(rows), "capacity_site_code_missing",
                    "capacity")
  }
}

#' @keywords internal
#' @noRd
.ap_check_capacity_positive <- function(df, acc) {
  if (!"capacity" %in% names(df)) {
    .ap_add_check(acc, "capacity_positive",
                   "Capacity > 0 where reported",
                   "PASS", 0L, NA_character_)
    return(invisible(NULL))
  }
  cap <- df$capacity
  bad <- !is.na(cap) & cap <= 0
  n <- sum(bad)
  status <- if (n == 0L) "PASS" else "ERROR"
  details <- if (n > 0L) sprintf("%d row(s) with capacity <= 0", n) else NA_character_
  .ap_add_check(acc, "capacity_positive",
                 "Capacity > 0 where reported",
                 status, n, details)
  if (n > 0L) {
    rows <- df[bad, intersect(c("site_code", "site_name", "capacity"),
                                names(df)), drop = FALSE]
    .ap_add_issues(acc, tibble::as_tibble(rows), "capacity_non_positive",
                    "capacity")
  }
}

#' @keywords internal
#' @noRd
.ap_check_enrollment_le_capacity <- function(df, acc) {
  if (!all(c("enrollment", "capacity") %in% names(df))) {
    .ap_add_check(acc, "enrollment_le_capacity",
                   "enrollment <= capacity",
                   "PASS", 0L, NA_character_)
    return(invisible(NULL))
  }
  bad <- !is.na(df$enrollment) & !is.na(df$capacity) & df$enrollment > df$capacity
  n <- sum(bad)
  status <- if (n == 0L) "PASS" else "WARN"
  details <- if (n > 0L) sprintf("%d site(s) with enrollment > capacity", n) else NA_character_
  .ap_add_check(acc, "enrollment_le_capacity",
                 "enrollment <= capacity",
                 status, n, details)
  if (n > 0L) {
    rows <- df[bad, intersect(c("site_code", "site_name",
                                  "enrollment", "capacity"), names(df)),
                drop = FALSE]
    .ap_add_issues(acc, tibble::as_tibble(rows), "enrollment_exceeds_capacity",
                    "capacity")
  }
}

#' @keywords internal
#' @noRd
.ap_check_waitlist_non_negative <- function(df, acc) {
  cols <- intersect(c("waitlist", "n_classrooms"), names(df))
  if (length(cols) == 0L) {
    .ap_add_check(acc, "waitlist_non_negative",
                   "waitlist and n_classrooms >= 0",
                   "PASS", 0L, NA_character_)
    return(invisible(NULL))
  }
  bad <- rep(FALSE, nrow(df))
  for (col in cols) {
    bad <- bad | (!is.na(df[[col]]) & df[[col]] < 0)
  }
  n <- sum(bad)
  status <- if (n == 0L) "PASS" else "ERROR"
  details <- if (n > 0L) sprintf("%d row(s) with negative waitlist or n_classrooms", n)
             else NA_character_
  .ap_add_check(acc, "waitlist_non_negative",
                 "waitlist and n_classrooms >= 0",
                 status, n, details)
}

#' @keywords internal
#' @noRd
.ap_check_application_id_unique <- function(df, acc) {
  if (!"application_id" %in% names(df)) {
    .ap_add_check(acc, "application_id_unique",
                   "application_id present and unique",
                   "ERROR", nrow(df), "application_id column missing")
    return(invisible(NULL))
  }
  app_id <- trimws(as.character(df$application_id))
  bad <- is.na(df$application_id) | !nzchar(app_id)
  dup <- !bad & duplicated(app_id)
  n <- sum(bad, na.rm = TRUE) + sum(dup, na.rm = TRUE)
  status <- if (n == 0L) "PASS" else "ERROR"
  details <- if (n > 0L) {
    sprintf("%d missing/duplicate application_id row(s)", n)
  } else NA_character_
  .ap_add_check(acc, "application_id_unique",
                 "application_id present and unique",
                 status, n, details)
  if (n > 0L) {
    rows <- df[bad | dup, intersect(c("application_id", "source_sheet",
                                      "raw_row_index", "lineage_id",
                                      "organization_name", "project_name"),
                                    names(df)), drop = FALSE]
    .ap_add_issues(acc, tibble::as_tibble(rows), "application_id_invalid",
                    "reconciled")
  }
}

#' @keywords internal
#' @noRd
.ap_check_every_row_has_bucket <- function(df, acc) {
  if (!"bucket" %in% names(df)) {
    .ap_add_check(acc, "every_row_has_bucket",
                   "Every row has a bucket assignment (A/B/C/D)",
                   "ERROR", nrow(df), "bucket column missing")
    return(invisible(NULL))
  }
  ok <- df$bucket %in% c("A", "B", "C", "D")
  n <- sum(!ok)
  status <- if (n == 0L) "PASS" else "ERROR"
  details <- if (n > 0L) sprintf("%d row(s) without valid bucket", n) else NA_character_
  .ap_add_check(acc, "every_row_has_bucket",
                 "Every row has a bucket assignment (A/B/C/D)",
                 status, n, details)
}

#' @keywords internal
#' @noRd
.ap_check_reconciliation_log_coverage <- function(df, log_df, acc) {
  if (is.null(log_df) || !is.data.frame(log_df)) {
    .ap_add_check(acc, "reconciliation_log_coverage",
                   "Reconciliation audit log has one chosen decision per row",
                   "ERROR", nrow(df), "reconciliation_log missing")
    return(invisible(NULL))
  }
  required <- c("application_id", "match_method", "decision_source",
                "decision_seed")
  missing_cols <- setdiff(required, names(log_df))
  if (length(missing_cols) > 0L || !"application_id" %in% names(df)) {
    n <- length(missing_cols) + as.integer(!"application_id" %in% names(df))
    details <- paste(c(
      if (length(missing_cols) > 0L)
        paste("audit log missing:", paste(missing_cols, collapse = ", ")),
      if (!"application_id" %in% names(df)) "reconciled data missing application_id"
    ), collapse = "; ")
    .ap_add_check(acc, "reconciliation_log_coverage",
                   "Reconciliation audit log has one chosen decision per row",
                   "ERROR", n, details)
    return(invisible(NULL))
  }

  chosen_methods <- c("exact", "fuzzy_auto", "no_match", "no_panel")
  allowed_methods <- c(chosen_methods, "fuzzy_candidate")
  log_ids <- as.character(log_df$application_id)
  app_ids <- as.character(df$application_id)
  chosen <- log_df[log_df$match_method %in% chosen_methods, , drop = FALSE]
  chosen_counts <- table(as.character(chosen$application_id))
  candidate <- log_df[log_df$match_method == "fuzzy_candidate", , drop = FALSE]
  candidate_counts <- table(as.character(candidate$application_id))

  missing_chosen <- setdiff(app_ids, names(chosen_counts))
  duplicate_chosen <- names(chosen_counts)[chosen_counts > 1L]
  duplicate_chosen <- intersect(duplicate_chosen, app_ids)
  extra_chosen <- setdiff(names(chosen_counts), app_ids)
  too_many_candidates <- names(candidate_counts)[candidate_counts > 3L]
  unknown_methods <- setdiff(unique(as.character(log_df$match_method)),
                             allowed_methods)
  missing_source <- is.na(log_df$decision_source) |
    !nzchar(trimws(as.character(log_df$decision_source)))
  missing_seed <- is.na(log_df$decision_seed)

  n <- length(missing_chosen) + length(duplicate_chosen) +
    length(extra_chosen) + length(too_many_candidates) +
    length(unknown_methods) + sum(missing_source, na.rm = TRUE) +
    sum(missing_seed, na.rm = TRUE)
  status <- if (n == 0L) "PASS" else "ERROR"
  details <- if (n > 0L) {
    parts <- character(0)
    if (length(missing_chosen) > 0L) {
      parts <- c(parts, sprintf("%d application(s) missing chosen decision",
                                length(missing_chosen)))
    }
    if (length(duplicate_chosen) > 0L) {
      parts <- c(parts, sprintf("%d application(s) with duplicate chosen decisions",
                                length(duplicate_chosen)))
    }
    if (length(extra_chosen) > 0L) {
      parts <- c(parts, sprintf("%d extra chosen decision id(s)", length(extra_chosen)))
    }
    if (length(too_many_candidates) > 0L) {
      parts <- c(parts, sprintf("%d application(s) with >3 fuzzy candidates",
                                length(too_many_candidates)))
    }
    if (length(unknown_methods) > 0L) {
      parts <- c(parts, paste("unknown method(s):",
                              paste(unknown_methods, collapse = ", ")))
    }
    if (sum(missing_source, na.rm = TRUE) > 0L) {
      parts <- c(parts, sprintf("%d log row(s) missing decision_source",
                                sum(missing_source, na.rm = TRUE)))
    }
    if (sum(missing_seed, na.rm = TRUE) > 0L) {
      parts <- c(parts, sprintf("%d log row(s) missing decision_seed",
                                sum(missing_seed, na.rm = TRUE)))
    }
    paste(parts, collapse = "; ")
  } else NA_character_
  .ap_add_check(acc, "reconciliation_log_coverage",
                 "Reconciliation audit log has one chosen decision per row",
                 status, n, details)

  issue_ids <- union(union(missing_chosen, duplicate_chosen), too_many_candidates)
  if (length(issue_ids) > 0L) {
    rows <- df[app_ids %in% issue_ids,
               intersect(c("application_id", "source_sheet", "bucket",
                           "match_method", "raw_row_index", "lineage_id",
                           "organization_name", "project_name"),
                         names(df)), drop = FALSE]
    .ap_add_issues(acc, tibble::as_tibble(rows),
                    "reconciliation_log_coverage", "reconciled")
  }
}

#' @keywords internal
#' @noRd
.ap_check_match_score_in_unit_interval <- function(df, acc) {
  if (!"match_score" %in% names(df)) {
    .ap_add_check(acc, "match_score_in_unit_interval",
                   "match_score in [0, 1]",
                   "PASS", 0L, NA_character_)
    return(invisible(NULL))
  }
  ms <- df$match_score
  bad <- !is.na(ms) & (ms < 0 | ms > 1)
  n <- sum(bad)
  status <- if (n == 0L) "PASS" else "ERROR"
  details <- if (n > 0L) sprintf("%d row(s) with match_score outside [0,1]", n)
             else NA_character_
  .ap_add_check(acc, "match_score_in_unit_interval",
                 "match_score in [0, 1]",
                 status, n, details)
}


#' @keywords internal
#' @noRd
.ap_assemble_result <- function(checks, issues, kind, strict) {
  checks_df <- if (length(checks) > 0L) {
    dplyr::bind_rows(lapply(checks, tibble::as_tibble))
  } else {
    tibble::tibble(check_name = character(0),
                    check_description = character(0),
                    status = character(0),
                    n_issues = integer(0),
                    details = character(0))
  }

  n_errors   <- sum(checks_df$status == "ERROR")
  n_warnings <- sum(checks_df$status == "WARN")
  n_info     <- sum(checks_df$status == "INFO")

  passed <- if (isTRUE(strict)) {
    n_errors == 0L && n_warnings == 0L
  } else {
    n_errors == 0L
  }

  structure(list(
    passed     = passed,
    n_errors   = as.integer(n_errors),
    n_warnings = as.integer(n_warnings),
    n_info     = as.integer(n_info),
    kind       = kind,
    checks     = checks_df,
    issues     = tibble::as_tibble(issues)
  ), class = "alprek_applications_validation")
}


# ============================================================================
# Linkage-specific validation (Step 6.3)
# ============================================================================

#' Validate an `alprek_applications_linkage` object.
#'
#' Checks:
#' * `renewals_have_classroom_code` - Every `app_is_renewal == TRUE` row in
#'   `classroom_level` must have non-NA `classroom_code` (the join key).
#' * `new_apps_classroom_code_optional` - `app_is_new == TRUE` rows MAY have
#'   NA `matched_classroom_code` (bucket D was routed to unmatched_applications,
#'   so classroom_level new apps should have classroom_code via site_code
#'   aggregation - informational only).
#' * `tier_prev_renewal_only` - `app_tier_prev_*` columns should be NA when
#'   `app_is_renewal != TRUE`.
#' * `unmatched_bucket_is_d` - Every row in `unmatched_applications` must
#'   have bucket/app_bucket `%in% c("D","unknown")`.
#' * `linkage_lineage_recorded` - linked and unmatched application rows retain
#'   row-level lineage IDs.
#' * `diagnostics_consistent` - classroom and application diagnostics add up.
#'
#' @keywords internal
#' @noRd
.ap_validate_linkage <- function(x, acc) {
  cl <- x$classroom_level
  ua <- x$unmatched_applications
  diag <- x$diagnostics

  # 1. renewals_have_classroom_code
  if ("app_is_renewal" %in% names(cl)) {
    is_ren <- isTRUE(any(cl$app_is_renewal)) | any(cl$app_is_renewal %in% TRUE)
    if (is_ren) {
      bad <- which(cl$app_is_renewal %in% TRUE &
                     (is.na(cl$classroom_code) | !nzchar(cl$classroom_code)))
      n <- length(bad)
      .ap_add_check(acc, "renewals_have_classroom_code",
                     "is_renewal rows have classroom_code",
                     if (n == 0L) "PASS" else "ERROR", n,
                     if (n > 0L)
                       sprintf("%d renewal row(s) without classroom_code", n)
                     else NA_character_)
      if (n > 0L) {
        rows <- cl[bad, intersect(c("app_application_id", "site_code",
                                       "app_organization_name",
                                       "app_project_name"), names(cl)),
                    drop = FALSE]
        .ap_add_issues(acc, tibble::as_tibble(rows),
                        "renewal_missing_classroom_code", "linkage")
      }
    } else {
      .ap_add_check(acc, "renewals_have_classroom_code",
                     "is_renewal rows have classroom_code",
                     "PASS", 0L, "no renewals in classroom_level")
    }
  } else {
    .ap_add_check(acc, "renewals_have_classroom_code",
                   "is_renewal rows have classroom_code",
                   "PASS", 0L, "app_is_renewal column absent")
  }

  # 2. new_apps may have classroom_code via site aggregation (informational)
  if ("site_n_new_apps" %in% names(cl)) {
    n_with_new <- sum(cl$site_n_new_apps > 0L, na.rm = TRUE)
    .ap_add_check(acc, "new_apps_attached_to_sites",
                   "Sites carrying new applications via site_code agg",
                   "INFO", as.integer(n_with_new),
                   sprintf("%d classrooms at sites with new apps",
                            n_with_new))
  } else {
    .ap_add_check(acc, "new_apps_attached_to_sites",
                   "Sites carrying new applications via site_code agg",
                   "PASS", 0L, "no new-app site aggregation present")
  }

  # 3. tier_prev_renewal_only
  if ("app_tier_prev_band" %in% names(cl) &&
      "app_is_renewal" %in% names(cl)) {
    bad <- which(!isTRUE(any(cl$app_is_renewal %in% TRUE)) | FALSE)  # placeholder
    bad <- which(!(cl$app_is_renewal %in% TRUE) &
                   !is.na(cl$app_tier_prev_band))
    n <- length(bad)
    .ap_add_check(acc, "tier_prev_renewal_only",
                   "tier_prev_* present only for renewals",
                   if (n == 0L) "PASS" else "WARN", n,
                   if (n > 0L)
                     sprintf("%d non-renewal row(s) have tier_prev_band", n)
                   else NA_character_)
  } else {
    .ap_add_check(acc, "tier_prev_renewal_only",
                   "tier_prev_* present only for renewals",
                   "PASS", 0L, "tier_prev columns absent")
  }

  # 4. unmatched_bucket_is_d
  if (!is.null(ua) && nrow(ua) > 0L) {
    bucket_col <- intersect(c("app_bucket", "bucket"), names(ua))[1]
    if (!is.na(bucket_col)) {
      bucket_values <- ua[[bucket_col]]
      bad <- !(bucket_values %in% c("D", "unknown"))
      n <- sum(bad)
      .ap_add_check(acc, "unmatched_bucket_is_d",
                     "Unmatched applications are bucket D or unknown",
                     if (n == 0L) "PASS" else "ERROR", n,
                     if (n > 0L)
                       sprintf("%d unmatched row(s) in unexpected bucket: %s",
                                n,
                                paste(unique(bucket_values[bad])[1:min(3, sum(bad))],
                                      collapse = ", "))
                     else NA_character_)
    } else {
      .ap_add_check(acc, "unmatched_bucket_is_d",
                     "Unmatched applications are bucket D or unknown",
                     "ERROR", nrow(ua),
                     "unmatched_applications lacks bucket/app_bucket column")
    }
  } else {
    .ap_add_check(acc, "unmatched_bucket_is_d",
                   "Unmatched applications are bucket D or unknown",
                   "PASS", 0L, "unmatched_applications empty")
  }

  # 5. linkage_lineage_recorded
  linked_has_app <- if ("app_application_id" %in% names(cl)) {
    !is.na(cl$app_application_id) &
      nzchar(trimws(as.character(cl$app_application_id)))
  } else {
    rep(FALSE, nrow(cl))
  }
  linked_bad <- if ("app_lineage_id" %in% names(cl)) {
    linked_has_app & (is.na(cl$app_lineage_id) |
      !nzchar(trimws(as.character(cl$app_lineage_id))))
  } else {
    linked_has_app
  }

  unmatched_bad <- rep(FALSE, if (is.null(ua)) 0L else nrow(ua))
  if (!is.null(ua) && nrow(ua) > 0L) {
    ua_lineage_col <- intersect(c("app_lineage_id", "lineage_id"), names(ua))[1]
    if (is.na(ua_lineage_col)) {
      unmatched_bad <- rep(TRUE, nrow(ua))
    } else {
      lineage_values <- ua[[ua_lineage_col]]
      unmatched_bad <- is.na(lineage_values) |
        !nzchar(trimws(as.character(lineage_values)))
    }
  }
  n_lineage <- sum(linked_bad, na.rm = TRUE) + sum(unmatched_bad, na.rm = TRUE)
  .ap_add_check(acc, "linkage_lineage_recorded",
                 "Linked/unmatched application rows retain lineage_id",
                 if (n_lineage == 0L) "PASS" else "ERROR",
                 n_lineage,
                 if (n_lineage > 0L)
                   sprintf("%d linked/unmatched application row(s) lack lineage",
                           n_lineage)
                 else NA_character_)

  # 6. diagnostics_consistent
  if (!is.null(diag) && all(c("metric", "value") %in% names(diag))) {
    m <- setNames(diag$value, diag$metric)
    class_expected <- m["n_matched_to_classroom"] + m["n_only_classroom"]
    actual_class_rows <- m["n_classroom_rows"]
    bad_class <- !is.na(class_expected) & !is.na(actual_class_rows) &
      (class_expected != actual_class_rows)

    app_needed <- c("n_applications_in", "n_applications_direct_classroom",
                    "n_applications_site_aggregated",
                    "n_only_application_unmatched")
    has_app_diag <- all(app_needed %in% names(m))
    bad_app <- FALSE
    app_expected <- NA_real_
    actual_apps <- NA_real_
    if (has_app_diag) {
      app_expected <- m["n_applications_direct_classroom"] +
        m["n_applications_site_aggregated"] +
        m["n_only_application_unmatched"]
      actual_apps <- m["n_applications_in"]
      bad_app <- !is.na(app_expected) & !is.na(actual_apps) &
        (app_expected != actual_apps)
    }
    bad <- bad_class || bad_app || !has_app_diag
    detail <- if (bad) {
      parts <- character(0)
      if (bad_class) {
        parts <- c(parts, sprintf("classroom rows: expected %d, got %d",
                                  class_expected, actual_class_rows))
      }
      if (!has_app_diag) {
        parts <- c(parts, "application-side diagnostics absent")
      } else if (bad_app) {
        parts <- c(parts, sprintf("application rows: expected %d, got %d",
                                  app_expected, actual_apps))
      }
      paste(parts, collapse = "; ")
    } else NA_character_
    .ap_add_check(acc, "diagnostics_consistent",
                   "classroom and application diagnostics are internally consistent",
                   if (!bad) "PASS" else "ERROR",
                   as.integer(bad),
                   detail)
  } else {
    .ap_add_check(acc, "diagnostics_consistent",
                   "classroom and application diagnostics are internally consistent",
                   "PASS", 0L, "diagnostics absent")
  }

  # 7. row_count_positive
  if (nrow(cl) == 0L) {
    .ap_add_check(acc, "row_count_positive",
                   "classroom_level has at least one row",
                   "WARN", 1L, "classroom_level is empty")
  } else {
    .ap_add_check(acc, "row_count_positive",
                   "classroom_level has at least one row",
                   "PASS", 0L, NA_character_)
  }
}
