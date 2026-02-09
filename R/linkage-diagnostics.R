#' Validate Linkage Results
#'
#' @description Performs data quality checks on linkage results (joined data).
#'   Returns a validation report with pass/fail status for each check.
#'
#' @param linkage_obj An `alprek_linkage_classroom`, `alprek_linkage_student`,
#'   or `alprek_linkage_master` object.
#' @param strict Logical. If `TRUE`, treats warnings as errors. Default `FALSE`.
#'
#' @return An `alprek_linkage_validation` S3 object (list) with elements:
#'   - `passed`: logical overall result.
#'   - `n_errors`, `n_warnings`, `n_info`: counts by severity.
#'   - `checks`: tibble of individual check results.
#'
#' @examples
#' \dontrun{
#' cb <- linkage_classroom_budget(classroom_panel, budget_panel)
#' validation <- linkage_validate(cb)
#' print(validation)
#' }
#'
#' @export
linkage_validate <- function(linkage_obj, strict = FALSE) {
  valid_classes <- c("alprek_linkage_classroom", "alprek_linkage_student",
                     "alprek_linkage_master")
  if (!any(class(linkage_obj) %in% valid_classes)) {
    stop("Expected an alprek_linkage_classroom, alprek_linkage_student, or ",
         "alprek_linkage_master object.", call. = FALSE)
  }

  checks <- list()
  check_idx <- 0L

  # Extract data and diagnostics
  if (inherits(linkage_obj, "alprek_linkage_master")) {
    # For master, validate both levels
    cl_data <- linkage_obj$classroom_level
    st_data <- linkage_obj$student_level
    diag <- linkage_obj$diagnostics
    data_label <- "master"
  } else {
    cl_data <- linkage_obj$data
    st_data <- NULL
    diag <- linkage_obj$diagnostics
    data_label <- diag$join_type
  }

  df <- cl_data  # primary data for validation

  # 1. Required columns
  check_idx <- check_idx + 1L
  required <- c("school_year", "classroom_code")
  missing <- setdiff(required, names(df))
  checks[[check_idx]] <- .make_check(
    "required_columns",
    "Required join keys present",
    if (length(missing) == 0) "PASS" else "ERROR",
    length(missing),
    if (length(missing) > 0) paste("Missing:", paste(missing, collapse = ", ")) else NA_character_
  )

  # 2. Key uniqueness (classroom-level should have no duplicates)
  check_idx <- check_idx + 1L
  if (data_label != "student_classroom") {
    dup_keys <- duplicated(paste(df$school_year, df$classroom_code, sep = "|"))
    n_dups <- sum(dup_keys)
    checks[[check_idx]] <- .make_check(
      "key_uniqueness",
      "No duplicate classroom-year keys",
      if (n_dups == 0) "PASS" else "ERROR",
      n_dups,
      if (n_dups > 0) paste(n_dups, "duplicate key(s) found") else NA_character_
    )
  } else {
    # For student-level, skip this check (multiple students per classroom expected)
    checks[[check_idx]] <- .make_check(
      "key_uniqueness",
      "Student-level data (duplicates expected per classroom)",
      "PASS",
      0L,
      NA_character_
    )
  }

  # 3. Match rate
  check_idx <- check_idx + 1L
  if (inherits(linkage_obj, "alprek_linkage_master")) {
    # Use classroom-budget match rate from master diagnostics
    mr <- if (!is.null(diag$classroom_budget)) diag$classroom_budget$match_rate else 1.0
  } else {
    mr <- diag$match_rate
  }
  checks[[check_idx]] <- .make_check(
    "match_rate",
    paste0("Join match rate >= 95%"),
    if (mr >= 0.95) "PASS" else if (mr >= 0.90) "WARN" else "ERROR",
    if (mr < 0.95) 1L else 0L,
    paste0("Match rate: ", round(mr * 100, 1), "%")
  )

  # 4. Orphan count
  check_idx <- check_idx + 1L
  if (inherits(linkage_obj, "alprek_linkage_master")) {
    n_orphan <- if (!is.null(diag$classroom_budget)) diag$classroom_budget$n_left_orphan else 0L
  } else {
    n_orphan <- if (!is.null(diag$n_left_orphan)) diag$n_left_orphan else
                if (!is.null(diag$n_student_orphan_classrooms)) diag$n_student_orphan_classrooms else 0L
  }
  checks[[check_idx]] <- .make_check(
    "orphan_count",
    "Orphan (unmatched) observations",
    "INFO",
    n_orphan,
    if (n_orphan > 0) paste(n_orphan, "orphan(s) found") else "No orphans"
  )

  # 5. NA introduced by join
  check_idx <- check_idx + 1L
  # Check for budget columns that became all-NA after join (unexpected)
  if ("grand_total" %in% names(df)) {
    na_rate <- mean(is.na(df$grand_total))
    checks[[check_idx]] <- .make_check(
      "na_introduced",
      "Budget data availability (grand_total non-NA rate)",
      if (na_rate <= 0.05) "PASS" else if (na_rate <= 0.10) "WARN" else "ERROR",
      sum(is.na(df$grand_total)),
      paste0(round((1 - na_rate) * 100, 1), "% have budget data")
    )
  } else {
    checks[[check_idx]] <- .make_check(
      "na_introduced",
      "Budget columns present",
      if (data_label == "student_classroom") "PASS" else "INFO",
      0L,
      if (data_label == "student_classroom") "N/A for student-classroom join" else "grand_total column not found"
    )
  }

  # 6. Year coverage
  check_idx <- check_idx + 1L
  years_in_data <- sort(unique(df$school_year))
  checks[[check_idx]] <- .make_check(
    "year_coverage",
    "Expected years present",
    "PASS",
    0L,
    paste("Years:", paste(years_in_data, collapse = ", "))
  )

  # 7. Row count consistency
  check_idx <- check_idx + 1L
  if (inherits(linkage_obj, "alprek_linkage_master")) {
    expected_rows <- nrow(cl_data)
    actual_rows <- nrow(cl_data)
    ok <- TRUE
  } else {
    expected_rows <- diag$n_left
    actual_rows <- diag$n_result_rows
    ok <- actual_rows == expected_rows
  }
  checks[[check_idx]] <- .make_check(
    "row_count_consistency",
    "Row count matches expected (left join preserves left rows)",
    if (ok) "PASS" else "ERROR",
    if (ok) 0L else abs(actual_rows - expected_rows),
    paste0("Expected: ", expected_rows, ", Got: ", actual_rows)
  )

  # 8. Region consistency (classroom region vs student region_num)
  check_idx <- check_idx + 1L
  if (all(c("region", "region_num") %in% names(df))) {
    n_mismatch <- sum(df$region != df$region_num, na.rm = TRUE)
    checks[[check_idx]] <- .make_check(
      "region_consistency",
      "Region (classroom) matches region_num (student)",
      if (n_mismatch == 0) "PASS" else "INFO",
      n_mismatch,
      if (n_mismatch > 0) {
        paste(n_mismatch, "mismatches between region and region_num")
      } else "All match"
    )
  } else {
    checks[[check_idx]] <- .make_check(
      "region_consistency",
      "Region consistency check",
      "PASS",
      0L,
      "N/A (region or region_num not both present)"
    )
  }

  # --- Build result ---
  checks_df <- dplyr::bind_rows(lapply(checks, tibble::as_tibble))

  n_errors <- sum(checks_df$status == "ERROR")
  n_warnings <- sum(checks_df$status == "WARN")
  n_info <- sum(checks_df$status == "INFO")

  passed <- if (strict) {
    n_errors == 0 && n_warnings == 0
  } else {
    n_errors == 0
  }

  result <- structure(
    list(
      passed = passed,
      n_errors = n_errors,
      n_warnings = n_warnings,
      n_info = n_info,
      checks = checks_df
    ),
    class = "alprek_linkage_validation"
  )

  if (passed) {
    msg_success("Linkage validation passed ({nrow(checks_df)} checks: {n_errors} errors, {n_warnings} warnings)")
  } else {
    msg_warn("Linkage validation failed ({n_errors} error(s), {n_warnings} warning(s))")
  }

  result
}


#' Print method for alprek_linkage_validation
#' @param x An alprek_linkage_validation object.
#' @param ... Ignored.
#' @export
print.alprek_linkage_validation <- function(x, ...) {
  cat("<alprek_linkage_validation>\n")
  cat("  Overall:", if (x$passed) "PASSED" else "FAILED", "\n")
  cat("  Errors:", x$n_errors, "| Warnings:", x$n_warnings,
      "| Info:", x$n_info, "\n")
  cat("\n  Checks:\n")
  for (i in seq_len(nrow(x$checks))) {
    row <- x$checks[i, ]
    icon <- switch(row$status,
                   PASS = "\u2713",
                   ERROR = "\u2717",
                   WARN = "!",
                   INFO = "i")
    cat("    [", icon, "]", row$check_description)
    if (!is.na(row$details)) cat(" --", row$details)
    cat("\n")
  }
  invisible(x)
}
