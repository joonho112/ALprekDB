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
  cb_diag <- NULL
  sc_diag <- NULL
  if (inherits(linkage_obj, "alprek_linkage_master")) {
    cb_diag <- diag$classroom_budget
    sc_diag <- diag$student_classroom
  } else if (identical(data_label, "classroom_budget")) {
    cb_diag <- diag
  } else if (identical(data_label, "student_classroom")) {
    sc_diag <- diag
  }

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
  if (!is.null(cb_diag)) {
    no_budget_overlap <- !is.null(cb_diag$year_coverage$overlap_years) &&
      length(cb_diag$year_coverage$overlap_years) == 0 &&
      length(.linkage_diag_value(cb_diag, "missing_budget_years", character())) > 0
    mr <- if (no_budget_overlap) {
      1
    } else if (!is.null(cb_diag$match_rate_overlap_years) &&
               !is.na(cb_diag$match_rate_overlap_years)) {
      cb_diag$match_rate_overlap_years
    } else {
      cb_diag$match_rate
    }
    match_desc <- "Budget-classroom join match rate >= 95% in overlapping budget years"
  } else if (!is.null(sc_diag)) {
    no_classroom_overlap <- !is.null(sc_diag$year_coverage$overlap_years) &&
      length(sc_diag$year_coverage$overlap_years) == 0 &&
      length(.linkage_diag_value(sc_diag, "missing_classroom_years", character())) > 0
    mr <- if (no_classroom_overlap) {
      0
    } else if (!is.null(sc_diag$match_rate_overlap_years) &&
               !is.na(sc_diag$match_rate_overlap_years)) {
      sc_diag$match_rate_overlap_years
    } else {
      sc_diag$match_rate
    }
    match_desc <- "Student-classroom join match rate >= 95% in overlapping classroom years"
  } else {
    mr <- diag$match_rate
    match_desc <- "Join match rate >= 95%"
  }
  checks[[check_idx]] <- .make_check(
    "match_rate",
    match_desc,
    if (mr >= 0.95) "PASS" else if (mr >= 0.90) "WARN" else "ERROR",
    if (mr < 0.95) 1L else 0L,
    paste0("Match rate: ", round(mr * 100, 1), "%")
  )

  # 4. Orphan count
  check_idx <- check_idx + 1L
  if (inherits(linkage_obj, "alprek_linkage_master")) {
    n_orphan <- 0L
    if (!is.null(diag$classroom_budget)) {
      n_orphan <- n_orphan + diag$classroom_budget$n_left_orphan
    }
    if (!is.null(diag$student_classroom)) {
      n_orphan <- n_orphan +
        diag$student_classroom$n_student_orphan_classrooms +
        diag$student_classroom$n_classroom_orphan
    }
  } else {
    n_orphan <- if (!is.null(diag$n_left_orphan)) diag$n_left_orphan else
                if (!is.null(diag$n_student_orphan_classrooms)) diag$n_student_orphan_classrooms else 0L
  }
  checks[[check_idx]] <- .make_check(
    "orphan_count",
    "Orphan (unmatched) observations",
    "INFO",
    n_orphan,
    if (!is.null(cb_diag) && n_orphan > 0) {
      paste0(
        n_orphan, " orphan(s) found; ",
        cb_diag$n_left_orphan_overlap_years, " in overlapping years; ",
        cb_diag$n_left_orphan_missing_budget_years, " due to missing budget years"
      )
    } else if (!is.null(sc_diag) && n_orphan > 0) {
      paste0(
        n_orphan, " orphan signal(s) found; ",
        sc_diag$n_student_orphan_overlap_years, " student classroom code(s) missing in overlapping years; ",
        sc_diag$n_student_orphan_missing_classroom_years, " due to missing classroom years; ",
        sc_diag$n_classroom_orphan_overlap_years, " classroom(s) with no students in overlapping years"
      )
    } else if (n_orphan > 0) paste(n_orphan, "orphan(s) found") else "No orphans"
  )

  # 4a. True budget-overlap orphans
  if (!is.null(cb_diag)) {
    check_idx <- check_idx + 1L
    n_left_overlap <- .linkage_diag_value(cb_diag, "n_left_orphan_overlap_years", 0L)
    n_right_overlap <- .linkage_diag_value(cb_diag, "n_right_orphan_overlap_years", 0L)
    n_overlap_orphans <- n_left_overlap + n_right_overlap
    overlap_mr <- .linkage_diag_value(cb_diag, "match_rate_overlap_years", cb_diag$match_rate)
    no_budget_overlap <- !is.null(cb_diag$year_coverage$overlap_years) &&
      length(cb_diag$year_coverage$overlap_years) == 0
    checks[[check_idx]] <- .make_check(
      "budget_overlap_orphans",
      "Budget-classroom orphans in overlapping budget years",
      if (no_budget_overlap) {
        "PASS"
      } else if (n_overlap_orphans == 0) {
        "PASS"
      } else if (!is.na(overlap_mr) && overlap_mr >= 0.95) {
        "WARN"
      } else {
        "ERROR"
      },
      n_overlap_orphans,
      if (no_budget_overlap) {
        "No overlapping budget years to evaluate"
      } else {
        paste0(
          n_left_overlap, " classroom row(s) without budget; ",
          n_right_overlap, " budget row(s) without classroom in overlapping years"
        )
      }
    )

    check_idx <- check_idx + 1L
    missing_budget_years <- .linkage_diag_value(cb_diag, "missing_budget_years", character())
    checks[[check_idx]] <- .make_check(
      "budget_missing_coverage",
      "Budget coverage gaps are explicit",
      if (length(missing_budget_years) > 0) "INFO" else "PASS",
      length(missing_budget_years),
      if (length(missing_budget_years) > 0) {
        paste("Budget unavailable:", paste(missing_budget_years, collapse = ", "))
      } else {
        "Budget available for all classroom years"
      }
    )
  }

  # 4b. Student-classroom orphan checks
  if (!is.null(sc_diag)) {
    check_idx <- check_idx + 1L
    missing_classroom_years <- .linkage_diag_value(sc_diag, "missing_classroom_years", character())
    missing_student_years <- .linkage_diag_value(sc_diag, "missing_student_years", character())
    n_missing_classroom <- .linkage_diag_value(
      sc_diag,
      "n_student_orphan_missing_classroom_years",
      0L
    )
    n_missing_classroom_rows <- .linkage_diag_value(
      sc_diag,
      "n_student_orphan_missing_classroom_year_rows",
      0L
    )
    n_missing_student_rows <- .linkage_diag_value(
      sc_diag,
      "n_classroom_orphan_missing_student_years",
      0L
    )
    checks[[check_idx]] <- .make_check(
      "student_classroom_missing_coverage",
      "Student and classroom coverage gaps are explicit",
      if (n_missing_classroom > 0) {
        "ERROR"
      } else if (length(missing_student_years) > 0) {
        "WARN"
      } else {
        "PASS"
      },
      n_missing_classroom + n_missing_student_rows,
      paste(
        if (length(missing_classroom_years) > 0) {
          paste0("Classroom unavailable for student year(s): ",
                 paste(missing_classroom_years, collapse = ", "),
                 " (", n_missing_classroom_rows, " student row(s))")
        } else {
          "Classroom available for all student years"
        },
        if (length(missing_student_years) > 0) {
          paste0("Student unavailable for classroom year(s): ",
                 paste(missing_student_years, collapse = ", "),
                 " (", n_missing_student_rows, " classroom row(s))")
        } else {
          "Student available for all classroom years"
        },
        sep = " | "
      )
    )

    check_idx <- check_idx + 1L
    n_student_overlap <- .linkage_diag_value(sc_diag, "n_student_orphan_overlap_years", 0L)
    n_student_overlap_rows <- .linkage_diag_value(
      sc_diag,
      "n_student_orphan_overlap_year_rows",
      0L
    )
    sc_overlap_mr <- .linkage_diag_value(sc_diag, "match_rate_overlap_years", sc_diag$match_rate)
    checks[[check_idx]] <- .make_check(
      "student_classroom_overlap_orphans",
      "Student classroom codes match classroom records in overlapping years",
      if (n_student_overlap == 0) {
        "PASS"
      } else if (!is.na(sc_overlap_mr) && sc_overlap_mr >= 0.95) {
        "WARN"
      } else {
        "ERROR"
      },
      n_student_overlap,
      paste0(
        n_student_overlap, " student classroom code(s) missing classroom records; ",
        n_student_overlap_rows, " student row(s) affected in overlapping years"
      )
    )

    check_idx <- check_idx + 1L
    n_empty_classrooms <- .linkage_diag_value(sc_diag, "n_classroom_orphan", 0L)
    n_empty_overlap <- .linkage_diag_value(sc_diag, "n_classroom_orphan_overlap_years", n_empty_classrooms)
    n_empty_missing_student <- .linkage_diag_value(
      sc_diag,
      "n_classroom_orphan_missing_student_years",
      0L
    )
    checks[[check_idx]] <- .make_check(
      "empty_classrooms",
      "Classrooms with no linked student rows are retained",
      if (n_empty_classrooms > 0) "INFO" else "PASS",
      n_empty_classrooms,
      paste0(
        n_empty_overlap, " empty classroom row(s) in overlapping years; ",
        n_empty_missing_student, " classroom row(s) in missing student years"
      )
    )
  }

  # 5. NA introduced by join
  check_idx <- check_idx + 1L
  # Check for budget columns that became all-NA after join (unexpected)
  if ("grand_total" %in% names(df)) {
    budget_eval_idx <- rep(TRUE, nrow(df))
    if (!is.null(cb_diag) && !is.null(cb_diag$year_coverage$overlap_years)) {
      budget_eval_idx <- as.character(df$school_year) %in%
        cb_diag$year_coverage$overlap_years
    }
    if (any(budget_eval_idx)) {
      na_rate <- mean(is.na(df$grand_total[budget_eval_idx]))
      n_na_budget <- sum(is.na(df$grand_total[budget_eval_idx]))
    } else {
      na_rate <- 0
      n_na_budget <- 0L
    }
    missing_year_rows <- if (!is.null(cb_diag)) {
      cb_diag$n_left_orphan_missing_budget_years
    } else {
      0L
    }
    checks[[check_idx]] <- .make_check(
      "na_introduced",
      "Budget data availability in overlapping coverage years",
      if (na_rate <= 0.05) "PASS" else if (na_rate <= 0.10) "WARN" else "ERROR",
      n_na_budget,
      paste0(
        round((1 - na_rate) * 100, 1),
        "% have budget data in overlapping years",
        if (missing_year_rows > 0) {
          paste0("; ", missing_year_rows,
                 " row(s) are in missing budget year(s) and excluded from this rate")
        } else {
          ""
        }
      )
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
  missing_budget_years <- character()
  if (!is.null(cb_diag) && !is.null(cb_diag$missing_budget_years)) {
    missing_budget_years <- cb_diag$missing_budget_years
  } else if (inherits(linkage_obj, "alprek_linkage_master") &&
             !is.null(diag$coverage$missing_budget_years)) {
    missing_budget_years <- diag$coverage$missing_budget_years
  }
  checks[[check_idx]] <- .make_check(
    "year_coverage",
    "Expected years present",
    if (length(missing_budget_years) > 0) "INFO" else "PASS",
    length(missing_budget_years),
    paste(
      paste("Years:", paste(years_in_data, collapse = ", ")),
      if (length(missing_budget_years) > 0) {
        paste("Budget unavailable:", paste(missing_budget_years, collapse = ", "))
      } else {
        "Budget available for all joined years"
      },
      sep = " | "
    )
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


#' Safely read a diagnostic field with a default
#' @keywords internal
.linkage_diag_value <- function(diag, name, default = NULL) {
  value <- diag[[name]]
  if (is.null(value)) {
    return(default)
  }
  value
}
