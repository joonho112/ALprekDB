#' Validate Linkage Results
#'
#' @description Performs data quality checks on linkage results (joined data).
#'   Returns a validation report with pass/fail status for each check.
#'
#'   **Geocode extension (v0.8.0).** When the input is an
#'   `alprek_linkage_master` whose `classroom_level` carries the prefixed
#'   `geocode_*` columns produced by [linkage_create_master()] with a
#'   `geocode` panel, the validator additionally runs five geocode-specific
#'   checks on the master:
#'
#'   * `geocode_coverage_classroom` -- WARN if `geocode_lat_final` non-NA
#'     coverage falls below `geocode_coverage_min` (default 0.95).
#'   * `followup_reason_completeness` -- ERROR if any row with
#'     `geocode_needs_followup_geocoding == TRUE` has NA
#'     `geocode_followup_reason`.
#'   * `county_check_agreement` -- WARN if Melissa-vs-classroom county
#'     agreement rate falls below `county_agreement_min` (default 0.95).
#'     Gracefully skipped when no county join column is present.
#'   * `new_site_followup_visibility` -- INFO count of bucket-D applications
#'     (NA `matched_classroom_code`) that need followup geocoding. Only
#'     fires when the master was built with an `applications` panel.
#'   * `model_ready_threshold` -- WARN if `pct_model_ready` (from
#'     `diagnostics$geocode_coverage`) is below
#'     `model_ready_min` (default 0.70).
#'
#'   The geocode checks gracefully skip on 3-arg (no `geocode`) master
#'   objects and on non-master linkage results.
#'
#' @param linkage_obj An `alprek_linkage_classroom`, `alprek_linkage_student`,
#'   or `alprek_linkage_master` object.
#' @param strict Logical. If `TRUE`, treats warnings as errors. Default `FALSE`.
#' @param geocode_coverage_min Numeric in `[0, 1]`. Minimum acceptable
#'   `geocode_lat_final` non-NA coverage on classroom-level rows for the
#'   `geocode_coverage_classroom` check. Default `0.95`.
#' @param county_agreement_min Numeric in `[0, 1]`. Minimum acceptable
#'   Melissa-vs-classroom county agreement rate for the
#'   `county_check_agreement` check. Default `0.95`.
#' @param model_ready_min Numeric in `[0, 1]`. Minimum acceptable share of
#'   `model_ready` rows for the `model_ready_threshold` check. Default
#'   `0.70` (the real-data target is `0.80`).
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
#'
#' # Master with geocode panel: extra geocode checks fire
#' master <- linkage_create_master(budget, classroom, student, geocode = gp)
#' linkage_validate(master)
#' }
#'
#' @export
linkage_validate <- function(linkage_obj, strict = FALSE,
                              geocode_coverage_min = 0.95,
                              county_agreement_min = 0.95,
                              model_ready_min      = 0.70) {
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

  # 9. Geocode-specific checks (v0.8.0)
  # These only fire when the input is an alprek_linkage_master whose
  # classroom_level carries the geocode_* columns. They no-op on the
  # 3-arg master case and on classroom/student linkage objects.
  geocode_checks <- .linkage_validate_geocode_checks(
    linkage_obj,
    coverage_min     = geocode_coverage_min,
    agreement_min    = county_agreement_min,
    model_ready_min  = model_ready_min
  )
  if (length(geocode_checks) > 0L) {
    for (gc in geocode_checks) {
      check_idx <- check_idx + 1L
      checks[[check_idx]] <- gc
    }
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


# ===========================================================================
# Geocode validation checks (v0.8.0)
# ===========================================================================

#' Build the geocode-specific check rows for `linkage_validate()`.
#'
#' Returns a (possibly empty) list of `.make_check()`-shaped objects. The
#' checks gracefully no-op when the input is not an
#' `alprek_linkage_master`, when its `classroom_level` has no `geocode_*`
#' columns (3-arg master path), or when an individual check's required
#' column(s) are absent on the master.
#'
#' The five checks (each produces 0 or 1 result row):
#'   1. `geocode_coverage_classroom`   -- WARN if non-NA `geocode_lat_final`
#'                                        coverage < `coverage_min`.
#'   2. `followup_reason_completeness` -- ERROR if any row with
#'                                        `geocode_needs_followup_geocoding == TRUE`
#'                                        has NA `geocode_followup_reason`.
#'   3. `county_check_agreement`       -- WARN if Melissa-vs-classroom county
#'                                        agreement rate < `agreement_min`.
#'                                        Skipped if no county comparison
#'                                        column is materialized on the
#'                                        master.
#'   4. `new_site_followup_visibility` -- INFO surfacing the count of
#'                                        bucket-D applications needing
#'                                        followup geocoding. Skipped if
#'                                        no applications branch fired.
#'   5. `model_ready_threshold`        -- WARN if `pct_model_ready` (from
#'                                        `diagnostics$geocode_coverage`) is
#'                                        below `model_ready_min`.
#'
#' @keywords internal
.linkage_validate_geocode_checks <- function(linkage_obj,
                                              coverage_min     = 0.95,
                                              agreement_min    = 0.95,
                                              model_ready_min  = 0.70) {
  # Only meaningful for master objects with a populated geocode branch.
  if (!inherits(linkage_obj, "alprek_linkage_master")) {
    return(list())
  }
  cl <- linkage_obj$classroom_level
  if (is.null(cl) || !is.data.frame(cl) || nrow(cl) == 0L) {
    return(list())
  }
  has_geocode <- any(grepl("^geocode_", names(cl)))
  if (!has_geocode) {
    return(list())
  }

  out <- list()

  # ---- 1. geocode_coverage_classroom ---------------------------------------
  if ("geocode_lat_final" %in% names(cl)) {
    n_total      <- nrow(cl)
    n_with_coord <- sum(!is.na(cl$geocode_lat_final))
    coverage     <- if (n_total > 0L) n_with_coord / n_total else NA_real_
    n_missing    <- n_total - n_with_coord
    status <- if (is.na(coverage)) {
      "INFO"
    } else if (coverage >= coverage_min) {
      "PASS"
    } else {
      "WARN"
    }
    out[[length(out) + 1L]] <- .make_check(
      "geocode_coverage_classroom",
      sprintf("Classroom rows with reconciled geocode coord >= %.0f%%",
              100 * coverage_min),
      status,
      as.integer(n_missing),
      sprintf("Coverage: %.1f%% (%d of %d classroom-year rows have non-NA geocode_lat_final)",
              if (is.na(coverage)) NA_real_ else 100 * coverage,
              n_with_coord, n_total)
    )
  }

  # ---- 2. followup_reason_completeness -------------------------------------
  if (all(c("geocode_needs_followup_geocoding", "geocode_followup_reason")
          %in% names(cl))) {
    needs_fu <- !is.na(cl$geocode_needs_followup_geocoding) &
                  .linkage_is_true_vec(cl$geocode_needs_followup_geocoding)
    reason_na <- is.na(cl$geocode_followup_reason)
    n_followup <- sum(needs_fu, na.rm = TRUE)
    n_missing_reason <- sum(needs_fu & reason_na, na.rm = TRUE)
    status <- if (n_followup == 0L) {
      "PASS"
    } else if (n_missing_reason == 0L) {
      "PASS"
    } else {
      "ERROR"
    }
    details <- if (n_followup == 0L) {
      "No rows need followup geocoding"
    } else if (n_missing_reason == 0L) {
      sprintf("All %d followup row(s) carry a geocode_followup_reason",
              n_followup)
    } else {
      sprintf("%d of %d followup row(s) are MISSING geocode_followup_reason",
              n_missing_reason, n_followup)
    }
    out[[length(out) + 1L]] <- .make_check(
      "followup_reason_completeness",
      "Every needs-followup row carries a followup_reason",
      status,
      as.integer(n_missing_reason),
      details
    )
  }

  # ---- 3. county_check_agreement -------------------------------------------
  # Look for a materialized county comparison on the master. We try three
  # plausible signal columns, in priority order:
  #   (a) a pre-computed boolean `geocode_county_check_match` (best case),
  #   (b) both a Melissa-side county string (`melissa_county_name` or
  #       `geocode_county_name`) AND a classroom-side `county_name` /
  #       `county`, where we recompute the agreement rate ourselves,
  #   (c) the geocode-transform-derived `county_check_match` if it
  #       happens to survive the join unprefixed.
  ag_result <- .linkage_geocode_county_agreement(cl)
  if (!is.null(ag_result)) {
    rate <- ag_result$rate
    status <- if (is.na(rate)) {
      "INFO"
    } else if (rate >= agreement_min) {
      "PASS"
    } else {
      "WARN"
    }
    out[[length(out) + 1L]] <- .make_check(
      "county_check_agreement",
      sprintf("Melissa-vs-classroom county agreement >= %.0f%%",
              100 * agreement_min),
      status,
      as.integer(ag_result$n_mismatch),
      sprintf("Agreement: %s (matched %d / mismatched %d / NA %d; source: %s)",
              if (is.na(rate)) "NA" else sprintf("%.1f%%", 100 * rate),
              ag_result$n_match, ag_result$n_mismatch, ag_result$n_na,
              ag_result$source)
    )
  }

  # ---- 4. new_site_followup_visibility -------------------------------------
  # INFO-only check: report visibility of bucket-D applications that need
  # followup geocoding. Skipped if no applications branch fired.
  has_apps <- isTRUE(linkage_obj$meta$has_applications)
  if (has_apps) {
    n_bd_total    <- NA_integer_
    n_bd_followup <- NA_integer_
    detail_src    <- character(0)

    # Source A: master's classroom_level rows with NA classroom_code +
    # bucket marker, if any.
    bucket_col <- intersect(c("app_bucket", "bucket"), names(cl))
    bucket_col <- if (length(bucket_col) > 0L) bucket_col[1L] else NA_character_

    # Source B: applications-linkage diagnostics, which carry the canonical
    # bucket-D row counts and an n_unmatched_applications signal.
    app_diag <- linkage_obj$diagnostics$applications_linkage
    geo_diag <- linkage_obj$diagnostics$geocode_linkage

    # Prefer recomputing from classroom_level when bucket column is visible
    # and the row carries a needs_followup signal. This is the surface the
    # check is really about: are the bucket-D rows visible to validators?
    if (!is.na(bucket_col) &&
        "geocode_needs_followup_geocoding" %in% names(cl)) {
      is_bd <- !is.na(cl[[bucket_col]]) & as.character(cl[[bucket_col]]) == "D"
      n_bd_total <- sum(is_bd, na.rm = TRUE)
      n_bd_followup <- sum(
        is_bd & !is.na(cl$geocode_needs_followup_geocoding) &
          .linkage_is_true_vec(cl$geocode_needs_followup_geocoding),
        na.rm = TRUE
      )
      detail_src <- c(detail_src,
                      sprintf("classroom_level$%s + geocode_needs_followup_geocoding",
                              bucket_col))
    }
    # Fall back to diagnostics if classroom_level didn't have the bucket col.
    if (is.na(n_bd_total) && is.data.frame(app_diag)) {
      ix <- !is.na(app_diag$group_by) &
              app_diag$group_by == "bucket=D"
      vals <- app_diag$value[ix]
      if (length(vals) > 0L) {
        n_bd_total <- as.integer(vals[1L])
        detail_src <- c(detail_src, "diagnostics$applications_linkage")
      }
    }

    if (is.na(n_bd_total)) n_bd_total <- 0L
    if (is.na(n_bd_followup)) n_bd_followup <- 0L

    out[[length(out) + 1L]] <- .make_check(
      "new_site_followup_visibility",
      "Bucket-D applications needing followup are visible in master diagnostics",
      "INFO",
      as.integer(n_bd_followup),
      sprintf("Bucket-D rows: %d total, %d need followup geocoding (source: %s)",
              n_bd_total, n_bd_followup,
              if (length(detail_src) > 0L) paste(detail_src, collapse = "; ") else "n/a")
    )
  }

  # ---- 5. model_ready_threshold --------------------------------------------
  geo_cov <- linkage_obj$diagnostics$geocode_coverage
  if (is.list(geo_cov) &&
      isTRUE(geo_cov$n_classroom_total > 0L) &&
      !is.null(geo_cov$pct_model_ready) &&
      !is.na(geo_cov$pct_model_ready)) {
    rate <- geo_cov$pct_model_ready / 100  # coverage stored as percent
    status <- if (rate >= model_ready_min) "PASS" else "WARN"
    out[[length(out) + 1L]] <- .make_check(
      "model_ready_threshold",
      sprintf("Model-ready coverage >= %.0f%% (real-data target: 80%%)",
              100 * model_ready_min),
      status,
      as.integer(geo_cov$n_classroom_total - geo_cov$n_model_ready),
      sprintf("Model-ready: %.1f%% (%d of %d classroom-year rows)",
              geo_cov$pct_model_ready,
              geo_cov$n_model_ready, geo_cov$n_classroom_total)
    )
  }

  out
}


#' Compute Melissa-vs-classroom county agreement on a classroom_level tibble.
#'
#' Returns NULL when no usable comparison exists. Otherwise returns a list
#' with `rate`, `n_match`, `n_mismatch`, `n_na`, `source` (string tag for
#' diagnostics).
#'
#' Comparison sources in priority order:
#'   1. A precomputed boolean `geocode_county_check_match` (best case).
#'   2. Both a Melissa-side county string column AND a classroom-side
#'      county string column. We recompute agreement case-insensitively.
#'      Melissa-side candidates: `melissa_county_name`, `geocode_county_name`,
#'      `COUNTYNAME`. Classroom-side candidates: `county_name`, `county`.
#'   3. Unprefixed `county_check_match` (survives the join in some setups).
#'
#' @keywords internal
.linkage_geocode_county_agreement <- function(cl) {
  if (is.null(cl) || !is.data.frame(cl) || nrow(cl) == 0L) return(NULL)

  # Source 1: prefixed boolean
  if ("geocode_county_check_match" %in% names(cl)) {
    v <- cl$geocode_county_check_match
    return(.linkage_county_summarize(v, "geocode_county_check_match"))
  }
  # Source 3: unprefixed boolean (less likely but cheap to check)
  if ("county_check_match" %in% names(cl)) {
    v <- cl$county_check_match
    return(.linkage_county_summarize(v, "county_check_match"))
  }
  # Source 2: recompute from string pair
  melissa_candidates <- c("melissa_county_name",
                          "geocode_county_name",
                          "COUNTYNAME")
  classroom_candidates <- c("county_name", "county")
  mel_col <- intersect(melissa_candidates, names(cl))
  cl_col  <- intersect(classroom_candidates, names(cl))
  if (length(mel_col) == 0L || length(cl_col) == 0L) {
    return(NULL)
  }
  mel <- tolower(trimws(as.character(cl[[mel_col[1L]]])))
  ad  <- tolower(trimws(as.character(cl[[cl_col[1L]]])))
  v <- rep(NA, length(mel))
  valid <- !is.na(mel) & nzchar(mel) & !is.na(ad) & nzchar(ad)
  v[valid] <- mel[valid] == ad[valid]
  v <- as.logical(v)
  .linkage_county_summarize(v, sprintf("%s vs %s", mel_col[1L], cl_col[1L]))
}


#' Summarize a boolean county-agreement vector into the shape consumed by
#' `.linkage_validate_geocode_checks()`.
#'
#' @keywords internal
.linkage_county_summarize <- function(v, src_label) {
  v <- as.logical(v)
  n_match    <- sum(v, na.rm = TRUE)
  n_mismatch <- sum(!v & !is.na(v))
  n_na       <- sum(is.na(v))
  n_valid    <- n_match + n_mismatch
  rate <- if (n_valid > 0L) n_match / n_valid else NA_real_
  list(
    rate       = rate,
    n_match    = as.integer(n_match),
    n_mismatch = as.integer(n_mismatch),
    n_na       = as.integer(n_na),
    source     = src_label
  )
}


#' Logical TRUE-ness with NA tolerance, vectorized.
#' Returns FALSE for NA. Mirrors `isTRUE()` semantics extended to vectors.
#'
#' @keywords internal
.linkage_is_true_vec <- function(x) {
  if (is.null(x)) return(logical(0))
  !is.na(x) & as.logical(x)
}
