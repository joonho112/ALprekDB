#' Validate Cleaned Classroom Data
#'
#' @description Performs comprehensive data quality checks on cleaned classroom
#'   data. Returns a validation report with pass/fail status for each check.
#'   Validation is advisory — it does not block downstream processing.
#'
#' @param clean_obj An `alprek_classroom_clean` object from [classroom_clean()].
#' @param strict Logical. If `TRUE`, treats warnings as errors. Default `FALSE`.
#'
#' @return An `alprek_classroom_validation` S3 object (list) with elements:
#'   - `passed`: logical overall result.
#'   - `n_errors`, `n_warnings`, `n_info`: counts by severity.
#'   - `checks`: tibble of individual check results.
#'   - `issues`: tibble of specific rows with problems.
#'
#' @examples
#' \dontrun{
#' raw <- classroom_read("FCPK Classroom Details 21-22.xlsx")
#' clean <- classroom_clean(raw)
#' val <- classroom_validate(clean)
#' print(val)
#' }
#'
#' @export
classroom_validate <- function(clean_obj, strict = FALSE) {

  if (!inherits(clean_obj, "alprek_classroom_clean")) {
    stop("Expected an 'alprek_classroom_clean' object. Use classroom_clean() first.",
         call. = FALSE)
  }

  df <- clean_obj$data
  checks <- list()
  issues <- tibble::tibble()
  check_idx <- 0L

  msg_info("Validating classroom data for {clean_obj$meta$school_year}")

  # 1. Required columns
  check_idx <- check_idx + 1L
  required <- c("classroom_code", "classroom_name", "school_year", "year",
                "delivery_type", "region", "county_name")
  missing <- setdiff(required, names(df))
  checks[[check_idx]] <- .make_check(
    "required_columns",
    "Required columns present",
    if (length(missing) == 0) "PASS" else "ERROR",
    length(missing),
    if (length(missing) > 0) paste("Missing:", paste(missing, collapse = ", ")) else NA_character_
  )

  # 2. Classroom code format
  check_idx <- check_idx + 1L
  code_pattern <- "^\\d{3}[A-Z]\\d{5}\\.\\d{2}$"
  if ("classroom_code" %in% names(df)) {
    valid_codes <- grepl(code_pattern, df$classroom_code)
    n_invalid <- sum(!valid_codes & !is.na(df$classroom_code))
  } else {
    n_invalid <- NA_integer_
  }
  checks[[check_idx]] <- .make_check(
    "classroom_code_format",
    "Classroom codes match expected pattern",
    if (is.na(n_invalid)) "ERROR" else if (n_invalid == 0) "PASS" else "WARN",
    if (is.na(n_invalid)) 1L else n_invalid,
    if (is.na(n_invalid)) "classroom_code column missing" else
      if (n_invalid > 0) paste(n_invalid, "code(s) don't match pattern") else NA_character_
  )

  # 3. Classroom code uniqueness within year
  check_idx <- check_idx + 1L
  if ("classroom_code" %in% names(df)) {
    dup_codes <- df$classroom_code[duplicated(df$classroom_code) & !is.na(df$classroom_code)]
    n_dups <- length(unique(dup_codes))
  } else {
    n_dups <- 0L
  }
  checks[[check_idx]] <- .make_check(
    "classroom_code_unique",
    "No duplicate classroom codes within year",
    if (n_dups == 0) "PASS" else "WARN",
    n_dups,
    if (n_dups > 0) paste(n_dups, "duplicate classroom code(s)") else NA_character_
  )

  # 4. Delivery type coverage
  check_idx <- check_idx + 1L
  expected_dt <- c("Public School", "Private Child Care", "Head Start",
                   "Community Organization", "Faith-Based Organization",
                   "University Operated", "Private School")
  if ("delivery_type" %in% names(df)) {
    dt_present <- unique(as.character(df$delivery_type[!is.na(df$delivery_type)]))
    missing_dt <- setdiff(expected_dt, dt_present)
  } else {
    missing_dt <- expected_dt
  }
  checks[[check_idx]] <- .make_check(
    "delivery_type_coverage",
    "All 7 delivery types represented",
    if (length(missing_dt) == 0) "PASS" else "INFO",
    length(missing_dt),
    if (length(missing_dt) > 0) paste("Missing:", paste(missing_dt, collapse = ", ")) else NA_character_
  )

  # 5. Missing rates for key variables
  check_idx <- check_idx + 1L
  key_vars <- c("classroom_code", "delivery_type", "total_grant",
                "lead_tch_degree_raw", "lead_tch_race")
  missing_rates <- vapply(key_vars, function(v) {
    if (v %in% names(df)) {
      sum(is.na(df[[v]])) / nrow(df) * 100
    } else {
      100
    }
  }, numeric(1))
  high_missing <- names(missing_rates)[missing_rates > 20]
  checks[[check_idx]] <- .make_check(
    "missing_rates",
    "Key variables have acceptable missing rates (<20%)",
    if (length(high_missing) == 0) "PASS" else "WARN",
    length(high_missing),
    if (length(high_missing) > 0) {
      paste(paste0(high_missing, ": ", round(missing_rates[high_missing], 1), "%"),
            collapse = "; ")
    } else NA_character_
  )

  # 6. Grant range
  check_idx <- check_idx + 1L
  if ("total_grant" %in% names(df)) {
    bad_grants <- df$total_grant[!is.na(df$total_grant) &
                                  (df$total_grant < 0 | df$total_grant > 500000)]
    n_bad <- length(bad_grants)
  } else {
    n_bad <- 0L
  }
  checks[[check_idx]] <- .make_check(
    "grant_range",
    "Grant amounts in reasonable range ($0-$500K)",
    if (n_bad == 0) "PASS" else "WARN",
    n_bad,
    if (n_bad > 0) paste(n_bad, "grant(s) outside expected range") else NA_character_
  )

  # 7. Degree classification coverage
  check_idx <- check_idx + 1L
  degree_cols <- list(
    list(raw = "lead_tch_degree_raw", level = "lead_tch_degree_level",
         label = "Lead", threshold = 95),
    list(raw = "aux_tch_degree_raw", level = "aux_tch_degree_level",
         label = "Aux", threshold = 85)
  )
  degree_msgs <- character()
  degree_issues <- 0L
  for (dc in degree_cols) {
    if (dc$raw %in% names(df) && dc$level %in% names(df)) {
      has_raw <- !is.na(df[[dc$raw]]) & nchar(stringr::str_trim(as.character(df[[dc$raw]]))) > 0
      has_level <- !is.na(df[[dc$level]])
      n_has_raw <- sum(has_raw)
      if (n_has_raw > 0) {
        coverage <- sum(has_level & has_raw) / n_has_raw * 100
        if (coverage < dc$threshold) {
          degree_msgs <- c(degree_msgs, paste0(dc$label, ": ", round(coverage, 1),
                                               "% (target: ", dc$threshold, "%)"))
          degree_issues <- degree_issues + 1L
        }
      }
    }
  }
  checks[[check_idx]] <- .make_check(
    "degree_classification_coverage",
    "Degree classification meets coverage targets",
    if (degree_issues == 0) "PASS" else "WARN",
    degree_issues,
    if (degree_issues > 0) paste(degree_msgs, collapse = "; ") else NA_character_
  )

  # 8. Experience range
  check_idx <- check_idx + 1L
  exp_cols <- c("lead_tch_osr_exp", "lead_tch_total_exp",
                "aux_tch_osr_exp", "aux_tch_total_exp")
  n_exp_bad <- 0L
  for (ec in intersect(exp_cols, names(df))) {
    vals <- df[[ec]][!is.na(df[[ec]])]
    n_exp_bad <- n_exp_bad + sum(vals < 0 | vals > 50)
  }
  checks[[check_idx]] <- .make_check(
    "experience_range",
    "Experience values in reasonable range (0-50 years)",
    if (n_exp_bad == 0) "PASS" else "WARN",
    n_exp_bad,
    if (n_exp_bad > 0) paste(n_exp_bad, "value(s) outside 0-50 range") else NA_character_
  )

  # 9. Coordinate range (Alabama bounds)
  check_idx <- check_idx + 1L
  n_coord_bad <- 0L
  if ("latitude" %in% names(df)) {
    lats <- df$latitude[!is.na(df$latitude)]
    n_coord_bad <- n_coord_bad + sum(lats < 30 | lats > 36)
  }
  if ("longitude" %in% names(df)) {
    lons <- df$longitude[!is.na(df$longitude)]
    n_coord_bad <- n_coord_bad + sum(lons < -89 | lons > -84)
  }
  checks[[check_idx]] <- .make_check(
    "coordinate_range",
    "Coordinates within Alabama bounds (lat 30-36, lon -89 to -84)",
    if (n_coord_bad == 0) "PASS" else "WARN",
    n_coord_bad,
    if (n_coord_bad > 0) paste(n_coord_bad, "coordinate(s) outside Alabama") else NA_character_
  )

  # 10. School year consistency
  check_idx <- check_idx + 1L
  if ("school_year" %in% names(df)) {
    unique_sy <- unique(df$school_year)
    n_sy <- length(unique_sy)
  } else {
    n_sy <- 0L
  }
  checks[[check_idx]] <- .make_check(
    "school_year_consistency",
    "All rows have same school_year",
    if (n_sy == 1) "PASS" else if (n_sy == 0) "ERROR" else "WARN",
    if (n_sy <= 1) 0L else n_sy - 1L,
    if (n_sy == 0) "No school_year column" else
      if (n_sy > 1) paste("Found", n_sy, "unique school years") else NA_character_
  )

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
      checks = checks_df,
      issues = issues
    ),
    class = "alprek_classroom_validation"
  )

  if (passed) {
    msg_success("Validation passed ({check_idx} checks: {n_errors} errors, {n_warnings} warnings)")
  } else {
    msg_warn("Validation failed ({n_errors} error(s), {n_warnings} warning(s))")
  }

  result
}


#' Print method for alprek_classroom_validation
#' @param x An alprek_classroom_validation object.
#' @param ... Ignored.
#' @export
print.alprek_classroom_validation <- function(x, ...) {
  cat("<alprek_classroom_validation>\n")
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
