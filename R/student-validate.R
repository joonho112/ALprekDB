#' Validate Cleaned Student Data
#'
#' @description Performs comprehensive data quality checks on cleaned student
#'   data. Returns a validation report with pass/fail status for each check.
#'   Validation is advisory — it does not block downstream processing.
#'
#' @param clean_obj An `alprek_student_clean` object from [student_clean()].
#' @param strict Logical. If `TRUE`, treats warnings as errors. Default `FALSE`.
#'
#' @return An `alprek_student_validation` S3 object (list) with elements:
#'   - `passed`: logical overall result.
#'   - `n_errors`, `n_warnings`, `n_info`: counts by severity.
#'   - `checks`: tibble of individual check results.
#'   - `issues`: tibble of specific rows with problems.
#'
#' @examples
#' \dontrun{
#' raw <- student_read("FCPK Student Details 21-22.xlsx", "2021-2022")
#' clean <- student_clean(raw)
#' val <- student_validate(clean)
#' print(val)
#' }
#'
#' @export
student_validate <- function(clean_obj, strict = FALSE) {

  if (!inherits(clean_obj, "alprek_student_clean")) {
    stop("Expected an 'alprek_student_clean' object. Use student_clean() first.",
         call. = FALSE)
  }

  df <- clean_obj$data
  checks <- list()
  issues <- tibble::tibble()
  check_idx <- 0L

  msg_info("Validating student data for {clean_obj$meta$school_year}")

  # 1. Required columns
  check_idx <- check_idx + 1L
  required <- c("school_year", "classroom_code", "adece_id", "gender", "race")
  missing <- setdiff(required, names(df))
  checks[[check_idx]] <- .make_student_check(
    "required_columns",
    "Required columns present",
    if (length(missing) == 0) "PASS" else "ERROR",
    length(missing),
    if (length(missing) > 0) paste("Missing:", paste(missing, collapse = ", ")) else NA_character_
  )

  # 2. ADECE ID format
  check_idx <- check_idx + 1L
  if ("adece_id" %in% names(df)) {
    all_na <- all(is.na(df$adece_id))
    if (!all_na) {
      valid_ids <- !is.na(df$adece_id) & nchar(as.character(df$adece_id)) > 0
      n_invalid <- sum(!valid_ids)
    } else {
      n_invalid <- nrow(df)
    }
  } else {
    all_na <- TRUE
    n_invalid <- NA_integer_
  }
  checks[[check_idx]] <- .make_student_check(
    "adece_id_format",
    "ADECE IDs present and valid",
    if (is.na(n_invalid)) "ERROR" else if (all_na) "ERROR" else if (n_invalid == 0) "PASS" else "WARN",
    if (is.na(n_invalid)) 1L else n_invalid,
    if (is.na(n_invalid)) "adece_id column missing" else
      if (all_na) "All ADECE IDs are NA" else
      if (n_invalid > 0) paste(n_invalid, "invalid/missing ADECE ID(s)") else NA_character_
  )

  # 3. Student uniqueness (adece_id + classroom_code within year)
  check_idx <- check_idx + 1L
  if (all(c("adece_id", "classroom_code") %in% names(df))) {
    combo <- paste(df$adece_id, df$classroom_code, sep = "|")
    combo_no_na <- combo[!is.na(df$adece_id) & !is.na(df$classroom_code)]
    n_dups <- sum(duplicated(combo_no_na))
  } else {
    n_dups <- 0L
  }
  checks[[check_idx]] <- .make_student_check(
    "student_unique",
    "No duplicate (adece_id + classroom_code) within year",
    if (n_dups == 0) "PASS" else "WARN",
    n_dups,
    if (n_dups > 0) paste(n_dups, "duplicate student-classroom pair(s)") else NA_character_
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
  checks[[check_idx]] <- .make_student_check(
    "delivery_type_coverage",
    "All 7 delivery types represented",
    if (length(missing_dt) == 0) "PASS" else "INFO",
    length(missing_dt),
    if (length(missing_dt) > 0) paste("Missing:", paste(missing_dt, collapse = ", ")) else NA_character_
  )

  # 5. Gender distribution
  check_idx <- check_idx + 1L
  if ("gender" %in% names(df)) {
    gender_vals <- as.character(df$gender[!is.na(df$gender)])
    n_gender <- length(gender_vals)
    if (n_gender > 0) {
      pct_male <- mean(gender_vals == "Male", na.rm = TRUE) * 100
      pct_female <- mean(gender_vals == "Female", na.rm = TRUE) * 100
      out_of_range <- (pct_male < 40 | pct_male > 60) | (pct_female < 40 | pct_female > 60)
    } else {
      out_of_range <- TRUE
      pct_male <- NA_real_
      pct_female <- NA_real_
    }
  } else {
    out_of_range <- FALSE
    pct_male <- NA_real_
    pct_female <- NA_real_
  }
  checks[[check_idx]] <- .make_student_check(
    "gender_distribution",
    "Gender split within expected range (40-60%)",
    if (!out_of_range) "PASS" else "WARN",
    if (out_of_range) 1L else 0L,
    if (out_of_range && !is.na(pct_male)) {
      paste0("Male: ", round(pct_male, 1), "%, Female: ", round(pct_female, 1), "%")
    } else if (out_of_range) {
      "No valid gender data"
    } else NA_character_
  )

  # 6. Missing rates for key variables
  check_idx <- check_idx + 1L
  thresholds <- c(gender = 1, race = 2, gold_literacy_fall_raw = 10)
  high_missing <- character()
  missing_details <- character()
  for (var in names(thresholds)) {
    if (var %in% names(df)) {
      pct <- sum(is.na(df[[var]])) / nrow(df) * 100
      if (pct > thresholds[var]) {
        high_missing <- c(high_missing, var)
        missing_details <- c(missing_details,
                             paste0(var, ": ", round(pct, 1), "% (max ", thresholds[var], "%)"))
      }
    }
  }
  checks[[check_idx]] <- .make_student_check(
    "missing_rates",
    "Key variables within acceptable missing rates",
    if (length(high_missing) == 0) "PASS" else "WARN",
    length(high_missing),
    if (length(high_missing) > 0) paste(missing_details, collapse = "; ") else NA_character_
  )

  # 7. Age range
  check_idx <- check_idx + 1L
  if ("age" %in% names(df)) {
    ages <- df$age[!is.na(df$age)]
    n_outlier <- sum(ages < 3 | ages > 7)
  } else {
    n_outlier <- 0L
  }
  checks[[check_idx]] <- .make_student_check(
    "age_range",
    "Ages within expected range (3-7)",
    if (n_outlier == 0) "PASS" else "WARN",
    n_outlier,
    if (n_outlier > 0) paste(n_outlier, "student(s) outside age 3-7 range") else NA_character_
  )

  # 8. Attendance range
  check_idx <- check_idx + 1L
  attend_cols <- c("days_absent_sem1", "days_absent_sem2")
  n_attend_bad <- 0L
  for (ac in intersect(attend_cols, names(df))) {
    vals <- df[[ac]][!is.na(df[[ac]])]
    n_attend_bad <- n_attend_bad + sum(vals < 0 | vals > 180)
  }
  checks[[check_idx]] <- .make_student_check(
    "attendance_range",
    "Absence counts in valid range (0-180 days)",
    if (n_attend_bad == 0) "PASS" else "WARN",
    n_attend_bad,
    if (n_attend_bad > 0) paste(n_attend_bad, "value(s) outside 0-180 range") else NA_character_
  )

  # 9. Income parse rate
  check_idx <- check_idx + 1L
  if (all(c("gross_income", "gross_income_midpoint") %in% names(df))) {
    has_income <- !is.na(df$gross_income) & nchar(as.character(df$gross_income)) > 0
    n_has_income <- sum(has_income)
    if (n_has_income > 0) {
      parse_rate <- sum(!is.na(df$gross_income_midpoint[has_income])) / n_has_income * 100
      low_parse <- parse_rate < 70
    } else {
      parse_rate <- NA_real_
      low_parse <- FALSE
    }
  } else {
    parse_rate <- NA_real_
    low_parse <- FALSE
  }
  checks[[check_idx]] <- .make_student_check(
    "income_parse_rate",
    "Income midpoint parsed for >=70% of records with income",
    if (!low_parse) "PASS" else "WARN",
    if (low_parse) 1L else 0L,
    if (low_parse && !is.na(parse_rate)) {
      paste0("Parse rate: ", round(parse_rate, 1), "% (target: 70%)")
    } else NA_character_
  )

  # 10. GOLD completeness
  check_idx <- check_idx + 1L
  gold_fall_raw <- grep("^gold_.*_fall_raw$", names(df), value = TRUE)
  if (length(gold_fall_raw) > 0) {
    gold_non_na_rates <- vapply(gold_fall_raw, function(col) {
      mean(!is.na(df[[col]])) * 100
    }, numeric(1))
    avg_gold_rate <- mean(gold_non_na_rates)
    low_gold <- avg_gold_rate < 80
  } else {
    avg_gold_rate <- NA_real_
    low_gold <- FALSE
  }
  checks[[check_idx]] <- .make_student_check(
    "gold_completeness",
    "GOLD fall raw scores >=80% non-NA",
    if (!low_gold) "PASS" else "WARN",
    if (low_gold) 1L else 0L,
    if (low_gold && !is.na(avg_gold_rate)) {
      paste0("Average GOLD fall completeness: ", round(avg_gold_rate, 1), "% (target: 80%)")
    } else NA_character_
  )

  # 11. Assessment consistency (GOLD scale scores non-negative)
  check_idx <- check_idx + 1L
  gold_scale_cols <- grep("^gold_.*_scale$", names(df), value = TRUE)
  n_neg_scores <- 0L
  for (col in gold_scale_cols) {
    vals <- df[[col]][!is.na(df[[col]])]
    n_neg_scores <- n_neg_scores + sum(vals < 0)
  }
  checks[[check_idx]] <- .make_student_check(
    "assessment_consistency",
    "GOLD scale scores non-negative where not NA",
    if (n_neg_scores == 0) "PASS" else "WARN",
    n_neg_scores,
    if (n_neg_scores > 0) paste(n_neg_scores, "negative GOLD scale score(s)") else NA_character_
  )

  # 12. School year consistency
  check_idx <- check_idx + 1L
  if ("school_year" %in% names(df)) {
    unique_sy <- unique(df$school_year)
    n_sy <- length(unique_sy)
  } else {
    n_sy <- 0L
  }
  checks[[check_idx]] <- .make_student_check(
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
    class = "alprek_student_validation"
  )

  if (passed) {
    msg_success("Validation passed ({check_idx} checks: {n_errors} errors, {n_warnings} warnings)")
  } else {
    msg_warn("Validation failed ({n_errors} error(s), {n_warnings} warning(s))")
  }

  result
}


#' Print method for alprek_student_validation
#' @param x An alprek_student_validation object.
#' @param ... Ignored.
#' @export
print.alprek_student_validation <- function(x, ...) {
  cat("<alprek_student_validation>\n")
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


# ---- Internal helpers ----

#' Create a student check result row
#' @keywords internal
.make_student_check <- function(check_name, check_description, status, n_issues, details) {
  list(
    check_name = check_name,
    check_description = check_description,
    status = status,
    n_issues = n_issues,
    details = details
  )
}
