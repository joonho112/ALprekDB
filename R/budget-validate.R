#' Validate Budget Data Quality
#'
#' @description Performs comprehensive data quality checks on cleaned budget
#'   data. Returns a validation report with pass/fail status for each check.
#'
#' @param budget_long_obj An `alprek_budget_long` object from [budget_clean()].
#' @param tolerance Numeric. Dollar tolerance for reconciliation mismatches.
#'   Default is `1.00`.
#' @param strict Logical. If `TRUE`, treats warnings as errors (overall result
#'   fails if any warning occurs). Default is `FALSE`.
#'
#' @return An `alprek_budget_validation` S3 object (list) with elements:
#'   - `passed`: logical overall result.
#'   - `n_errors`, `n_warnings`, `n_info`: counts by severity.
#'   - `checks`: tibble of individual check results.
#'   - `issues`: tibble of specific rows with problems.
#'
#' @examples
#' \dontrun{
#' raw <- budget_read("rptClassBudgets 2023-2024.xlsx", "2023-2024")
#' cleaned <- budget_clean(raw)
#' validation <- budget_validate(cleaned)
#' print(validation)
#' }
#'
#' @export
budget_validate <- function(budget_long_obj,
                            tolerance = 1.00,
                            strict = FALSE) {

  if (!inherits(budget_long_obj, "alprek_budget_long")) {
    stop("Expected an 'alprek_budget_long' object. Use budget_clean() first.",
         call. = FALSE)
  }

  long <- budget_long_obj$long
  totals <- budget_long_obj$totals
  checks <- list()
  issues <- tibble::tibble()

  msg_step(2, 3, "Validating budget data")

  # 1. Required columns
  required <- c("school_year", "classroom_code", "category_detail",
                "category_group", "source_type", "amount")
  missing <- setdiff(required, names(long))
  checks[[1]] <- .make_check(
    "required_columns",
    "Required columns present",
    if (length(missing) == 0) "PASS" else "ERROR",
    length(missing),
    if (length(missing) > 0) paste("Missing:", paste(missing, collapse = ", ")) else NA_character_
 )

  # 2. Key uniqueness (no duplicate classroom-year-category-source combos)
  dups <- long |>
    dplyr::group_by(.data$school_year, .data$classroom_code,
                    .data$category_detail, .data$source_type) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()

  checks[[2]] <- .make_check(
    "key_uniqueness",
    "No duplicate classroom-category-source combinations",
    if (nrow(dups) == 0) "PASS" else "ERROR",
    nrow(dups),
    if (nrow(dups) > 0) {
      paste(nrow(dups), "duplicate rows found")
    } else NA_character_
  )

  # 3. No negative values
  neg <- long |> dplyr::filter(.data$amount < 0)
  checks[[3]] <- .make_check(
    "no_negative_values",
    "No negative budget amounts",
    if (nrow(neg) == 0) "PASS" else "ERROR",
    nrow(neg),
    if (nrow(neg) > 0) {
      codes <- unique(neg$classroom_code)
      paste(nrow(neg), "negative values in", length(codes), "classroom(s)")
    } else NA_character_
  )

  # 4. Budget range (warn if any classroom total > $1M)
  classroom_totals <- long |>
    dplyr::group_by(.data$school_year, .data$classroom_code) |>
    dplyr::summarise(total = sum(.data$amount, na.rm = TRUE), .groups = "drop")

  out_of_range <- classroom_totals |>
    dplyr::filter(.data$total > 1000000 | .data$total < 0)

  checks[[4]] <- .make_check(
    "budget_range",
    "Budget values in reasonable range ($0-$1M per classroom)",
    if (nrow(out_of_range) == 0) "PASS" else "WARN",
    nrow(out_of_range),
    if (nrow(out_of_range) > 0) {
      paste(nrow(out_of_range), "classroom(s) outside expected range")
    } else NA_character_
  )

  # 5. Total reconciliation
  n_mismatch <- sum(totals$flag_total_mismatch, na.rm = TRUE)
  checks[[5]] <- .make_check(
    "total_reconciliation",
    paste0("Category sums match reported totals (within $", tolerance, ")"),
    if (n_mismatch == 0) "PASS" else "WARN",
    n_mismatch,
    if (n_mismatch > 0) {
      paste(n_mismatch, "classroom(s) with total mismatch")
    } else NA_character_
  )

  if (n_mismatch > 0) {
    flagged <- totals |> dplyr::filter(.data$flag_total_mismatch)
    issues <- dplyr::bind_rows(issues, flagged |>
      dplyr::select(dplyr::any_of(c("classroom_code", "school_year"))) |>
      dplyr::mutate(issue_type = "total_mismatch"))
  }

  # 6. Delivery type coverage
  dt_in_data <- unique(long$delivery_type[!is.na(long$delivery_type)])
  expected_dt <- c("Public School", "Private Child Care", "Head Start",
                   "Community Organization", "Faith-Based Organization",
                   "University Operated", "Private School")
  missing_dt <- setdiff(expected_dt, dt_in_data)

  checks[[6]] <- .make_check(
    "delivery_type_coverage",
    "All 7 delivery types represented",
    if (length(missing_dt) == 0) "PASS" else "INFO",
    length(missing_dt),
    if (length(missing_dt) > 0) {
      paste("Missing:", paste(missing_dt, collapse = ", "))
    } else NA_character_
  )

  # 7. Missing key identifiers
  na_codes <- sum(is.na(long$classroom_code))
  na_years <- sum(is.na(long$school_year))
  n_na <- na_codes + na_years

  checks[[7]] <- .make_check(
    "missing_identifiers",
    "No missing values in key identifier columns",
    if (n_na == 0) "PASS" else "WARN",
    n_na,
    if (n_na > 0) {
      paste("NA classroom_code:", na_codes, "| NA school_year:", na_years)
    } else NA_character_
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
    class = "alprek_budget_validation"
  )

  if (passed) {
    msg_success("Validation passed ({nrow(checks_df)} checks: {n_errors} errors, {n_warnings} warnings)")
  } else {
    msg_warn("Validation failed ({n_errors} error(s), {n_warnings} warning(s))")
  }

  result
}


#' Print method for alprek_budget_validation
#' @param x An alprek_budget_validation object.
#' @param ... Ignored.
#' @export
print.alprek_budget_validation <- function(x, ...) {
  cat("<alprek_budget_validation>\n")
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

#' Create a check result row
#' @keywords internal
.make_check <- function(check_name, check_description, status, n_issues, details) {
  list(
    check_name = check_name,
    check_description = check_description,
    status = status,
    n_issues = n_issues,
    details = details
  )
}
