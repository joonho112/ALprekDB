#' Clean Budget Data to Long Format
#'
#' @description Transforms raw budget data into a long-format intermediate
#'   representation: one row per (classroom x category_detail x source_type).
#'   Also produces reconciliation totals for validation.
#'
#' @param raw An `alprek_budget_raw` object from [budget_read()].
#' @param category_groups Optional tibble with `category_detail` and
#'   `category_group` columns. Defaults to the package codebook.
#' @param tolerance Numeric. Dollar tolerance for reconciliation diagnostics.
#'   Default is `1.00`.
#'
#' @return An `alprek_budget_long` S3 object (list) with elements:
#'   - `long`: tibble in long format with budget amounts.
#'   - `totals`: tibble with reconciliation diagnostics per classroom.
#'   - `meta`: list of metadata.
#'
#' @examples
#' \dontrun{
#' raw <- budget_read("rptClassBudgets 2023-2024.xlsx", "2023-2024")
#' cleaned <- budget_clean(raw)
#' cleaned$long      # long format data
#' cleaned$totals    # reconciliation diagnostics
#' }
#'
#' @importFrom dplyr mutate filter select rename across all_of any_of bind_rows
#'   left_join group_by summarise coalesce n
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom stringr str_detect str_remove str_squish str_to_lower str_replace_all
#' @importFrom rlang .data
#' @export
budget_clean <- function(raw,
                         category_groups = NULL,
                         tolerance = 1.00) {

  if (!inherits(raw, "alprek_budget_raw")) {
    stop("Expected an 'alprek_budget_raw' object. Use budget_read() first.",
         call. = FALSE)
  }

  if (is.null(category_groups)) {
    category_groups <- alprek_category_groups()
  }

  format_type <- raw$meta$format
  msg_step(1, 3, "Cleaning {format_type} format budget data")

  if (format_type == "legacy") {
    result <- .clean_legacy(raw$data, raw$meta, category_groups, tolerance)
  } else if (format_type == "new") {
    result <- .clean_new(raw$data, raw$meta, category_groups, tolerance)
  } else {
    stop("Unknown format: ", format_type, call. = FALSE)
  }

  # Build S3 object
  output <- structure(
    list(
      long = result$budget_long,
      totals = result$budget_totals,
      meta = list(
        format = format_type,
        school_year = raw$meta$school_year,
        year = raw$meta$year,
        n_classrooms = length(unique(result$budget_long$classroom_code)),
        n_long_rows = nrow(result$budget_long),
        category_groups_used = category_groups,
        cleaned_at = Sys.time()
      )
    ),
    class = "alprek_budget_long"
  )

  msg_success(
    "Cleaned {output$meta$n_classrooms} classrooms into {output$meta$n_long_rows} long-format rows"
  )
  output
}


#' Print method for alprek_budget_long
#' @param x An alprek_budget_long object.
#' @param ... Ignored.
#' @export
print.alprek_budget_long <- function(x, ...) {
  cat("<alprek_budget_long>\n")
  cat("  School year:", x$meta$school_year, "\n")
  cat("  Format:", x$meta$format, "\n")
  cat("  Classrooms:", x$meta$n_classrooms, "\n")
  cat("  Long-format rows:", x$meta$n_long_rows, "\n")

  # Reconciliation summary
  n_flagged <- sum(x$totals$flag_total_mismatch, na.rm = TRUE)
  if (n_flagged > 0) {
    cat("  Total mismatches:", n_flagged, "\n")
  } else {
    cat("  Reconciliation: all totals match\n")
  }
  invisible(x)
}


# ============================================================================
# LEGACY FORMAT CLEANING (2021-2024)
# ============================================================================

#' @keywords internal
.clean_legacy <- function(df, meta, category_groups, tolerance) {

  # Identify the Budget Version column (variable name includes year)
  bv_col <- grep("^Budget Version", names(df), value = TRUE)[1]
  if (is.na(bv_col)) bv_col <- "budget_version"  # fallback

  school_year <- meta$school_year
  year <- meta$year

  # Rename core ID/total columns
  rename_map <- c(
    classroom_name = "Classroom Name",
    classroom_code = "Classroom Code",
    total_osr_reported = "Total From OSR Funds",
    total_other_reported = "Total From Additional Sources",
    total_grand_reported = "Grand Total"
  )
  # Add budget version with dynamic name
  if (bv_col %in% names(df) && bv_col != "budget_version") {
    rename_map <- c(rename_map, setNames(bv_col, "budget_version"))
  }

  for (new_nm in names(rename_map)) {
    old_nm <- rename_map[new_nm]
    if (old_nm %in% names(df)) {
      names(df)[names(df) == old_nm] <- new_nm
    }
  }

  # Ensure budget_version exists
 if (!"budget_version" %in% names(df)) {
    df$budget_version <- NA_character_
  }

  key_cols <- c("school_year", "year", "classroom_code", "classroom_name",
                "budget_version")

  # --- OSR line items ---
  osr_cols <- grep("From OSR Funds$", names(df), value = TRUE)
  # Exclude the total column
  osr_cols <- setdiff(osr_cols, "total_osr_reported")

  osr_long <- df |>
    dplyr::select(dplyr::all_of(c(key_cols, osr_cols))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(osr_cols),
      names_to = "category_detail",
      values_to = "amount"
    ) |>
    dplyr::mutate(
      category_detail = stringr::str_remove(.data$category_detail, " From OSR Funds$"),
      source_type = "osr",
      source_detail = "osr",
      amount = dplyr::coalesce(as.numeric(.data$amount), 0)
    )

  # --- Additional Funds line items ---
  add_fund_cols <- grep("From Additional Funds [12]$", names(df), value = TRUE)
  add_src_cols <- grep("Additional Source [12]$", names(df), value = TRUE)

  if (length(add_fund_cols) > 0) {
    add_long <- df |>
      dplyr::select(dplyr::all_of(c(key_cols, add_fund_cols))) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(add_fund_cols),
        names_to = "raw_name",
        values_to = "amount"
      ) |>
      dplyr::mutate(
        amount = dplyr::coalesce(as.numeric(.data$amount), 0),
        # Parse: "{Category} From Additional Funds {slot}"
        slot = as.integer(stringr::str_extract(.data$raw_name, "[12]$")),
        category_detail = stringr::str_remove(.data$raw_name, " From Additional Funds [12]$")
      )

    # Get source labels if available
    if (length(add_src_cols) > 0) {
      src_long <- df |>
        dplyr::select(dplyr::all_of(c(key_cols, add_src_cols))) |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(add_src_cols),
          names_to = "raw_name",
          values_to = "source_detail"
        ) |>
        dplyr::mutate(
          slot = as.integer(stringr::str_extract(.data$raw_name, "[12]$")),
          category_detail = stringr::str_remove(.data$raw_name, " Additional Source [12]$"),
          source_detail = stringr::str_squish(as.character(.data$source_detail)),
          source_detail = dplyr::na_if(.data$source_detail, "")
        ) |>
        dplyr::select(dplyr::all_of(c(key_cols, "category_detail", "slot", "source_detail")))

      add_long <- add_long |>
        dplyr::left_join(src_long,
                         by = c(key_cols, "category_detail", "slot"))
    } else {
      add_long$source_detail <- NA_character_
    }

    add_long <- add_long |>
      dplyr::mutate(
        source_type = "other",
        source_detail = dplyr::coalesce(.data$source_detail, "unspecified")
      ) |>
      dplyr::filter(.data$amount != 0) |>
      dplyr::select(-"raw_name", -"slot") |>
      # Aggregate Additional Funds 1 & 2 into one row per category-classroom
      dplyr::group_by(dplyr::across(dplyr::all_of(c(key_cols, "category_detail",
                                                      "source_type")))) |>
      dplyr::summarise(
        amount = sum(.data$amount, na.rm = TRUE),
        source_detail = paste(unique(.data$source_detail[.data$source_detail != "unspecified"]),
                              collapse = "; ") |>
          (\(x) ifelse(x == "", "unspecified", x))(),
        .groups = "drop"
      )

  } else {
    add_long <- tibble::tibble(
      school_year = character(), year = integer(),
      classroom_code = character(), classroom_name = character(),
      budget_version = character(),
      category_detail = character(), amount = numeric(),
      source_type = character(), source_detail = character()
    )
  }

  # --- Combine ---
  budget_long <- dplyr::bind_rows(osr_long, add_long)

  # --- Attach category_group from codebook ---
  budget_long <- budget_long |>
    dplyr::left_join(
      category_groups |> dplyr::select("category_detail", "category_group"),
      by = "category_detail"
    ) |>
    dplyr::mutate(
      category_group = dplyr::coalesce(.data$category_group, "unmapped")
    )

  # --- Parse classroom codes ---
  parsed <- parse_classroom_codes(budget_long$classroom_code)
  budget_long <- dplyr::bind_cols(
    budget_long |> dplyr::select(-dplyr::any_of(names(parsed))),
    parsed
  )

  # --- Reconciliation totals ---
  budget_totals <- .build_totals_legacy(budget_long, df, key_cols, tolerance)

  list(budget_long = budget_long, budget_totals = budget_totals)
}


# ============================================================================
# NEW FORMAT CLEANING (2024-2025+)
# ============================================================================

#' @keywords internal
.clean_new <- function(df, meta, category_groups, tolerance) {

  bv_col <- grep("^Budget Version", names(df), value = TRUE)[1]
  if (is.na(bv_col)) bv_col <- "budget_version"

  school_year <- meta$school_year
  year <- meta$year

  # Rename core/total columns
  rename_pairs <- list(
    c("Classroom Name", "classroom_name"),
    c("Classroom Code", "classroom_code"),
    c("Total", "total_expenditure"),
    c("OSR Grant Amount", "osr_grant_amount"),
    c("Proration Total", "proration_total"),
    c("Federal Funds used for Other Funds Expenditures", "other_federal"),
    c("Local Funds used for Other Funds Expenditures", "other_local"),
    c("Other Funds used for Other Funds Expenditures", "other_other"),
    c("Total Federal, Local, and Other Funds used for Other Funds Expenditures", "total_other_expenditure"),
    c("Total Additional OSR Funds", "total_additional_osr_funds"),
    c("Total of ALL OSR Grants", "total_osr_grants")
  )

  for (pair in rename_pairs) {
    if (pair[1] %in% names(df)) {
      names(df)[names(df) == pair[1]] <- pair[2]
    }
  }

  if (bv_col %in% names(df) && bv_col != "budget_version") {
    names(df)[names(df) == bv_col] <- "budget_version"
  }
  if (!"budget_version" %in% names(df)) {
    df$budget_version <- NA_character_
  }

  key_cols <- c("school_year", "year", "classroom_code", "classroom_name",
                "budget_version")

  total_cols <- c("total_expenditure", "osr_grant_amount", "proration_total",
                  "other_federal", "other_local", "other_other",
                  "total_other_expenditure", "total_additional_osr_funds",
                  "total_osr_grants")

  # Identify category columns by pattern
  all_cols <- names(df)

  osr_cat_cols <- all_cols[
    grepl("\\bOSR\\b", all_cols) &
      !(all_cols %in% c("osr_grant_amount", "total_osr_grants",
                        "total_additional_osr_funds")) &
      !grepl("^total_", all_cols) &
      !(all_cols %in% c(key_cols, total_cols))
  ]

  other_cat_cols <- all_cols[
    grepl("\\bOther\\b", all_cols) &
      !grepl("Funds used for Other Funds Expenditures", all_cols) &
      !(all_cols %in% c(key_cols, total_cols))
  ]

  # Normalize category name: strip "(including Payroll Taxes)", then "OSR"/"Other"
  # ORDER MATTERS: must remove parenthetical FIRST so "OSR$" regex can match
  normalize_cat <- function(x) {
    x <- stringr::str_squish(x)
    x <- stringr::str_remove(x, "\\s*\\(including Payroll Taxes\\)")
    x <- stringr::str_remove(x, "\\s+OSR$")
    x <- stringr::str_remove(x, "\\s+Other$")
    stringr::str_squish(x)
  }

  # --- OSR categories to long ---
  osr_long <- df |>
    dplyr::select(dplyr::all_of(c(key_cols, osr_cat_cols))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(osr_cat_cols),
      names_to = "category_detail",
      values_to = "amount"
    ) |>
    dplyr::mutate(
      category_detail = normalize_cat(.data$category_detail),
      source_type = "osr",
      source_detail = "osr",
      amount = dplyr::coalesce(as.numeric(.data$amount), 0)
    )

  # --- Other categories to long ---
  other_long <- df |>
    dplyr::select(dplyr::all_of(c(key_cols, other_cat_cols))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(other_cat_cols),
      names_to = "category_detail",
      values_to = "amount"
    ) |>
    dplyr::mutate(
      category_detail = normalize_cat(.data$category_detail),
      source_type = "other",
      source_detail = "other",
      amount = dplyr::coalesce(as.numeric(.data$amount), 0)
    )

  # --- Combine ---
  budget_long <- dplyr::bind_rows(osr_long, other_long)

  # --- Attach category_group from codebook ---
  budget_long <- budget_long |>
    dplyr::left_join(
      category_groups |> dplyr::select("category_detail", "category_group"),
      by = "category_detail"
    ) |>
    dplyr::mutate(
      category_group = dplyr::coalesce(
        .data$category_group,
        stringr::str_to_lower(.data$category_detail) |>
          stringr::str_replace_all("[^a-z0-9]+", "_") |>
          stringr::str_replace_all("^_|_$", "")
      )
    )

  # --- Parse classroom codes ---
  parsed <- parse_classroom_codes(budget_long$classroom_code)
  budget_long <- dplyr::bind_cols(
    budget_long |> dplyr::select(-dplyr::any_of(names(parsed))),
    parsed
  )

  # --- Reconciliation totals ---
  budget_totals <- .build_totals_new(budget_long, df, key_cols, total_cols, tolerance)

  list(budget_long = budget_long, budget_totals = budget_totals)
}


# ============================================================================
# RECONCILIATION HELPERS
# ============================================================================

#' Build reconciliation totals for legacy format
#' @keywords internal
.build_totals_legacy <- function(budget_long, df_raw, key_cols, tolerance) {
  # Sum by source_type
  sums <- budget_long |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(key_cols, "source_type")))) |>
    dplyr::summarise(source_sum = sum(.data$amount, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = "source_type",
      values_from = "source_sum",
      values_fill = 0
    )

  # Ensure osr and other columns exist
  if (!"osr" %in% names(sums)) sums$osr <- 0
  if (!"other" %in% names(sums)) sums$other <- 0

  sums <- sums |>
    dplyr::rename(osr_line_sum = "osr", other_line_sum = "other")

  # Get reported totals
  reported <- df_raw |>
    dplyr::select(dplyr::all_of(c(key_cols,
                                  "total_osr_reported", "total_other_reported",
                                  "total_grand_reported"))) |>
    dplyr::distinct()

  sums |>
    dplyr::left_join(reported, by = key_cols) |>
    dplyr::mutate(
      osr_diff = .data$osr_line_sum - as.numeric(.data$total_osr_reported),
      other_diff = .data$other_line_sum - as.numeric(.data$total_other_reported),
      grand_diff = (.data$osr_line_sum + .data$other_line_sum) -
        as.numeric(.data$total_grand_reported),
      flag_total_mismatch = abs(.data$grand_diff) > tolerance,
      flag_osr_mismatch = abs(.data$osr_diff) > tolerance,
      flag_other_mismatch = abs(.data$other_diff) > tolerance
    )
}


#' Build reconciliation totals for new format
#' @keywords internal
.build_totals_new <- function(budget_long, df_raw, key_cols, total_cols, tolerance) {
  # Sum by source_type
  sums <- budget_long |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(key_cols, "source_type")))) |>
    dplyr::summarise(source_sum = sum(.data$amount, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = "source_type",
      values_from = "source_sum",
      values_fill = 0
    )

  if (!"osr" %in% names(sums)) sums$osr <- 0
  if (!"other" %in% names(sums)) sums$other <- 0

  sums <- sums |>
    dplyr::rename(osr_expenditure_sum = "osr", other_expenditure_sum = "other")

  # Get reported totals
  existing_total_cols <- intersect(total_cols, names(df_raw))
  reported <- df_raw |>
    dplyr::select(dplyr::all_of(c(key_cols, existing_total_cols))) |>
    dplyr::distinct()

  result <- sums |>
    dplyr::left_join(reported, by = key_cols) |>
    dplyr::mutate(
      computed_grand = .data$osr_expenditure_sum + .data$other_expenditure_sum,
      grand_diff = .data$computed_grand - as.numeric(.data$total_expenditure),
      other_diff = .data$other_expenditure_sum -
        as.numeric(.data$total_other_expenditure),
      flag_total_mismatch = abs(.data$grand_diff) > tolerance,
      flag_other_mismatch = abs(.data$other_diff) > tolerance
    )

  result
}
