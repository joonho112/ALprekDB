#' Transform Budget Data to Wide Format with Derived Variables
#'
#' @description Transforms long-format budget data into a wide, analysis-ready
#'   master dataset with one row per classroom-year. Adds derived variables
#'   including category totals, percentage shares, and delivery type groupings.
#'
#' @param budget_long_obj An `alprek_budget_long` object from [budget_clean()].
#' @param add_groups Logical. Add delivery type groupings (binary and 3-way)?
#'   Default is `TRUE`.
#' @param add_shares Logical. Add percentage share columns? Default is `TRUE`.
#' @param allocate_payroll Logical. Allocate payroll taxes proportionally to
#'   lead/aux teacher benefits? Applies only to legacy format. Default is `TRUE`.
#'
#' @return An `alprek_budget_master` S3 object (list) with elements:
#'   - `data`: tibble in wide format, one row per classroom-year.
#'   - `meta`: list of metadata.
#'
#' @examples
#' \dontrun{
#' raw <- budget_read("rptClassBudgets 2023-2024.xlsx", "2023-2024")
#' cleaned <- budget_clean(raw)
#' master <- budget_transform(cleaned)
#' master$data
#' }
#'
#' @importFrom dplyr mutate select group_by summarise across all_of any_of
#'   left_join ungroup case_when
#' @importFrom tidyr pivot_wider
#' @export
budget_transform <- function(budget_long_obj,
                             add_groups = TRUE,
                             add_shares = TRUE,
                             allocate_payroll = TRUE) {

  if (!inherits(budget_long_obj, "alprek_budget_long")) {
    stop("Expected an 'alprek_budget_long' object. Use budget_clean() first.",
         call. = FALSE)
  }

  msg_step(3, 3, "Transforming to wide format with derived variables")

  long <- budget_long_obj$long
  format_type <- budget_long_obj$meta$format

  # --- Pivot to wide: {source_type}_{category_group} ---
  wide <- long |>
    dplyr::group_by(
      .data$school_year, .data$year,
      .data$classroom_code, .data$classroom_name,
      .data$budget_version,
      .data$county_code, .data$delivery_type_code, .data$delivery_type,
      .data$program_code, .data$class_num,
      .data$source_type, .data$category_group
    ) |>
    dplyr::summarise(amount = sum(.data$amount, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      col_name = paste0(.data$source_type, "_", .data$category_group)
    ) |>
    dplyr::select(-"source_type", -"category_group") |>
    tidyr::pivot_wider(
      names_from = "col_name",
      values_from = "amount",
      values_fill = 0
    )

  # --- Payroll tax allocation (legacy only) ---
  if (allocate_payroll && format_type == "legacy") {
    wide <- .allocate_payroll_taxes(wide)
  }

  # --- Derived totals: total_{category} = osr + other ---
  category_groups <- c("lead_teacher_salary", "lead_teacher_benefits",
                       "aux_teacher_salary", "aux_teacher_benefits",
                       "instructional_support", "operations_and_maintenance",
                       "equipment", "administrative")

  for (cg in category_groups) {
    osr_col <- paste0("osr_", cg)
    oth_col <- paste0("other_", cg)
    tot_col <- paste0("total_", cg)

    # Ensure columns exist
    if (!osr_col %in% names(wide)) wide[[osr_col]] <- 0
    if (!oth_col %in% names(wide)) wide[[oth_col]] <- 0

    wide[[tot_col]] <- wide[[osr_col]] + wide[[oth_col]]
  }

  # --- Aggregate measures ---
  wide <- wide |>
    dplyr::mutate(
      total_lead_teacher = .data$total_lead_teacher_salary +
        .data$total_lead_teacher_benefits,
      total_aux_teacher = .data$total_aux_teacher_salary +
        .data$total_aux_teacher_benefits,
      total_teacher_compensation = .data$total_lead_teacher +
        .data$total_aux_teacher,
      osr_total = safe_row_sum(
        wide,
        paste0("osr_", category_groups)
      ),
      other_total = safe_row_sum(
        wide,
        paste0("other_", category_groups)
      ),
      grand_total = .data$osr_total + .data$other_total
    )

  # --- Percentage shares ---
  if (add_shares) {
    wide <- wide |>
      dplyr::mutate(
        share_osr = ifelse(.data$grand_total > 0,
                           .data$osr_total / .data$grand_total * 100, 0),
        share_other = ifelse(.data$grand_total > 0,
                             .data$other_total / .data$grand_total * 100, 0),
        share_teacher_compensation = ifelse(.data$grand_total > 0,
                                            .data$total_teacher_compensation /
                                              .data$grand_total * 100, 0)
      )

    for (cg in category_groups) {
      share_col <- paste0("share_", cg)
      total_col <- paste0("total_", cg)
      wide[[share_col]] <- ifelse(
        wide$grand_total > 0,
        wide[[total_col]] / wide$grand_total * 100,
        0
      )
    }
  }

  # --- Delivery type groupings ---
  if (add_groups) {
    wide <- wide |>
      dplyr::mutate(
        delivery_type_binary = dplyr::case_when(
          .data$delivery_type == "Public School" ~ "Public School",
          TRUE ~ "Non-Public Provider"
        ),
        delivery_type_3way = dplyr::case_when(
          .data$delivery_type == "Public School" ~ "Public School",
          .data$delivery_type == "Head Start" ~ "Head Start",
          TRUE ~ "Other Non-Public"
        )
      )
  }

  # --- Remove payroll_taxes columns if they exist (already allocated) ---
  payroll_cols <- grep("payroll_taxes", names(wide), value = TRUE)
  if (length(payroll_cols) > 0) {
    wide <- wide |> dplyr::select(-dplyr::all_of(payroll_cols))
  }

  # --- Build S3 object ---
  result <- structure(
    list(
      data = wide,
      meta = list(
        school_year = budget_long_obj$meta$school_year,
        format = format_type,
        n_classrooms = nrow(wide),
        n_columns = ncol(wide),
        has_payroll_allocation = allocate_payroll && format_type == "legacy",
        has_shares = add_shares,
        has_groups = add_groups,
        transformed_at = Sys.time()
      )
    ),
    class = "alprek_budget_master"
  )

  msg_success("Transformed {nrow(wide)} classrooms with {ncol(wide)} columns")
  result
}


#' Print method for alprek_budget_master
#' @param x An alprek_budget_master object.
#' @param ... Ignored.
#' @export
print.alprek_budget_master <- function(x, ...) {
  cat("<alprek_budget_master>\n")
  cat("  School year:", x$meta$school_year, "\n")
  cat("  Classrooms:", x$meta$n_classrooms, "\n")
  cat("  Columns:", x$meta$n_columns, "\n")
  cat("  Payroll allocated:", x$meta$has_payroll_allocation, "\n")
  cat("  Shares included:", x$meta$has_shares, "\n")
  invisible(x)
}


# ---- Internal helpers ----

#' Allocate payroll taxes proportionally to lead/aux teacher benefits
#'
#' @description In legacy format, payroll taxes are a single line item.
#'   This function allocates them proportionally based on the ratio of
#'   lead vs aux teacher salary, within each funding source.
#'
#' @param wide Wide-format tibble with osr_payroll_taxes and other_payroll_taxes.
#' @return Modified tibble with payroll allocated to benefits columns.
#' @keywords internal
.allocate_payroll_taxes <- function(wide) {
  # For each source: allocate payroll proportionally to salary ratios
  for (src in c("osr", "other")) {
    pt_col <- paste0(src, "_payroll_taxes")
    lt_sal_col <- paste0(src, "_lead_teacher_salary")
    at_sal_col <- paste0(src, "_aux_teacher_salary")
    lt_ben_col <- paste0(src, "_lead_teacher_benefits")
    at_ben_col <- paste0(src, "_aux_teacher_benefits")

    if (!pt_col %in% names(wide)) next
    if (!lt_sal_col %in% names(wide)) wide[[lt_sal_col]] <- 0
    if (!at_sal_col %in% names(wide)) wide[[at_sal_col]] <- 0
    if (!lt_ben_col %in% names(wide)) wide[[lt_ben_col]] <- 0
    if (!at_ben_col %in% names(wide)) wide[[at_ben_col]] <- 0

    total_sal <- wide[[lt_sal_col]] + wide[[at_sal_col]]
    lt_ratio <- ifelse(total_sal > 0,
                       wide[[lt_sal_col]] / total_sal, 0.5)
    at_ratio <- 1 - lt_ratio

    pt <- wide[[pt_col]]
    wide[[lt_ben_col]] <- wide[[lt_ben_col]] + pt * lt_ratio
    wide[[at_ben_col]] <- wide[[at_ben_col]] + pt * at_ratio
  }

  wide
}
