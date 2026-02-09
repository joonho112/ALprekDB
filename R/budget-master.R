#' Combine Multiple Years into a Panel Dataset
#'
#' @description Binds multiple single-year `alprek_budget_master` objects into
#'   a longitudinal panel dataset.
#'
#' @param ... `alprek_budget_master` objects to combine.
#' @param master_list Optional list of `alprek_budget_master` objects.
#'   Alternative to `...` for programmatic use.
#'
#' @return An `alprek_budget_panel` S3 object.
#'
#' @examples
#' \dontrun{
#' panel <- budget_bind_years(master_2122, master_2223, master_2324)
#' panel$data
#' }
#'
#' @importFrom dplyr bind_rows arrange
#' @export
budget_bind_years <- function(..., master_list = NULL) {
  if (is.null(master_list)) {
    master_list <- list(...)
  }

  if (length(master_list) == 0) {
    stop("No data to combine. Provide alprek_budget_master objects.", call. = FALSE)
  }

  # Validate inputs
  for (i in seq_along(master_list)) {
    if (!inherits(master_list[[i]], "alprek_budget_master")) {
      stop("Element ", i, " is not an alprek_budget_master object.", call. = FALSE)
    }
  }

  # Combine data
  combined <- dplyr::bind_rows(lapply(master_list, function(m) m$data))
  combined <- combined |>
    dplyr::arrange(.data$school_year, .data$classroom_code)

  years <- sort(unique(combined$school_year))
  by_year <- lapply(master_list, function(m) {
    list(
      school_year = m$meta$school_year,
      format = m$meta$format,
      n_classrooms = m$meta$n_classrooms
    )
  })
  names(by_year) <- sapply(master_list, function(m) m$meta$school_year)

  result <- structure(
    list(
      data = combined,
      years = years,
      n_years = length(years),
      by_year = by_year
    ),
    class = "alprek_budget_panel"
  )

  msg_success("Combined {length(years)} years: {paste(years, collapse = ', ')}")
  msg_info("Total rows: {nrow(combined)}")
  result
}


#' Print method for alprek_budget_panel
#' @param x An alprek_budget_panel object.
#' @param ... Ignored.
#' @export
print.alprek_budget_panel <- function(x, ...) {
  cat("<alprek_budget_panel>\n")
  cat("  Years:", paste(x$years, collapse = ", "), "\n")
  cat("  Total rows:", nrow(x$data), "\n")
  for (yr in names(x$by_year)) {
    info <- x$by_year[[yr]]
    cat("    ", yr, ":", info$n_classrooms, "classrooms (", info$format, ")\n")
  }
  invisible(x)
}


#' Track Classroom Presence Across Years
#'
#' @description Creates a summary showing which classrooms are present in
#'   which years, useful for identifying new, continuing, and exiting classrooms.
#'
#' @param panel An `alprek_budget_panel` object.
#' @return A tibble with one row per classroom and logical columns for each year.
#'
#' @importFrom dplyr group_by summarise across distinct mutate n_distinct
#' @importFrom tidyr pivot_wider
#' @export
budget_track_classrooms <- function(panel) {
  if (!inherits(panel, "alprek_budget_panel")) {
    stop("Expected an alprek_budget_panel object.", call. = FALSE)
  }

  # Which years is each classroom present in?
  presence <- panel$data |>
    dplyr::distinct(.data$classroom_code, .data$school_year,
                    .data$delivery_type, .data$county_code) |>
    dplyr::mutate(present = TRUE) |>
    tidyr::pivot_wider(
      names_from = "school_year",
      values_from = "present",
      values_fill = FALSE
    )

  # Add summary columns
  year_cols <- intersect(panel$years, names(presence))
  presence$n_years_present <- rowSums(presence[year_cols])
  presence$all_years <- presence$n_years_present == panel$n_years

  presence
}


#' Summarize Year-Over-Year Changes
#'
#' @description Calculates aggregate year-over-year changes in budget measures.
#'
#' @param panel An `alprek_budget_panel` object.
#' @return A tibble with YoY statistics by year.
#'
#' @importFrom dplyr group_by summarise n across
#' @export
budget_yoy_summary <- function(panel) {
  if (!inherits(panel, "alprek_budget_panel")) {
    stop("Expected an alprek_budget_panel object.", call. = FALSE)
  }

  panel$data |>
    dplyr::group_by(.data$school_year) |>
    dplyr::summarise(
      n_classrooms = dplyr::n(),
      mean_grand_total = mean(.data$grand_total, na.rm = TRUE),
      median_grand_total = stats::median(.data$grand_total, na.rm = TRUE),
      mean_osr_total = mean(.data$osr_total, na.rm = TRUE),
      mean_other_total = mean(.data$other_total, na.rm = TRUE),
      mean_share_osr = mean(.data$share_osr, na.rm = TRUE),
      mean_share_teacher_comp = mean(.data$share_teacher_compensation,
                                     na.rm = TRUE),
      .groups = "drop"
    )
}


#' Convert Master/Panel to Long Format
#'
#' @description Converts a wide-format master or panel dataset back to long
#'   format for visualization or further analysis.
#'
#' @param x An `alprek_budget_master` or `alprek_budget_panel` object.
#' @param include_zeros Logical. Include zero-amount rows? Default is `TRUE`.
#'
#' @return A tibble in long format.
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter select any_of
#' @export
budget_to_long <- function(x, include_zeros = TRUE) {
  df <- if (inherits(x, "alprek_budget_master")) {
    x$data
  } else if (inherits(x, "alprek_budget_panel")) {
    x$data
  } else {
    stop("Expected alprek_budget_master or alprek_budget_panel.", call. = FALSE)
  }

  # Identify osr_ and other_ budget columns (not total_, share_, etc.)
  budget_cols <- grep("^(osr_|other_)", names(df), value = TRUE)
  id_cols <- setdiff(names(df), c(budget_cols,
                                  grep("^(total_|share_|grand_|delivery_type_binary|delivery_type_3way)",
                                       names(df), value = TRUE)))

  long <- df |>
    dplyr::select(dplyr::all_of(c(id_cols, budget_cols))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(budget_cols),
      names_to = "budget_column",
      values_to = "amount"
    ) |>
    dplyr::mutate(
      source_type = sub("_.*", "", .data$budget_column),
      category_group = sub("^(osr|other)_", "", .data$budget_column)
    )

  if (!include_zeros) {
    long <- long |> dplyr::filter(.data$amount != 0)
  }

  long
}


#' Calculate Summary Statistics by Group
#'
#' @description Calculates descriptive statistics for budget data grouped by
#'   one or more variables.
#'
#' @param x An `alprek_budget_master` or `alprek_budget_panel` object.
#' @param by Character vector of grouping variable names.
#'   Default is `"school_year"`.
#'
#' @return A tibble with summary statistics.
#'
#' @importFrom dplyr group_by across summarise n all_of
#' @export
budget_summary_stats <- function(x, by = "school_year") {
  df <- if (inherits(x, "alprek_budget_master")) {
    x$data
  } else if (inherits(x, "alprek_budget_panel")) {
    x$data
  } else {
    stop("Expected alprek_budget_master or alprek_budget_panel.", call. = FALSE)
  }

  numeric_cols <- c("grand_total", "osr_total", "other_total",
                    "total_teacher_compensation",
                    "share_osr", "share_teacher_compensation")
  numeric_cols <- intersect(numeric_cols, names(df))

  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(
      n = dplyr::n(),
      dplyr::across(
        dplyr::all_of(numeric_cols),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd = ~ stats::sd(.x, na.rm = TRUE),
          median = ~ stats::median(.x, na.rm = TRUE),
          min = ~ min(.x, na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
}
