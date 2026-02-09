#' Combine Multiple Years into a Student Panel Dataset
#'
#' @description Binds multiple single-year `alprek_student_clean` objects into
#'   a longitudinal panel dataset. Unlike the classroom module, no imputation
#'   is applied since student attributes change year-to-year.
#'
#' @param ... `alprek_student_clean` objects to combine.
#' @param clean_list Optional list of `alprek_student_clean` objects.
#'   Alternative to `...` for programmatic use.
#'
#' @return An `alprek_student_panel` S3 object (list) with elements:
#'   - `data`: tibble of combined panel data.
#'   - `years`: character vector of school years included.
#'   - `n_total`: total number of student-year observations.
#'   - `n_unique_students`: count of unique ADECE IDs.
#'   - `by_year`: list with per-year metadata.
#'
#' @examples
#' \dontrun{
#' panel <- student_bind_years(clean_2122, clean_2223, clean_2324)
#' panel$data
#' }
#'
#' @importFrom dplyr bind_rows arrange
#' @export
student_bind_years <- function(..., clean_list = NULL) {
  if (is.null(clean_list)) {
    clean_list <- list(...)
  }

  if (length(clean_list) == 0) {
    stop("No data to combine. Provide alprek_student_clean objects.", call. = FALSE)
  }

  # Validate inputs
  for (i in seq_along(clean_list)) {
    if (!inherits(clean_list[[i]], "alprek_student_clean")) {
      stop("Element ", i, " is not an alprek_student_clean object.", call. = FALSE)
    }
  }

  msg_info("Building student panel from {length(clean_list)} year(s)")

  # Combine data — bind_rows handles column union automatically
  combined <- dplyr::bind_rows(lapply(clean_list, function(m) m$data))
  combined <- combined |>
    dplyr::arrange(.data$school_year, .data$adece_id)

  years <- sort(unique(combined$school_year))

  # Count unique students
  if ("adece_id" %in% names(combined)) {
    n_unique <- length(unique(combined$adece_id[!is.na(combined$adece_id)]))
  } else {
    n_unique <- NA_integer_
  }

  by_year <- lapply(clean_list, function(m) {
    list(
      school_year = m$meta$school_year,
      format = m$meta$format,
      n_students = m$meta$n_students,
      n_cols = m$meta$n_cols
    )
  })
  names(by_year) <- sapply(clean_list, function(m) m$meta$school_year)

  result <- structure(
    list(
      data = combined,
      years = years,
      n_total = nrow(combined),
      n_unique_students = n_unique,
      by_year = by_year
    ),
    class = "alprek_student_panel"
  )

  msg_success("Panel: {nrow(combined)} student-years across {length(years)} year(s) ({n_unique} unique students)")
  result
}


#' Print method for alprek_student_panel
#' @param x An alprek_student_panel object.
#' @param ... Ignored.
#' @export
print.alprek_student_panel <- function(x, ...) {
  cat("<alprek_student_panel>\n")
  cat("  Years:", paste(x$years, collapse = ", "), "\n")
  cat("  Total observations:", x$n_total, "\n")
  cat("  Unique students:", x$n_unique_students, "\n")
  cat("  Columns:", ncol(x$data), "\n")
  for (yr in names(x$by_year)) {
    info <- x$by_year[[yr]]
    cat("    ", yr, ":", info$n_students, "students (",
        info$format, ",", info$n_cols, "cols)\n")
  }
  invisible(x)
}


#' Track Student Presence Across Years
#'
#' @description Creates a binary matrix showing which students are present
#'   in which school years (based on ADECE ID).
#'
#' @param panel An `alprek_student_panel` object.
#'
#' @return A tibble with adece_id and one logical column per school year.
#'
#' @examples
#' \dontrun{
#' panel <- student_bind_years(clean_list = list(c1, c2, c3, c4))
#' track <- student_track(panel)
#' # Count students present in all years
#' sum(rowSums(track[, -1]) == length(panel$years))
#' }
#'
#' @importFrom tidyr pivot_wider
#' @export
student_track <- function(panel) {
  if (!inherits(panel, "alprek_student_panel")) {
    stop("Expected an 'alprek_student_panel' object.", call. = FALSE)
  }

  df <- panel$data
  presence <- unique(df[, c("adece_id", "school_year")])
  presence <- presence[!is.na(presence$adece_id), ]
  presence$present <- TRUE

  wide <- tidyr::pivot_wider(
    presence,
    names_from = "school_year",
    values_from = "present",
    values_fill = FALSE
  )

  # Sort columns by year
  year_cols <- setdiff(names(wide), "adece_id")
  year_cols <- sort(year_cols)
  wide[, c("adece_id", year_cols)]
}


#' Summary Statistics for Student Data
#'
#' @description Computes summary statistics for student data, either
#'   by school year (default) or overall.
#'
#' @param x An `alprek_student_clean` or `alprek_student_panel` object.
#' @param by Character. Grouping variable. Default `"school_year"`.
#'   Use `NULL` for overall statistics.
#'
#' @return A tibble of summary statistics.
#'
#' @importFrom dplyr group_by summarise n
#' @export
student_summary_stats <- function(x, by = "school_year") {
  if (inherits(x, "alprek_student_clean")) {
    df <- x$data
  } else if (inherits(x, "alprek_student_panel")) {
    df <- x$data
  } else {
    stop("Expected alprek_student_clean or alprek_student_panel.", call. = FALSE)
  }

  .compute_student_stats <- function(d) {
    n_students <- nrow(d)

    # Demographics
    pct_male <- if ("gender" %in% names(d)) {
      mean(as.character(d$gender) == "Male", na.rm = TRUE) * 100
    } else NA_real_

    pct_poverty <- if ("poverty_dum" %in% names(d)) {
      mean(d$poverty_dum == 1L, na.rm = TRUE) * 100
    } else NA_real_

    pct_iep <- if ("iep" %in% names(d)) {
      mean(d$iep == 1L, na.rm = TRUE) * 100
    } else NA_real_

    mean_absent <- if ("days_absent_total" %in% names(d)) {
      mean(d$days_absent_total, na.rm = TRUE)
    } else NA_real_

    # Assessment completeness
    gold_literacy_fall <- if ("gold_literacy_fall_raw" %in% names(d)) {
      mean(!is.na(d$gold_literacy_fall_raw)) * 100
    } else NA_real_

    gold_literacy_spring <- if ("gold_literacy_spring_raw" %in% names(d)) {
      mean(!is.na(d$gold_literacy_spring_raw)) * 100
    } else NA_real_

    tibble::tibble(
      n_students = n_students,
      pct_male = round(pct_male, 1),
      pct_poverty = round(pct_poverty, 1),
      pct_iep = round(pct_iep, 1),
      mean_days_absent = round(mean_absent, 1),
      gold_lit_fall_pct = round(gold_literacy_fall, 1),
      gold_lit_spring_pct = round(gold_literacy_spring, 1)
    )
  }

  if (!is.null(by) && by %in% names(df)) {
    groups <- unique(df[[by]])
    groups <- sort(groups)
    stats_list <- lapply(groups, function(g) {
      sub_df <- df[df[[by]] == g, , drop = FALSE]
      row <- .compute_student_stats(sub_df)
      row[[by]] <- g
      row
    })
    stats <- dplyr::bind_rows(stats_list)
    # Reorder so 'by' column is first
    stats <- stats[, c(by, setdiff(names(stats), by)), drop = FALSE]
  } else {
    stats <- .compute_student_stats(df)
  }

  stats
}
