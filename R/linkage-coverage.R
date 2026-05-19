# Internal helpers for linkage year-coverage metadata.

#' Extract school years from a panel object
#' @keywords internal
.linkage_panel_years <- function(panel) {
  years <- panel$years
  if (is.null(years) && !is.null(panel$data) && "school_year" %in% names(panel$data)) {
    years <- unique(panel$data$school_year)
  }
  sort(unique(as.character(years)))
}


#' Build two-panel year coverage metadata
#' @keywords internal
.linkage_year_coverage <- function(left_years, right_years,
                                   left_label = "left",
                                   right_label = "right") {
  left_years <- sort(unique(as.character(left_years)))
  right_years <- sort(unique(as.character(right_years)))

  list(
    left_label = left_label,
    right_label = right_label,
    left_years = left_years,
    right_years = right_years,
    overlap_years = intersect(left_years, right_years),
    left_only_years = setdiff(left_years, right_years),
    right_only_years = setdiff(right_years, left_years)
  )
}


#' Build three-panel year coverage metadata for linked master objects
#' @keywords internal
.linkage_master_coverage <- function(budget, classroom, student) {
  budget_years <- .linkage_panel_years(budget)
  classroom_years <- .linkage_panel_years(classroom)
  student_years <- .linkage_panel_years(student)
  analysis_years <- sort(unique(c(budget_years, classroom_years, student_years)))
  classroom_student_years <- sort(unique(c(classroom_years, student_years)))
  coverage_table <- data.frame(
    school_year = analysis_years,
    has_budget = analysis_years %in% budget_years,
    has_classroom = analysis_years %in% classroom_years,
    has_student = analysis_years %in% student_years,
    n_budget_rows = vapply(
      analysis_years,
      function(yr) .linkage_count_year_rows(budget$data, yr),
      integer(1)
    ),
    n_classroom_rows = vapply(
      analysis_years,
      function(yr) .linkage_count_year_rows(classroom$data, yr),
      integer(1)
    ),
    n_student_rows = vapply(
      analysis_years,
      function(yr) .linkage_count_year_rows(student$data, yr),
      integer(1)
    ),
    stringsAsFactors = FALSE
  )
  coverage_table$budget_status <- ifelse(
    coverage_table$has_budget,
    "available",
    "missing_budget"
  )

  list(
    analysis_years = analysis_years,
    budget_years = budget_years,
    classroom_years = classroom_years,
    student_years = student_years,
    by_year = coverage_table,
    all_modules_years = Reduce(intersect, list(budget_years, classroom_years, student_years)),
    classroom_student_years = classroom_student_years,
    missing_budget_years = setdiff(classroom_student_years, budget_years),
    missing_classroom_years = setdiff(sort(unique(c(budget_years, student_years))), classroom_years),
    missing_student_years = setdiff(sort(unique(c(budget_years, classroom_years))), student_years),
    budget_only_years = setdiff(budget_years, classroom_student_years)
  )
}


#' Count data rows in selected school years
#' @keywords internal
.linkage_count_year_rows <- function(df, years) {
  if (length(years) == 0 || !"school_year" %in% names(df)) {
    return(0L)
  }
  sum(as.character(df$school_year) %in% years)
}
