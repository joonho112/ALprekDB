#' Join Classroom and Budget Data
#'
#' @description Performs a left join of classroom panel data with budget panel
#'   data, using `school_year` and `classroom_code` as join keys. Budget-only
#'   columns are appended to the classroom data (shared columns use classroom
#'   as the authoritative source).
#'
#' @param classroom An `alprek_classroom_panel` object.
#' @param budget An `alprek_budget_panel` object.
#'
#' @return An `alprek_linkage_classroom` S3 object with elements:
#'   - `data`: tibble of joined classroom + budget data.
#'   - `diagnostics`: list with join statistics.
#'   - `meta`: list with metadata.
#'
#' @examples
#' \dontrun{
#' cb <- linkage_classroom_budget(classroom_panel, budget_panel)
#' cb$data
#' cb$diagnostics
#' }
#'
#' @importFrom dplyr left_join select any_of all_of
#' @export
linkage_classroom_budget <- function(classroom, budget) {
  if (!inherits(classroom, "alprek_classroom_panel")) {
    stop("Expected an 'alprek_classroom_panel' object.", call. = FALSE)
  }
  if (!inherits(budget, "alprek_budget_panel")) {
    stop("Expected an 'alprek_budget_panel' object.", call. = FALSE)
  }

  msg_info("Joining classroom + budget data")

  classroom_df <- classroom$data
  budget_df <- budget$data

  # Identify budget-only columns (not shared with classroom)
  shared_cols <- intersect(names(classroom_df), names(budget_df))
  join_keys <- c("school_year", "classroom_code")

  # Validate join keys exist
  for (k in join_keys) {
    if (!k %in% names(classroom_df)) {
      stop("Join key '", k, "' not found in classroom data.", call. = FALSE)
    }
    if (!k %in% names(budget_df)) {
      stop("Join key '", k, "' not found in budget data.", call. = FALSE)
    }
  }

  # Select budget-only columns + join keys
  budget_only_cols <- setdiff(names(budget_df), shared_cols)
  budget_selected <- budget_df[, c(join_keys, budget_only_cols), drop = FALSE]

  # Perform left join (classroom is the "left" / authoritative side)
  joined <- dplyr::left_join(classroom_df, budget_selected,
                              by = join_keys)

  # Compute diagnostics
  n_classroom <- nrow(classroom_df)
  n_budget <- nrow(budget_df)
  year_coverage <- .linkage_year_coverage(
    .linkage_panel_years(classroom),
    .linkage_panel_years(budget),
    left_label = "classroom",
    right_label = "budget"
  )

  classroom_keys <- paste(classroom_df$school_year, classroom_df$classroom_code, sep = "|")
  budget_keys <- paste(budget_df$school_year, budget_df$classroom_code, sep = "|")
  classroom_year <- as.character(classroom_df$school_year)
  budget_year <- as.character(budget_df$school_year)

  n_matched <- sum(classroom_keys %in% budget_keys)
  n_classroom_orphan <- sum(!classroom_keys %in% budget_keys)
  n_budget_orphan <- sum(!budget_keys %in% classroom_keys)
  match_rate <- n_matched / n_classroom
  in_missing_budget_year <- classroom_year %in% year_coverage$left_only_years
  in_overlap_year <- classroom_year %in% year_coverage$overlap_years
  n_left_orphan_missing_budget_years <- sum(
    !classroom_keys %in% budget_keys & in_missing_budget_year
  )
  n_left_orphan_overlap_years <- sum(
    !classroom_keys %in% budget_keys & in_overlap_year
  )
  n_overlap_left <- sum(in_overlap_year)
  n_overlap_matched <- sum(classroom_keys %in% budget_keys & in_overlap_year)
  match_rate_overlap <- if (n_overlap_left > 0) {
    n_overlap_matched / n_overlap_left
  } else {
    NA_real_
  }
  n_right_only_year_rows <- .linkage_count_year_rows(
    budget_df,
    year_coverage$right_only_years
  )
  n_right_orphan_overlap_years <- sum(
    !budget_keys %in% classroom_keys & budget_year %in% year_coverage$overlap_years
  )
  n_right_orphan_missing_classroom_years <- sum(
    !budget_keys %in% classroom_keys & budget_year %in% year_coverage$right_only_years
  )
  orphan_summary_by_year <- .linkage_classroom_budget_orphans_by_year(
    classroom_df,
    budget_df,
    year_coverage
  )

  # Identify orphan codes
  classroom_orphan_codes <- unique(classroom_df$classroom_code[!classroom_keys %in% budget_keys])
  budget_orphan_codes <- unique(budget_df$classroom_code[!budget_keys %in% classroom_keys])

  diagnostics <- list(
    join_type = "classroom_budget",
    n_left = n_classroom,
    n_right = n_budget,
    n_matched = n_matched,
    n_left_orphan = n_classroom_orphan,
    n_right_orphan = n_budget_orphan,
    match_rate = match_rate,
    n_left_orphan_overlap_years = n_left_orphan_overlap_years,
    n_left_orphan_missing_budget_years = n_left_orphan_missing_budget_years,
    missing_budget_years = year_coverage$left_only_years,
    n_right_orphan_overlap_years = n_right_orphan_overlap_years,
    n_right_orphan_missing_classroom_years = n_right_orphan_missing_classroom_years,
    n_overlap_left = n_overlap_left,
    n_overlap_matched = n_overlap_matched,
    match_rate_overlap_years = match_rate_overlap,
    n_right_only_year_rows = n_right_only_year_rows,
    year_coverage = year_coverage,
    orphan_summary_by_year = orphan_summary_by_year,
    n_result_rows = nrow(joined),
    n_result_cols = ncol(joined),
    n_budget_cols_added = length(budget_only_cols),
    shared_cols_resolved = setdiff(shared_cols, join_keys),
    classroom_orphan_codes = classroom_orphan_codes,
    budget_orphan_codes = budget_orphan_codes
  )

  meta <- list(
    years = sort(unique(joined$school_year)),
    coverage = year_coverage,
    created_at = Sys.time()
  )

  result <- structure(
    list(
      data = joined,
      diagnostics = diagnostics,
      meta = meta
    ),
    class = "alprek_linkage_classroom"
  )

  if (length(year_coverage$left_only_years) > 0 && !is.na(match_rate_overlap)) {
    msg_success("Classroom-Budget join: {n_overlap_matched}/{n_overlap_left} matched in overlapping budget years ({round(match_rate_overlap * 100, 1)}%); {n_matched}/{n_classroom} matched overall")
  } else {
    msg_success("Classroom-Budget join: {n_matched}/{n_classroom} matched ({round(match_rate * 100, 1)}%)")
  }
  if (n_left_orphan_missing_budget_years > 0) {
    missing_years <- paste(year_coverage$left_only_years, collapse = ", ")
    msg_info("  Budget unavailable for year(s): {missing_years}; {n_left_orphan_missing_budget_years} classroom row(s) retained with missing budget columns")
  }
  if (n_left_orphan_overlap_years > 0) {
    msg_info("  {n_left_orphan_overlap_years} classroom(s) without budget data in overlapping years")
  }
  if (n_budget_orphan > 0) {
    msg_warn("  {n_budget_orphan} budget row(s) without matching classroom")
  }

  result
}


#' Print method for alprek_linkage_classroom
#' @param x An alprek_linkage_classroom object.
#' @param ... Ignored.
#' @export
print.alprek_linkage_classroom <- function(x, ...) {
  cat("<alprek_linkage_classroom>\n")
  cat("  Years:", paste(x$meta$years, collapse = ", "), "\n")
  cat("  Rows:", nrow(x$data), "\n")
  cat("  Columns:", ncol(x$data), "\n")
  d <- x$diagnostics
  if (!is.null(d$match_rate_overlap_years) && !is.na(d$match_rate_overlap_years) &&
      length(d$missing_budget_years) > 0) {
    cat("  Overlap-year match rate:", round(d$match_rate_overlap_years * 100, 1), "%\n")
    cat("  All-year match rate:", round(d$match_rate * 100, 1), "%\n")
  } else {
    cat("  Match rate:", round(d$match_rate * 100, 1), "%\n")
  }
  cat("  Classroom orphans:", d$n_left_orphan,
      "| Budget orphans:", d$n_right_orphan, "\n")
  invisible(x)
}


#' Join Student and Classroom Data
#'
#' @description Performs a left join of student panel data with classroom panel
#'   data, using `school_year` and `classroom_code` as join keys. Classroom-only
#'   columns are appended to the student data (shared columns use student
#'   as the authoritative source).
#'
#' @param student An `alprek_student_panel` object.
#' @param classroom An `alprek_classroom_panel` object.
#'
#' @return An `alprek_linkage_student` S3 object with elements:
#'   - `data`: tibble of joined student + classroom data.
#'   - `diagnostics`: list with join statistics.
#'   - `meta`: list with metadata.
#'
#' @examples
#' \dontrun{
#' sc <- linkage_student_classroom(student_panel, classroom_panel)
#' sc$data
#' sc$diagnostics
#' }
#'
#' @importFrom dplyr left_join
#' @export
linkage_student_classroom <- function(student, classroom) {
  if (!inherits(student, "alprek_student_panel")) {
    stop("Expected an 'alprek_student_panel' object.", call. = FALSE)
  }
  if (!inherits(classroom, "alprek_classroom_panel")) {
    stop("Expected an 'alprek_classroom_panel' object.", call. = FALSE)
  }

  msg_info("Joining student + classroom data")

  student_df <- student$data
  classroom_df <- classroom$data

  # Identify shared columns and join keys
  shared_cols <- intersect(names(student_df), names(classroom_df))
  join_keys <- c("school_year", "classroom_code")

  # Validate join keys
  for (k in join_keys) {
    if (!k %in% names(student_df)) {
      stop("Join key '", k, "' not found in student data.", call. = FALSE)
    }
    if (!k %in% names(classroom_df)) {
      stop("Join key '", k, "' not found in classroom data.", call. = FALSE)
    }
  }

  # Select classroom-only columns + join keys
  classroom_only_cols <- setdiff(names(classroom_df), shared_cols)
  classroom_selected <- classroom_df[, c(join_keys, classroom_only_cols), drop = FALSE]

  # Perform left join (student is the "left" / authoritative side)
  joined <- dplyr::left_join(student_df, classroom_selected,
                              by = join_keys)

  # Compute diagnostics
  n_student <- nrow(student_df)
  n_classroom <- nrow(classroom_df)
  year_coverage <- .linkage_year_coverage(
    .linkage_panel_years(student),
    .linkage_panel_years(classroom),
    left_label = "student",
    right_label = "classroom"
  )

  student_row_keys <- paste(student_df$school_year, student_df$classroom_code, sep = "|")
  student_keys <- unique(student_row_keys)
  classroom_keys <- paste(classroom_df$school_year, classroom_df$classroom_code, sep = "|")
  student_key_year <- sub("\\|.*$", "", student_keys)
  student_row_year <- as.character(student_df$school_year)
  classroom_year <- as.character(classroom_df$school_year)

  n_student_classrooms <- length(student_keys)
  n_matched_classrooms <- sum(student_keys %in% classroom_keys)
  n_student_orphan_classrooms <- sum(!student_keys %in% classroom_keys)
  n_classroom_orphan <- sum(!classroom_keys %in% student_keys)

  # Match rate at classroom-code level
  match_rate <- n_matched_classrooms / max(n_student_classrooms, 1)

  # Orphan codes
  student_orphan_codes <- character(0)
  if (n_student_orphan_classrooms > 0) {
    orphan_keys <- student_keys[!student_keys %in% classroom_keys]
    student_orphan_codes <- unique(sub("^.*\\|", "", orphan_keys))
  }
  classroom_orphan_codes <- unique(classroom_df$classroom_code[!classroom_keys %in% student_keys])
  n_student_orphan_missing_classroom_years <- sum(
    !student_keys %in% classroom_keys & student_key_year %in% year_coverage$left_only_years
  )
  n_student_orphan_overlap_years <- sum(
    !student_keys %in% classroom_keys & student_key_year %in% year_coverage$overlap_years
  )
  n_student_orphan_missing_classroom_year_rows <- sum(
    !student_row_keys %in% classroom_keys & student_row_year %in% year_coverage$left_only_years
  )
  n_student_orphan_overlap_year_rows <- sum(
    !student_row_keys %in% classroom_keys & student_row_year %in% year_coverage$overlap_years
  )
  n_classroom_orphan_missing_student_years <- sum(
    !classroom_keys %in% student_keys & classroom_year %in% year_coverage$right_only_years
  )
  n_classroom_orphan_overlap_years <- sum(
    !classroom_keys %in% student_keys & classroom_year %in% year_coverage$overlap_years
  )
  n_overlap_student_classrooms <- sum(student_key_year %in% year_coverage$overlap_years)
  n_overlap_matched_classrooms <- sum(
    student_keys %in% classroom_keys & student_key_year %in% year_coverage$overlap_years
  )
  match_rate_overlap <- if (n_overlap_student_classrooms > 0) {
    n_overlap_matched_classrooms / n_overlap_student_classrooms
  } else {
    NA_real_
  }
  orphan_summary_by_year <- .linkage_student_classroom_orphans_by_year(
    student_df,
    classroom_df,
    year_coverage
  )

  diagnostics <- list(
    join_type = "student_classroom",
    n_left = n_student,
    n_right = n_classroom,
    n_student_classrooms = n_student_classrooms,
    n_matched_classrooms = n_matched_classrooms,
    n_student_orphan_classrooms = n_student_orphan_classrooms,
    n_classroom_orphan = n_classroom_orphan,
    match_rate = match_rate,
    n_student_orphan_overlap_years = n_student_orphan_overlap_years,
    n_student_orphan_missing_classroom_years = n_student_orphan_missing_classroom_years,
    n_student_orphan_overlap_year_rows = n_student_orphan_overlap_year_rows,
    n_student_orphan_missing_classroom_year_rows = n_student_orphan_missing_classroom_year_rows,
    n_classroom_orphan_overlap_years = n_classroom_orphan_overlap_years,
    n_classroom_orphan_missing_student_years = n_classroom_orphan_missing_student_years,
    missing_classroom_years = year_coverage$left_only_years,
    missing_student_years = year_coverage$right_only_years,
    n_overlap_student_classrooms = n_overlap_student_classrooms,
    n_overlap_matched_classrooms = n_overlap_matched_classrooms,
    match_rate_overlap_years = match_rate_overlap,
    year_coverage = year_coverage,
    orphan_summary_by_year = orphan_summary_by_year,
    n_result_rows = nrow(joined),
    n_result_cols = ncol(joined),
    n_classroom_cols_added = length(classroom_only_cols),
    shared_cols_resolved = setdiff(shared_cols, join_keys),
    student_orphan_codes = student_orphan_codes,
    classroom_orphan_codes = classroom_orphan_codes
  )

  meta <- list(
    years = sort(unique(joined$school_year)),
    n_students = nrow(joined),
    coverage = year_coverage,
    created_at = Sys.time()
  )

  result <- structure(
    list(
      data = joined,
      diagnostics = diagnostics,
      meta = meta
    ),
    class = "alprek_linkage_student"
  )

  if (length(year_coverage$left_only_years) > 0 && !is.na(match_rate_overlap)) {
    msg_success("Student-Classroom join: {n_overlap_matched_classrooms}/{n_overlap_student_classrooms} classroom codes matched in overlapping classroom years ({round(match_rate_overlap * 100, 1)}%); {n_matched_classrooms}/{n_student_classrooms} matched overall")
  } else {
    msg_success("Student-Classroom join: {n_matched_classrooms}/{n_student_classrooms} classroom codes matched ({round(match_rate * 100, 1)}%)")
  }
  msg_info("  Result: {nrow(joined)} students x {ncol(joined)} columns")
  if (n_student_orphan_missing_classroom_years > 0) {
    missing_years <- paste(year_coverage$left_only_years, collapse = ", ")
    msg_warn("  Classroom data unavailable for year(s): {missing_years}; {n_student_orphan_missing_classroom_year_rows} student row(s) retained with missing classroom columns")
  }
  if (n_student_orphan_overlap_years > 0) {
    msg_warn("  {n_student_orphan_overlap_years} student classroom code(s) not found in classroom data for overlapping years")
  }
  if (n_classroom_orphan_missing_student_years > 0) {
    missing_years <- paste(year_coverage$right_only_years, collapse = ", ")
    msg_info("  Student data unavailable for classroom year(s): {missing_years}; {n_classroom_orphan_missing_student_years} classroom row(s) have no student aggregate")
  }
  if (n_classroom_orphan_overlap_years > 0) {
    msg_info("  {n_classroom_orphan_overlap_years} classroom(s) with no students in overlapping years")
  }

  result
}


#' Print method for alprek_linkage_student
#' @param x An alprek_linkage_student object.
#' @param ... Ignored.
#' @export
print.alprek_linkage_student <- function(x, ...) {
  cat("<alprek_linkage_student>\n")
  cat("  Years:", paste(x$meta$years, collapse = ", "), "\n")
  cat("  Students:", nrow(x$data), "\n")
  cat("  Columns:", ncol(x$data), "\n")
  d <- x$diagnostics
  if (!is.null(d$match_rate_overlap_years) && !is.na(d$match_rate_overlap_years) &&
      length(d$missing_classroom_years) > 0) {
    cat("  Overlap-year classroom match rate:", round(d$match_rate_overlap_years * 100, 1), "%\n")
    cat("  All-year classroom match rate:", round(d$match_rate * 100, 1), "%\n")
  } else {
    cat("  Classroom match rate:", round(d$match_rate * 100, 1), "%\n")
  }
  cat("  Empty classrooms:", d$n_classroom_orphan, "\n")
  invisible(x)
}


#' Build classroom-budget orphan diagnostics by school year
#' @keywords internal
.linkage_classroom_budget_orphans_by_year <- function(classroom_df, budget_df,
                                                      year_coverage) {
  years <- sort(unique(c(
    as.character(classroom_df$school_year),
    as.character(budget_df$school_year)
  )))
  rows <- lapply(years, function(yr) {
    left_df <- classroom_df[as.character(classroom_df$school_year) == yr, , drop = FALSE]
    right_df <- budget_df[as.character(budget_df$school_year) == yr, , drop = FALSE]
    left_keys <- paste(left_df$school_year, left_df$classroom_code, sep = "|")
    right_keys <- paste(right_df$school_year, right_df$classroom_code, sep = "|")
    n_left <- length(left_keys)
    n_matched <- sum(left_keys %in% right_keys)
    data.frame(
      school_year = yr,
      coverage_status = .linkage_year_status(yr, year_coverage),
      n_classroom_rows = nrow(left_df),
      n_budget_rows = nrow(right_df),
      n_classrooms_with_budget = n_matched,
      n_classrooms_without_budget = sum(!left_keys %in% right_keys),
      n_budget_rows_without_classroom = sum(!right_keys %in% left_keys),
      match_rate = if (n_left > 0) n_matched / n_left else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}


#' Build student-classroom orphan diagnostics by school year
#' @keywords internal
.linkage_student_classroom_orphans_by_year <- function(student_df, classroom_df,
                                                       year_coverage) {
  years <- sort(unique(c(
    as.character(student_df$school_year),
    as.character(classroom_df$school_year)
  )))
  rows <- lapply(years, function(yr) {
    student_year <- student_df[as.character(student_df$school_year) == yr, , drop = FALSE]
    classroom_year <- classroom_df[as.character(classroom_df$school_year) == yr, , drop = FALSE]
    student_row_keys <- paste(student_year$school_year, student_year$classroom_code, sep = "|")
    student_keys <- unique(student_row_keys)
    classroom_keys <- paste(classroom_year$school_year, classroom_year$classroom_code, sep = "|")
    n_student_classrooms <- length(student_keys)
    n_matched <- sum(student_keys %in% classroom_keys)
    missing_student_keys <- student_keys[!student_keys %in% classroom_keys]
    data.frame(
      school_year = yr,
      coverage_status = .linkage_year_status(yr, year_coverage),
      n_student_rows = nrow(student_year),
      n_student_classrooms = n_student_classrooms,
      n_classroom_rows = nrow(classroom_year),
      n_student_classrooms_with_classroom = n_matched,
      n_student_classrooms_without_classroom = length(missing_student_keys),
      n_student_rows_without_classroom = sum(student_row_keys %in% missing_student_keys),
      n_classrooms_without_students = sum(!classroom_keys %in% student_keys),
      match_rate = if (n_student_classrooms > 0) {
        n_matched / n_student_classrooms
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}


#' Label a school year using two-panel coverage metadata
#' @keywords internal
.linkage_year_status <- function(year, coverage) {
  if (year %in% coverage$overlap_years) {
    return("overlap")
  }
  if (year %in% coverage$left_only_years) {
    return(paste0("missing_", coverage$right_label))
  }
  if (year %in% coverage$right_only_years) {
    return(paste0("missing_", coverage$left_label))
  }
  "absent"
}
