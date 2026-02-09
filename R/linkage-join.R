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

  classroom_keys <- paste(classroom_df$school_year, classroom_df$classroom_code, sep = "|")
  budget_keys <- paste(budget_df$school_year, budget_df$classroom_code, sep = "|")

  n_matched <- sum(classroom_keys %in% budget_keys)
  n_classroom_orphan <- sum(!classroom_keys %in% budget_keys)
  n_budget_orphan <- sum(!budget_keys %in% classroom_keys)
  match_rate <- n_matched / n_classroom

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
    n_result_rows = nrow(joined),
    n_result_cols = ncol(joined),
    n_budget_cols_added = length(budget_only_cols),
    shared_cols_resolved = setdiff(shared_cols, join_keys),
    classroom_orphan_codes = classroom_orphan_codes,
    budget_orphan_codes = budget_orphan_codes
  )

  meta <- list(
    years = sort(unique(joined$school_year)),
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

  msg_success("Classroom-Budget join: {n_matched}/{n_classroom} matched ({round(match_rate * 100, 1)}%)")
  if (n_classroom_orphan > 0) {
    msg_info("  {n_classroom_orphan} classroom(s) without budget data")
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
  cat("  Match rate:", round(d$match_rate * 100, 1), "%\n")
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

  student_keys <- unique(paste(student_df$school_year, student_df$classroom_code, sep = "|"))
  classroom_keys <- paste(classroom_df$school_year, classroom_df$classroom_code, sep = "|")

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

  diagnostics <- list(
    join_type = "student_classroom",
    n_left = n_student,
    n_right = n_classroom,
    n_student_classrooms = n_student_classrooms,
    n_matched_classrooms = n_matched_classrooms,
    n_student_orphan_classrooms = n_student_orphan_classrooms,
    n_classroom_orphan = n_classroom_orphan,
    match_rate = match_rate,
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

  msg_success("Student-Classroom join: {n_matched_classrooms}/{n_student_classrooms} classroom codes matched ({round(match_rate * 100, 1)}%)")
  msg_info("  Result: {nrow(joined)} students x {ncol(joined)} columns")
  if (n_classroom_orphan > 0) {
    msg_info("  {n_classroom_orphan} classroom(s) with no students")
  }
  if (n_student_orphan_classrooms > 0) {
    msg_warn("  {n_student_orphan_classrooms} student classroom code(s) not found in classroom data")
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
  cat("  Classroom match rate:", round(d$match_rate * 100, 1), "%\n")
  cat("  Empty classrooms:", d$n_classroom_orphan, "\n")
  invisible(x)
}
