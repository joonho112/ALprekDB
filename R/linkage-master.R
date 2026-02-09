#' Create Master Linked Dataset
#'
#' @description Creates a fully linked master dataset at two levels:
#'   1. **Classroom-level**: classroom + budget + student aggregates + derived vars
#'   2. **Student-level**: student + classroom + budget columns
#'
#' @param budget An `alprek_budget_panel` object.
#' @param classroom An `alprek_classroom_panel` object.
#' @param student An `alprek_student_panel` object.
#'
#' @return An `alprek_linkage_master` S3 object (list) with elements:
#'   - `classroom_level`: tibble with 1 row per classroom-year.
#'   - `student_level`: tibble with 1 row per student-year.
#'   - `diagnostics`: list of all join diagnostics.
#'   - `meta`: list with metadata.
#'
#' @examples
#' \dontrun{
#' master <- linkage_create_master(budget_panel, classroom_panel, student_panel)
#' master$classroom_level
#' master$student_level
#' }
#'
#' @importFrom dplyr left_join mutate
#' @export
linkage_create_master <- function(budget, classroom, student) {
  if (!inherits(budget, "alprek_budget_panel")) {
    stop("Expected an 'alprek_budget_panel' object.", call. = FALSE)
  }
  if (!inherits(classroom, "alprek_classroom_panel")) {
    stop("Expected an 'alprek_classroom_panel' object.", call. = FALSE)
  }
  if (!inherits(student, "alprek_student_panel")) {
    stop("Expected an 'alprek_student_panel' object.", call. = FALSE)
  }

  msg_info("Creating master linked dataset")

  # Step 1: Classroom + Budget
  msg_step(1, 4, "Joining classroom + budget")
  cb <- linkage_classroom_budget(classroom, budget)

  # Step 2: Aggregate students to classroom level
  msg_step(2, 4, "Aggregating students to classroom level")
  agg <- linkage_aggregate_students(student)

  # Step 3: Classroom-level master = cb + student aggregates + derived vars
  msg_step(3, 4, "Building classroom-level master")
  classroom_level <- dplyr::left_join(
    cb$data, agg,
    by = c("school_year", "classroom_code")
  )

  # Derive per-child budget variables
  if ("grand_total" %in% names(classroom_level) &&
      "n_children" %in% names(classroom_level)) {
    classroom_level <- classroom_level |>
      dplyr::mutate(
        per_child_budget = ifelse(
          !is.na(.data$grand_total) & !is.na(.data$n_children) & .data$n_children > 0,
          round(.data$grand_total / .data$n_children, 2),
          NA_real_
        )
      )
  }

  if ("grand_total" %in% names(classroom_level) &&
      "seat_count" %in% names(classroom_level)) {
    classroom_level <- classroom_level |>
      dplyr::mutate(
        per_seat_budget = ifelse(
          !is.na(.data$grand_total) & !is.na(.data$seat_count) & .data$seat_count > 0,
          round(.data$grand_total / .data$seat_count, 2),
          NA_real_
        )
      )
  }

  # Step 4: Student-level master = student + classroom + budget
  msg_step(4, 4, "Building student-level master")
  sc <- linkage_student_classroom(student, classroom)

  # Add budget columns to student-level data
  student_df <- sc$data
  budget_df <- budget$data

  # Budget-only columns (not already in student data from classroom join)
  budget_shared <- intersect(names(student_df), names(budget_df))
  budget_join_keys <- c("school_year", "classroom_code")
  budget_only_cols <- setdiff(names(budget_df), c(budget_shared))
  if (length(budget_only_cols) > 0) {
    budget_selected <- budget_df[, c(budget_join_keys, budget_only_cols), drop = FALSE]
    student_level <- dplyr::left_join(student_df, budget_selected,
                                       by = budget_join_keys)
  } else {
    student_level <- student_df
  }

  # Diagnostics
  diagnostics <- list(
    classroom_budget = cb$diagnostics,
    student_classroom = sc$diagnostics,
    n_classroom_level = nrow(classroom_level),
    n_student_level = nrow(student_level),
    n_classroom_cols = ncol(classroom_level),
    n_student_cols = ncol(student_level)
  )

  # Metadata
  years <- sort(unique(c(cb$meta$years, sc$meta$years)))
  meta <- list(
    years = years,
    n_classroom_rows = nrow(classroom_level),
    n_student_rows = nrow(student_level),
    n_classroom_cols = ncol(classroom_level),
    n_student_cols = ncol(student_level),
    created_at = Sys.time()
  )

  result <- structure(
    list(
      classroom_level = classroom_level,
      student_level = student_level,
      diagnostics = diagnostics,
      meta = meta
    ),
    class = "alprek_linkage_master"
  )

  msg_success("Master dataset created:")
  msg_info("  Classroom-level: {nrow(classroom_level)} rows x {ncol(classroom_level)} cols")
  msg_info("  Student-level: {nrow(student_level)} rows x {ncol(student_level)} cols")

  result
}


#' Print method for alprek_linkage_master
#' @param x An alprek_linkage_master object.
#' @param ... Ignored.
#' @export
print.alprek_linkage_master <- function(x, ...) {
  cat("<alprek_linkage_master>\n")
  cat("  Years:", paste(x$meta$years, collapse = ", "), "\n")
  cat("  Classroom-level:", nrow(x$classroom_level), "rows x",
      ncol(x$classroom_level), "cols\n")
  cat("  Student-level:", nrow(x$student_level), "rows x",
      ncol(x$student_level), "cols\n")
  d <- x$diagnostics
  if (!is.null(d$classroom_budget)) {
    cat("  Budget match:", round(d$classroom_budget$match_rate * 100, 1), "%\n")
  }
  if (!is.null(d$student_classroom)) {
    cat("  Classroom match:", round(d$student_classroom$match_rate * 100, 1), "%\n")
  }
  invisible(x)
}


#' Summary Statistics for Linkage Data
#'
#' @description Computes summary statistics for linked data by school year.
#'
#' @param x An `alprek_linkage_master`, `alprek_linkage_classroom`, or
#'   `alprek_linkage_student` object.
#' @param by Character. Grouping variable. Default `"school_year"`.
#'
#' @return A tibble of summary statistics.
#'
#' @importFrom dplyr group_by summarise n across
#' @export
linkage_summary_stats <- function(x, by = "school_year") {
  if (inherits(x, "alprek_linkage_master")) {
    df <- x$classroom_level
  } else if (inherits(x, "alprek_linkage_classroom")) {
    df <- x$data
  } else if (inherits(x, "alprek_linkage_student")) {
    df <- x$data
  } else {
    stop("Expected an alprek_linkage object.", call. = FALSE)
  }

  .compute_linkage_stats <- function(d) {
    n_rows <- nrow(d)

    mean_grand_total <- if ("grand_total" %in% names(d)) {
      mean(d$grand_total, na.rm = TRUE)
    } else NA_real_

    mean_per_child <- if ("per_child_budget" %in% names(d)) {
      mean(d$per_child_budget, na.rm = TRUE)
    } else NA_real_

    mean_n_children <- if ("n_children" %in% names(d)) {
      mean(d$n_children, na.rm = TRUE)
    } else NA_real_

    pct_with_budget <- if ("grand_total" %in% names(d)) {
      mean(!is.na(d$grand_total)) * 100
    } else NA_real_

    tibble::tibble(
      n = n_rows,
      mean_grand_total = round(mean_grand_total, 0),
      mean_per_child_budget = round(mean_per_child, 0),
      mean_n_children = round(mean_n_children, 1),
      pct_with_budget = round(pct_with_budget, 1)
    )
  }

  if (!is.null(by) && by %in% names(df)) {
    groups <- sort(unique(df[[by]]))
    stats_list <- lapply(groups, function(g) {
      sub_df <- df[df[[by]] == g, , drop = FALSE]
      row <- .compute_linkage_stats(sub_df)
      row[[by]] <- g
      row
    })
    stats <- dplyr::bind_rows(stats_list)
    stats <- stats[, c(by, setdiff(names(stats), by)), drop = FALSE]
  } else {
    stats <- .compute_linkage_stats(df)
  }

  stats
}
