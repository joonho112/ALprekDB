#' Read Panel Data from DuckDB
#'
#' @description Reads panel data from the database and reconstructs the original
#'   S3 object with proper R types (factors, Dates, integers). Optionally
#'   filters by school year.
#'
#' @param conn A DBI connection object from [db_init()].
#' @param module Character. Which module to read: `"budget"`, `"classroom"`,
#'   or `"student"`.
#' @param years Character vector or `NULL`. School years to include (e.g.,
#'   `c("2023-2024", "2024-2025")`). `NULL` reads all years.
#'
#' @return An `alprek_budget_panel`, `alprek_classroom_panel`, or
#'   `alprek_student_panel` S3 object.
#'
#' @examples
#' \dontrun{
#' conn <- db_init("output/alprekdb.duckdb", read_only = TRUE)
#' budget <- db_read_panel(conn, "budget")
#' student <- db_read_panel(conn, "student", years = "2024-2025")
#' db_close(conn)
#' }
#'
#' @export
db_read_panel <- function(conn, module = c("budget", "classroom", "student"),
                          years = NULL) {
  .db_require_packages()
  .db_validate_conn(conn)
  module <- match.arg(module)

  table_name <- paste0(module, "_panel")

  if (!table_name %in% DBI::dbListTables(conn)) {
    stop("Table '", table_name, "' not found in database.", call. = FALSE)
  }

  # Build query
  sql <- paste0("SELECT * FROM \"", table_name, "\"")
  if (!is.null(years)) {
    year_list <- paste0("'", years, "'", collapse = ", ")
    sql <- paste0(sql, " WHERE school_year IN (", year_list, ")")
  }
  sql <- paste0(sql, " ORDER BY school_year")

  df <- tibble::as_tibble(DBI::dbGetQuery(conn, sql))

  # Reconstruct R types
  type_registry <- .db_get_column_types(conn, table_name)
  df <- .db_reconstruct_types(df, type_registry)

  # Build S3 object
  .db_build_panel_object(df, module)
}


#' Read Master Linked Dataset from DuckDB
#'
#' @description Reads both classroom-level and student-level master data from
#'   the database and reconstructs the `alprek_linkage_master` S3 object.
#'   Optionally filters by school year.
#'
#' @param conn A DBI connection object from [db_init()].
#' @param years Character vector or `NULL`. School years to include. `NULL`
#'   reads all years.
#'
#' @return An `alprek_linkage_master` S3 object.
#'
#' @examples
#' \dontrun{
#' conn <- db_init("output/alprekdb.duckdb", read_only = TRUE)
#' master <- db_read_master(conn)
#' master$classroom_level
#' master$student_level
#' db_close(conn)
#' }
#'
#' @export
db_read_master <- function(conn, years = NULL) {
  .db_require_packages()
  .db_validate_conn(conn)

  tables <- c("master_classroom", "master_student")
  for (tbl in tables) {
    if (!tbl %in% DBI::dbListTables(conn)) {
      stop("Table '", tbl, "' not found in database. ",
           "Write master data first with db_write_master().", call. = FALSE)
    }
  }

  # Build queries with optional year filter
  year_clause <- ""
  if (!is.null(years)) {
    year_list <- paste0("'", years, "'", collapse = ", ")
    year_clause <- paste0(" WHERE school_year IN (", year_list, ")")
  }

  classroom_df <- tibble::as_tibble(DBI::dbGetQuery(conn,
    paste0("SELECT * FROM master_classroom", year_clause, " ORDER BY school_year")
  ))
  student_df <- tibble::as_tibble(DBI::dbGetQuery(conn,
    paste0("SELECT * FROM master_student", year_clause, " ORDER BY school_year")
  ))

  # Reconstruct types
  cl_types <- .db_get_column_types(conn, "master_classroom")
  st_types <- .db_get_column_types(conn, "master_student")
  classroom_df <- .db_reconstruct_types(classroom_df, cl_types)
  student_df <- .db_reconstruct_types(student_df, st_types)

  # Build S3 object
  years_found <- sort(unique(c(classroom_df$school_year, student_df$school_year)))
  diagnostics <- .db_build_master_diagnostics(classroom_df, student_df)
  coverage <- diagnostics$coverage

  structure(
    list(
      classroom_level = classroom_df,
      student_level = student_df,
      diagnostics = c(
        diagnostics[c("classroom_budget", "student_classroom", "coverage")],
        list(
        n_classroom_level = nrow(classroom_df),
        n_student_level = nrow(student_df),
        n_classroom_cols = ncol(classroom_df),
        n_student_cols = ncol(student_df),
        source = "duckdb"
        )
      ),
      meta = list(
        years = years_found,
        coverage = coverage,
        n_classroom_rows = nrow(classroom_df),
        n_student_rows = nrow(student_df),
        n_classroom_cols = ncol(classroom_df),
        n_student_cols = ncol(student_df),
        created_at = Sys.time(),
        source = "duckdb"
      )
    ),
    class = "alprek_linkage_master"
  )
}


# ---- Internal helpers ----

#' Build panel S3 object from data frame
#' @keywords internal
.db_build_panel_object <- function(df, module) {
  all_years <- sort(unique(df$school_year))

  if (module == "budget") {
    by_year <- lapply(all_years, function(yr) {
      sub <- df[df$school_year == yr, ]
      list(school_year = yr, format = "db", n_classrooms = nrow(sub))
    })
    names(by_year) <- all_years

    structure(
      list(
        data = df,
        years = all_years,
        n_years = length(all_years),
        by_year = by_year
      ),
      class = "alprek_budget_panel"
    )

  } else if (module == "classroom") {
    by_year <- lapply(all_years, function(yr) {
      sub <- df[df$school_year == yr, ]
      list(school_year = yr, format = "db", n_classrooms = nrow(sub))
    })
    names(by_year) <- all_years

    structure(
      list(
        data = df,
        years = all_years,
        n_total = nrow(df),
        by_year = by_year,
        imputation_log = tibble::tibble(
          classroom_code = character(), school_year = character(),
          variable = character(), imputed_value = character(),
          method = character()
        )
      ),
      class = "alprek_classroom_panel"
    )

  } else if (module == "student") {
    n_unique <- if ("adece_id" %in% names(df)) {
      length(unique(df$adece_id[!is.na(df$adece_id)]))
    } else {
      NA_integer_
    }

    by_year <- lapply(all_years, function(yr) {
      sub <- df[df$school_year == yr, ]
      list(school_year = yr, format = "db",
           n_students = nrow(sub), n_cols = ncol(sub))
    })
    names(by_year) <- all_years

    structure(
      list(
        data = df,
        years = all_years,
        n_total = nrow(df),
        n_unique_students = n_unique,
        by_year = by_year
      ),
      class = "alprek_student_panel"
    )
  }
}


#' Reconstruct master diagnostics from DuckDB master tables
#' @keywords internal
.db_build_master_diagnostics <- function(classroom_df, student_df) {
  coverage <- .db_build_master_coverage(classroom_df, student_df)
  classroom_budget <- .db_build_classroom_budget_diagnostics(classroom_df)
  student_classroom <- .db_build_student_classroom_diagnostics(
    classroom_df,
    student_df
  )

  list(
    classroom_budget = classroom_budget,
    student_classroom = student_classroom,
    coverage = coverage
  )
}


#' Reconstruct master coverage metadata from DuckDB master tables
#' @keywords internal
.db_build_master_coverage <- function(classroom_df, student_df) {
  classroom_years <- sort(unique(as.character(classroom_df$school_year)))
  student_years <- sort(unique(as.character(student_df$school_year)))
  budget_years <- .db_master_budget_years(classroom_df)
  analysis_years <- sort(unique(c(budget_years, classroom_years, student_years)))
  classroom_student_years <- sort(unique(c(classroom_years, student_years)))

  coverage_table <- data.frame(
    school_year = analysis_years,
    has_budget = analysis_years %in% budget_years,
    has_classroom = analysis_years %in% classroom_years,
    has_student = analysis_years %in% student_years,
    n_budget_rows = vapply(
      analysis_years,
      function(yr) {
        if (!"grand_total" %in% names(classroom_df)) return(0L)
        sum(
          as.character(classroom_df$school_year) == yr &
            !is.na(classroom_df$grand_total)
        )
      },
      integer(1)
    ),
    n_classroom_rows = vapply(
      analysis_years,
      function(yr) sum(as.character(classroom_df$school_year) == yr),
      integer(1)
    ),
    n_student_rows = vapply(
      analysis_years,
      function(yr) sum(as.character(student_df$school_year) == yr),
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
    budget_only_years = setdiff(budget_years, classroom_student_years),
    source = "duckdb_derived"
  )
}


#' Reconstruct classroom-budget diagnostics from master classroom table
#' @keywords internal
.db_build_classroom_budget_diagnostics <- function(classroom_df) {
  budget_years <- .db_master_budget_years(classroom_df)
  year_coverage <- .linkage_year_coverage(
    sort(unique(as.character(classroom_df$school_year))),
    budget_years,
    left_label = "classroom",
    right_label = "budget"
  )

  classroom_year <- as.character(classroom_df$school_year)
  has_budget <- .db_master_has_budget(classroom_df)
  n_classroom <- nrow(classroom_df)
  n_matched <- sum(has_budget)
  n_classroom_orphan <- sum(!has_budget)
  in_missing_budget_year <- classroom_year %in% year_coverage$left_only_years
  in_overlap_year <- classroom_year %in% year_coverage$overlap_years
  n_left_orphan_missing_budget_years <- sum(!has_budget & in_missing_budget_year)
  n_left_orphan_overlap_years <- sum(!has_budget & in_overlap_year)
  n_overlap_left <- sum(in_overlap_year)
  n_overlap_matched <- sum(has_budget & in_overlap_year)
  match_rate_overlap <- if (n_overlap_left > 0) {
    n_overlap_matched / n_overlap_left
  } else {
    NA_real_
  }

  orphan_summary_by_year <- .db_classroom_budget_orphans_by_year(
    classroom_df,
    year_coverage
  )

  list(
    join_type = "classroom_budget",
    n_left = n_classroom,
    n_right = n_matched,
    n_matched = n_matched,
    n_left_orphan = n_classroom_orphan,
    n_right_orphan = 0L,
    match_rate = if (n_classroom > 0) n_matched / n_classroom else NA_real_,
    n_left_orphan_overlap_years = n_left_orphan_overlap_years,
    n_left_orphan_missing_budget_years = n_left_orphan_missing_budget_years,
    missing_budget_years = year_coverage$left_only_years,
    n_right_orphan_overlap_years = 0L,
    n_right_orphan_missing_classroom_years = 0L,
    n_overlap_left = n_overlap_left,
    n_overlap_matched = n_overlap_matched,
    match_rate_overlap_years = match_rate_overlap,
    n_right_only_year_rows = 0L,
    year_coverage = year_coverage,
    orphan_summary_by_year = orphan_summary_by_year,
    n_result_rows = nrow(classroom_df),
    n_result_cols = ncol(classroom_df),
    n_budget_cols_added = NA_integer_,
    shared_cols_resolved = character(),
    classroom_orphan_codes = character(),
    budget_orphan_codes = character(),
    source = "duckdb_derived"
  )
}


#' Reconstruct student-classroom diagnostics from DuckDB master tables
#' @keywords internal
.db_build_student_classroom_diagnostics <- function(classroom_df, student_df) {
  year_coverage <- .linkage_year_coverage(
    sort(unique(as.character(student_df$school_year))),
    sort(unique(as.character(classroom_df$school_year))),
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

  list(
    join_type = "student_classroom",
    n_left = nrow(student_df),
    n_right = nrow(classroom_df),
    n_student_classrooms = n_student_classrooms,
    n_matched_classrooms = n_matched_classrooms,
    n_student_orphan_classrooms = n_student_orphan_classrooms,
    n_classroom_orphan = n_classroom_orphan,
    match_rate = n_matched_classrooms / max(n_student_classrooms, 1),
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
    orphan_summary_by_year = .linkage_student_classroom_orphans_by_year(
      student_df,
      classroom_df,
      year_coverage
    ),
    n_result_rows = nrow(student_df),
    n_result_cols = ncol(student_df),
    n_classroom_cols_added = NA_integer_,
    shared_cols_resolved = character(),
    student_orphan_codes = character(),
    classroom_orphan_codes = character(),
    source = "duckdb_derived"
  )
}


#' Infer budget-covered years from a master classroom table
#' @keywords internal
.db_master_budget_years <- function(classroom_df) {
  if (!"grand_total" %in% names(classroom_df)) {
    return(character())
  }
  sort(unique(as.character(classroom_df$school_year[
    !is.na(classroom_df$grand_total)
  ])))
}


#' Determine rows with reconstructed budget data
#' @keywords internal
.db_master_has_budget <- function(classroom_df) {
  if (!"grand_total" %in% names(classroom_df)) {
    return(rep(FALSE, nrow(classroom_df)))
  }
  !is.na(classroom_df$grand_total)
}


#' Build classroom-budget orphan summary from a master classroom table
#' @keywords internal
.db_classroom_budget_orphans_by_year <- function(classroom_df, year_coverage) {
  years <- sort(unique(as.character(classroom_df$school_year)))
  has_budget <- .db_master_has_budget(classroom_df)
  rows <- lapply(years, function(yr) {
    idx <- as.character(classroom_df$school_year) == yr
    n_left <- sum(idx)
    n_matched <- sum(idx & has_budget)
    data.frame(
      school_year = yr,
      coverage_status = .linkage_year_status(yr, year_coverage),
      n_classroom_rows = n_left,
      n_budget_rows = n_matched,
      n_classrooms_with_budget = n_matched,
      n_classrooms_without_budget = sum(idx & !has_budget),
      n_budget_rows_without_classroom = 0L,
      match_rate = if (n_left > 0) n_matched / n_left else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
