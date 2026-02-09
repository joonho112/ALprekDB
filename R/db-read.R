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

  structure(
    list(
      classroom_level = classroom_df,
      student_level = student_df,
      diagnostics = list(
        n_classroom_level = nrow(classroom_df),
        n_student_level = nrow(student_df),
        n_classroom_cols = ncol(classroom_df),
        n_student_cols = ncol(student_df),
        source = "duckdb"
      ),
      meta = list(
        years = years_found,
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
