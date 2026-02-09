#' Initialize ALprekDB DuckDB Database
#'
#' @description Creates a new or opens an existing DuckDB database for storing
#'   ALprekDB panel data. New databases are initialized with the ALprekDB schema
#'   (metadata and column type registry tables). Existing databases are validated
#'   for schema compatibility.
#'
#' @param path Character. File path for the DuckDB database. Use `":memory:"`
#'   for an in-memory database (data lost when connection is closed).
#' @param read_only Logical. Open in read-only mode? Default `FALSE`.
#'
#' @return A DBI connection object to the DuckDB database.
#'
#' @examples
#' \dontrun{
#' conn <- db_init("output/alprekdb.duckdb")
#' db_list_tables(conn)
#' db_close(conn)
#' }
#'
#' @export
db_init <- function(path, read_only = FALSE) {
  .db_require_packages()

  is_new <- !file.exists(path) || path == ":memory:"

  if (!is_new && !read_only) {
    # Ensure directory exists for existing path (not needed, but safe)
    .ensure_dir(path)
  }

  if (is_new && path != ":memory:") {
    .ensure_dir(path)
  }

  drv <- duckdb::duckdb()
  conn <- DBI::dbConnect(drv, dbdir = path, read_only = read_only)

  if (is_new) {
    .db_create_schema(conn)
    msg_success("Created new ALprekDB database: {path}")
  } else {
    .db_validate_schema(conn)
    msg_info("Opened existing ALprekDB database: {path}")
  }

  conn
}


#' Close ALprekDB DuckDB Connection
#'
#' @description Properly closes the DuckDB database connection and shuts down
#'   the database driver.
#'
#' @param conn A DBI connection object from [db_init()].
#'
#' @return Invisible `TRUE` on success.
#'
#' @export
db_close <- function(conn) {
  .db_require_packages()
  if (DBI::dbIsValid(conn)) {
    DBI::dbDisconnect(conn, shutdown = TRUE)
  }
  invisible(TRUE)
}


#' List User Tables in ALprekDB Database
#'
#' @description Lists all user data tables, excluding internal metadata tables
#'   (those prefixed with `_alprek_`).
#'
#' @param conn A DBI connection object from [db_init()].
#'
#' @return Character vector of table names.
#'
#' @export
db_list_tables <- function(conn) {
  .db_require_packages()
  .db_validate_conn(conn)

  all_tables <- DBI::dbListTables(conn)
  # Exclude internal tables
  user_tables <- all_tables[!grepl("^_alprek_", all_tables)]
  sort(user_tables)
}


#' Get Column Metadata for a Database Table
#'
#' @description Returns column names, DuckDB types, and R type information
#'   (from the column type registry) for a given table.
#'
#' @param conn A DBI connection object from [db_init()].
#' @param table Character. Table name.
#'
#' @return A tibble with columns: `column_name`, `duckdb_type`, `r_type`,
#'   `is_factor`.
#'
#' @export
db_table_info <- function(conn, table) {
  .db_require_packages()
  .db_validate_conn(conn)

  if (!table %in% DBI::dbListTables(conn)) {
    stop("Table '", table, "' not found in database.", call. = FALSE)
  }

  # Get DuckDB column info
  col_info <- DBI::dbGetQuery(conn, paste0(
    "SELECT column_name, data_type FROM information_schema.columns ",
    "WHERE table_name = '", table, "' ORDER BY ordinal_position"
  ))

  # Get R type registry
  type_reg <- .db_get_column_types(conn, table)

  # Merge
  result <- tibble::tibble(
    column_name = col_info$column_name,
    duckdb_type = col_info$data_type
  )

  if (nrow(type_reg) > 0) {
    type_lookup <- stats::setNames(type_reg$r_type, type_reg$column_name)
    factor_lookup <- stats::setNames(
      !is.na(type_reg$factor_levels),
      type_reg$column_name
    )
    result$r_type <- unname(type_lookup[result$column_name])
    result$is_factor <- unname(factor_lookup[result$column_name])
  } else {
    result$r_type <- NA_character_
    result$is_factor <- FALSE
  }

  # Replace NAs with defaults
  result$r_type[is.na(result$r_type)] <- "unknown"
  result$is_factor[is.na(result$is_factor)] <- FALSE

  result
}


#' Execute SQL Query on ALprekDB Database
#'
#' @description Executes an arbitrary SQL query and returns the result as a
#'   tibble. Useful for custom aggregation, filtering, and analysis directly
#'   in the database.
#'
#' @param conn A DBI connection object from [db_init()].
#' @param sql Character. SQL query string.
#'
#' @return A tibble with the query results.
#'
#' @examples
#' \dontrun{
#' conn <- db_init("output/alprekdb.duckdb", read_only = TRUE)
#' result <- db_query(conn, "
#'   SELECT school_year, COUNT(*) as n,
#'          AVG(grand_total) as mean_budget
#'   FROM master_classroom
#'   GROUP BY school_year
#'   ORDER BY school_year
#' ")
#' db_close(conn)
#' }
#'
#' @export
db_query <- function(conn, sql) {
  .db_require_packages()
  .db_validate_conn(conn)
  tibble::as_tibble(DBI::dbGetQuery(conn, sql))
}
