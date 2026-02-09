#' Write Panel Data to DuckDB
#'
#' @description Writes an ALprekDB panel object to the database. Automatically
#'   determines the table name from the S3 class. Factor columns are converted
#'   to character for storage; their levels are preserved in the column type
#'   registry for reconstruction on read.
#'
#' @param conn A DBI connection object from [db_init()].
#' @param panel_obj An `alprek_budget_panel`, `alprek_classroom_panel`, or
#'   `alprek_student_panel` object.
#' @param overwrite Logical. If `TRUE`, drops and recreates the table.
#'   Default `FALSE`.
#'
#' @return Invisible table name.
#'
#' @export
db_write_panel <- function(conn, panel_obj, overwrite = FALSE) {
  .db_require_packages()
  .db_validate_conn(conn)

  table_name <- .db_table_name_panel(panel_obj)
  df <- .db_extract_panel_data(panel_obj)

  if (!overwrite && table_name %in% DBI::dbListTables(conn)) {
    stop("Table '", table_name, "' already exists. ",
         "Use overwrite = TRUE to replace.", call. = FALSE)
  }

  if (overwrite && table_name %in% DBI::dbListTables(conn)) {
    DBI::dbRemoveTable(conn, table_name)
  }

  # Register column types BEFORE converting factors
  .db_register_column_types(conn, table_name, df)

  # Convert factors to character for storage
  write_df <- .db_prepare_for_write(df)

  DBI::dbWriteTable(conn, table_name, write_df)

  # Update metadata
  .db_upsert_meta(conn, "last_modified_at",
                   format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

  msg_success("Wrote {nrow(df)} rows x {ncol(df)} cols to '{table_name}'")
  invisible(table_name)
}


#' Write Master Linked Dataset to DuckDB
#'
#' @description Writes both classroom-level and student-level data from a
#'   master linked dataset to the database.
#'
#' @param conn A DBI connection object from [db_init()].
#' @param master_obj An `alprek_linkage_master` object.
#' @param overwrite Logical. If `TRUE`, drops and recreates the tables.
#'   Default `FALSE`.
#'
#' @return Invisible character vector of table names written.
#'
#' @export
db_write_master <- function(conn, master_obj, overwrite = FALSE) {
  .db_require_packages()
  .db_validate_conn(conn)

  if (!inherits(master_obj, "alprek_linkage_master")) {
    stop("Expected an 'alprek_linkage_master' object.", call. = FALSE)
  }

  tables <- c("master_classroom", "master_student")
  dfs <- list(master_obj$classroom_level, master_obj$student_level)

  for (i in seq_along(tables)) {
    tbl <- tables[i]
    df <- dfs[[i]]

    if (!overwrite && tbl %in% DBI::dbListTables(conn)) {
      stop("Table '", tbl, "' already exists. ",
           "Use overwrite = TRUE to replace.", call. = FALSE)
    }

    if (overwrite && tbl %in% DBI::dbListTables(conn)) {
      DBI::dbRemoveTable(conn, tbl)
    }

    .db_register_column_types(conn, tbl, df)
    write_df <- .db_prepare_for_write(df)
    DBI::dbWriteTable(conn, tbl, write_df)

    msg_success("Wrote {nrow(df)} rows x {ncol(df)} cols to '{tbl}'")
  }

  .db_upsert_meta(conn, "last_modified_at",
                   format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

  invisible(tables)
}


#' Append Single Year to Existing Database Table
#'
#' @description Appends data for a single year to an existing table. Validates
#'   that the year does not already exist and that columns are compatible.
#'   Factor levels are merged (union of existing + new).
#'
#' @param conn A DBI connection object from [db_init()].
#' @param panel_obj An `alprek_budget_panel`, `alprek_classroom_panel`, or
#'   `alprek_student_panel` object containing data for a single year.
#' @param table Character or `NULL`. Override table name. If `NULL` (default),
#'   auto-detected from S3 class.
#'
#' @return Invisible table name.
#'
#' @export
db_write_year <- function(conn, panel_obj, table = NULL) {
  .db_require_packages()
  .db_validate_conn(conn)

  if (is.null(table)) {
    table <- .db_table_name_panel(panel_obj)
  }

  # Table must already exist
  if (!table %in% DBI::dbListTables(conn)) {
    stop("Table '", table, "' does not exist. ",
         "Use db_write_panel() to create it first.", call. = FALSE)
  }

  df <- .db_extract_panel_data(panel_obj)

  # Check for duplicate years
  new_years <- unique(df$school_year)
  existing_years <- .db_existing_years(conn, table)
  overlap <- intersect(new_years, existing_years)
  if (length(overlap) > 0) {
    stop("Year(s) already exist in table '", table, "': ",
         paste(overlap, collapse = ", "),
         ". Remove existing data first or use db_write_panel(overwrite = TRUE).",
         call. = FALSE)
  }

  # Check column compatibility
  existing_cols <- DBI::dbListFields(conn, table)
  new_cols <- names(.db_prepare_for_write(df))
  missing_in_db <- setdiff(new_cols, existing_cols)
  if (length(missing_in_db) > 0) {
    msg_warn("New data has {length(missing_in_db)} column(s) not in existing table: {paste(missing_in_db[1:min(5, length(missing_in_db))], collapse = ', ')}")
  }

  # Merge factor levels before writing
  .db_merge_factor_levels(conn, table, df)

  # Append data
  write_df <- .db_prepare_for_write(df)
  # Only write columns that exist in the table
  common_cols <- intersect(names(write_df), existing_cols)
  write_df <- write_df[, common_cols, drop = FALSE]

  DBI::dbAppendTable(conn, table, write_df)

  .db_upsert_meta(conn, "last_modified_at",
                   format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

  msg_success("Appended {nrow(df)} rows for {paste(new_years, collapse = ', ')} to '{table}'")
  invisible(table)
}


# ---- Internal helpers ----

#' Map S3 class to table name
#' @keywords internal
.db_table_name_panel <- function(obj) {
  if (inherits(obj, "alprek_budget_panel")) return("budget_panel")
  if (inherits(obj, "alprek_classroom_panel")) return("classroom_panel")
  if (inherits(obj, "alprek_student_panel")) return("student_panel")
  stop("Unsupported panel object class: ",
       paste(class(obj), collapse = ", "), call. = FALSE)
}


#' Extract data frame from panel object
#' @keywords internal
.db_extract_panel_data <- function(obj) {
  if (inherits(obj, "alprek_budget_panel")) return(obj$data)
  if (inherits(obj, "alprek_classroom_panel")) return(obj$data)
  if (inherits(obj, "alprek_student_panel")) return(obj$data)
  stop("Cannot extract data from this object.", call. = FALSE)
}


#' Prepare data frame for DuckDB write (factor → character)
#' @keywords internal
.db_prepare_for_write <- function(df) {
  for (col in names(df)) {
    if (is.factor(df[[col]])) {
      df[[col]] <- as.character(df[[col]])
    }
  }
  # Convert tibble to data.frame for DBI compatibility
  as.data.frame(df, stringsAsFactors = FALSE)
}


#' Get existing school_year values from a table
#' @keywords internal
.db_existing_years <- function(conn, table) {
  result <- DBI::dbGetQuery(conn, paste0(
    "SELECT DISTINCT school_year FROM \"", table, "\" ORDER BY school_year"
  ))
  result$school_year
}
