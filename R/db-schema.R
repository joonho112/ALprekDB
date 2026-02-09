# ---- Database Schema Management ----
# Internal functions for managing DuckDB schema, metadata, and column type registry.
# These support the factor/Date/integer round-trip strategy.

#' @keywords internal
.db_require_packages <- function() {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("The 'DBI' package is required for database operations. ",
         "Install with: install.packages('DBI')", call. = FALSE)
  }
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("The 'duckdb' package is required for database operations. ",
         "Install with: install.packages('duckdb')", call. = FALSE)
  }
}


#' @keywords internal
.db_validate_conn <- function(conn) {
  if (!DBI::dbIsValid(conn)) {
    stop("Database connection is not valid.", call. = FALSE)
  }
}


#' Create database schema (metadata + column type registry tables)
#' @keywords internal
.db_create_schema <- function(conn) {
  # Metadata table

DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS _alprek_meta (
      key    VARCHAR PRIMARY KEY,
      value  VARCHAR
    )
  ")

  # Column type registry
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS _alprek_column_types (
      table_name   VARCHAR NOT NULL,
      column_name  VARCHAR NOT NULL,
      r_type       VARCHAR NOT NULL,
      factor_levels VARCHAR,
      PRIMARY KEY (table_name, column_name)
    )
  ")

  # Insert initial metadata
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  pkg_version <- tryCatch(
    as.character(utils::packageVersion("ALprekDB")),
    error = function(e) "unknown"
  )

  .db_upsert_meta(conn, "schema_version", "1")
  .db_upsert_meta(conn, "created_at", now)
  .db_upsert_meta(conn, "alprekdb_version", pkg_version)
  .db_upsert_meta(conn, "last_modified_at", now)
}


#' Upsert a metadata key-value pair
#' @keywords internal
.db_upsert_meta <- function(conn, key, value) {
  # Delete then insert (DuckDB-compatible upsert)
  DBI::dbExecute(conn,
    "DELETE FROM _alprek_meta WHERE key = ?",
    params = list(key)
  )
  DBI::dbExecute(conn,
    "INSERT INTO _alprek_meta (key, value) VALUES (?, ?)",
    params = list(key, value)
  )
}


#' Validate existing database schema version
#' @keywords internal
.db_validate_schema <- function(conn) {
  # Check meta table exists
  tables <- DBI::dbListTables(conn)
  if (!"_alprek_meta" %in% tables) {
    stop("Database does not contain ALprekDB schema. ",
         "Use db_init() to create a new database.", call. = FALSE)
  }

  # Check schema version
  result <- DBI::dbGetQuery(conn,
    "SELECT value FROM _alprek_meta WHERE key = 'schema_version'"
  )
  if (nrow(result) == 0) {
    stop("Database schema version not found.", call. = FALSE)
  }
  if (result$value[1] != "1") {
    stop("Database schema version mismatch. Expected '1', got '",
         result$value[1], "'. ",
         "Database may have been created by a different version of ALprekDB.",
         call. = FALSE)
  }

  invisible(TRUE)
}


#' Register column types from an R data frame into the type registry
#' @keywords internal
.db_register_column_types <- function(conn, table_name, df) {
  # Remove existing entries for this table
  DBI::dbExecute(conn,
    "DELETE FROM _alprek_column_types WHERE table_name = ?",
    params = list(table_name)
  )

  # Build registry entries
  entries <- lapply(names(df), function(col_name) {
    col <- df[[col_name]]
    r_type <- .detect_r_type(col)
    factor_lvls <- if (is.factor(col)) {
      .serialize_factor_levels(levels(col))
    } else {
      NA_character_
    }
    data.frame(
      table_name = table_name,
      column_name = col_name,
      r_type = r_type,
      factor_levels = factor_lvls,
      stringsAsFactors = FALSE
    )
  })

  registry <- do.call(rbind, entries)
  DBI::dbAppendTable(conn, "_alprek_column_types", registry)
}


#' Get column type registry for a table
#' @keywords internal
.db_get_column_types <- function(conn, table_name) {
  DBI::dbGetQuery(conn,
    "SELECT column_name, r_type, factor_levels FROM _alprek_column_types WHERE table_name = ?",
    params = list(table_name)
  )
}


#' Reconstruct R types from registry after reading from DuckDB
#' @keywords internal
.db_reconstruct_types <- function(df, type_registry) {
  if (nrow(type_registry) == 0) return(df)

  for (i in seq_len(nrow(type_registry))) {
    col_name <- type_registry$column_name[i]
    r_type <- type_registry$r_type[i]
    factor_lvls_str <- type_registry$factor_levels[i]

    if (!col_name %in% names(df)) next

    col <- df[[col_name]]

    if (r_type == "factor" && !is.na(factor_lvls_str)) {
      lvls <- .deserialize_factor_levels(factor_lvls_str)
      df[[col_name]] <- factor(col, levels = lvls)
    } else if (r_type == "Date") {
      if (!inherits(col, "Date")) {
        df[[col_name]] <- as.Date(col)
      }
    } else if (r_type == "integer") {
      if (!is.integer(col)) {
        df[[col_name]] <- as.integer(col)
      }
    } else if (r_type == "logical") {
      if (!is.logical(col)) {
        df[[col_name]] <- as.logical(col)
      }
    }
    # numeric and character are already correct from DuckDB
  }

  df
}


# ---- Type detection helpers ----

#' Detect R type of a column
#' @keywords internal
.detect_r_type <- function(x) {
  if (is.factor(x)) return("factor")
  if (inherits(x, "Date")) return("Date")
  if (inherits(x, "POSIXct")) return("POSIXct")
  if (is.logical(x)) return("logical")
  if (is.integer(x)) return("integer")
  if (is.numeric(x)) return("numeric")
  "character"
}


#' Serialize factor levels to JSON-like string (without jsonlite)
#' @keywords internal
.serialize_factor_levels <- function(levels) {
  # Escape any internal quotes
  escaped <- gsub('"', '\\"', levels, fixed = TRUE)
  paste0('["', paste(escaped, collapse = '","'), '"]')
}


#' Deserialize factor levels from JSON-like string (without jsonlite)
#' @keywords internal
.deserialize_factor_levels <- function(str) {
  # Remove brackets
  inner <- sub('^\\["?', "", str)
  inner <- sub('"?\\]$', "", inner)
  # Split on ","
  parts <- strsplit(inner, '","', fixed = TRUE)[[1]]
  # Remove leading/trailing quotes
  parts <- gsub('^"|"$', "", parts)
  # Unescape quotes
  parts <- gsub('\\"', '"', parts, fixed = TRUE)
  parts
}


#' Merge factor levels (union of old and new)
#' @keywords internal
.db_merge_factor_levels <- function(conn, table_name, df) {
  existing_types <- .db_get_column_types(conn, table_name)

  for (col_name in names(df)) {
    if (!is.factor(df[[col_name]])) next

    new_levels <- levels(df[[col_name]])

    existing_row <- existing_types[existing_types$column_name == col_name, ]
    if (nrow(existing_row) > 0 && !is.na(existing_row$factor_levels[1])) {
      old_levels <- .deserialize_factor_levels(existing_row$factor_levels[1])
      merged <- unique(c(old_levels, new_levels))
    } else {
      merged <- new_levels
    }

    # Update the registry
    DBI::dbExecute(conn,
      "DELETE FROM _alprek_column_types WHERE table_name = ? AND column_name = ?",
      params = list(table_name, col_name)
    )
    DBI::dbExecute(conn,
      "INSERT INTO _alprek_column_types (table_name, column_name, r_type, factor_levels) VALUES (?, ?, ?, ?)",
      params = list(table_name, col_name, "factor", .serialize_factor_levels(merged))
    )
  }
}
