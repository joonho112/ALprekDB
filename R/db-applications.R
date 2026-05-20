#' Write Applications Master to DuckDB
#'
#' @description Persists an `alprek_applications_master` into DuckDB across
#'   up to four tables:
#'   * `applications_clean` - per-cycle, applications-grain rows
#'     (`master$data`).
#'   * `applications_capacity` - per-cycle, capacity-grain rows
#'     (`master$capacity_data`); only created when `master$capacity_data`
#'     is non-NULL.
#'   * `applications_lineage` - one row per write, capturing
#'     `cycle_year`, `file_sha256`, `git_sha`, `reconciled_at`,
#'     `transformed_at`, and `written_at` for downstream traceability.
#'   * `applications_derived_log` - per-cycle derivation audit rows from
#'     `master$derived_log`.
#'
#'   Type registry is updated through the shared
#'   `.db_register_column_types()` helper so reads reconstruct factor /
#'   integer / numeric columns correctly.
#'
#' @param conn A DBI connection (from `db_init()`).
#' @param master An `alprek_applications_master`.
#' @param overwrite Logical. If `TRUE`, drop and recreate the tables before
#'   writing. Default `FALSE` - duplicates `(cycle_year)` rows are rejected.
#'
#' @return Invisible character vector of tables written.
#' @export
db_write_applications_master <- function(conn, master, overwrite = FALSE) {
  .db_require_packages()
  .db_validate_conn(conn)

  if (!inherits(master, "alprek_applications_master")) {
    stop("Expected an 'alprek_applications_master' object.", call. = FALSE)
  }

  cy <- master$meta$cycle_year %||% NA_character_
  if (is.na(cy)) {
    stop("master$meta$cycle_year is NA; refusing to write without a cycle.",
         call. = FALSE)
  }

  written <- character(0)

  # ---- 1. applications_clean ----
  written <- c(written,
                .db_app_write_one(conn,
                                    table_name = "applications_clean",
                                    df = master$data,
                                    cycle_year = cy,
                                    overwrite = overwrite))

  # ---- 2. applications_capacity ----
  if (!is.null(master$capacity_data) && nrow(master$capacity_data) > 0L) {
    written <- c(written,
                  .db_app_write_one(conn,
                                      table_name = "applications_capacity",
                                      df = master$capacity_data,
                                      cycle_year = cy,
                                      overwrite = overwrite))
  }

  # ---- 3. applications_lineage ----
  lineage_row <- tibble::tibble(
    cycle_year     = cy,
    file_sha256    = master$meta$file_sha256 %||% NA_character_,
    git_sha        = master$meta$git_sha %||% NA_character_,
    reconciled_at  = master$meta$reconciled_at %||% NA_character_,
    transformed_at = master$meta$transformed_at %||% NA_character_,
    fuzzy_threshold = master$meta$fuzzy_threshold %||% NA_real_,
    seed            = master$meta$seed %||% NA_integer_,
    n_apps          = nrow(master$data),
    n_capacity      = if (is.null(master$capacity_data)) 0L
                        else nrow(master$capacity_data),
    written_at      = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  written <- c(written, .db_app_write_lineage(conn, lineage_row, overwrite))

  # ---- 4. applications_derived_log ----
  if (!is.null(master$derived_log) && nrow(master$derived_log) > 0L) {
    derived_log <- master$derived_log
    if (!"cycle_year" %in% names(derived_log)) {
      derived_log$cycle_year <- cy
    }
    derived_log <- derived_log[, c("cycle_year",
                                   setdiff(names(derived_log), "cycle_year")),
                               drop = FALSE]
    written <- c(written,
                  .db_app_write_one(conn,
                                    table_name = "applications_derived_log",
                                    df = derived_log,
                                    cycle_year = cy,
                                    overwrite = overwrite))
  }

  .db_upsert_meta(conn, "last_modified_at",
                   format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  invisible(written)
}


#' Write Applications Panel to DuckDB
#'
#' @description Persists an `alprek_applications_panel` (multi-cycle) to
#'   `applications_panel` + `applications_capacity_panel` (if capacity
#'   present) + `applications_lineage` (one row per cycle).
#'
#' @param conn A DBI connection.
#' @param panel An `alprek_applications_panel`.
#' @param overwrite Logical. Default `FALSE`.
#'
#' @return Invisible character vector of tables written.
#' @export
db_write_applications_panel <- function(conn, panel, overwrite = FALSE) {
  .db_require_packages()
  .db_validate_conn(conn)
  if (!inherits(panel, "alprek_applications_panel")) {
    stop("Expected an 'alprek_applications_panel' object.", call. = FALSE)
  }

  written <- character(0)

  # applications_panel
  if (!overwrite && "applications_panel" %in% DBI::dbListTables(conn)) {
    stop("Table 'applications_panel' already exists. Use overwrite = TRUE.",
         call. = FALSE)
  }
  if (overwrite && "applications_panel" %in% DBI::dbListTables(conn)) {
    DBI::dbRemoveTable(conn, "applications_panel")
  }
  .db_register_column_types(conn, "applications_panel", panel$data)
  DBI::dbWriteTable(conn, "applications_panel",
                      .db_prepare_for_write(panel$data))
  written <- c(written, "applications_panel")

  # applications_capacity_panel
  if (!is.null(panel$capacity_data) && nrow(panel$capacity_data) > 0L) {
    if (!overwrite && "applications_capacity_panel" %in% DBI::dbListTables(conn)) {
      stop("Table 'applications_capacity_panel' already exists. Use overwrite = TRUE.",
           call. = FALSE)
    }
    if (overwrite && "applications_capacity_panel" %in% DBI::dbListTables(conn)) {
      DBI::dbRemoveTable(conn, "applications_capacity_panel")
    }
    .db_register_column_types(conn, "applications_capacity_panel",
                                panel$capacity_data)
    DBI::dbWriteTable(conn, "applications_capacity_panel",
                        .db_prepare_for_write(panel$capacity_data))
    written <- c(written, "applications_capacity_panel")
  }

  # lineage - one row per cycle
  for (yr in panel$cycle_years) {
    info <- panel$by_cycle[[yr]]
    lineage_row <- tibble::tibble(
      cycle_year     = yr,
      file_sha256    = info$file_sha256 %||% NA_character_,
      git_sha        = info$git_sha %||% NA_character_,
      reconciled_at  = info$reconciled_at %||% NA_character_,
      transformed_at = info$transformed_at %||% panel$meta$binded_at %||% NA_character_,
      fuzzy_threshold = info$fuzzy_threshold %||% NA_real_,
      seed            = info$seed %||% NA_integer_,
      n_apps          = as.integer(info$n_apps),
      n_capacity      = as.integer(info$n_capacity),
      written_at      = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    .db_app_write_lineage(conn, lineage_row, overwrite = FALSE)
  }
  written <- c(written, "applications_lineage")

  .db_upsert_meta(conn, "last_modified_at",
                   format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  invisible(written)
}


#' Read Applications Master from DuckDB
#'
#' @description Reconstructs an `alprek_applications_master` from
#'   DuckDB. Filters on `cycle_year`; if `cycle_year` is `NULL`, returns the
#'   most recent cycle present.
#'
#' @param conn A DBI connection.
#' @param cycle_year Character or `NULL`. The cycle to load.
#'
#' @return An `alprek_applications_master`.
#' @export
db_read_applications_master <- function(conn, cycle_year = NULL) {
  .db_require_packages()
  .db_validate_conn(conn)

  if (!"applications_clean" %in% DBI::dbListTables(conn)) {
    stop("No applications_clean table found in this database.",
         call. = FALSE)
  }

  if (is.null(cycle_year)) {
    yrs <- DBI::dbGetQuery(conn,
      "SELECT DISTINCT cycle_year FROM applications_clean ORDER BY cycle_year DESC")$cycle_year
    if (length(yrs) == 0L) stop("applications_clean is empty.", call. = FALSE)
    cycle_year <- yrs[1L]
  }

  d <- DBI::dbGetQuery(conn,
    sprintf("SELECT * FROM applications_clean WHERE cycle_year = '%s'", cycle_year))
  d <- .db_reconstruct_types(d,
                                 .db_get_column_types(conn, "applications_clean"))

  cap <- NULL
  if ("applications_capacity" %in% DBI::dbListTables(conn)) {
    cd <- DBI::dbGetQuery(conn,
      sprintf("SELECT * FROM applications_capacity WHERE cycle_year = '%s'",
               cycle_year))
    if (nrow(cd) > 0L) {
      cap <- .db_reconstruct_types(cd,
        .db_get_column_types(conn, "applications_capacity"))
    }
  }

  lin <- if ("applications_lineage" %in% DBI::dbListTables(conn)) {
    DBI::dbGetQuery(conn,
      sprintf("SELECT * FROM applications_lineage WHERE cycle_year = '%s'",
               cycle_year))
  } else NULL

  derived_log <- tibble::tibble()
  if ("applications_derived_log" %in% DBI::dbListTables(conn)) {
    dl <- DBI::dbGetQuery(conn,
      sprintf("SELECT * FROM applications_derived_log WHERE cycle_year = '%s'",
               cycle_year))
    if (nrow(dl) > 0L) {
      derived_log <- .db_reconstruct_types(
        dl,
        .db_get_column_types(conn, "applications_derived_log")
      )
    }
  }

  meta <- list(
    cycle_year = cycle_year,
    file_sha256 = if (!is.null(lin) && nrow(lin) > 0L) lin$file_sha256[1L] else NA_character_,
    git_sha     = if (!is.null(lin) && nrow(lin) > 0L) lin$git_sha[1L] else NA_character_,
    reconciled_at  = if (!is.null(lin) && nrow(lin) > 0L) lin$reconciled_at[1L] else NA_character_,
    transformed_at = if (!is.null(lin) && nrow(lin) > 0L) lin$transformed_at[1L] else NA_character_,
    fuzzy_threshold = if (!is.null(lin) && nrow(lin) > 0L) lin$fuzzy_threshold[1L] else NA_real_,
    seed            = if (!is.null(lin) && nrow(lin) > 0L) lin$seed[1L] else NA_integer_,
    loaded_from_db  = TRUE,
    db_loaded_at    = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )

	structure(list(
	    data = tibble::as_tibble(d),
	    capacity_data = if (!is.null(cap)) tibble::as_tibble(cap) else NULL,
	    derived_log = tibble::as_tibble(derived_log),
	    meta = meta
	  ), class = "alprek_applications_master")
}


#' Read Applications Panel from DuckDB
#'
#' @description Reconstructs an `alprek_applications_panel` from DuckDB.
#'
#' @param conn A DBI connection.
#' @param cycle_years Optional character vector. If `NULL`, returns all
#'   cycles present.
#' @return An `alprek_applications_panel`.
#' @export
db_read_applications_panel <- function(conn, cycle_years = NULL) {
  .db_require_packages()
  .db_validate_conn(conn)

  if (!"applications_panel" %in% DBI::dbListTables(conn)) {
    stop("No applications_panel table found in this database.",
         call. = FALSE)
  }

  query <- "SELECT * FROM applications_panel"
  if (!is.null(cycle_years)) {
    yrs <- paste0("'", cycle_years, "'", collapse = ", ")
    query <- paste0(query, sprintf(" WHERE cycle_year IN (%s)", yrs))
  }
  d <- DBI::dbGetQuery(conn, query)
  d <- .db_reconstruct_types(d,
                                 .db_get_column_types(conn, "applications_panel"))

  cap <- NULL
  if ("applications_capacity_panel" %in% DBI::dbListTables(conn)) {
    cq <- "SELECT * FROM applications_capacity_panel"
    if (!is.null(cycle_years)) {
      cq <- paste0(cq, sprintf(" WHERE cycle_year IN (%s)",
                                  paste0("'", cycle_years, "'", collapse = ", ")))
    }
    cap_df <- DBI::dbGetQuery(conn, cq)
    if (nrow(cap_df) > 0L) {
      cap <- .db_reconstruct_types(cap_df,
        .db_get_column_types(conn, "applications_capacity_panel"))
    }
  }

  cy_sorted <- sort(unique(d$cycle_year))
  lin <- if ("applications_lineage" %in% DBI::dbListTables(conn)) {
    lq <- "SELECT * FROM applications_lineage"
    if (!is.null(cycle_years)) {
      lq <- paste0(lq, sprintf(" WHERE cycle_year IN (%s)",
                               paste0("'", cycle_years, "'", collapse = ", ")))
    }
    DBI::dbGetQuery(conn, lq)
  } else NULL

  structure(list(
    data = tibble::as_tibble(d),
    capacity_data = if (!is.null(cap)) tibble::as_tibble(cap) else NULL,
    cycle_years = cy_sorted,
    n_cycles = length(cy_sorted),
	    by_cycle = lapply(cy_sorted, function(yr) {
	      lin_yr <- if (!is.null(lin) && nrow(lin) > 0L) {
	        rows <- lin[lin$cycle_year == yr, , drop = FALSE]
	        if (nrow(rows) > 0L) rows[1L, , drop = FALSE] else NULL
	      } else NULL
	      list(cycle_year = yr,
	            n_apps    = sum(d$cycle_year == yr),
	            n_capacity = if (is.null(cap)) 0L else sum(cap$cycle_year == yr),
	            n_buckets = if ("bucket" %in% names(d))
	                          as.list(table(factor(d$bucket[d$cycle_year == yr],
	                                                  levels = c("A","B","C","D","unknown"))))
	                        else list(),
	            file_sha256 = if (!is.null(lin_yr)) lin_yr$file_sha256[1L] else NA_character_,
	            git_sha = if (!is.null(lin_yr)) lin_yr$git_sha[1L] else NA_character_,
	            reconciled_at = if (!is.null(lin_yr)) lin_yr$reconciled_at[1L] else NA_character_,
	            transformed_at = if (!is.null(lin_yr)) lin_yr$transformed_at[1L] else NA_character_,
	            fuzzy_threshold = if (!is.null(lin_yr)) lin_yr$fuzzy_threshold[1L] else NA_real_,
	            seed = if (!is.null(lin_yr)) lin_yr$seed[1L] else NA_integer_)
	    }),
    meta = list(
      binded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      loaded_from_db = TRUE
    )
  ), class = "alprek_applications_panel")
}


# ============================================================================
# Internal helpers
# ============================================================================

#' @keywords internal
#' @noRd
.db_app_write_one <- function(conn, table_name, df, cycle_year, overwrite) {
  if (!"cycle_year" %in% names(df)) {
    df$cycle_year <- cycle_year
  }

  if (!overwrite && table_name %in% DBI::dbListTables(conn)) {
    existing_cycles <- DBI::dbGetQuery(conn,
      sprintf("SELECT DISTINCT cycle_year FROM \"%s\"", table_name))$cycle_year
    if (cycle_year %in% existing_cycles) {
      stop("Table '", table_name, "' already contains rows for cycle_year '",
           cycle_year, "'. Pass overwrite = TRUE or delete the cycle first.",
           call. = FALSE)
    }
    .db_register_column_types(conn, table_name, df)
    DBI::dbWriteTable(conn, table_name, .db_prepare_for_write(df),
                        append = TRUE)
    return(table_name)
  }

  if (overwrite && table_name %in% DBI::dbListTables(conn)) {
    DBI::dbRemoveTable(conn, table_name)
  }
  .db_register_column_types(conn, table_name, df)
  DBI::dbWriteTable(conn, table_name, .db_prepare_for_write(df))
  table_name
}

#' @keywords internal
#' @noRd
.db_app_write_lineage <- function(conn, lineage_row, overwrite) {
  if (overwrite && "applications_lineage" %in% DBI::dbListTables(conn)) {
    DBI::dbRemoveTable(conn, "applications_lineage")
  }
  .db_register_column_types(conn, "applications_lineage", lineage_row)
  if ("applications_lineage" %in% DBI::dbListTables(conn)) {
    DBI::dbWriteTable(conn, "applications_lineage",
                        .db_prepare_for_write(lineage_row),
                        append = TRUE)
  } else {
    DBI::dbWriteTable(conn, "applications_lineage",
                        .db_prepare_for_write(lineage_row))
  }
  "applications_lineage"
}
