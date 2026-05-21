# R/db-geocode.R
#
# DuckDB persistence for the v0.8.0 geocoding module. Mirrors the
# R/db-applications.R pattern: per-run writes/reads + a long-format panel
# table + a lineage audit table. Schema version is unchanged (stays at
# "1"); the four new tables are additive (Decision §11.5).
#
# Tables managed here:
#   * geocode_clean        — per geocode_run_id, cleaned standardized cols
#                            from alprek_geocode_clean$data.
#   * geocode_reconciled   — per geocode_run_id, full reconciled rows with
#                            the 10 authoritative columns + lineage_id.
#   * geocode_panel        — cross-run, long-format multi-snapshot view
#                            from alprek_geocode_panel$data.
#   * geocode_lineage      — one row per write, capturing run-level meta
#                            (file_sha256, geocode_run_id, source,
#                            cycle_year, git_sha, n_rows, n_followup,
#                            distance_threshold_rules, flat_threshold_m,
#                            written_at).
#
# Ordered-factor levels round-trip via .db_register_column_types() /
# .db_reconstruct_types() (the registry was extended in db-schema.R to
# emit `"ordered_factor"` for is.ordered() inputs).


# ============================================================================
# 1. geocode_clean
# ============================================================================

#' Write a cleaned geocode object to DuckDB
#'
#' @description Persists `alprek_geocode_clean$data` into the
#'   `geocode_clean` table, partitioned by `geocode_run_id`. The run_id is
#'   either passed explicitly via `run_id =` (preferred) or derived from
#'   `clean$meta` using the same `<source>_v1_<YYYY-MM>` scheme that
#'   [geocode_transform()] emits, so a `clean -> transform -> write` and a
#'   `clean -> write` pair share the same partition.
#'
#'   Also calls [db_write_geocode_lineage()] to record one lineage row per
#'   write.
#'
#' @param conn A DBI connection from [db_init()].
#' @param clean An `alprek_geocode_clean` object.
#' @param run_id Optional character scalar overriding the derived
#'   `geocode_run_id`. When `NULL` (default), derived from `clean$meta`.
#' @param overwrite Logical. If `TRUE`, drop any existing rows for this
#'   `geocode_run_id` before writing. Default `FALSE` — duplicates are
#'   rejected.
#'
#' @return Invisible character vector of tables written.
#' @export
db_write_geocode_clean <- function(conn, clean, run_id = NULL,
                                     overwrite = FALSE) {
  .db_require_packages()
  .db_validate_conn(conn)
  .db_geocode_validate_schema(conn)

  if (!inherits(clean, "alprek_geocode_clean")) {
    stop("Expected an 'alprek_geocode_clean' object ",
         "(from geocode_clean()).", call. = FALSE)
  }

  rid <- .db_geocode_resolve_run_id(run_id, clean$meta)
  if (is.na(rid) || !nzchar(rid)) {
    stop("Could not resolve a geocode_run_id; pass run_id= explicitly.",
         call. = FALSE)
  }

  written <- character(0)
  written <- c(written,
                .db_geocode_write_one(conn,
                                      table_name = "geocode_clean",
                                      df         = clean$data,
                                      run_id     = rid,
                                      overwrite  = overwrite))

  # Lineage row (one per write).
  written <- c(written, db_write_geocode_lineage(conn, clean, run_id = rid))

  .db_upsert_meta(conn, "last_modified_at",
                   format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  invisible(written)
}


#' Read a cleaned geocode object back from DuckDB
#'
#' @description Reconstructs an `alprek_geocode_clean` from the
#'   `geocode_clean` table for a single `geocode_run_id`. When `run_id`
#'   is `NULL`, the most-recent run (lexicographic max — run_ids encode
#'   YYYY-MM dates) is returned.
#'
#' @param conn A DBI connection from [db_init()].
#' @param run_id Optional character scalar. The run to load.
#'
#' @return An `alprek_geocode_clean` object.
#' @export
db_read_geocode_clean <- function(conn, run_id = NULL) {
  .db_require_packages()
  .db_validate_conn(conn)

  if (!"geocode_clean" %in% DBI::dbListTables(conn)) {
    stop("No geocode_clean table found in this database.", call. = FALSE)
  }

  if (is.null(run_id)) {
    rids <- DBI::dbGetQuery(conn,
      "SELECT DISTINCT geocode_run_id FROM geocode_clean
       ORDER BY geocode_run_id DESC")$geocode_run_id
    if (length(rids) == 0L) {
      stop("geocode_clean is empty.", call. = FALSE)
    }
    run_id <- rids[1L]
  }

  d <- DBI::dbGetQuery(conn,
    sprintf("SELECT * FROM geocode_clean WHERE geocode_run_id = '%s'",
            run_id))
  d <- .db_reconstruct_types(d, .db_get_column_types(conn, "geocode_clean"))

  meta <- .db_geocode_read_lineage_meta(conn, run_id)
  meta$loaded_from_db <- TRUE
  meta$db_loaded_at   <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

  cleaning_log <- tibble::tibble(
    rule       = character(0),
    n_affected = integer(0),
    details    = character(0),
    severity   = character(0)
  )

  structure(list(
    data         = tibble::as_tibble(d),
    cleaning_log = cleaning_log,
    meta         = meta
  ), class = "alprek_geocode_clean")
}


# ============================================================================
# 2. geocode_reconciled
# ============================================================================

#' Write a reconciled geocode object to DuckDB
#'
#' @description Persists `alprek_geocode_reconciled$data` into the
#'   `geocode_reconciled` table, partitioned by `geocode_run_id`. Includes
#'   all 10 authoritative columns from [geocode_reconcile()] plus the
#'   `lineage_id` lineage key from Step 3.1 (so the row-level lineage
#'   survives the round-trip).
#'
#'   Also calls [db_write_geocode_lineage()].
#'
#' @param conn A DBI connection from [db_init()].
#' @param reconciled An `alprek_geocode_reconciled` object.
#' @param run_id Optional character. When `NULL`, derived from
#'   `reconciled$meta`.
#' @param overwrite Logical. Default `FALSE`.
#'
#' @return Invisible character vector of tables written.
#' @export
db_write_geocode_reconciled <- function(conn, reconciled, run_id = NULL,
                                         overwrite = FALSE) {
  .db_require_packages()
  .db_validate_conn(conn)
  .db_geocode_validate_schema(conn)

  if (!inherits(reconciled, "alprek_geocode_reconciled")) {
    stop("Expected an 'alprek_geocode_reconciled' object ",
         "(from geocode_reconcile()).", call. = FALSE)
  }

  rid <- .db_geocode_resolve_run_id(run_id, reconciled$meta)
  if (is.na(rid) || !nzchar(rid)) {
    stop("Could not resolve a geocode_run_id; pass run_id= explicitly.",
         call. = FALSE)
  }

  written <- character(0)
  written <- c(written,
                .db_geocode_write_one(conn,
                                      table_name = "geocode_reconciled",
                                      df         = reconciled$data,
                                      run_id     = rid,
                                      overwrite  = overwrite))

  written <- c(written, db_write_geocode_lineage(conn, reconciled,
                                                  run_id = rid))

  .db_upsert_meta(conn, "last_modified_at",
                   format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  invisible(written)
}


#' Read a reconciled geocode object back from DuckDB
#'
#' @description Reconstructs an `alprek_geocode_reconciled` from the
#'   `geocode_reconciled` table. Ordered factor levels for `lat_precision`
#'   and `coord_model_status` round-trip via the column type registry.
#'   The `reconciliation_log` is reconstructed as an empty placeholder
#'   (the original log is not persisted in v0.8.0; downstream consumers
#'   that need it should call [geocode_reconcile()] again).
#'
#' @param conn A DBI connection from [db_init()].
#' @param run_id Optional character. When `NULL`, the most-recent run.
#'
#' @return An `alprek_geocode_reconciled` object.
#' @export
db_read_geocode_reconciled <- function(conn, run_id = NULL) {
  .db_require_packages()
  .db_validate_conn(conn)

  if (!"geocode_reconciled" %in% DBI::dbListTables(conn)) {
    stop("No geocode_reconciled table found in this database.",
         call. = FALSE)
  }

  if (is.null(run_id)) {
    rids <- DBI::dbGetQuery(conn,
      "SELECT DISTINCT geocode_run_id FROM geocode_reconciled
       ORDER BY geocode_run_id DESC")$geocode_run_id
    if (length(rids) == 0L) {
      stop("geocode_reconciled is empty.", call. = FALSE)
    }
    run_id <- rids[1L]
  }

  d <- DBI::dbGetQuery(conn,
    sprintf("SELECT * FROM geocode_reconciled WHERE geocode_run_id = '%s'",
            run_id))
  d <- .db_reconstruct_types(d,
                              .db_get_column_types(conn, "geocode_reconciled"))

  meta <- .db_geocode_read_lineage_meta(conn, run_id)
  meta$loaded_from_db <- TRUE
  meta$db_loaded_at   <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

  empty_log <- tibble::tibble(
    row_id                   = character(0),
    lineage_id               = character(0),
    raw_row_index            = integer(0),
    matrix_cell              = character(0),
    adece_present            = logical(0),
    melissa_present          = logical(0),
    result_code              = character(0),
    distance_adece_melissa_m = numeric(0),
    coord_agreement_band     = character(0),
    lat_source               = character(0),
    lat_precision            = character(0),
    threshold_used_m         = numeric(0),
    threshold_name           = character(0),
    needs_followup_geocoding = logical(0),
    followup_reason          = character(0),
    decision_source          = character(0),
    decision_timestamp       = character(0),
    note                     = character(0)
  )
  empty_summary <- tibble::tibble(
    matrix_cell = character(0),
    n           = integer(0),
    description = character(0)
  )

  structure(list(
    data               = tibble::as_tibble(d),
    reconciliation_log = empty_log,
    summary            = empty_summary,
    meta               = meta
  ), class = "alprek_geocode_reconciled")
}


# ============================================================================
# 3. geocode_panel (cross-run, multi-snapshot view)
# ============================================================================

#' Write a geocode panel to DuckDB
#'
#' @description Persists `alprek_geocode_panel$data` into the
#'   `geocode_panel` table. The panel's `data` already carries
#'   `geocode_run_id` (the row-level discriminator from
#'   [geocode_bind_years()]). One lineage row per run is written via
#'   [db_write_geocode_lineage()] using `panel$binding_log`.
#'
#' @param conn A DBI connection from [db_init()].
#' @param panel An `alprek_geocode_panel` object.
#' @param overwrite Logical. Default `FALSE`.
#'
#' @return Invisible character vector of tables written.
#' @export
db_write_geocode_panel <- function(conn, panel, overwrite = FALSE) {
  .db_require_packages()
  .db_validate_conn(conn)
  .db_geocode_validate_schema(conn)

  if (!inherits(panel, "alprek_geocode_panel")) {
    stop("Expected an 'alprek_geocode_panel' object ",
         "(from geocode_bind_years()).", call. = FALSE)
  }

  written <- character(0)

  # ---- geocode_panel ----
  if (!overwrite && "geocode_panel" %in% DBI::dbListTables(conn)) {
    stop("Table 'geocode_panel' already exists. Use overwrite = TRUE.",
         call. = FALSE)
  }
  if (overwrite && "geocode_panel" %in% DBI::dbListTables(conn)) {
    DBI::dbRemoveTable(conn, "geocode_panel")
  }
  .db_register_column_types(conn, "geocode_panel", panel$data)
  DBI::dbWriteTable(conn, "geocode_panel",
                    .db_prepare_for_write(panel$data))
  written <- c(written, "geocode_panel")

  # ---- geocode_lineage (one per run) ----
  written <- c(written, db_write_geocode_lineage(conn, panel))

  .db_upsert_meta(conn, "last_modified_at",
                   format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  invisible(written)
}


#' Read a geocode panel from DuckDB
#'
#' @description Reconstructs an `alprek_geocode_panel` from the
#'   `geocode_panel` table. Ordered factors (`lat_precision`,
#'   `coord_model_status`, `precision_tier`) round-trip via the column
#'   type registry.
#'
#' @param conn A DBI connection.
#' @param run_ids Optional character vector. When `NULL`, returns all
#'   runs present.
#'
#' @return An `alprek_geocode_panel` object.
#' @export
db_read_geocode_panel <- function(conn, run_ids = NULL) {
  .db_require_packages()
  .db_validate_conn(conn)

  if (!"geocode_panel" %in% DBI::dbListTables(conn)) {
    stop("No geocode_panel table found in this database.", call. = FALSE)
  }

  query <- "SELECT * FROM geocode_panel"
  if (!is.null(run_ids)) {
    rs <- paste0("'", run_ids, "'", collapse = ", ")
    query <- paste0(query, sprintf(" WHERE geocode_run_id IN (%s)", rs))
  }
  d <- DBI::dbGetQuery(conn, query)
  d <- .db_reconstruct_types(d, .db_get_column_types(conn, "geocode_panel"))
  d <- tibble::as_tibble(d)

  # Per-run lineage (best effort: pull whatever rows we have).
  lin <- if ("geocode_lineage" %in% DBI::dbListTables(conn)) {
    lq <- "SELECT * FROM geocode_lineage"
    if (!is.null(run_ids)) {
      lq <- paste0(lq, sprintf(" WHERE geocode_run_id IN (%s)",
                                paste0("'", run_ids, "'",
                                        collapse = ", ")))
    }
    DBI::dbGetQuery(conn, lq)
  } else NULL

  rid_col <- if ("geocode_run_id" %in% names(d)) {
    as.character(d$geocode_run_id)
  } else {
    rep(NA_character_, nrow(d))
  }
  unique_rids <- sort(unique(rid_col[!is.na(rid_col)]))

  # Build a minimal binding_log mirror so the panel object is structurally
  # complete. snapshot_date is recovered from lineage when present.
  binding_log <- if (length(unique_rids) > 0L) {
    do.call(rbind, lapply(unique_rids, function(rid) {
      n_rows  <- sum(rid_col == rid)
      lin_row <- if (!is.null(lin) && nrow(lin) > 0L) {
        rows <- lin[lin$geocode_run_id == rid, , drop = FALSE]
        if (nrow(rows) > 0L) rows[1L, , drop = FALSE] else NULL
      } else NULL
      tibble::tibble(
        geocode_run_id = rid,
        snapshot_date  = .db_geocode_extract_snapshot_date(lin_row, rid),
        file_sha256    = if (!is.null(lin_row)) as.character(lin_row$file_sha256)
                         else NA_character_,
        n_rows         = as.integer(n_rows),
        n_columns      = ncol(d),
        severity       = "INFO",
        details        = NA_character_
      )
    }))
  } else {
    tibble::tibble(
      geocode_run_id = character(0),
      snapshot_date  = as.Date(character(0)),
      file_sha256    = character(0),
      n_rows         = integer(0),
      n_columns      = integer(0),
      severity       = character(0),
      details        = character(0)
    )
  }

  n_per_run <- vapply(unique_rids, function(rid) sum(rid_col == rid),
                      integer(1))
  names(n_per_run) <- unique_rids

  vendors <- if (!is.null(lin) && nrow(lin) > 0L) {
    vapply(unique_rids, function(rid) {
      rows <- lin[lin$geocode_run_id == rid, , drop = FALSE]
      if (nrow(rows) == 0L) NA_character_
      else as.character(rows$source[1L])
    }, character(1))
  } else {
    rep(NA_character_, length(unique_rids))
  }

  snap_dates <- as.Date(vapply(unique_rids, function(rid) {
    if (is.null(lin) || nrow(lin) == 0L) return(NA_character_)
    rows <- lin[lin$geocode_run_id == rid, , drop = FALSE]
    if (nrow(rows) == 0L) return(NA_character_)
    sd <- rows$snapshot_date[1L] %||% NA_character_
    as.character(sd)
  }, character(1)))

  file_shas <- if (!is.null(lin) && nrow(lin) > 0L) {
    vapply(unique_rids, function(rid) {
      rows <- lin[lin$geocode_run_id == rid, , drop = FALSE]
      if (nrow(rows) == 0L) NA_character_
      else as.character(rows$file_sha256[1L])
    }, character(1))
  } else {
    rep(NA_character_, length(unique_rids))
  }

  meta_out <- list(
    n_runs                = length(unique_rids),
    run_ids               = unique_rids,
    snapshot_dates        = snap_dates,
    snapshot_file_sha256s = file_shas,
    vendors               = vendors,
    bound_at              = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    n_rows_total          = nrow(d),
    n_rows_per_run        = n_per_run,
    schema_warn           = character(0),
    loaded_from_db        = TRUE,
    db_loaded_at          = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )

  structure(list(
    data        = d,
    meta        = meta_out,
    binding_log = binding_log
  ), class = c("alprek_geocode_panel", "list"))
}


# ============================================================================
# 4. geocode_lineage
# ============================================================================

#' Write a geocode lineage row to DuckDB
#'
#' @description Records one lineage row per write into `geocode_lineage`.
#'   The input `x` may be an `alprek_geocode_clean`,
#'   `alprek_geocode_reconciled`, `alprek_geocode_master`, or
#'   `alprek_geocode_panel`. For panels, one row per `geocode_run_id`
#'   in `panel$binding_log` is appended.
#'
#'   Lineage columns:
#'   * `geocode_run_id`            (character)
#'   * `source`                    (character; e.g., `"melissa"`)
#'   * `cycle_year`                (character)
#'   * `snapshot_date`             (character / ISO date)
#'   * `file_sha256`               (character)
#'   * `git_sha`                   (character)
#'   * `n_rows`                    (integer)
#'   * `n_followup`                (integer; counts when known)
#'   * `distance_threshold_rules`  (character)
#'   * `flat_threshold_m`          (integer)
#'   * `written_at`                (character timestamp)
#'
#' @param conn A DBI connection.
#' @param x One of `alprek_geocode_clean`, `alprek_geocode_reconciled`,
#'   `alprek_geocode_master`, or `alprek_geocode_panel`.
#' @param run_id Optional character — override the derived
#'   `geocode_run_id` (only honored when `x` is non-panel).
#'
#' @return Invisible character `"geocode_lineage"`.
#' @export
db_write_geocode_lineage <- function(conn, x, run_id = NULL) {
  .db_require_packages()
  .db_validate_conn(conn)
  .db_geocode_validate_schema(conn)

  rows <- .db_geocode_extract_lineage_rows(x, run_id_override = run_id)
  if (nrow(rows) == 0L) {
    return(invisible("geocode_lineage"))
  }

  # First write creates the table; subsequent writes append.
  .db_register_column_types(conn, "geocode_lineage", rows)
  if ("geocode_lineage" %in% DBI::dbListTables(conn)) {
    DBI::dbWriteTable(conn, "geocode_lineage",
                       .db_prepare_for_write(rows),
                       append = TRUE)
  } else {
    DBI::dbWriteTable(conn, "geocode_lineage",
                       .db_prepare_for_write(rows))
  }

  invisible("geocode_lineage")
}


#' Read the geocode lineage table
#'
#' @description Returns the full `geocode_lineage` table as a tibble.
#'
#' @param conn A DBI connection.
#'
#' @return A tibble (0 rows if the table is absent).
#' @export
db_read_geocode_lineage <- function(conn) {
  .db_require_packages()
  .db_validate_conn(conn)

  if (!"geocode_lineage" %in% DBI::dbListTables(conn)) {
    return(tibble::tibble(
      geocode_run_id           = character(0),
      source                   = character(0),
      cycle_year               = character(0),
      snapshot_date            = character(0),
      file_sha256              = character(0),
      git_sha                  = character(0),
      n_rows                   = integer(0),
      n_followup               = integer(0),
      distance_threshold_rules = character(0),
      flat_threshold_m         = integer(0),
      written_at               = character(0)
    ))
  }

  d <- DBI::dbGetQuery(conn,
                      "SELECT * FROM geocode_lineage ORDER BY written_at")
  d <- .db_reconstruct_types(d, .db_get_column_types(conn, "geocode_lineage"))
  tibble::as_tibble(d)
}


# ============================================================================
# Internal helpers
# ============================================================================

#' Validate that a DuckDB connection was initialized for ALprekDB writes.
#'
#' @keywords internal
#' @noRd
.db_geocode_validate_schema <- function(conn) {
  .db_validate_schema(conn)
  tables <- DBI::dbListTables(conn)
  if (!"_alprek_column_types" %in% tables) {
    stop("Database does not contain ALprekDB column-type registry. ",
         "Use db_init() to create or initialize the database before ",
         "writing geocode tables.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Resolve a geocode_run_id from explicit arg or meta-derived default.
#'
#' Mirrors the [geocode_transform()] derivation
#' (`<source>_v1_<YYYY-MM>`) so that a `clean -> write` path and a
#' `clean -> transform -> write` path agree on the partition.
#'
#' @keywords internal
#' @noRd
.db_geocode_resolve_run_id <- function(run_id, meta) {
  if (!is.null(run_id) && is.character(run_id) && length(run_id) == 1L &&
      nzchar(run_id)) {
    return(as.character(run_id))
  }
  # Already computed (e.g., transform / panel meta)
  if (!is.null(meta$geocode_run_id) && length(meta$geocode_run_id) == 1L &&
      nzchar(meta$geocode_run_id)) {
    return(as.character(meta$geocode_run_id))
  }
  # Derive from source + receipt_date (clean / reconciled meta)
  vendor <- as.character(meta$source %||% "unknown")
  recv <- meta$receipt_date
  if (inherits(recv, "Date")) {
    date_token <- format(recv, "%Y-%m")
  } else if (is.character(recv) && length(recv) == 1L && nzchar(recv)) {
    parsed <- suppressWarnings(as.Date(recv))
    date_token <- if (is.na(parsed)) "unknown-date" else format(parsed, "%Y-%m")
  } else {
    date_token <- "unknown-date"
  }
  sprintf("%s_v1_%s",
          if (nzchar(vendor)) vendor else "unknown",
          date_token)
}


#' Write a per-run geocode table (clean or reconciled).
#'
#' Stamps `geocode_run_id` onto the data frame if absent, then writes
#' via the shared registry/prepare pipeline.
#'
#' @keywords internal
#' @noRd
.db_geocode_write_one <- function(conn, table_name, df, run_id, overwrite) {
  if (!"geocode_run_id" %in% names(df)) {
    df$geocode_run_id <- run_id
  } else {
    # Ensure stamped run_id matches; tolerate NA but reject explicit conflict.
    existing <- as.character(df$geocode_run_id)
    bad <- !is.na(existing) & nzchar(existing) & existing != run_id
    if (any(bad)) {
      stop("data carries geocode_run_id(s) inconsistent with the ",
           "requested run_id = '", run_id, "'. First mismatch: '",
           existing[which(bad)[1L]], "'.", call. = FALSE)
    }
    df$geocode_run_id <- run_id
  }

  if (!overwrite && table_name %in% DBI::dbListTables(conn)) {
    existing_runs <- DBI::dbGetQuery(conn,
      sprintf("SELECT DISTINCT geocode_run_id FROM \"%s\"",
              table_name))$geocode_run_id
    if (run_id %in% existing_runs) {
      stop("Table '", table_name,
           "' already contains rows for geocode_run_id '",
           run_id,
           "'. Pass overwrite = TRUE or delete the run first.",
           call. = FALSE)
    }
    .db_register_column_types(conn, table_name, df)
    DBI::dbWriteTable(conn, table_name,
                      .db_prepare_for_write(df), append = TRUE)
    return(table_name)
  }

  if (overwrite && table_name %in% DBI::dbListTables(conn)) {
    DBI::dbRemoveTable(conn, table_name)
  }
  .db_register_column_types(conn, table_name, df)
  DBI::dbWriteTable(conn, table_name, .db_prepare_for_write(df))
  table_name
}


#' Extract one or more lineage rows from a geocode S3 object.
#'
#' @keywords internal
#' @noRd
.db_geocode_extract_lineage_rows <- function(x, run_id_override = NULL) {

  if (inherits(x, "alprek_geocode_panel")) {
    return(.db_geocode_lineage_from_panel(x))
  }
  if (inherits(x, "alprek_geocode_clean") ||
      inherits(x, "alprek_geocode_reconciled") ||
      inherits(x, "alprek_geocode_master")) {
    return(.db_geocode_lineage_from_singleton(x,
                                                run_id_override = run_id_override))
  }
  stop("db_write_geocode_lineage(): unsupported input class: ",
       paste(class(x), collapse = "/"), call. = FALSE)
}


#' Build a one-row lineage record from a non-panel geocode object.
#'
#' @keywords internal
#' @noRd
.db_geocode_lineage_from_singleton <- function(x, run_id_override = NULL) {
  meta <- x$meta %||% list()
  rid <- .db_geocode_resolve_run_id(run_id_override, meta)
  # n_rows / n_followup
  data_df <- x$data
  n_rows <- if (!is.null(data_df)) nrow(data_df) else NA_integer_
  n_followup <- if (!is.null(data_df) &&
                     "needs_followup_geocoding" %in% names(data_df)) {
    as.integer(sum(as.logical(data_df$needs_followup_geocoding),
                    na.rm = TRUE))
  } else if (!is.null(meta$n_needs_followup)) {
    as.integer(meta$n_needs_followup)
  } else {
    NA_integer_
  }

  rule <- as.character(meta$distance_threshold_rules %||% NA_character_)
  flat_m <- meta$flat_threshold_m
  flat_m_int <- if (is.null(flat_m) || (is.atomic(flat_m) &&
                                          all(is.na(flat_m)))) {
    NA_integer_
  } else {
    suppressWarnings(as.integer(flat_m))
  }

  snap_date <- meta$receipt_date
  snap_date_chr <- if (inherits(snap_date, "Date")) {
    format(snap_date, "%Y-%m-%d")
  } else if (is.null(snap_date)) {
    NA_character_
  } else {
    as.character(snap_date)
  }

  tibble::tibble(
    geocode_run_id           = as.character(rid),
    source                   = as.character(meta$source %||% NA_character_),
    cycle_year               = as.character(meta$cycle_year %||% NA_character_),
    snapshot_date            = snap_date_chr,
    file_sha256              = as.character(meta$file_sha256 %||% NA_character_),
    git_sha                  = as.character(meta$git_sha %||% NA_character_),
    n_rows                   = as.integer(n_rows),
    n_followup               = as.integer(n_followup),
    distance_threshold_rules = rule,
    flat_threshold_m         = flat_m_int,
    written_at               = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
}


#' Build a multi-row lineage record from a panel.
#'
#' @keywords internal
#' @noRd
.db_geocode_lineage_from_panel <- function(panel) {
  bl <- panel$binding_log
  if (is.null(bl) || nrow(bl) == 0L) {
    return(tibble::tibble(
      geocode_run_id           = character(0),
      source                   = character(0),
      cycle_year               = character(0),
      snapshot_date            = character(0),
      file_sha256              = character(0),
      git_sha                  = character(0),
      n_rows                   = integer(0),
      n_followup               = integer(0),
      distance_threshold_rules = character(0),
      flat_threshold_m         = integer(0),
      written_at               = character(0)
    ))
  }

  data_df <- panel$data
  followup_per_run <- if (!is.null(data_df) &&
                            "geocode_run_id" %in% names(data_df) &&
                            "needs_followup_geocoding" %in% names(data_df)) {
    tapply(as.logical(data_df$needs_followup_geocoding),
            as.character(data_df$geocode_run_id),
            FUN = function(v) sum(v, na.rm = TRUE),
            simplify = TRUE)
  } else NULL

  vendor_per_run <- if (!is.null(panel$meta$run_ids) &&
                          !is.null(panel$meta$vendors)) {
    stats::setNames(panel$meta$vendors, panel$meta$run_ids)
  } else NULL

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  out <- lapply(seq_len(nrow(bl)), function(i) {
    rid <- as.character(bl$geocode_run_id[i])
    sd  <- bl$snapshot_date[i]
    sd_chr <- if (inherits(sd, "Date") && !is.na(sd)) {
      format(sd, "%Y-%m-%d")
    } else if (is.na(sd)) {
      NA_character_
    } else {
      as.character(sd)
    }
    nf <- if (!is.null(followup_per_run) && rid %in% names(followup_per_run)) {
      as.integer(followup_per_run[[rid]])
    } else NA_integer_

    tibble::tibble(
      geocode_run_id           = rid,
      source                   = if (!is.null(vendor_per_run) &&
                                       rid %in% names(vendor_per_run))
                                  as.character(vendor_per_run[[rid]])
                                else NA_character_,
      cycle_year               = NA_character_,
      snapshot_date            = sd_chr,
      file_sha256              = as.character(bl$file_sha256[i]),
      git_sha                  = NA_character_,
      n_rows                   = as.integer(bl$n_rows[i]),
      n_followup               = nf,
      distance_threshold_rules = NA_character_,
      flat_threshold_m         = NA_integer_,
      written_at               = now
    )
  })
  do.call(rbind, out)
}


#' Read a single run's meta row back from geocode_lineage.
#'
#' Returns a list shaped roughly like the in-memory `meta` slot so a
#' DuckDB-loaded `alprek_geocode_clean` / `alprek_geocode_reconciled`
#' object carries forward source provenance.
#'
#' @keywords internal
#' @noRd
.db_geocode_read_lineage_meta <- function(conn, run_id) {
  base <- list(
    geocode_run_id           = as.character(run_id),
    source                   = NA_character_,
    cycle_year               = NA_character_,
    receipt_date             = NA_character_,
    file_sha256              = NA_character_,
    git_sha                  = NA_character_,
    n_rows                   = NA_integer_,
    n_needs_followup         = NA_integer_,
    distance_threshold_rules = NA_character_,
    flat_threshold_m         = NA_integer_
  )
  if (!"geocode_lineage" %in% DBI::dbListTables(conn)) return(base)
  lin <- DBI::dbGetQuery(conn,
    sprintf("SELECT * FROM geocode_lineage WHERE geocode_run_id = '%s'
             ORDER BY written_at DESC LIMIT 1", run_id))
  if (nrow(lin) == 0L) return(base)
  base$source                   <- as.character(lin$source[1L])
  base$cycle_year               <- as.character(lin$cycle_year[1L])
  base$receipt_date             <- as.character(lin$snapshot_date[1L])
  base$file_sha256              <- as.character(lin$file_sha256[1L])
  base$git_sha                  <- as.character(lin$git_sha[1L])
  base$n_rows                   <- suppressWarnings(as.integer(lin$n_rows[1L]))
  base$n_needs_followup         <- suppressWarnings(as.integer(lin$n_followup[1L]))
  base$distance_threshold_rules <- as.character(lin$distance_threshold_rules[1L])
  base$flat_threshold_m         <- suppressWarnings(
    as.integer(lin$flat_threshold_m[1L]))
  base
}


#' Extract a snapshot_date (Date) from a single lineage row.
#'
#' @keywords internal
#' @noRd
.db_geocode_extract_snapshot_date <- function(lin_row, rid) {
  if (is.null(lin_row)) return(as.Date(NA_character_))
  sd <- lin_row$snapshot_date
  if (is.null(sd) || length(sd) == 0L) return(as.Date(NA_character_))
  if (is.character(sd) && is.na(sd[1L])) return(as.Date(NA_character_))
  if (inherits(sd, "Date")) return(sd[1L])
  parsed <- suppressWarnings(as.Date(sd[1L]))
  parsed
}


# ---------------------------------------------------------------------------
# %||% fallback (parity with other geocode_* files).
# ---------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a
