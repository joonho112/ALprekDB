#' Bind Multiple Geocode Master Snapshots Into a Panel (Step 5.2)
#'
#' @description Stacks one or more `alprek_geocode_master` objects (each one
#'   the output of a single Melissa delivery / vendor run) into a longitudinal
#'   `alprek_geocode_panel`.
#'
#'   **Important — what "panel" means here.** A single Melissa delivery is
#'   *already* a 5-year long panel (`school_year` ∈ \{2021-2022, ...,
#'   2025-2026_new\}) — that within-delivery long shape is materialized
#'   upstream of this function. `geocode_bind_years()` is for binding
#'   **multiple Melissa runs across release cycles** (e.g., a future v0.9.0
#'   delivery on top of the current v0.8.0 delivery). For v0.8.0, only one
#'   Melissa run exists, so the typical call collapses to a degenerate
#'   identity (`geocode_bind_years(master_v1)` returns a 1-run panel whose
#'   `$data` is the input's `$data`).
#'
#'   Each input master's `geocode_run_id` (built in
#'   [geocode_transform()] from `vendor_v1_YYYY-MM`) becomes the panel-row
#'   discriminator. By design, the same `row_id` may appear in multiple runs
#'   (a renewal site re-geocoded each release cycle); the unique key in the
#'   bound panel is `(row_id, geocode_run_id)`.
#'
#' @param masters A single `alprek_geocode_master` object OR a `list` of
#'   them. Mixed-class lists are rejected.
#'
#' @return An `alprek_geocode_panel` S3 list with elements:
#'   * `data` — bound rows; for single-run input this is `masters$data`
#'     verbatim (degenerate identity). For multi-run input, rows are
#'     `dplyr::bind_rows`'d; the `geocode_run_id` column distinguishes
#'     them.
#'   * `meta` — list with `n_runs`, `run_ids` (character),
#'     `snapshot_dates` (Date vector), `snapshot_file_sha256s`
#'     (character), `bound_at`, `n_rows_total`, `n_rows_per_run`
#'     (named integer), `vendors` (character).
#'   * `binding_log` — per-run tibble: `geocode_run_id`, `snapshot_date`,
#'     `file_sha256`, `n_rows`, `n_columns`.
#'
#' @section Schema compatibility:
#'   Phase 5 contract: every master is expected to carry the same 29 + 10
#'   "standard" geocode columns (29-col Melissa contract + 10 derived from
#'   reconcile + transform). If runs differ on column membership (e.g., a
#'   future delivery introduces a new RESULTCODE level or an extra Melissa
#'   field), the function emits a `WARN` row in `$binding_log` and still
#'   binds via `dplyr::bind_rows` (which tolerates missing columns by
#'   filling `NA`). It does NOT error.
#'
#' @section Key uniqueness:
#'   `(row_id, geocode_run_id)` MUST be unique in the bound panel. If a run
#'   contains internally-duplicated `row_id` values (a Phase 3 validator
#'   should have caught this upstream), the function errors with a list of
#'   the colliding keys.
#'
#' @section Phase 5 column preservation:
#'   `lineage_id` (stable row lineage from Step 3.1) and `coord_model_status`
#'   (ordered factor from Step 4.3) are preserved in the bound panel exactly
#'   as they appear in each input master. No silent promotion or dropping.
#'
#' @examples
#' \dontrun{
#' # Degenerate single-run panel (v0.8.0 typical use)
#' mst_v1 <- geocode_transform(geocode_reconcile(geocode_clean(geocode_read("..."))))
#' panel  <- geocode_bind_years(mst_v1)
#' panel
#'
#' # Future multi-run use (v0.9.0+)
#' panel2 <- geocode_bind_years(list(mst_v1, mst_v2))
#' }
#'
#' @seealso [geocode_transform()], [geocode_reconcile()].
#'
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble as_tibble
#' @export
geocode_bind_years <- function(masters) {

  # ---- 1. Coerce single input to list -------------------------------------
  if (inherits(masters, "alprek_geocode_master")) {
    master_list <- list(masters)
  } else if (is.list(masters) && !inherits(masters, "alprek_geocode_master")) {
    master_list <- masters
  } else {
    stop("`masters` must be an alprek_geocode_master or a list of them.",
         call. = FALSE)
  }

  if (length(master_list) == 0L) {
    stop("`masters` list is empty; nothing to bind.", call. = FALSE)
  }

  # ---- 2. Validate every element is alprek_geocode_master -----------------
  for (i in seq_along(master_list)) {
    if (!inherits(master_list[[i]], "alprek_geocode_master")) {
      stop("Element ", i, " of `masters` is not an alprek_geocode_master ",
           "object (got class: ",
           paste(class(master_list[[i]]), collapse = "/"), ").",
           call. = FALSE)
    }
  }

  n_runs <- length(master_list)

  # ---- 3. Pull per-run metadata -------------------------------------------
  run_ids <- vapply(master_list, function(m) {
    rid <- m$meta$geocode_run_id %||% NA_character_
    as.character(rid)
  }, character(1))

  snapshot_dates <- as.Date(vapply(master_list, function(m) {
    d <- m$meta$receipt_date
    if (is.null(d)) return(NA_character_)
    if (inherits(d, "Date")) return(format(d, "%Y-%m-%d"))
    as.character(d)
  }, character(1)))

  file_sha256s <- vapply(master_list, function(m) {
    as.character(m$meta$file_sha256 %||% NA_character_)
  }, character(1))

  vendors <- vapply(master_list, function(m) {
    as.character(m$meta$source %||% NA_character_)
  }, character(1))

  # ---- 4. Distinct run_id check -------------------------------------------
  # Same physical run pasted twice -> deterministic ERROR (avoids accidental
  # double-counting). NA run_ids are rare (transform always assigns one) but
  # we treat them as distinct sentinel values for the duplicate check.
  if (any(!is.na(run_ids)) && anyDuplicated(run_ids[!is.na(run_ids)])) {
    dups <- run_ids[!is.na(run_ids)]
    dups <- unique(dups[duplicated(dups)])
    stop("Duplicate geocode_run_id(s) supplied: ",
         paste(dups, collapse = ", "),
         ". Each Melissa run must appear at most once in a panel.",
         call. = FALSE)
  }

  # ---- 5. Per-master row-id internal uniqueness check ---------------------
  for (i in seq_along(master_list)) {
    d_i <- master_list[[i]]$data
    if ("row_id" %in% names(d_i)) {
      rid_i <- as.character(d_i$row_id)
      # NA row_ids are intentionally not deduplicated here (NA != NA)
      non_na <- rid_i[!is.na(rid_i)]
      if (anyDuplicated(non_na)) {
        d <- unique(non_na[duplicated(non_na)])
        stop("Master ", i, " (geocode_run_id = '", run_ids[i],
             "') has duplicated row_id(s) within a single run: ",
             paste(utils::head(d, 5L), collapse = ", "),
             if (length(d) > 5L) sprintf(" ... (%d total)", length(d)) else "",
             ". Resolve in Phase 3 before binding.",
             call. = FALSE)
      }
    }
  }

  # ---- 6. Schema compatibility check (WARN, not ERROR) --------------------
  binding_log_rows <- list()
  add_log <- function(rid, sd, sha, n_rows, n_cols, severity = "INFO",
                       details = NA_character_) {
    binding_log_rows[[length(binding_log_rows) + 1L]] <<- tibble::tibble(
      geocode_run_id = as.character(rid),
      snapshot_date  = as.Date(sd),
      file_sha256    = as.character(sha),
      n_rows         = as.integer(n_rows),
      n_columns      = as.integer(n_cols),
      severity       = as.character(severity),
      details        = as.character(details)
    )
  }

  col_sets <- lapply(master_list, function(m) names(m$data))
  ref_cols <- col_sets[[1L]]
  schema_warn <- character(0)
  if (n_runs >= 2L) {
    for (i in seq.int(2L, n_runs)) {
      missing_in_i <- setdiff(ref_cols, col_sets[[i]])
      extra_in_i   <- setdiff(col_sets[[i]], ref_cols)
      if (length(missing_in_i) > 0L || length(extra_in_i) > 0L) {
        details <- sprintf(
          "Schema differs from run %d: missing=[%s], extra=[%s]",
          1L,
          paste(missing_in_i, collapse = ","),
          paste(extra_in_i, collapse = ","))
        schema_warn <- c(schema_warn, sprintf("run %d", i))
        # We attach a WARN per run downstream; we still bind below.
        attr(master_list[[i]], ".schema_warn_details") <- details
      }
    }
  }

  # ---- 7. Bind rows --------------------------------------------------------
  # For a single-run input, we want byte-identical `$data` to the input
  # (degenerate identity). `dplyr::bind_rows()` may alter attributes or
  # column ordering; short-circuit instead.
  if (n_runs == 1L) {
    combined <- master_list[[1L]]$data
  } else {
    combined <- dplyr::bind_rows(lapply(master_list, function(m) m$data))
  }

  combined <- tibble::as_tibble(combined)

  # ---- 8. (row_id, geocode_run_id) uniqueness across the panel -----------
  if (all(c("row_id", "geocode_run_id") %in% names(combined))) {
    key_str <- paste0(as.character(combined$row_id), "||",
                      as.character(combined$geocode_run_id))
    if (anyDuplicated(key_str)) {
      dups <- unique(key_str[duplicated(key_str)])
      stop("(row_id, geocode_run_id) is non-unique in bound panel; ",
           length(dups), " duplicated key(s). First few: ",
           paste(utils::head(dups, 3L), collapse = "; "),
           ". This indicates either a within-run row_id collision or two ",
           "runs sharing geocode_run_id.",
           call. = FALSE)
    }
  }

  # ---- 9. Build binding log -----------------------------------------------
  for (i in seq_along(master_list)) {
    m_i <- master_list[[i]]
    swd <- attr(m_i, ".schema_warn_details")
    if (!is.null(swd) && nzchar(swd)) {
      add_log(run_ids[i], snapshot_dates[i], file_sha256s[i],
              nrow(m_i$data), ncol(m_i$data), severity = "WARN",
              details = swd)
    } else {
      add_log(run_ids[i], snapshot_dates[i], file_sha256s[i],
              nrow(m_i$data), ncol(m_i$data), severity = "INFO",
              details = NA_character_)
    }
  }
  binding_log <- if (length(binding_log_rows) > 0L) {
    do.call(rbind, binding_log_rows)
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

  # ---- 10. Per-run row counts --------------------------------------------
  n_rows_per_run <- vapply(master_list, function(m) nrow(m$data), integer(1))
  names(n_rows_per_run) <- run_ids

  # ---- 11. Assemble panel ------------------------------------------------
  meta_out <- list(
    n_runs                = as.integer(n_runs),
    run_ids               = as.character(run_ids),
    snapshot_dates        = snapshot_dates,
    snapshot_file_sha256s = as.character(file_sha256s),
    vendors               = as.character(vendors),
    bound_at              = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    n_rows_total          = nrow(combined),
    n_rows_per_run        = n_rows_per_run,
    schema_warn           = if (length(schema_warn) > 0L) schema_warn
                            else character(0)
  )

  structure(
    list(
      data        = combined,
      meta        = meta_out,
      binding_log = binding_log
    ),
    class = c("alprek_geocode_panel", "list")
  )
}


#' Print method for `alprek_geocode_panel`
#'
#' @param x An `alprek_geocode_panel` object.
#' @param ... Ignored.
#' @export
print.alprek_geocode_panel <- function(x, ...) {
  m <- x$meta
  cat("<alprek_geocode_panel>\n")
  cat("  n_runs:        ", m$n_runs, "\n", sep = "")
  cat("  run_ids:       ",
      paste(m$run_ids, collapse = ", "), "\n", sep = "")
  if (length(m$snapshot_dates) > 0L) {
    dates_fmt <- vapply(m$snapshot_dates, function(d) {
      if (is.na(d)) "NA" else format(d, "%Y-%m-%d")
    }, character(1))
    cat("  snapshot_dates:", paste(dates_fmt, collapse = ", "), "\n", sep = " ")
  }
  cat("  n_rows_total:  ", m$n_rows_total, "\n", sep = "")
  if (length(m$n_rows_per_run) > 0L) {
    rpr <- m$n_rows_per_run
    rpr_str <- paste(sprintf("%s=%d", names(rpr), as.integer(rpr)),
                      collapse = ", ")
    cat("  rows per run:  ", rpr_str, "\n", sep = "")
  }
  if (length(m$schema_warn) > 0L) {
    cat("  schema_warn:   ", paste(m$schema_warn, collapse = ", "), "\n",
        sep = "")
  }
  cat("  bound_at:      ", m$bound_at, "\n", sep = "")
  invisible(x)
}


# ---------------------------------------------------------------------------
# %||% fallback (self-contained, parity with R/geocode-transform.R).
# ---------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a
