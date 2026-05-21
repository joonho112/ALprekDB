#' Link Geocode Panel to Classroom Panel (Step 6.1)
#'
#' @description Joins an `alprek_geocode_panel` onto an `alprek_classroom_panel`
#'   so that every classroom-year row inherits the per-site authoritative
#'   coordinates produced by the geocode reconciler. Geocoding happens at
#'   the **site** grain (Melissa addresses are site-level), but the classroom
#'   panel is at classroom-year grain, so multiple classrooms at the same
#'   site share one geocode row.
#'
#'   **Join logic.** Left-join `classroom_panel$data` onto a slim view of the
#'   geocode panel keyed by `(site_code, school_year)`. Classroom rows that
#'   have no matching geocode row keep `NA` for the 12 attached columns; the
#'   per-classroom diagnostic surfaces the unmatched count.
#'
#'   **Preserved through the join:**
#'   * `coord_model_status` -- the ordered factor from `geocode_reconcile()`.
#'     Analysts MUST see model-readiness at the classroom-year row, otherwise
#'     a downstream SAE pipeline cannot honor the "do not promote
#'     provisional rows" rule.
#'   * `lineage_id` -- both the classroom-side (kept verbatim if present)
#'     and the geocode-side (attached as `geocode_lineage_id`). The two are
#'     separate strings; downstream models choose which one to anchor on.
#'   * `geocode_run_id` -- panel-stable identifier from
#'     `geocode_transform()`.
#'
#'   **No row inflation.** `nrow(out$data) == nrow(classroom_panel$data)`.
#'   If the geocode panel happens to have multiple rows per
#'   `(site_code, school_year)` (e.g., a renewal site re-geocoded in two
#'   release cycles), the join deduplicates by taking the most recent run
#'   per (site_code, school_year) using a stable `geocode_run_id` sort
#'   (lexicographic on `YYYY-MM` token) so the join stays 1:1.
#'
#'   **Renaming.** All 12 attached columns are prefixed `geocode_` (e.g.,
#'   `geocode_lat_final`, `geocode_lat_source`, `geocode_lineage_id`,
#'   `geocode_run_id`) to avoid collisions with classroom-panel columns
#'   like `latitude` or `lineage_id`. The classroom panel's own ADECE
#'   `latitude` / `longitude` columns are left untouched (Decision §11.4:
#'   escape hatch / inspection).
#'
#' @param geocode_panel An `alprek_geocode_panel` object from
#'   `geocode_bind_years()`.
#' @param classroom_panel An `alprek_classroom_panel` object.
#'
#' @return An `alprek_geocode_linkage_classroom` S3 list:
#'   * `data` -- classroom panel rows + 12 attached geocode columns
#'     (prefixed `geocode_*`). `nrow == nrow(classroom_panel$data)`.
#'   * `diagnostics` -- tibble with `metric`, `value`, `group_by`. Includes
#'     `n_classroom_total`, `n_matched`, `n_unmatched_geocode`,
#'     `n_unmatched_classroom`, and coverage broken out by `school_year`
#'     and `lat_source`.
#'   * `meta` -- list with `linked_at`, input panel meta summaries,
#'     `n_geocode_rows_in`, `n_classroom_rows_in`, `match_rate`.
#'
#' @section Behavior on missing keys:
#'   * Classroom rows with `site_code = NA` cannot join; they appear as
#'     unmatched. The diagnostic `n_unmatched_classroom` includes them.
#'   * Geocode rows with `site_code = NA` (the bucket-D `_new` cohort) are
#'     excluded from the classroom join entirely; they are surfaced
#'     through `linkage_geocode_applications()` instead.
#'   * Geocode rows whose `(site_code, school_year)` does not appear in
#'     the classroom panel show up in `n_unmatched_geocode`.
#'
#' @examples
#' \dontrun{
#' panel_g <- geocode_bind_years(geocode_master)
#' panel_c <- classroom_bind_years(c2122, c2223, c2324, c2425)
#' lk <- linkage_geocode_classroom(panel_g, panel_c)
#' lk
#' lk$diagnostics
#' }
#'
#' @seealso [linkage_geocode_applications()], [geocode_bind_years()],
#'   [classroom_bind_years()].
#'
#' @importFrom dplyr left_join arrange
#' @importFrom tibble tibble as_tibble
#' @export
linkage_geocode_classroom <- function(geocode_panel, classroom_panel) {

  if (!inherits(geocode_panel, "alprek_geocode_panel")) {
    stop("geocode_panel must be an alprek_geocode_panel object ",
         "(from geocode_bind_years()).", call. = FALSE)
  }
  if (!inherits(classroom_panel, "alprek_classroom_panel")) {
    stop("classroom_panel must be an alprek_classroom_panel object ",
         "(from classroom_bind_years()).", call. = FALSE)
  }

  cl_df <- classroom_panel$data
  gc_df <- geocode_panel$data

  if (!"school_year" %in% names(cl_df)) {
    stop("classroom_panel$data lacks `school_year` column.", call. = FALSE)
  }
  if (!"site_code" %in% names(cl_df)) {
    stop("classroom_panel$data lacks `site_code` column (required join key).",
         call. = FALSE)
  }
  if (!"school_year" %in% names(gc_df)) {
    stop("geocode_panel$data lacks `school_year` column.", call. = FALSE)
  }
  if (!"site_code" %in% names(gc_df)) {
    stop("geocode_panel$data lacks `site_code` column (required join key).",
         call. = FALSE)
  }

  # ---- Build slim, dedup'd geocode view at (site_code, school_year) -------
  geo_slim <- .lg_geocode_slim(gc_df)

  # ---- Track unmatched geocode rows (geocode (site, year) not in classroom)
  cl_keys <- paste0(as.character(cl_df$site_code), "||",
                    as.character(cl_df$school_year))
  gc_keys <- paste0(as.character(geo_slim$site_code), "||",
                    as.character(geo_slim$school_year))
  unmatched_geo_mask <- !(gc_keys %in% cl_keys) &
                        !is.na(geo_slim$site_code)

  # ---- Left-join classroom <- geocode (slim) ------------------------------
  out_df <- dplyr::left_join(
    cl_df, geo_slim,
    by = c("site_code", "school_year")
  )

  # ---- Match flag (per classroom row) -------------------------------------
  # We use geocode_lineage_id presence -> matched. Choose the most
  # universally-present geocode column so the match-rate isn't a function of
  # which classroom row happened to have NA reconciled coords.
  out_df$.geocode_matched <- !is.na(out_df$geocode_lineage_id) |
                              !is.na(out_df$geocode_run_id)

  # ---- Diagnostics --------------------------------------------------------
  n_total       <- nrow(cl_df)
  n_matched     <- sum(out_df$.geocode_matched, na.rm = TRUE)
  n_unmatched_c <- n_total - n_matched
  n_unmatched_g <- sum(unmatched_geo_mask, na.rm = TRUE)

  diag_overall <- tibble::tibble(
    metric = c("n_classroom_total",
               "n_matched",
               "n_unmatched_classroom",
               "n_unmatched_geocode"),
    value = c(as.integer(n_total),
              as.integer(n_matched),
              as.integer(n_unmatched_c),
              as.integer(n_unmatched_g)),
    group_by = NA_character_
  )

  diag_year <- .lg_coverage_by(out_df, "school_year")
  diag_src  <- .lg_coverage_by(out_df, "geocode_lat_source")

  diagnostics <- rbind(diag_overall, diag_year, diag_src)

  # Drop the helper match flag before returning
  out_df$.geocode_matched <- NULL

  meta_out <- list(
    linked_at            = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    n_geocode_rows_in    = as.integer(nrow(gc_df)),
    n_classroom_rows_in  = as.integer(n_total),
    n_matched            = as.integer(n_matched),
    n_unmatched_classroom = as.integer(n_unmatched_c),
    n_unmatched_geocode  = as.integer(n_unmatched_g),
    match_rate           = if (n_total > 0L) n_matched / n_total
                            else NA_real_,
    geocode_run_ids      = geocode_panel$meta$run_ids %||% NA_character_,
    classroom_years      = classroom_panel$years %||% NA_character_
  )

  structure(list(
    data        = tibble::as_tibble(out_df),
    diagnostics = diagnostics,
    meta        = meta_out
  ), class = "alprek_geocode_linkage_classroom")
}


#' Print method for `alprek_geocode_linkage_classroom`
#'
#' @param x An `alprek_geocode_linkage_classroom` object.
#' @param ... Ignored.
#' @export
print.alprek_geocode_linkage_classroom <- function(x, ...) {
  m <- x$meta
  cat("<alprek_geocode_linkage_classroom>\n")
  cat("  Classroom rows:       ", m$n_classroom_rows_in, "\n", sep = "")
  cat("  Geocode rows (in):    ", m$n_geocode_rows_in, "\n", sep = "")
  cat("  Matched (classroom):  ", m$n_matched, " (",
      sprintf("%.1f%%", 100 * (m$match_rate %||% 0)), ")\n", sep = "")
  cat("  Unmatched classroom:  ", m$n_unmatched_classroom, "\n", sep = "")
  cat("  Unmatched geocode:    ", m$n_unmatched_geocode, "\n", sep = "")
  if (!is.null(m$geocode_run_ids) && length(m$geocode_run_ids) > 0L) {
    cat("  Geocode runs:         ",
        paste(m$geocode_run_ids, collapse = ", "), "\n", sep = "")
  }
  if (!is.null(m$classroom_years) && length(m$classroom_years) > 0L) {
    cat("  Classroom years:      ",
        paste(m$classroom_years, collapse = ", "), "\n", sep = "")
  }
  cat("  Linked at:            ", m$linked_at, "\n", sep = "")
  cat("  Diagnostics:          ", nrow(x$diagnostics),
      " metric row(s)\n", sep = "")
  invisible(x)
}


#' Link Geocode Panel to Applications Master (Step 6.1)
#'
#' @description Joins an `alprek_geocode_panel` onto an
#'   `alprek_applications_master`. For applications that have a resolved
#'   `matched_site_code` (renewals + bucket-C new applications), joins on
#'   `(matched_site_code, school_year)`. For bucket-D "new" applications
#'   (no site_code yet), joins on `row_id` because the Melissa file's
#'   `2025-2026_new_NNNN` row_ids correspond directly to bucket-D
#'   applications.
#'
#'   **Two-phase join.** The function performs the site_code join first,
#'   then routes the remaining unmatched applications through a `row_id`
#'   join against the geocode panel. The two phases are tracked separately
#'   in the diagnostics.
#'
#'   **fuzzy fallback (optional, off by default).** `stringdist` (already in
#'   `DESCRIPTION`) is available if a future enhancement wants to match on
#'   `organization_name` / Melissa `site_name` for stragglers. The current
#'   implementation only uses exact key joins; a `fuzzy_threshold` argument
#'   reserves the API for later.
#'
#'   **Preserved through the join:**
#'   `coord_model_status`, `lineage_id` (both applications-side and
#'   geocode-side under `geocode_lineage_id`), and `geocode_run_id`.
#'
#'   **No row inflation.** `nrow(out$data) == nrow(applications$data)`.
#'   Geocode panel is deduplicated to one row per `(site_code, school_year)`
#'   and one row per `row_id` (deterministic on `geocode_run_id`).
#'
#' @param geocode_panel An `alprek_geocode_panel` object.
#' @param applications An `alprek_applications_master` object.
#' @param fuzzy_threshold Reserved for a future fuzzy-name fallback (NULL,
#'   ignored currently). When non-NULL and `stringdist` is available, the
#'   function will try to match stragglers on Melissa `site_name` vs.
#'   `organization_name`. Default `NULL`.
#'
#' @return An `alprek_geocode_linkage_applications` S3 list:
#'   * `data` -- applications data + 12 attached geocode columns
#'     (prefixed `geocode_*`). `nrow == nrow(applications$data)`.
#'   * `diagnostics` -- tibble with `metric`, `value`, `group_by`.
#'   * `meta` -- list with `linked_at`, run identifiers, match counts.
#'
#' @examples
#' \dontrun{
#' lk <- linkage_geocode_applications(panel_g, app_master)
#' lk$diagnostics
#' }
#'
#' @seealso [linkage_geocode_classroom()].
#'
#' @importFrom dplyr left_join arrange
#' @importFrom tibble tibble as_tibble
#' @export
linkage_geocode_applications <- function(geocode_panel,
                                          applications,
                                          fuzzy_threshold = NULL) {

  if (!inherits(geocode_panel, "alprek_geocode_panel")) {
    stop("geocode_panel must be an alprek_geocode_panel object ",
         "(from geocode_bind_years()).", call. = FALSE)
  }
  if (!inherits(applications, "alprek_applications_master")) {
    stop("applications must be an alprek_applications_master object ",
         "(from applications_transform()).", call. = FALSE)
  }
  if (!is.null(fuzzy_threshold)) {
    if (!is.numeric(fuzzy_threshold) || length(fuzzy_threshold) != 1L ||
        is.na(fuzzy_threshold) || fuzzy_threshold < 0 ||
        fuzzy_threshold > 1) {
      stop("fuzzy_threshold must be NULL or a numeric scalar in [0, 1].",
           call. = FALSE)
    }
  }

  app_df <- applications$data
  gc_df  <- geocode_panel$data

  if (!"row_id" %in% names(gc_df)) {
    stop("geocode_panel$data lacks `row_id` column (Phase 3 should have ",
         "assigned it).", call. = FALSE)
  }

  # ---- Build slim geocode views ------------------------------------------
  # By site_code + school_year
  geo_by_site <- .lg_geocode_slim(gc_df)
  # By row_id (deduplicate by latest run id)
  geo_by_rowid <- .lg_geocode_slim_by_rowid(gc_df)

  # ---- Identify applications join strategy --------------------------------
  has_site <- "matched_site_code" %in% names(app_df) &
                 !is.na(app_df$matched_site_code) &
                 nzchar(as.character(app_df$matched_site_code))
  # bucket D fallback: row_id-based (Melissa `_new` rows align with
  # bucket-D applications). When app$data lacks `school_year`,
  # we still try `row_id` for D rows.
  is_bucket_d <- if ("bucket" %in% names(app_df)) {
    app_df$bucket %in% "D"
  } else {
    rep(FALSE, nrow(app_df))
  }

  # Determine school_year to join on
  school_year_app <- if ("school_year_target" %in% names(app_df)) {
    as.character(app_df$school_year_target)
  } else if ("cycle_year_std" %in% names(app_df)) {
    as.character(app_df$cycle_year_std)
  } else if (!is.null(applications$meta$cycle_year)) {
    rep(as.character(applications$meta$cycle_year), nrow(app_df))
  } else {
    rep(NA_character_, nrow(app_df))
  }

  # ---- Phase 1: site_code + school_year join ------------------------------
  # Build a temp df to do the join
  app_tmp <- app_df
  app_tmp$.app_idx <- seq_len(nrow(app_df))
  app_tmp$.join_site <- ifelse(has_site,
                                as.character(app_df$matched_site_code),
                                NA_character_)
  app_tmp$.join_year <- school_year_app

  joined_phase1 <- dplyr::left_join(
    app_tmp,
    geo_by_site,
    by = c(".join_site" = "site_code",
           ".join_year" = "school_year")
  )

  matched_phase1 <- !is.na(joined_phase1$geocode_lineage_id) |
                      !is.na(joined_phase1$geocode_run_id)

  # ---- Phase 2: row_id join for bucket D (and any other unmatched) -------
  need_phase2 <- !matched_phase1 & is_bucket_d
  # Also allow rows with no site_code to attempt a row_id-based join
  # (defensive: if matched_site_code is NA but the application carries a
  # row_id matching a Melissa _new row).
  if ("row_id" %in% names(app_df)) {
    app_rowids <- as.character(app_df$row_id)
  } else {
    app_rowids <- rep(NA_character_, nrow(app_df))
  }
  need_phase2 <- need_phase2 | (!matched_phase1 & !is.na(app_rowids))

  if (any(need_phase2) && !is.null(geo_by_rowid) &&
      nrow(geo_by_rowid) > 0L) {
    # Build a slim row_id -> geocode-cols lookup, then patch into
    # joined_phase1 in the phase-2 rows.
    p2_keys <- app_rowids[need_phase2]
    p2_keys_df <- tibble::tibble(
      .app_idx = which(need_phase2),
      .p2_rowid = p2_keys
    )
    p2_join <- dplyr::left_join(
      p2_keys_df, geo_by_rowid,
      by = c(".p2_rowid" = "row_id")
    )

    # Patch the matched columns back into joined_phase1
    geo_cols <- .lg_geocode_attach_cols()
    for (col in geo_cols) {
      if (col %in% names(p2_join)) {
        joined_phase1[[col]][p2_join$.app_idx] <- p2_join[[col]]
      }
    }
  }

  matched_final <- !is.na(joined_phase1$geocode_lineage_id) |
                     !is.na(joined_phase1$geocode_run_id)

  # ---- Compute counts -----------------------------------------------------
  n_app_total <- nrow(app_df)
  n_phase1    <- sum(matched_phase1, na.rm = TRUE)
  n_matched   <- sum(matched_final, na.rm = TRUE)
  n_phase2    <- max(0L, n_matched - n_phase1)
  n_unmatched_app <- n_app_total - n_matched

  # Unmatched geocode rows: geocode (site_code, school_year) keys not used
  # in phase 1, AND row_ids not used in phase 2.
  used_site_keys <- if (any(matched_phase1)) {
    paste0(joined_phase1$.join_site[matched_phase1], "||",
           joined_phase1$.join_year[matched_phase1])
  } else {
    character(0)
  }
  used_rowids <- if (any(need_phase2 & matched_final)) {
    app_rowids[need_phase2 & matched_final]
  } else {
    character(0)
  }
  geo_site_keys <- paste0(as.character(geo_by_site$site_code), "||",
                          as.character(geo_by_site$school_year))
  geo_rowids_all <- as.character(geo_by_rowid$row_id)
  unused_site <- !(geo_site_keys %in% used_site_keys)
  unused_rowid <- !(geo_rowids_all %in% used_rowids)
  # A geocode panel row is "unmatched" when neither its (site_code,
  # school_year) key nor its row_id participated in any join. Conservative
  # count: unique row_ids whose row_id was not used as a phase-2 target
  # AND whose site key was not used either. (Approximation: counts rows;
  # geo_by_rowid has unique row_ids by construction.)
  n_unmatched_geo <- if (length(geo_rowids_all) == 0L) 0L
                     else sum(unused_rowid)

  # ---- Diagnostics --------------------------------------------------------
  diag_overall <- tibble::tibble(
    metric = c("n_applications_total",
               "n_matched",
               "n_matched_phase1_site",
               "n_matched_phase2_rowid",
               "n_unmatched_applications",
               "n_unmatched_geocode"),
    value = c(as.integer(n_app_total),
              as.integer(n_matched),
              as.integer(n_phase1),
              as.integer(n_phase2),
              as.integer(n_unmatched_app),
              as.integer(n_unmatched_geo)),
    group_by = NA_character_
  )

  # Bucket-level coverage
  diag_bucket <- if ("bucket" %in% names(app_df)) {
    .lg_coverage_by(
      cbind(joined_phase1[, "bucket", drop = FALSE],
            geocode_lineage_id = joined_phase1$geocode_lineage_id,
            geocode_run_id = joined_phase1$geocode_run_id,
            geocode_lat_source = joined_phase1$geocode_lat_source),
      "bucket"
    )
  } else {
    tibble::tibble(metric = character(0), value = integer(0),
                   group_by = character(0))
  }

  diag_src <- .lg_coverage_by(joined_phase1, "geocode_lat_source")

  diagnostics <- rbind(diag_overall, diag_bucket, diag_src)

  # ---- Strip the .* helper cols ------------------------------------------
  helper_cols <- intersect(c(".app_idx", ".join_site", ".join_year"),
                            names(joined_phase1))
  if (length(helper_cols) > 0L) {
    joined_phase1[helper_cols] <- NULL
  }

  meta_out <- list(
    linked_at              = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    n_geocode_rows_in      = as.integer(nrow(gc_df)),
    n_applications_in      = as.integer(n_app_total),
    n_matched              = as.integer(n_matched),
    n_matched_phase1_site  = as.integer(n_phase1),
    n_matched_phase2_rowid = as.integer(n_phase2),
    n_unmatched_apps       = as.integer(n_unmatched_app),
    n_unmatched_geocode    = as.integer(n_unmatched_geo),
    match_rate             = if (n_app_total > 0L) n_matched / n_app_total
                              else NA_real_,
    geocode_run_ids        = geocode_panel$meta$run_ids %||% NA_character_,
    cycle_year             = applications$meta$cycle_year %||%
                              NA_character_,
    fuzzy_threshold_used   = fuzzy_threshold
  )

  structure(list(
    data        = tibble::as_tibble(joined_phase1),
    diagnostics = diagnostics,
    meta        = meta_out
  ), class = "alprek_geocode_linkage_applications")
}


#' Print method for `alprek_geocode_linkage_applications`
#'
#' @param x An `alprek_geocode_linkage_applications` object.
#' @param ... Ignored.
#' @export
print.alprek_geocode_linkage_applications <- function(x, ...) {
  m <- x$meta
  cat("<alprek_geocode_linkage_applications>\n")
  cat("  Application rows:        ", m$n_applications_in, "\n", sep = "")
  cat("  Geocode rows (in):       ", m$n_geocode_rows_in, "\n", sep = "")
  cat("  Matched (applications):  ", m$n_matched, " (",
      sprintf("%.1f%%", 100 * (m$match_rate %||% 0)), ")\n", sep = "")
  cat("    phase 1 (site_code):   ", m$n_matched_phase1_site, "\n", sep = "")
  cat("    phase 2 (row_id):      ", m$n_matched_phase2_rowid, "\n", sep = "")
  cat("  Unmatched applications:  ", m$n_unmatched_apps, "\n", sep = "")
  cat("  Unmatched geocode:       ", m$n_unmatched_geocode, "\n", sep = "")
  if (!is.null(m$cycle_year) && !is.na(m$cycle_year)) {
    cat("  Cycle year:              ", m$cycle_year, "\n", sep = "")
  }
  if (!is.null(m$geocode_run_ids) && length(m$geocode_run_ids) > 0L) {
    cat("  Geocode runs:            ",
        paste(m$geocode_run_ids, collapse = ", "), "\n", sep = "")
  }
  cat("  Linked at:               ", m$linked_at, "\n", sep = "")
  cat("  Diagnostics:             ", nrow(x$diagnostics),
      " metric row(s)\n", sep = "")
  invisible(x)
}


# ============================================================================
# Internal helpers
# ============================================================================

#' Build a slim deduplicated geocode view keyed by (site_code, school_year).
#'
#' Drops the 29 raw Melissa columns and keeps:
#'   * The 10 authoritative reconcile columns
#'   * `geocode_run_id` (from transform)
#'   * `lineage_id` -> renamed `geocode_lineage_id` so it doesn't collide
#'     with classroom-panel `lineage_id`
#'
#' Site_code NA rows are excluded (they belong to the bucket-D
#' applications join path). When two geocode rows share
#' (site_code, school_year) -- e.g., same site re-geocoded in two release
#' cycles -- the one with the lexicographically-largest `geocode_run_id`
#' wins (most recent YYYY-MM snapshot).
#'
#' @keywords internal
#' @noRd
.lg_geocode_slim <- function(gc_df) {
  if (!"site_code" %in% names(gc_df) || !"school_year" %in% names(gc_df)) {
    stop("internal: geocode_panel data missing site_code or school_year",
         call. = FALSE)
  }

  # Exclude rows with NA site_code -> those go to the row_id path.
  has_site <- !is.na(gc_df$site_code) &
                nzchar(as.character(gc_df$site_code))
  df <- gc_df[has_site, , drop = FALSE]

  # Sort by run_id descending so the first dup-key row is the most recent run.
  if ("geocode_run_id" %in% names(df)) {
    ord <- order(as.character(df$site_code),
                 as.character(df$school_year),
                 as.character(df$geocode_run_id),
                 decreasing = c(FALSE, FALSE, TRUE),
                 method = "radix")
    df <- df[ord, , drop = FALSE]
  }

  keys <- paste0(as.character(df$site_code), "||",
                 as.character(df$school_year))
  df <- df[!duplicated(keys), , drop = FALSE]

  # Select columns we want to attach. We select using the ORIGINAL
  # (un-prefixed) names from the panel data, then rename to the
  # `geocode_*`-prefixed form below. This avoids the "intersect with
  # already-prefixed names returns nothing" trap.
  rec_cols  <- .lg_reconcile_auth_cols()
  base_cols <- c("site_code", "school_year")
  src_cols  <- c(base_cols, rec_cols, "lineage_id", "geocode_run_id")
  keep_in_df <- intersect(src_cols, names(df))
  df <- df[, keep_in_df, drop = FALSE]

  # Rename lineage_id -> geocode_lineage_id (avoid collision with
  # classroom-panel `lineage_id`).
  if ("lineage_id" %in% names(df)) {
    names(df)[names(df) == "lineage_id"] <- "geocode_lineage_id"
  }
  # Prefix the 10 reconcile auth columns with geocode_*.
  # `geocode_provenance` is already prefixed upstream, so skip it.
  for (col in intersect(rec_cols, names(df))) {
    if (col == "geocode_provenance") next
    names(df)[names(df) == col] <- paste0("geocode_", col)
  }

  tibble::as_tibble(df)
}


#' Build a slim deduplicated geocode view keyed by row_id (bucket-D path).
#'
#' @keywords internal
#' @noRd
.lg_geocode_slim_by_rowid <- function(gc_df) {
  if (!"row_id" %in% names(gc_df)) return(NULL)

  df <- gc_df

  # Sort by run_id desc so first per row_id is the most recent run.
  if ("geocode_run_id" %in% names(df)) {
    ord <- order(as.character(df$row_id),
                 as.character(df$geocode_run_id),
                 decreasing = c(FALSE, TRUE),
                 method = "radix")
    df <- df[ord, , drop = FALSE]
  }
  df <- df[!duplicated(as.character(df$row_id)), , drop = FALSE]
  df <- df[!is.na(df$row_id), , drop = FALSE]

  rec_cols  <- .lg_reconcile_auth_cols()
  src_cols  <- c("row_id", rec_cols, "lineage_id", "geocode_run_id")
  keep_in_df <- intersect(src_cols, names(df))
  df <- df[, keep_in_df, drop = FALSE]

  if ("lineage_id" %in% names(df)) {
    names(df)[names(df) == "lineage_id"] <- "geocode_lineage_id"
  }
  for (col in intersect(rec_cols, names(df))) {
    if (col == "geocode_provenance") next
    names(df)[names(df) == col] <- paste0("geocode_", col)
  }

  tibble::as_tibble(df)
}


#' Canonical list of the 10 reconcile authoritative columns.
#' Order matches the geocode_reconcile() spec section.
#'
#' @keywords internal
#' @noRd
.lg_reconcile_auth_cols <- function() {
  c("lat_final", "lng_final", "lat_source", "lat_precision",
    "distance_adece_melissa_m", "coord_agreement_band",
    "needs_followup_geocoding", "followup_reason",
    "coord_model_status", "geocode_provenance")
}


#' Canonical list of geocode columns to attach to the master row.
#' This is the 10 reconcile auth columns (prefixed `geocode_*`)
#' + `geocode_run_id` + `geocode_lineage_id`. 12 cols total, but
#' `geocode_run_id` and `geocode_lineage_id` are added separately
#' so this returns the 10 reconcile cols already prefixed.
#'
#' @keywords internal
#' @noRd
.lg_geocode_attach_cols <- function() {
  c(paste0("geocode_", .lg_reconcile_auth_cols()),
    "geocode_run_id", "geocode_lineage_id")
}


#' Build a coverage-by-group diagnostic block.
#'
#' Returns a tibble in (metric, value, group_by) shape mirroring
#' `R/linkage-coverage.R` so downstream rollups can concat blocks
#' without column-set surprises. The `group_by` column carries the
#' group key (e.g., "school_year=2024-2025", "lat_source=melissa").
#' For each group we emit two rows: `n` (group size) and `n_matched`.
#'
#' @keywords internal
#' @noRd
.lg_coverage_by <- function(df, by_col) {
  if (!by_col %in% names(df) || nrow(df) == 0L) {
    return(tibble::tibble(metric = character(0),
                          value = integer(0),
                          group_by = character(0)))
  }

  matched <- !is.na(df$geocode_lineage_id) | !is.na(df$geocode_run_id)
  if ("\\.geocode_matched" %in% names(df)) {
    matched <- df[[".geocode_matched"]]
  }
  if (".geocode_matched" %in% names(df)) {
    matched <- df[[".geocode_matched"]]
  }

  grp <- as.character(df[[by_col]])
  grp[is.na(grp)] <- "<NA>"
  groups <- sort(unique(grp))

  rows <- list()
  for (g in groups) {
    in_g <- grp == g
    n_g <- sum(in_g)
    n_m <- sum(in_g & matched, na.rm = TRUE)
    rows[[length(rows) + 1L]] <- tibble::tibble(
      metric   = "n",
      value    = as.integer(n_g),
      group_by = sprintf("%s=%s", by_col, g)
    )
    rows[[length(rows) + 1L]] <- tibble::tibble(
      metric   = "n_matched",
      value    = as.integer(n_m),
      group_by = sprintf("%s=%s", by_col, g)
    )
  }
  do.call(rbind, rows)
}


# ---------------------------------------------------------------------------
# %||% fallback (self-contained, parity with other geocode-linkage modules)
# ---------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a
