#' Link Applications Master to Classroom Panel
#'
#' @description Joins an `alprek_applications_master` object to an existing
#'   `alprek_classroom_panel`. For each classroom-school-year row in the
#'   panel, attaches the per-application columns describing whether the
#'   classroom **applied this cycle**, in which **bucket**, and (where
#'   available) the **tier carry-forward** and **capacity-grain indicators**
#'   for the matching site.
#'
#'   Join logic:
#'   * **Renewals (bucket A/B)**: exact join on `(matched_classroom_code,
#'     cycle_year to school_year_target)`. Carries `tier_prev_dollars`,
#'     `tier_prev_rank`, `tier_prev_band`, `bucket`, `match_method`,
#'     `match_score`.
#'   * **New applications (bucket C/D)**: bucket C is aggregated to the
#'     matched site via `matched_site_code`, even when reconciliation also
#'     recorded the nearest `matched_classroom_code`. Bucket D has no matched
#'     site and remains in `$unmatched_applications`.
#'   * **Capacity-grain merge**: optional left-join on `site_code` to attach
#'     `capacity_utilization`, `waitlist_ratio`, `is_oversubscribed`.
#'
#'   This is the **data-layer** linkage only - it carries application context
#'   into the panel without computing geocoded/ACS/Bayesian features (those
#'   are downstream packages).
#'
#' @param applications An `alprek_applications_master` object (from
#'   `applications_transform()`).
#' @param classroom An `alprek_classroom_panel` object. Joins only happen on
#'   the school year(s) inferred from the application's `cycle_year` (e.g.,
#'   `cycle_year = "2026-2027"` maps to a target school year of
#'   `"2025-2026"` for renewal-prior linkage, but here we use the cycle's
#'   own year for `applied_this_cycle` semantics).
#' @param target_school_year Optional character. The school_year value in
#'   `classroom$data` to attach applications context to. Default: derive
#'   from `applications$meta$cycle_year` (e.g., `"2026-2027"` ->
#'   `"2026-2027"`). The "prior" classroom panel used by reconcile is one
#'   year behind; this join uses the *current* cycle's classroom row.
#' @param attach_capacity Logical. Attach `capacity_utilization` /
#'   `waitlist_ratio` / `is_oversubscribed` via `site_code`? Default `TRUE`
#'   when applications has `capacity_data`.
#'
#' @return An `alprek_applications_linkage` S3 list:
#'   * `classroom_level`: tibble - `classroom$data` rows for the
#'     `target_school_year` joined with application columns (left-join, so
#'     classrooms that didn't apply still appear with `applied_this_cycle =
#'     FALSE`)
#'   * `unmatched_applications`: tibble - bucket D rows (truly new
#'     applications with no `matched_classroom_code` and no
#'     `matched_site_code`); downstream geocoding package will resolve
#'     these
#'   * `diagnostics`: tibble - join counts (n_classroom_rows,
#'     n_applications_in, n_matched, n_only_classroom,
#'     n_applications_direct_classroom, n_applications_site_aggregated,
#'     n_only_application_unmatched)
#'   * `meta`: `linked_at`, `cycle_year`, `target_school_year`,
#'     `attached_capacity`
#'
#' @examples
#' \dontrun{
#' mst <- applications_transform(rec, cap_clean)
#' panel <- readRDS("output/classroom/classroom_panel_2021-2025.rds")
#' lk <- linkage_applications_classroom(mst, panel)
#' lk
#' lk$classroom_level
#' }
#'
#' @importFrom dplyr left_join mutate filter select coalesce if_else distinct
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data
#' @export
linkage_applications_classroom <- function(applications,
                                             classroom,
                                             target_school_year = NULL,
                                             attach_capacity = NULL) {

  if (!inherits(applications, "alprek_applications_master")) {
    stop("applications must be an alprek_applications_master object ",
         "(from applications_transform()).", call. = FALSE)
  }
  if (!inherits(classroom, "alprek_classroom_panel")) {
    stop("classroom must be an alprek_classroom_panel object.",
         call. = FALSE)
  }

  cycle_year <- applications$meta$cycle_year %||% NA_character_
  if (is.null(target_school_year)) {
    target_school_year <- cycle_year
  }
  if (is.na(target_school_year)) {
    stop("target_school_year could not be inferred. Pass it explicitly.",
         call. = FALSE)
  }

  # Attach capacity if available and not explicitly disabled
  if (is.null(attach_capacity)) {
    attach_capacity <- !is.null(applications$capacity_data) &&
                        nrow(applications$capacity_data) > 0L
  }

  # ---- Classroom panel rows for the target school year ----
  cl_df <- classroom$data
  if (!"school_year" %in% names(cl_df)) {
    stop("classroom$data lacks 'school_year' column.", call. = FALSE)
  }
  cl_year <- cl_df[cl_df$school_year == target_school_year, , drop = FALSE]
  if (nrow(cl_year) == 0L) {
    warning(sprintf("classroom_panel has 0 rows for school_year='%s'. ",
                     target_school_year),
            "classroom_level will be empty; applications still tracked.",
            call. = FALSE)
  }

  # ---- Application rows to attach ----
  app_df <- applications$data
  app_keep <- c("application_id", "raw_row_index", "lineage_id",
                 "source_sheet", "bucket",
                 "matched_classroom_code", "matched_site_code",
                 "match_method", "match_score",
                 "organization_name", "project_name", "county",
                 "is_renewal", "is_new", "applied_this_cycle",
                 "cycle_year_std",
                 "tier_prev_dollars", "tier_prev_rank", "tier_prev_band",
                 "total_funding_request", "draft_award")
  app_keep <- intersect(app_keep, names(app_df))
  app_slim <- app_df[, app_keep, drop = FALSE]

  # Split by linkage strategy. Bucket C represents a new-classroom application
  # at an existing site, so it is site-aggregated even when reconciliation kept
  # the nearest classroom candidate as audit context.
  has_class_code <- !is.na(app_slim$matched_classroom_code) &
                     nzchar(app_slim$matched_classroom_code)
  has_site_code <- !is.na(app_slim$matched_site_code) &
                    nzchar(app_slim$matched_site_code)
  is_bucket_c <- if ("bucket" %in% names(app_slim)) {
    app_slim$bucket %in% "C"
  } else {
    rep(FALSE, nrow(app_slim))
  }

  direct_idx <- has_class_code & !is_bucket_c
  site_idx <- has_site_code & is_bucket_c
  unmatched_idx <- !(direct_idx | site_idx)

  app_with_classroom <- app_slim[direct_idx, , drop = FALSE]
  app_site_only <- app_slim[site_idx, , drop = FALSE]
  app_unmatched <- app_slim[unmatched_idx, , drop = FALSE]

  n_apps_direct <- nrow(app_with_classroom)
  n_apps_site <- nrow(app_site_only)

  # ---- Build classroom_level ----
  # Drop any join collisions by renaming the application's `bucket` etc.
  app_with_classroom <- .ap_rename_for_join(app_with_classroom)
  app_site_only      <- .ap_rename_for_join(app_site_only)

  classroom_level <- cl_year

  # Join 1: classroom_code-level (one classroom row per renewal)
  if (nrow(app_with_classroom) > 0L) {
    # If multiple apps land on the same classroom_code, keep the first
    # deterministically (lex by application_id)
    app_with_classroom <- app_with_classroom[
      order(app_with_classroom$matched_classroom_code,
             app_with_classroom$app_application_id), , drop = FALSE]
    app_with_classroom <- app_with_classroom[
      !duplicated(app_with_classroom$matched_classroom_code), , drop = FALSE]

    classroom_level <- dplyr::left_join(
      classroom_level,
      app_with_classroom,
      by = c("classroom_code" = "matched_classroom_code")
    )
  }

  # Join 2: site_code-level (for new-app bucket C site_code matches)
  if (nrow(app_site_only) > 0L) {
    # Aggregate to one row per site_code (count of new-apps)
    site_agg <- .ap_aggregate_site_app(app_site_only)
    classroom_level <- dplyr::left_join(
      classroom_level,
      site_agg,
      by = c("site_code" = "matched_site_code")
    )
  }

  # Fill applied_this_cycle = FALSE for un-matched classrooms
  if ("app_applied_this_cycle" %in% names(classroom_level)) {
    classroom_level$app_applied_this_cycle <- dplyr::coalesce(
      classroom_level$app_applied_this_cycle, FALSE)
  } else {
    classroom_level$app_applied_this_cycle <- FALSE
  }
  if ("site_n_new_apps" %in% names(classroom_level)) {
    classroom_level$site_n_new_apps <- dplyr::coalesce(
      classroom_level$site_n_new_apps, 0L)
  }

  # Attach capacity-grain features at site_code
  if (isTRUE(attach_capacity) && !is.null(applications$capacity_data)) {
    cap_keep <- c("site_code", "capacity_utilization",
                   "waitlist_ratio", "is_oversubscribed")
    cap_keep <- intersect(cap_keep, names(applications$capacity_data))
    if ("site_code" %in% cap_keep && length(cap_keep) > 1L) {
      cap_slim <- applications$capacity_data[, cap_keep, drop = FALSE]
      # Dedupe by site_code
      cap_slim <- cap_slim[!duplicated(cap_slim$site_code), , drop = FALSE]
      classroom_level <- dplyr::left_join(
        classroom_level, cap_slim, by = "site_code")
    }
  }

  # ---- Diagnostics ----
  n_cl <- nrow(cl_year)
  n_apps_in <- nrow(app_df)
  n_matched <- sum(classroom_level$app_applied_this_cycle, na.rm = TRUE)
  n_only_class <- n_cl - n_matched
  n_unmatched <- nrow(app_unmatched)
  n_apps_accounted <- n_apps_direct + n_apps_site + n_unmatched

  diagnostics <- tibble::tibble(
    metric = c("n_classroom_rows", "n_applications_in",
                "n_matched_to_classroom", "n_only_classroom",
                "n_applications_direct_classroom",
                "n_applications_site_aggregated",
                "n_applications_accounted",
                "n_only_application_unmatched"),
    value  = c(n_cl, n_apps_in, n_matched, n_only_class,
               n_apps_direct, n_apps_site, n_apps_accounted, n_unmatched)
  )

  structure(list(
    classroom_level         = tibble::as_tibble(classroom_level),
    unmatched_applications  = tibble::as_tibble(app_unmatched),
    diagnostics             = diagnostics,
    meta = list(
      linked_at          = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      cycle_year         = cycle_year,
      target_school_year = target_school_year,
      attached_capacity  = isTRUE(attach_capacity)
    )
  ), class = "alprek_applications_linkage")
}


#' Print method for `alprek_applications_linkage`
#' @param x An `alprek_applications_linkage` object.
#' @param ... Ignored.
#' @export
print.alprek_applications_linkage <- function(x, ...) {
  m <- x$meta
  cat("<alprek_applications_linkage>\n")
  cat("  Cycle:               ", m$cycle_year, "\n", sep = "")
  cat("  Target school year:  ", m$target_school_year, "\n", sep = "")
  cat("  Classroom_level rows:", nrow(x$classroom_level), "\n", sep = " ")
  cat("  Unmatched apps:      ", nrow(x$unmatched_applications), "\n", sep = "")
  cat("  Capacity attached:   ", isTRUE(m$attached_capacity), "\n", sep = "")
  cat("  Diagnostics:\n")
  for (i in seq_len(nrow(x$diagnostics))) {
    cat(sprintf("    %-30s %d\n",
                  x$diagnostics$metric[i], x$diagnostics$value[i]))
  }
  cat("  Linked at:           ", m$linked_at, "\n", sep = "")
  invisible(x)
}


# ============================================================================
# Internal helpers
# ============================================================================

#' Prefix application columns with `app_` (and site agg with `site_`) to
#' avoid collisions with classroom_panel columns during join.
#' @keywords internal
#' @noRd
.ap_rename_for_join <- function(df) {
  if (nrow(df) == 0L) return(df)
  keep_as_is <- c("matched_classroom_code", "matched_site_code")
  new_names <- vapply(names(df), function(nm) {
    if (nm %in% keep_as_is) nm else paste0("app_", nm)
  }, character(1))
  names(df) <- new_names
  df
}

#' @keywords internal
#' @noRd
.ap_aggregate_site_app <- function(df) {
  site_values <- unique(df$matched_site_code)
  out <- lapply(site_values, function(site) {
    rows <- df[df$matched_site_code == site, , drop = FALSE]
    id_col <- if ("app_application_id" %in% names(rows)) {
      "app_application_id"
    } else if ("application_id" %in% names(rows)) {
      "application_id"
    } else {
      NA_character_
    }
    lineage_col <- if ("app_lineage_id" %in% names(rows)) {
      "app_lineage_id"
    } else if ("lineage_id" %in% names(rows)) {
      "lineage_id"
    } else {
      NA_character_
    }
    tibble::tibble(
      matched_site_code = site,
      site_n_new_apps = nrow(rows),
      site_application_ids = if (!is.na(id_col)) {
        paste(rows[[id_col]], collapse = ";")
      } else {
        NA_character_
      },
      site_lineage_ids = if (!is.na(lineage_col)) {
        paste(rows[[lineage_col]], collapse = ";")
      } else {
        NA_character_
      }
    )
  })
  dplyr::bind_rows(out)
}
