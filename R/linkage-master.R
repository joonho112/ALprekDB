#' Create Master Linked Dataset
#'
#' @description Creates a fully linked master dataset at two levels:
#'   1. **Classroom-level**: classroom + budget + student aggregates + derived vars.
#'      Optionally augmented with reconciled-geocode columns (from
#'      [linkage_geocode_classroom()]) and per-cycle applications context
#'      (from [linkage_applications_classroom()]).
#'   2. **Student-level**: student + classroom + budget columns.
#'
#'   **Backward compatibility (v0.7.0).** Calling with only the three required
#'   panels produces output identical to v0.7.0: no `geocode_*` columns and
#'   no application-context columns. Passing `geocode` and/or `applications`
#'   activates the optional join branches without changing the row count of
#'   `classroom_level` or `student_level`.
#'
#'   **Geocode join (v0.8.0 critical-path).** When `geocode` is a
#'   `alprek_geocode_panel`, the classroom-level master inherits the 12
#'   prefixed `geocode_*` columns produced by [linkage_geocode_classroom()]:
#'   the 10 authoritative reconcile columns plus `geocode_run_id` and
#'   `geocode_lineage_id`. The ADECE `latitude` / `longitude` columns from
#'   the classroom panel are left untouched (Decision §11.4: escape-hatch /
#'   inspection).
#'
#'   **Applications join (v0.8.0).** When `applications` is a
#'   `alprek_applications_master`, [linkage_applications_classroom()] is
#'   invoked and its `classroom_level` rows are merged onto
#'   `classroom_level` for the applications' `cycle_year`. Rows outside
#'   that cycle year retain the master's pre-application schema with
#'   application-context columns left NA / FALSE.
#'
#'   **Order of operations.** budget -> student aggregates -> classroom-level
#'   master (with derived per-child/per-seat budget) -> optional geocode
#'   join -> optional applications join. The geocode join runs first so
#'   that bucket-D applications (no `site_code` yet) can be later wired
#'   against reconciled coordinates via the applications path.
#'
#' @param budget An `alprek_budget_panel` object.
#' @param classroom An `alprek_classroom_panel` object.
#' @param student An `alprek_student_panel` object.
#' @param geocode Optional. An `alprek_geocode_panel` object (typically
#'   from `geocode_bind_years()`). When supplied, the classroom-level
#'   master receives the 12 prefixed `geocode_*` columns described above.
#'   Default `NULL` (v0.7.0 behavior).
#' @param applications Optional. An `alprek_applications_master` object
#'   (typically from `applications_transform()`). When supplied, the
#'   classroom-level master receives per-cycle application context for
#'   the applications' `cycle_year`. Default `NULL`.
#'
#' @return An `alprek_linkage_master` S3 object (list) with elements:
#'   - `classroom_level`: tibble with 1 row per classroom-year.
#'   - `student_level`: tibble with 1 row per student-year.
#'   - `diagnostics`: list of all join diagnostics. When `geocode` is
#'     supplied, includes `geocode_coverage` (from
#'     `linkage_coverage_geocode()`) and `geocode_linkage` (the diagnostic
#'     tibble from `linkage_geocode_classroom()`). When `applications` is
#'     supplied, includes `applications_linkage` (the diagnostic tibble
#'     from `linkage_applications_classroom()`).
#'   - `meta`: list with metadata, including `geocode` and `applications`
#'     run identifiers when those branches fired.
#'
#' @examples
#' \dontrun{
#' # v0.7.0 (3-arg) usage -- unchanged
#' master <- linkage_create_master(budget_panel, classroom_panel, student_panel)
#'
#' # v0.8.0 with geocode panel
#' panel_g <- geocode_bind_years(geocode_master)
#' master  <- linkage_create_master(
#'   budget_panel, classroom_panel, student_panel,
#'   geocode = panel_g
#' )
#'
#' # Full v0.8.0 with geocode + applications
#' master <- linkage_create_master(
#'   budget_panel, classroom_panel, student_panel,
#'   geocode      = panel_g,
#'   applications = app_master
#' )
#' }
#'
#' @seealso [linkage_geocode_classroom()], [linkage_applications_classroom()].
#'
#' @importFrom dplyr left_join mutate
#' @export
linkage_create_master <- function(budget, classroom, student,
                                  geocode = NULL, applications = NULL) {
  if (!inherits(budget, "alprek_budget_panel")) {
    stop("Expected an 'alprek_budget_panel' object.", call. = FALSE)
  }
  if (!inherits(classroom, "alprek_classroom_panel")) {
    stop("Expected an 'alprek_classroom_panel' object.", call. = FALSE)
  }
  if (!inherits(student, "alprek_student_panel")) {
    stop("Expected an 'alprek_student_panel' object.", call. = FALSE)
  }
  if (!is.null(geocode) && !inherits(geocode, "alprek_geocode_panel")) {
    stop("`geocode` must be an 'alprek_geocode_panel' object ",
         "(from geocode_bind_years()) or NULL.", call. = FALSE)
  }
  if (!is.null(applications) &&
      !inherits(applications, "alprek_applications_master")) {
    stop("`applications` must be an 'alprek_applications_master' object ",
         "(from applications_transform()) or NULL.", call. = FALSE)
  }

  has_geocode <- !is.null(geocode)
  has_apps    <- !is.null(applications)
  n_total_steps <- 4L + as.integer(has_geocode) + as.integer(has_apps)

  msg_info("Creating master linked dataset")
  coverage <- .linkage_master_coverage(budget, classroom, student)

  # Step 1: Classroom + Budget
  msg_step(1L, n_total_steps, "Joining classroom + budget")
  cb <- linkage_classroom_budget(classroom, budget)

  # Step 2: Aggregate students to classroom level
  msg_step(2L, n_total_steps, "Aggregating students to classroom level")
  agg <- linkage_aggregate_students(student)

  # Step 3: Classroom-level master = cb + student aggregates + derived vars
  msg_step(3L, n_total_steps, "Building classroom-level master")
  classroom_level <- dplyr::left_join(
    cb$data, agg,
    by = c("school_year", "classroom_code")
  )

  # Derive per-child budget variables
  if ("grand_total" %in% names(classroom_level) &&
      "n_children" %in% names(classroom_level)) {
    classroom_level <- classroom_level |>
      dplyr::mutate(
        per_child_budget = ifelse(
          !is.na(.data$grand_total) & !is.na(.data$n_children) & .data$n_children > 0,
          round(.data$grand_total / .data$n_children, 2),
          NA_real_
        )
      )
  }

  if ("grand_total" %in% names(classroom_level) &&
      "seat_count" %in% names(classroom_level)) {
    classroom_level <- classroom_level |>
      dplyr::mutate(
        per_seat_budget = ifelse(
          !is.na(.data$grand_total) & !is.na(.data$seat_count) & .data$seat_count > 0,
          round(.data$grand_total / .data$seat_count, 2),
          NA_real_
        )
      )
  }

  # Step 4: Student-level master = student + classroom + budget
  msg_step(4L, n_total_steps, "Building student-level master")
  sc <- linkage_student_classroom(student, classroom)

  # Add budget columns to student-level data
  student_df <- sc$data
  budget_df <- budget$data

  # Budget-only columns (not already in student data from classroom join)
  budget_shared <- intersect(names(student_df), names(budget_df))
  budget_join_keys <- c("school_year", "classroom_code")
  budget_only_cols <- setdiff(names(budget_df), c(budget_shared))
  if (length(budget_only_cols) > 0) {
    budget_selected <- budget_df[, c(budget_join_keys, budget_only_cols), drop = FALSE]
    student_level <- dplyr::left_join(student_df, budget_selected,
                                       by = budget_join_keys)
  } else {
    student_level <- student_df
  }

  # ---- Optional Step: Geocode join onto classroom_level -----------------
  geocode_diag <- NULL
  geocode_meta <- NULL
  geocode_cov  <- .linkage_geocode_coverage_empty()
  next_step <- 5L
  n_rows_before_geocode <- nrow(classroom_level)

  if (has_geocode) {
    msg_step(next_step, n_total_steps,
             "Joining reconciled geocode onto classroom-level master")
    next_step <- next_step + 1L

    classroom_level <- .linkage_master_apply_geocode(
      classroom_level, classroom, geocode
    )

    if (nrow(classroom_level) != n_rows_before_geocode) {
      stop(sprintf(
        "Geocode join inflated/lost classroom rows (%d -> %d). ",
        n_rows_before_geocode, nrow(classroom_level)),
        "This indicates duplicate (site_code, school_year) keys in the ",
        "geocode panel or duplicate (classroom_code, school_year) keys ",
        "in classroom_level. Aborting.", call. = FALSE)
    }

    # Capture diagnostics + coverage rollup for the master object.
    gc_link <- attr(classroom_level, ".geocode_linkage_meta")
    attr(classroom_level, ".geocode_linkage_meta") <- NULL
    if (!is.null(gc_link)) {
      geocode_diag <- gc_link$diagnostics
      geocode_meta <- gc_link$meta
    }
    geocode_cov <- linkage_coverage_geocode(classroom_level)
  }

  # ---- Optional Step: Applications join onto classroom_level ------------
  applications_diag <- NULL
  applications_meta <- NULL
  n_rows_before_apps <- nrow(classroom_level)

  if (has_apps) {
    msg_step(next_step, n_total_steps,
             "Joining applications context onto classroom-level master")
    next_step <- next_step + 1L

    classroom_level <- .linkage_master_apply_applications(
      classroom_level, classroom, applications
    )

    if (nrow(classroom_level) != n_rows_before_apps) {
      stop(sprintf(
        "Applications join inflated/lost classroom rows (%d -> %d). Aborting.",
        n_rows_before_apps, nrow(classroom_level)),
        call. = FALSE)
    }

    apps_link <- attr(classroom_level, ".apps_linkage_meta")
    attr(classroom_level, ".apps_linkage_meta") <- NULL
    if (!is.null(apps_link)) {
      applications_diag <- apps_link$diagnostics
      applications_meta <- apps_link$meta
    }
  }

  # Diagnostics
  diagnostics <- list(
    classroom_budget = cb$diagnostics,
    student_classroom = sc$diagnostics,
    coverage = coverage,
    n_classroom_level = nrow(classroom_level),
    n_student_level = nrow(student_level),
    n_classroom_cols = ncol(classroom_level),
    n_student_cols = ncol(student_level),
    geocode_coverage = geocode_cov,
    geocode_linkage = geocode_diag,
    applications_linkage = applications_diag
  )

  # Metadata
  years <- sort(unique(c(cb$meta$years, sc$meta$years)))
  meta <- list(
    years = years,
    coverage = coverage,
    n_classroom_rows = nrow(classroom_level),
    n_student_rows = nrow(student_level),
    n_classroom_cols = ncol(classroom_level),
    n_student_cols = ncol(student_level),
    has_geocode = has_geocode,
    has_applications = has_apps,
    geocode = geocode_meta,
    applications = applications_meta,
    created_at = Sys.time()
  )

  result <- structure(
    list(
      classroom_level = classroom_level,
      student_level = student_level,
      diagnostics = diagnostics,
      meta = meta
    ),
    class = "alprek_linkage_master"
  )

  msg_success("Master dataset created:")
  msg_info("  Classroom-level: {nrow(classroom_level)} rows x {ncol(classroom_level)} cols")
  msg_info("  Student-level: {nrow(student_level)} rows x {ncol(student_level)} cols")
  if (length(coverage$missing_budget_years) > 0) {
    missing_years <- paste(coverage$missing_budget_years, collapse = ", ")
    msg_info("  Budget coverage unavailable for year(s): {missing_years}; budget-derived fields remain NA for those years")
  }
  if (has_geocode) {
    pct_ready <- round(geocode_cov$pct_model_ready, 1)
    n_fu      <- geocode_cov$n_needing_followup
    msg_info("  Geocode coverage: {pct_ready}% with model-ready coord; {n_fu} need followup")
  }

  result
}


#' Print method for alprek_linkage_master
#' @param x An alprek_linkage_master object.
#' @param ... Ignored.
#' @export
print.alprek_linkage_master <- function(x, ...) {
  cat("<alprek_linkage_master>\n")
  cat("  Years:", paste(x$meta$years, collapse = ", "), "\n")
  cat("  Classroom-level:", nrow(x$classroom_level), "rows x",
      ncol(x$classroom_level), "cols\n")
  cat("  Student-level:", nrow(x$student_level), "rows x",
      ncol(x$student_level), "cols\n")
  d <- x$diagnostics
  if (!is.null(d$classroom_budget)) {
    cb <- d$classroom_budget
    if (!is.null(cb$match_rate_overlap_years) && !is.na(cb$match_rate_overlap_years) &&
        length(cb$missing_budget_years) > 0) {
      cat("  Budget overlap match:", round(cb$match_rate_overlap_years * 100, 1), "%\n")
      cat("  Budget all-year match:", round(cb$match_rate * 100, 1), "%\n")
    } else {
      cat("  Budget match:", round(cb$match_rate * 100, 1), "%\n")
    }
  }
  if (!is.null(d$student_classroom)) {
    sc <- d$student_classroom
    if (!is.null(sc$match_rate_overlap_years) && !is.na(sc$match_rate_overlap_years) &&
        length(sc$missing_classroom_years) > 0) {
      cat("  Classroom overlap match:", round(sc$match_rate_overlap_years * 100, 1), "%\n")
      cat("  Classroom all-year match:", round(sc$match_rate * 100, 1), "%\n")
    } else {
      cat("  Classroom match:", round(sc$match_rate * 100, 1), "%\n")
    }
  }
  if (isTRUE(x$meta$has_geocode) && !is.null(d$geocode_coverage)) {
    gc <- d$geocode_coverage
    cat(sprintf(
      "  Geocode coverage: %s%% with model_ready coord; %d need followup\n",
      .linkage_pct_fmt(gc$pct_model_ready),
      gc$n_needing_followup %||% 0L
    ))
  }
  if (isTRUE(x$meta$has_applications) && !is.null(x$meta$applications)) {
    am <- x$meta$applications
    cyc <- am$cycle_year %||% NA_character_
    tgt <- am$target_school_year %||% NA_character_
    cat(sprintf("  Applications cycle: %s (target school_year: %s)\n",
                cyc, tgt))
  }
  invisible(x)
}


#' Summary Statistics for Linkage Data
#'
#' @description Computes summary statistics for linked data by school year.
#'
#' @param x An `alprek_linkage_master`, `alprek_linkage_classroom`, or
#'   `alprek_linkage_student` object.
#' @param by Character. Grouping variable. Default `"school_year"`.
#'
#' @return A tibble of summary statistics.
#'
#' @importFrom dplyr group_by summarise n across
#' @export
linkage_summary_stats <- function(x, by = "school_year") {
  if (inherits(x, "alprek_linkage_master")) {
    df <- x$classroom_level
  } else if (inherits(x, "alprek_linkage_classroom")) {
    df <- x$data
  } else if (inherits(x, "alprek_linkage_student")) {
    df <- x$data
  } else {
    stop("Expected an alprek_linkage object.", call. = FALSE)
  }

  .compute_linkage_stats <- function(d) {
    n_rows <- nrow(d)

    mean_grand_total <- if ("grand_total" %in% names(d)) {
      .linkage_mean_or_na(d$grand_total)
    } else NA_real_

    mean_per_child <- if ("per_child_budget" %in% names(d)) {
      .linkage_mean_or_na(d$per_child_budget)
    } else NA_real_

    mean_n_children <- if ("n_children" %in% names(d)) {
      .linkage_mean_or_na(d$n_children)
    } else NA_real_

    pct_with_budget <- if ("grand_total" %in% names(d)) {
      mean(!is.na(d$grand_total)) * 100
    } else NA_real_

    tibble::tibble(
      n = n_rows,
      mean_grand_total = round(mean_grand_total, 0),
      mean_per_child_budget = round(mean_per_child, 0),
      mean_n_children = round(mean_n_children, 1),
      pct_with_budget = round(pct_with_budget, 1)
    )
  }

  if (!is.null(by) && by %in% names(df)) {
    groups <- sort(unique(df[[by]]))
    stats_list <- lapply(groups, function(g) {
      sub_df <- df[df[[by]] == g, , drop = FALSE]
      row <- .compute_linkage_stats(sub_df)
      row[[by]] <- g
      row
    })
    stats <- dplyr::bind_rows(stats_list)
    stats <- stats[, c(by, setdiff(names(stats), by)), drop = FALSE]
  } else {
    stats <- .compute_linkage_stats(df)
  }

  stats
}


#' Mean helper that returns NA rather than NaN for all-missing groups
#' @keywords internal
.linkage_mean_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}


# ===========================================================================
# Internal helpers for the v0.8.0 geocode / applications extension
# ===========================================================================

#' Apply the geocode-classroom linkage to an in-progress classroom_level tibble.
#'
#' Calls [linkage_geocode_classroom()] on the geocode panel + the original
#' classroom panel to obtain the prefixed `geocode_*` columns, then attaches
#' the slim per-(site_code, school_year) lookup to `classroom_level` via a
#' left-join on `(classroom_code, school_year)`. We join on classroom_code
#' rather than re-running the site-level join here so that we preserve
#' classroom_level's exact row order.
#'
#' Returns the augmented tibble with an attached `.geocode_linkage_meta`
#' attribute carrying the diagnostics + meta from the underlying call.
#'
#' @keywords internal
.linkage_master_apply_geocode <- function(classroom_level,
                                          classroom_panel,
                                          geocode_panel) {
  lk <- linkage_geocode_classroom(geocode_panel, classroom_panel)
  lk_df <- lk$data

  # Slice the geocode columns by classroom_code+school_year. The classroom
  # panel may have duplicate (classroom_code, school_year) only if the input
  # panel is malformed (each classroom-year is unique by panel convention).
  # We trust that invariant; if violated the row-count check after this
  # function catches it.
  if (!all(c("classroom_code", "school_year") %in% names(lk_df))) {
    stop("internal: linkage_geocode_classroom() output missing ",
         "classroom_code or school_year join keys", call. = FALSE)
  }

  geocode_cols <- intersect(
    c(.linkage_geocode_attach_cols()),
    names(lk_df)
  )
  keep_cols <- c("classroom_code", "school_year", geocode_cols)
  geo_slim <- lk_df[, keep_cols, drop = FALSE]

  # Deduplicate by (classroom_code, school_year) defensively. Multiple
  # classrooms-at-same-site share the same site-level geocode result, so
  # we expect uniqueness here (one row per classroom-year).
  geo_slim <- geo_slim[!duplicated(paste0(
    as.character(geo_slim$classroom_code), "||",
    as.character(geo_slim$school_year)
  )), , drop = FALSE]

  out <- dplyr::left_join(
    classroom_level, geo_slim,
    by = c("classroom_code", "school_year")
  )

  attr(out, ".geocode_linkage_meta") <- list(
    diagnostics = lk$diagnostics,
    meta        = lk$meta
  )
  out
}


#' Apply the applications-classroom linkage to an in-progress classroom_level
#' tibble. Wraps [linkage_applications_classroom()] -- which itself only
#' returns rows for the target school_year -- by joining onto classroom_level
#' for that one school_year and leaving other-year rows unchanged.
#'
#' @keywords internal
.linkage_master_apply_applications <- function(classroom_level,
                                                classroom_panel,
                                                applications) {
  # Run the v0.7.0 applications linkage on the classroom panel so it can do
  # its own panel-aware target-year filtering, then attach the resulting
  # application columns back onto classroom_level for that year.
  lk <- linkage_applications_classroom(applications, classroom_panel)
  target_year <- lk$meta$target_school_year
  lk_df <- lk$classroom_level

  # Identify which columns came from the applications side. classroom_level
  # already has classroom-panel cols + master cols + (maybe) geocode cols.
  # We exclude those from the slim attach table.
  shared_cols <- intersect(names(lk_df), names(classroom_level))
  attach_cols <- setdiff(names(lk_df), names(classroom_level))
  # We MUST keep the join keys -- so re-introduce them.
  attach_cols <- c("classroom_code", "school_year", attach_cols)
  attach_cols <- intersect(attach_cols, names(lk_df))

  if (length(attach_cols) == 0L) {
    # Nothing new to attach (rare). Still preserve diagnostics.
    out <- classroom_level
  } else {
    lk_slim <- lk_df[, attach_cols, drop = FALSE]
    lk_slim <- lk_slim[!duplicated(paste0(
      as.character(lk_slim$classroom_code), "||",
      as.character(lk_slim$school_year)
    )), , drop = FALSE]

    out <- dplyr::left_join(
      classroom_level, lk_slim,
      by = c("classroom_code", "school_year")
    )
  }

  # Per linkage_applications_classroom(), applied_this_cycle defaults to
  # FALSE for unmatched classroom rows in the target year. For non-target
  # years it would be NA after the join; coalesce to FALSE so callers can
  # treat the column as a clean logical flag at master grain.
  if ("app_applied_this_cycle" %in% names(out)) {
    out$app_applied_this_cycle <- ifelse(
      is.na(out$app_applied_this_cycle), FALSE, out$app_applied_this_cycle
    )
  }
  if ("site_n_new_apps" %in% names(out)) {
    out$site_n_new_apps <- ifelse(
      is.na(out$site_n_new_apps), 0L, as.integer(out$site_n_new_apps)
    )
  }

  attr(out, ".apps_linkage_meta") <- list(
    diagnostics = lk$diagnostics,
    meta        = list(
      cycle_year         = lk$meta$cycle_year,
      target_school_year = target_year,
      attached_capacity  = lk$meta$attached_capacity,
      n_unmatched        = nrow(lk$unmatched_applications)
    )
  )
  out
}


#' Canonical list of the 12 prefixed geocode columns we attach onto the
#' classroom-level master. Mirrors `.lg_geocode_attach_cols()` in
#' R/linkage-geocode.R; redeclared here so master callers don't depend on
#' that file's internal helper signature.
#'
#' @keywords internal
.linkage_geocode_attach_cols <- function() {
  c(
    "geocode_lat_final", "geocode_lng_final",
    "geocode_lat_source", "geocode_lat_precision",
    "geocode_distance_adece_melissa_m",
    "geocode_coord_agreement_band",
    "geocode_needs_followup_geocoding",
    "geocode_followup_reason",
    "geocode_coord_model_status",
    "geocode_provenance",
    "geocode_run_id", "geocode_lineage_id"
  )
}


#' Format a percentage that may be NA / NaN for the print method.
#' @keywords internal
.linkage_pct_fmt <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x) || is.nan(x)) {
    return("NA")
  }
  formatC(x, digits = 1L, format = "f")
}


# Tiny null-coalesce. Multiple linkage files keep their own copy so this
# file remains independently sourceable in any test environment.
`%||%` <- function(a, b) if (is.null(a)) b else a
