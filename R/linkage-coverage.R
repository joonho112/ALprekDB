# Internal helpers for linkage year-coverage metadata.

#' Extract school years from a panel object
#' @keywords internal
.linkage_panel_years <- function(panel) {
  years <- panel$years
  if (is.null(years) && !is.null(panel$data) && "school_year" %in% names(panel$data)) {
    years <- unique(panel$data$school_year)
  }
  sort(unique(as.character(years)))
}


#' Build two-panel year coverage metadata
#' @keywords internal
.linkage_year_coverage <- function(left_years, right_years,
                                   left_label = "left",
                                   right_label = "right") {
  left_years <- sort(unique(as.character(left_years)))
  right_years <- sort(unique(as.character(right_years)))

  list(
    left_label = left_label,
    right_label = right_label,
    left_years = left_years,
    right_years = right_years,
    overlap_years = intersect(left_years, right_years),
    left_only_years = setdiff(left_years, right_years),
    right_only_years = setdiff(right_years, left_years)
  )
}


#' Build three-panel year coverage metadata for linked master objects
#' @keywords internal
.linkage_master_coverage <- function(budget, classroom, student) {
  budget_years <- .linkage_panel_years(budget)
  classroom_years <- .linkage_panel_years(classroom)
  student_years <- .linkage_panel_years(student)
  analysis_years <- sort(unique(c(budget_years, classroom_years, student_years)))
  classroom_student_years <- sort(unique(c(classroom_years, student_years)))
  coverage_table <- data.frame(
    school_year = analysis_years,
    has_budget = analysis_years %in% budget_years,
    has_classroom = analysis_years %in% classroom_years,
    has_student = analysis_years %in% student_years,
    n_budget_rows = vapply(
      analysis_years,
      function(yr) .linkage_count_year_rows(budget$data, yr),
      integer(1)
    ),
    n_classroom_rows = vapply(
      analysis_years,
      function(yr) .linkage_count_year_rows(classroom$data, yr),
      integer(1)
    ),
    n_student_rows = vapply(
      analysis_years,
      function(yr) .linkage_count_year_rows(student$data, yr),
      integer(1)
    ),
    stringsAsFactors = FALSE
  )
  coverage_table$budget_status <- ifelse(
    coverage_table$has_budget,
    "available",
    "missing_budget"
  )

  list(
    analysis_years = analysis_years,
    budget_years = budget_years,
    classroom_years = classroom_years,
    student_years = student_years,
    by_year = coverage_table,
    all_modules_years = Reduce(intersect, list(budget_years, classroom_years, student_years)),
    classroom_student_years = classroom_student_years,
    missing_budget_years = setdiff(classroom_student_years, budget_years),
    missing_classroom_years = setdiff(sort(unique(c(budget_years, student_years))), classroom_years),
    missing_student_years = setdiff(sort(unique(c(budget_years, classroom_years))), student_years),
    budget_only_years = setdiff(budget_years, classroom_student_years)
  )
}


#' Count data rows in selected school years
#' @keywords internal
.linkage_count_year_rows <- function(df, years) {
  if (length(years) == 0 || !"school_year" %in% names(df)) {
    return(0L)
  }
  sum(as.character(df$school_year) %in% years)
}


#' Summarize Geocode Coverage on a Classroom-Level Master
#'
#' @description Builds a small coverage tibble that quantifies how many
#'   classroom-year rows received an authoritative reconciled coordinate
#'   from the geocode panel, how many still need follow-up, and how many
#'   are model-ready for downstream Bayesian SAE consumers.
#'
#'   This helper is called by [linkage_create_master()] when a geocode
#'   panel is supplied, and is also useful as a standalone diagnostic on
#'   any tibble that has the prefixed `geocode_*` columns introduced by
#'   [linkage_geocode_classroom()].
#'
#'   Recognized columns (all optional; the helper degrades gracefully):
#'   * `geocode_lat_final`             -- presence -> "has coord"
#'   * `geocode_needs_followup_geocoding` -- TRUE -> "needs followup"
#'   * `geocode_coord_model_status`    -- "model_ready" -> "model ready"
#'   * `geocode_lat_source`            -- factor levels rolled up
#'
#' @param classroom_level A tibble (typically `master$classroom_level`)
#'   carrying the prefixed `geocode_*` columns. May also be any data
#'   frame; missing columns are treated as fully NA.
#'
#' @return A list with named scalar metrics:
#'   * `n_classroom_total`        -- total rows in `classroom_level`
#'   * `n_classroom_with_coord`   -- rows with non-NA `geocode_lat_final`
#'   * `n_needing_followup`       -- rows with `geocode_needs_followup_geocoding == TRUE`
#'   * `n_model_ready`            -- rows with `geocode_coord_model_status == "model_ready"`
#'   * `pct_with_coord`           -- 100 * (n_with_coord / n_total)
#'   * `pct_followup`             -- 100 * (n_needing_followup / n_total)
#'   * `pct_model_ready`          -- 100 * (n_model_ready / n_total)
#'   * `by_lat_source`            -- a `tibble` with `lat_source`, `n`, `pct`
#'                                   (or NULL if column absent)
#'   * `by_coord_model_status`    -- a `tibble` with `coord_model_status`,
#'                                   `n`, `pct` (or NULL if column absent)
#'
#' @keywords internal
linkage_coverage_geocode <- function(classroom_level) {
  if (is.null(classroom_level) || !is.data.frame(classroom_level)) {
    return(.linkage_geocode_coverage_empty())
  }
  n_total <- nrow(classroom_level)
  if (n_total == 0L) {
    return(.linkage_geocode_coverage_empty())
  }

  has_coord <- if ("geocode_lat_final" %in% names(classroom_level)) {
    !is.na(classroom_level$geocode_lat_final)
  } else {
    rep(FALSE, n_total)
  }
  needs_fu <- if ("geocode_needs_followup_geocoding" %in% names(classroom_level)) {
    isTRUE_vec <- function(x) !is.na(x) & x
    isTRUE_vec(classroom_level$geocode_needs_followup_geocoding)
  } else {
    rep(FALSE, n_total)
  }
  is_model_ready <- if ("geocode_coord_model_status" %in% names(classroom_level)) {
    !is.na(classroom_level$geocode_coord_model_status) &
      as.character(classroom_level$geocode_coord_model_status) == "model_ready"
  } else {
    rep(FALSE, n_total)
  }

  n_with_coord  <- sum(has_coord,      na.rm = TRUE)
  n_followup    <- sum(needs_fu,       na.rm = TRUE)
  n_model_ready <- sum(is_model_ready, na.rm = TRUE)

  by_src <- if ("geocode_lat_source" %in% names(classroom_level)) {
    .linkage_geocode_group_pct(
      classroom_level$geocode_lat_source, n_total, "lat_source"
    )
  } else {
    NULL
  }

  by_status <- if ("geocode_coord_model_status" %in% names(classroom_level)) {
    .linkage_geocode_group_pct(
      classroom_level$geocode_coord_model_status, n_total, "coord_model_status"
    )
  } else {
    NULL
  }

  list(
    n_classroom_total      = as.integer(n_total),
    n_classroom_with_coord = as.integer(n_with_coord),
    n_needing_followup     = as.integer(n_followup),
    n_model_ready          = as.integer(n_model_ready),
    pct_with_coord         = if (n_total > 0L) 100 * n_with_coord  / n_total else NA_real_,
    pct_followup           = if (n_total > 0L) 100 * n_followup    / n_total else NA_real_,
    pct_model_ready        = if (n_total > 0L) 100 * n_model_ready / n_total else NA_real_,
    by_lat_source          = by_src,
    by_coord_model_status  = by_status
  )
}


#' Empty geocode-coverage list (used when no geocode panel was supplied)
#' @keywords internal
.linkage_geocode_coverage_empty <- function() {
  list(
    n_classroom_total      = 0L,
    n_classroom_with_coord = 0L,
    n_needing_followup     = 0L,
    n_model_ready          = 0L,
    pct_with_coord         = NA_real_,
    pct_followup           = NA_real_,
    pct_model_ready        = NA_real_,
    by_lat_source          = NULL,
    by_coord_model_status  = NULL
  )
}


#' Build a `tibble` of group counts + percentages for a categorical column.
#' @keywords internal
.linkage_geocode_group_pct <- function(x, n_total, col_name) {
  if (n_total == 0L) {
    out <- tibble::tibble(
      value = character(0),
      n     = integer(0),
      pct   = double(0)
    )
    names(out)[1L] <- col_name
    return(out)
  }
  chr <- as.character(x)
  chr[is.na(chr)] <- "<NA>"
  tbl <- table(chr, useNA = "no")
  out <- tibble::tibble(
    value = names(tbl),
    n     = as.integer(unname(tbl)),
    pct   = 100 * as.integer(unname(tbl)) / n_total
  )
  names(out)[1L] <- col_name
  out[order(-out$n), , drop = FALSE]
}
