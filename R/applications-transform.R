#' Transform Reconciled Applications Into Master Object
#'
#' @description Adds simple, data-layer derived variables to a reconciled
#'   applications object and (optionally) to a cleaned capacity object.
#'   Mirrors the `budget_transform()` / `student_transform()` API.
#'
#'   **In-scope derivations** (this function):
#'   * Applications grain: `is_renewal`, `is_new`, `is_oversubscribed_app`
#'     (renewal had over-enrollment last cycle, if known),
#'     `applied_this_cycle` (always `TRUE` per row; the column gains meaning
#'     after `applications_bind_years()` or a join to `classroom_panel`),
#'     `cycle_year` (carried from meta), `tier_prev_dollars`,
#'     `tier_prev_rank` (1-6 inferred from observed cycle-0 thresholds),
#'     `tier_prev_band` (`"high"`/`"medium"`/`"low"`/NA).
#'   * Capacity grain (only if `capacity_clean` is provided):
#'     `capacity_utilization = enrollment / capacity` (NA-safe),
#'     `waitlist_ratio = waitlist / capacity` (NA-safe),
#'     `is_oversubscribed` (waitlist > 0 OR enrollment > capacity).
#'
#'   **Out-of-scope** (downstream packages - NOT computed here):
#'   geocoded coordinates, ACS-weighted indicators, isochrone-derived
#'   features, posterior tier from Bayesian SAE.
#'
#' @param reconciled An `alprek_applications_reconciled` object.
#' @param capacity_clean Optional `alprek_applications_clean` with
#'   `meta$kind == "capacity"`. If supplied, capacity-grain derivations are
#'   added in `$capacity_data`.
#' @param tier_bands Numeric vector of breakpoints (default
#'   `c(0, 2550, 3570, 4590, 5610)`) - observed cycle-0 carry-forward dollar
#'   amounts per tier. Used to infer `tier_prev_rank`.
#'
#' @return An `alprek_applications_master` S3 list with:
#'   * `data`: applications-grain tibble (reconciled + 7 derived cols)
#'   * `capacity_data`: capacity-grain tibble or NULL
#'   * `derived_log`: tibble (variable, formula, n_non_na, n_na, note)
#'   * `meta`: list inheriting from reconciled + `transformed_at`,
#'     `tier_bands`, `has_capacity`.
#'
#' @examples
#' \dontrun{
#' rec <- applications_reconcile(ren_clean, new_clean, panel)
#' mst <- applications_transform(rec, capacity_clean = cap_clean)
#' mst
#' }
#'
#' @importFrom dplyr mutate if_else case_when coalesce
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data
#' @export
applications_transform <- function(reconciled,
                                    capacity_clean = NULL,
                                    tier_bands = c(0, 2550, 3570, 4590, 5610)) {

  if (!inherits(reconciled, "alprek_applications_reconciled")) {
    stop("reconciled must be an alprek_applications_reconciled object ",
         "(from applications_reconcile()).", call. = FALSE)
  }
  if (!is.null(capacity_clean) &&
      !(inherits(capacity_clean, "alprek_applications_clean") &&
        identical(capacity_clean$meta$kind, "capacity"))) {
    stop("capacity_clean must be an alprek_applications_clean with ",
         "kind='capacity' (or NULL).", call. = FALSE)
  }
  if (!is.numeric(tier_bands) || length(tier_bands) < 2L ||
      is.unsorted(tier_bands)) {
    stop("tier_bands must be a non-decreasing numeric vector with >=2 ",
         "elements.", call. = FALSE)
  }

  cycle_year <- reconciled$meta$cycle_year %||% NA_character_

  log_rows <- list()
  add_log <- function(variable, formula, n_non_na, n_na, note = NA_character_) {
    log_rows[[length(log_rows) + 1L]] <<- tibble::tibble(
      variable = variable, formula = formula,
      n_non_na = as.integer(n_non_na), n_na = as.integer(n_na),
      note = as.character(note)
    )
  }

  # ---- Applications-grain derivations ----
  data <- .ap_derive_application_cols(reconciled$reconciled,
                                         cycle_year = cycle_year,
                                         tier_bands = tier_bands,
                                         add_log = add_log)

  # ---- Capacity-grain derivations (optional) ----
  capacity_data <- NULL
  if (!is.null(capacity_clean)) {
    capacity_data <- .ap_derive_capacity_cols(capacity_clean$data,
                                                add_log = add_log)
  }

  derived_log <- if (length(log_rows) > 0L)
                    dplyr::bind_rows(log_rows)
                 else tibble::tibble(variable = character(0),
                                       formula = character(0),
                                       n_non_na = integer(0),
                                       n_na = integer(0),
                                       note = character(0))

  meta_out <- c(reconciled$meta, list(
    transformed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    tier_bands     = tier_bands,
    has_capacity   = !is.null(capacity_data)
  ))

  structure(list(
    data          = tibble::as_tibble(data),
    capacity_data = if (!is.null(capacity_data)) tibble::as_tibble(capacity_data) else NULL,
    derived_log   = derived_log,
    meta          = meta_out
  ), class = "alprek_applications_master")
}


#' Print method for `alprek_applications_master`
#' @param x An `alprek_applications_master` object.
#' @param ... Ignored.
#' @export
print.alprek_applications_master <- function(x, ...) {
  m <- x$meta
  cat("<alprek_applications_master>\n")
  cat("  Cycle:        ", m$cycle_year %||% "?", "\n", sep = "")
  cat("  Apps rows:    ", nrow(x$data),
      " (", ncol(x$data), " cols)\n", sep = "")
  cat("  Capacity:     ",
      if (isTRUE(m$has_capacity)) sprintf("%d rows (%d cols)",
                                            nrow(x$capacity_data),
                                            ncol(x$capacity_data))
      else "-", "\n", sep = "")
  cat("  Derived log:  ", nrow(x$derived_log), " entries\n", sep = "")
  cat("  Tier bands:   $", paste(format(m$tier_bands, big.mark = ","),
                                    collapse = " / $"), "\n", sep = "")
  cat("  Transformed:  ", m$transformed_at, "\n", sep = "")
  invisible(x)
}


# ============================================================================
# Internal helpers
# ============================================================================

#' @keywords internal
#' @noRd
.ap_derive_application_cols <- function(df, cycle_year, tier_bands, add_log) {

  has_src <- "source_sheet" %in% names(df)
  has_tier <- "tier_adjustment" %in% names(df)

  df$is_renewal <- if (has_src) df$source_sheet == "renewals" else NA
  add_log("is_renewal", "source_sheet == 'renewals'",
          sum(!is.na(df$is_renewal)), sum(is.na(df$is_renewal)),
          if (!has_src) "source_sheet missing" else NA_character_)

  df$is_new <- if (has_src) df$source_sheet == "new_apps" else NA
  add_log("is_new", "source_sheet == 'new_apps'",
          sum(!is.na(df$is_new)), sum(is.na(df$is_new)),
          if (!has_src) "source_sheet missing" else NA_character_)

  df$cycle_year_std <- cycle_year
  add_log("cycle_year_std", "from meta$cycle_year",
          sum(!is.na(df$cycle_year_std)), sum(is.na(df$cycle_year_std)),
          NA_character_)

  df$applied_this_cycle <- rep(TRUE, nrow(df))
  add_log("applied_this_cycle", "TRUE per row (panel join semantics)",
          nrow(df), 0L,
          "Differentiates rows only after bind_years or classroom_panel join")

  # tier_prev_*: dual encoding from tier_adjustment
  if (has_tier) {
    ta <- df$tier_adjustment
    df$tier_prev_dollars <- ta
    add_log("tier_prev_dollars", "carry-forward = tier_adjustment",
            sum(!is.na(ta)), sum(is.na(ta)), NA_character_)

    # Rank 1 (highest need) = highest dollars; 5/6 = lowest = $0
    df$tier_prev_rank <- vapply(ta, function(v) {
      if (is.na(v)) return(NA_integer_)
      if (v >= tier_bands[5]) return(1L)
      if (v >= tier_bands[4]) return(2L)
      if (v >= tier_bands[3]) return(3L)
      if (v >= tier_bands[2]) return(4L)
      if (v >= tier_bands[1]) return(5L)
      NA_integer_
    }, integer(1))
    add_log("tier_prev_rank", "1..5 from tier_adjustment thresholds",
            sum(!is.na(df$tier_prev_rank)), sum(is.na(df$tier_prev_rank)),
            "Rank 5 collapses tier 5+6 ($0)")

    df$tier_prev_band <- dplyr::case_when(
      df$tier_prev_rank %in% c(1L, 2L) ~ "high",
      df$tier_prev_rank == 3L          ~ "medium",
      df$tier_prev_rank %in% c(4L, 5L) ~ "low",
      TRUE                             ~ NA_character_
    )
    add_log("tier_prev_band", "high/medium/low from tier_prev_rank",
            sum(!is.na(df$tier_prev_band)), sum(is.na(df$tier_prev_band)),
            NA_character_)
  } else {
    df$tier_prev_dollars <- NA_real_
    df$tier_prev_rank    <- NA_integer_
    df$tier_prev_band    <- NA_character_
    add_log("tier_prev_dollars", "tier_adjustment absent",
            0L, nrow(df), "all NA")
  }

  df
}


#' @keywords internal
#' @noRd
.ap_derive_capacity_cols <- function(df, add_log) {

  has_enrollment <- "enrollment" %in% names(df)
  has_capacity   <- "capacity" %in% names(df)
  has_waitlist   <- "waitlist" %in% names(df)

  if (has_capacity && has_enrollment) {
    df$capacity_utilization <- dplyr::if_else(
      df$capacity > 0 & !is.na(df$enrollment),
      df$enrollment / df$capacity, NA_real_)
    add_log("capacity_utilization",
            "if_else(capacity > 0 & !is.na(enrollment), enrollment/capacity, NA)",
            sum(!is.na(df$capacity_utilization)),
            sum(is.na(df$capacity_utilization)),
            sprintf("n_zero_capacity=%d",
                     sum(df$capacity == 0, na.rm = TRUE)))
  } else {
    df$capacity_utilization <- NA_real_
    add_log("capacity_utilization",
            "missing enrollment or capacity col", 0L, nrow(df),
            "skipped - required cols absent")
  }

  if (has_capacity && has_waitlist) {
    df$waitlist_ratio <- dplyr::if_else(
      df$capacity > 0, df$waitlist / df$capacity, NA_real_)
    add_log("waitlist_ratio",
            "if_else(capacity > 0, waitlist/capacity, NA)",
            sum(!is.na(df$waitlist_ratio)),
            sum(is.na(df$waitlist_ratio)), NA_character_)
  } else {
    df$waitlist_ratio <- NA_real_
    add_log("waitlist_ratio",
            "missing capacity or waitlist col", 0L, nrow(df),
            "skipped - required cols absent")
  }

  if (has_capacity && (has_waitlist || has_enrollment)) {
    wl <- if (has_waitlist) (!is.na(df$waitlist) & df$waitlist > 0) else FALSE
    en <- if (has_enrollment) (!is.na(df$enrollment) & !is.na(df$capacity) &
                                df$enrollment > df$capacity) else FALSE
    df$is_oversubscribed <- wl | en
    add_log("is_oversubscribed",
            "waitlist > 0 OR enrollment > capacity",
            sum(df$is_oversubscribed, na.rm = TRUE),
            sum(is.na(df$is_oversubscribed)),
            sprintf("n_oversubscribed=%d",
                     sum(df$is_oversubscribed, na.rm = TRUE)))
  } else {
    df$is_oversubscribed <- NA
    add_log("is_oversubscribed",
            "missing one of (capacity, waitlist, enrollment)",
            0L, nrow(df), "skipped - required cols absent")
  }

  df
}
