#' Reconcile ADECE Applications Against Prior Classroom Panel
#'
#' @description Assigns each cycle-1 application row (renewals + new) into one
#'   of four buckets and records every match decision (exact, fuzzy automatic,
#'   no match) plus the top-3 fuzzy candidates considered. Solves Gap #1 from
#'   the 2026-05 ad-hoc cycle (automatic match decisions weren't logged).
#'
#'   Buckets:
#'   * **A** - renewal row, exact key match against `prior_classroom_panel`
#'     (organization, prior project name, county). Carries the matched
#'     `classroom_code`.
#'   * **B** - renewal row, no exact match, fuzzy (Jaro-Winkler) similarity
#'     `>= fuzzy_threshold`. Carries best-match `classroom_code`, flagged for
#'     analyst review.
#'   * **C** - new-application row whose fuzzy similarity to a prior classroom
#'     `>= fuzzy_threshold` (probably an additional classroom at an existing
#'     program). Carries best-match `classroom_code`, flagged.
#'   * **D** - no candidate `>= fuzzy_threshold`. Treated as truly new;
#'     downstream geocoding + isochrone packages take it from here.
#'
#'   No geocoding, ACS integration, or Bayesian modelling here - those live in
#'   separate packages.
#'
#' @param renewals_clean An `alprek_applications_clean` object whose
#'   `$meta$kind` is `"renewals"`.
#' @param new_apps_clean An `alprek_applications_clean` object whose
#'   `$meta$kind` is `"new_apps"`.
#' @param prior_classroom_panel Optional `alprek_classroom_panel` produced by
#'   `classroom_panel()`. Required by default. When `NULL` and
#'   `allow_degraded = TRUE`, no fuzzy work is performed and all rows receive
#'   `bucket = "unknown"` so production workflows cannot confuse missing panel
#'   data with exact reconciliation.
#' @param prior_school_year Optional character (e.g., `"2024-2025"`). The
#'   school year in `prior_classroom_panel` to match against. When `NULL`,
#'   defaults to `max(prior_classroom_panel$years)`.
#' @param fuzzy_threshold Numeric in `[0, 1]`. Similarity at or above which a
#'   fuzzy candidate is auto-accepted. Default `0.85`.
#' @param seed Integer used for deterministic tie-breaking and recorded in the
#'   audit log. Default `20260519L`.
#' @param allow_degraded Logical. If `TRUE`, allow `prior_classroom_panel = NULL`
#'   for synthetic demos and return `bucket = "unknown"`. Default `FALSE` so
#'   production workflows cannot mistake missing reconciliation for an exact
#'   match.
#'
#' @return An `alprek_applications_reconciled` S3 list with elements:
#'   * `reconciled`: tibble with one row per input row, augmented with
#'     `application_id`, `source_sheet`, `bucket`, `matched_classroom_code`,
#'     `matched_site_code`, `match_method`, `match_score`.
#'   * `reconciliation_log`: long tibble. One row per chosen decision plus up
#'     to three runner-up `fuzzy_candidate` rows per non-exact decision.
#'     Columns: `application_id, source_sheet, name_raw, name_matched,
#'     match_method, score, threshold_used, decision_source,
#'     decision_timestamp, decision_seed, candidate_classroom_code,
#'     candidate_site_code, candidate_rank, score_margin, note`.
#'   * `summary`: tibble of bucket counts.
#'   * `meta`: list (`fuzzy_threshold`, `seed`, `reconciled_at`,
#'     `prior_school_year`, `n_in_renewals`, `n_in_new_apps`, `n_a`, `n_b`,
#'     `n_c`, `n_d`, `n_unknown`, `git_sha`).
#'
#' @examples
#' \dontrun{
#' r <- applications_read_renewals(path, cycle_year = "2026-2027")
#' n <- applications_read_new(path, cycle_year = "2026-2027")
#' rc <- applications_clean(r); nc <- applications_clean(n)
#' rec <- applications_reconcile(rc, nc, prior_classroom_panel = panel)
#' rec
#' rec$summary
#' }
#'
#' @importFrom dplyr filter mutate select left_join bind_rows arrange
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data
#' @importFrom stringdist stringsim
#' @export
applications_reconcile <- function(renewals_clean,
                                    new_apps_clean,
                                    prior_classroom_panel = NULL,
                                    prior_school_year = NULL,
                                    fuzzy_threshold = 0.85,
                                    seed = 20260519L,
                                    allow_degraded = FALSE) {

  # ---- 0. Validate inputs ----
  if (!inherits(renewals_clean, "alprek_applications_clean") ||
      !identical(renewals_clean$meta$kind, "renewals")) {
    stop("renewals_clean must be an alprek_applications_clean with kind='renewals'.",
         call. = FALSE)
  }
  if (!inherits(new_apps_clean, "alprek_applications_clean") ||
      !identical(new_apps_clean$meta$kind, "new_apps")) {
    stop("new_apps_clean must be an alprek_applications_clean with kind='new_apps'.",
         call. = FALSE)
  }
  if (!is.null(prior_classroom_panel) &&
      !inherits(prior_classroom_panel, "alprek_classroom_panel")) {
    stop("prior_classroom_panel must be an alprek_classroom_panel or NULL.",
         call. = FALSE)
  }
  if (!is.numeric(fuzzy_threshold) || length(fuzzy_threshold) != 1L ||
      fuzzy_threshold < 0 || fuzzy_threshold > 1) {
    stop("fuzzy_threshold must be a single number in [0, 1].", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1L) {
    stop("seed must be a single integer.", call. = FALSE)
  }
  if (!is.logical(allow_degraded) || length(allow_degraded) != 1L) {
    stop("allow_degraded must be TRUE/FALSE.", call. = FALSE)
  }
  seed <- as.integer(seed)
  set.seed(seed)

  reconciled_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

  # ---- 1. Assign application_id ----
  ren_df <- renewals_clean$data
  new_df <- new_apps_clean$data
  ren_idx <- if ("raw_row_index" %in% names(ren_df)) ren_df$raw_row_index else seq_len(nrow(ren_df))
  new_idx <- if ("raw_row_index" %in% names(new_df)) new_df$raw_row_index else seq_len(nrow(new_df))
  ren_df$application_id <- sprintf("ren_%04d", as.integer(ren_idx))
  new_df$application_id <- sprintf("new_%04d", as.integer(new_idx))
  ren_df$source_sheet <- "renewals"
  new_df$source_sheet <- "new_apps"

  # ---- 2. Degraded mode (no panel) ----
  if (is.null(prior_classroom_panel)) {
    if (!isTRUE(allow_degraded)) {
      stop("prior_classroom_panel is required for applications_reconcile(). ",
           "Set allow_degraded = TRUE only for synthetic demos/tests.",
           call. = FALSE)
    }
    ren_df$bucket <- "unknown"
    ren_df$matched_classroom_code <- NA_character_
    ren_df$matched_site_code <- NA_character_
    ren_df$match_method <- "no_panel"
    ren_df$match_score <- NA_real_
    new_df$bucket <- "unknown"
    new_df$matched_classroom_code <- NA_character_
    new_df$matched_site_code <- NA_character_
    new_df$match_method <- "no_panel"
    new_df$match_score <- NA_real_

    reconciled_keep <- c("application_id", "source_sheet", "bucket",
                          "matched_classroom_code", "matched_site_code",
                          "match_method", "match_score")
    reconciled <- dplyr::bind_rows(
      ren_df[, c(reconciled_keep, setdiff(names(ren_df), reconciled_keep))],
      new_df[, c(reconciled_keep, setdiff(names(new_df), reconciled_keep))]
    )
    log_df <- .reconcile_log_empty()
    log_df <- dplyr::bind_rows(log_df,
      .reconcile_log_row_batch(
        application_id    = c(ren_df$application_id, new_df$application_id),
        source_sheet      = c(rep("renewals", nrow(ren_df)),
                              rep("new_apps", nrow(new_df))),
        name_raw          = c(.compose_name(ren_df$organization_name,
                                              ren_df$project_name_prior),
                              .compose_name(new_df$organization_name,
                                              new_df$project_name)),
        name_matched      = NA_character_,
        match_method      = "no_panel",
        score             = NA_real_,
        threshold_used    = NA_real_,
        decision_source   = "code",
        decision_timestamp = reconciled_at,
        decision_seed     = seed,
        note              = "prior_classroom_panel was NULL"
      )
    )

    return(.build_reconcile_result(
	      reconciled        = reconciled,
	      reconciliation_log = log_df,
	      meta = list(fuzzy_threshold   = NA_real_,
	                   seed             = seed,
	                   reconciled_at    = reconciled_at,
	                   prior_school_year = NA_character_,
	                   n_in_renewals    = nrow(ren_df),
	                   n_in_new_apps    = nrow(new_df),
		                   cycle_year       = .ap_first_nonmissing(
		                     renewals_clean$meta$cycle_year,
		                     new_apps_clean$meta$cycle_year
		                   ),
		                   file_sha256     = .ap_collapse_nonmissing(
		                     renewals_clean$meta$file_sha256,
		                     new_apps_clean$meta$file_sha256
		                   ),
		                   receipt_date    = .ap_collapse_nonmissing(
		                     renewals_clean$meta$receipt_date,
		                     new_apps_clean$meta$receipt_date
		                   ),
		                   file_basename   = .ap_collapse_nonmissing(
		                     renewals_clean$meta$file_basename,
		                     new_apps_clean$meta$file_basename
		                   ),
		                   source_sheets   = .ap_collapse_nonmissing(
		                     renewals_clean$meta$sheet,
		                     new_apps_clean$meta$sheet
		                   ),
		                   git_sha          = .ap_first_nonmissing(
		                     renewals_clean$meta$git_sha,
		                     new_apps_clean$meta$git_sha,
	                     alprek_git_sha()
	                   ))
	    ))
	  }

  # ---- 3. Build prior-panel matching pool ----
  panel_df <- prior_classroom_panel$data
  available_years <- if (!is.null(prior_classroom_panel$years))
                       prior_classroom_panel$years
                     else
                       sort(unique(panel_df$school_year))
  if (is.null(prior_school_year)) {
    prior_school_year <- available_years[length(available_years)]
  }
  if (!prior_school_year %in% available_years) {
    stop(sprintf("prior_school_year='%s' not found in panel. Available: %s",
                  prior_school_year,
                  paste(available_years, collapse = ", ")),
         call. = FALSE)
  }
  pool <- panel_df[panel_df$school_year == prior_school_year, , drop = FALSE]
  # Drop rows missing key fields
  pool <- pool[!is.na(pool$program_name) &
                !is.na(pool$classroom_name) &
                !is.na(pool$county_name), , drop = FALSE]

  pool$program_norm   <- .normalize_name(pool$program_name)
  pool$classroom_norm <- .normalize_name(pool$classroom_name)
  pool$county_norm    <- .normalize_name(pool$county_name)
  pool$exact_key      <- paste(pool$program_norm, pool$classroom_norm,
                                pool$county_norm, sep = "||")

  # ---- 4. Build candidate keys for both input sheets ----
  ren_df$org_norm      <- .normalize_name(ren_df$organization_name)
  ren_df$proj_prior_norm <- .normalize_name(ren_df$project_name_prior)
  ren_df$proj_norm     <- .normalize_name(ren_df$project_name)
  ren_df$county_norm   <- .normalize_name(ren_df$county)
  ren_df$exact_key     <- paste(ren_df$org_norm, ren_df$proj_prior_norm,
                                  ren_df$county_norm, sep = "||")
  ren_df$fuzzy_text    <- .compose_name(ren_df$organization_name,
                                          ren_df$project_name)
  ren_df$fuzzy_text_norm <- .compose_name(ren_df$org_norm, ren_df$proj_norm)

  new_df$org_norm    <- .normalize_name(new_df$organization_name)
  new_df$proj_norm   <- .normalize_name(new_df$project_name)
  new_df$county_norm <- .normalize_name(new_df$county)
  new_df$fuzzy_text  <- .compose_name(new_df$organization_name,
                                        new_df$project_name)
  new_df$fuzzy_text_norm <- .compose_name(new_df$org_norm, new_df$proj_norm)

  log_rows <- list()
  add_log <- function(...) {
    log_rows[[length(log_rows) + 1L]] <<- .reconcile_log_row_batch(...)
  }

  # ---- 5. Renewals: exact join, then fuzzy on residual ----
  ren_df$bucket               <- NA_character_
  ren_df$matched_classroom_code <- NA_character_
  ren_df$matched_site_code     <- NA_character_
  ren_df$match_method          <- NA_character_
  ren_df$match_score           <- NA_real_

  # 5a. Exact join: first-match wins (pool may have duplicates; break by lex)
  pool_keyed <- pool[order(pool$exact_key, pool$classroom_code), , drop = FALSE]
  pool_keyed <- pool_keyed[!duplicated(pool_keyed$exact_key), , drop = FALSE]
  match_idx <- match(ren_df$exact_key, pool_keyed$exact_key)
  exact_hit <- !is.na(match_idx) &
                nzchar(ren_df$exact_key) &
                ren_df$exact_key != "NA||NA||NA"
  ren_df$bucket[exact_hit]                 <- "A"
  ren_df$matched_classroom_code[exact_hit] <- pool_keyed$classroom_code[match_idx[exact_hit]]
  ren_df$matched_site_code[exact_hit]      <- pool_keyed$site_code[match_idx[exact_hit]]
  ren_df$match_method[exact_hit]           <- "exact"
  ren_df$match_score[exact_hit]            <- 1.0

  if (any(exact_hit)) {
    idx <- which(exact_hit)
    add_log(
      application_id    = ren_df$application_id[idx],
      source_sheet      = "renewals",
      name_raw          = ren_df$fuzzy_text[idx],
      name_matched      = paste(pool_keyed$program_name[match_idx[idx]],
                                  pool_keyed$classroom_name[match_idx[idx]],
                                  sep = " | "),
      match_method      = "exact",
      score             = 1.0,
      threshold_used    = fuzzy_threshold,
      decision_source   = "code",
      decision_timestamp = reconciled_at,
      decision_seed     = seed,
      note              = "exact (org+proj_prior+county)"
    )
  }

  # 5b. Fuzzy on unmatched renewals
  ren_residual_idx <- which(!exact_hit)
  if (length(ren_residual_idx) > 0L) {
    fz <- .fuzzy_match_county_blocked(
	      candidates_df = ren_df[ren_residual_idx, ],
	      pool          = pool,
	      threshold     = fuzzy_threshold,
	      seed          = seed
	    )
    ren_df$match_method[ren_residual_idx]          <- fz$decisions$match_method
    ren_df$match_score[ren_residual_idx]           <- fz$decisions$score
    ren_df$matched_classroom_code[ren_residual_idx] <- fz$decisions$matched_classroom_code
    ren_df$matched_site_code[ren_residual_idx]     <- fz$decisions$matched_site_code
    ren_df$bucket[ren_residual_idx] <- ifelse(fz$decisions$match_method == "fuzzy_auto",
                                                "B", "D")
    # Append both decisions and runners-up to log
    if (nrow(fz$log) > 0L) {
      fz$log$source_sheet <- "renewals"
      log_rows[[length(log_rows) + 1L]] <- fz$log
    }
  }

  # ---- 6. New apps: fuzzy only -> C or D ----
  new_df$bucket               <- NA_character_
  new_df$matched_classroom_code <- NA_character_
  new_df$matched_site_code     <- NA_character_
  new_df$match_method          <- NA_character_
  new_df$match_score           <- NA_real_

  if (nrow(new_df) > 0L) {
    fz <- .fuzzy_match_county_blocked(
	      candidates_df = new_df,
	      pool          = pool,
	      threshold     = fuzzy_threshold,
	      seed          = seed
	    )
    new_df$match_method          <- fz$decisions$match_method
    new_df$match_score           <- fz$decisions$score
    new_df$matched_classroom_code <- fz$decisions$matched_classroom_code
    new_df$matched_site_code     <- fz$decisions$matched_site_code
    new_df$bucket <- ifelse(fz$decisions$match_method == "fuzzy_auto", "C", "D")
    if (nrow(fz$log) > 0L) {
      fz$log$source_sheet <- "new_apps"
      log_rows[[length(log_rows) + 1L]] <- fz$log
    }
  }

  # ---- 7. Assemble result ----
  if (length(log_rows) > 0L) {
    log_df <- dplyr::bind_rows(log_rows)
    log_df <- log_df[, c("application_id", "source_sheet", "name_raw",
                           "name_matched", "match_method", "score",
	                           "threshold_used", "decision_source",
	                           "decision_timestamp", "decision_seed",
	                           "candidate_classroom_code",
	                           "candidate_site_code", "candidate_rank",
	                           "score_margin", "note")]
  } else {
    log_df <- .reconcile_log_empty()
  }

  reconciled_keep <- c("application_id", "source_sheet", "bucket",
                        "matched_classroom_code", "matched_site_code",
                        "match_method", "match_score")
	  cleanup <- c("org_norm", "proj_prior_norm", "proj_norm", "county_norm",
	                "exact_key", "fuzzy_text", "fuzzy_text_norm",
	                "program_norm", "classroom_norm")
  ren_keep <- setdiff(names(ren_df), cleanup)
  new_keep <- setdiff(names(new_df), cleanup)

  reconciled <- dplyr::bind_rows(
    ren_df[, ren_keep],
    new_df[, new_keep]
  )
  reconciled <- reconciled[, c(reconciled_keep,
                                 setdiff(names(reconciled), reconciled_keep))]

  .build_reconcile_result(
    reconciled         = reconciled,
    reconciliation_log = log_df,
    meta = list(fuzzy_threshold   = fuzzy_threshold,
                 seed             = seed,
	                 reconciled_at    = reconciled_at,
	                 prior_school_year = prior_school_year,
	                 n_in_renewals    = nrow(ren_df),
	                 n_in_new_apps    = nrow(new_df),
		                 cycle_year       = .ap_first_nonmissing(
		                   renewals_clean$meta$cycle_year,
		                   new_apps_clean$meta$cycle_year
		                 ),
		                 file_sha256     = .ap_collapse_nonmissing(
		                   renewals_clean$meta$file_sha256,
		                   new_apps_clean$meta$file_sha256
		                 ),
		                 receipt_date    = .ap_collapse_nonmissing(
		                   renewals_clean$meta$receipt_date,
		                   new_apps_clean$meta$receipt_date
		                 ),
		                 file_basename   = .ap_collapse_nonmissing(
		                   renewals_clean$meta$file_basename,
		                   new_apps_clean$meta$file_basename
		                 ),
		                 source_sheets   = .ap_collapse_nonmissing(
		                   renewals_clean$meta$sheet,
		                   new_apps_clean$meta$sheet
		                 ),
		                 git_sha          = .ap_first_nonmissing(
		                   renewals_clean$meta$git_sha,
		                   new_apps_clean$meta$git_sha,
	                   alprek_git_sha()
	                 ))
	  )
	}


#' Print method for `alprek_applications_reconciled`
#' @param x An `alprek_applications_reconciled` object.
#' @param ... Ignored.
#' @export
print.alprek_applications_reconciled <- function(x, ...) {
  m <- x$meta
  cat("<alprek_applications_reconciled>\n")
  cat("  Prior school year: ", m$prior_school_year, "\n", sep = "")
  cat("  Fuzzy threshold:   ", m$fuzzy_threshold, "\n", sep = "")
  cat("  Inputs:  renewals=", m$n_in_renewals,
      "  new_apps=", m$n_in_new_apps, "\n", sep = "")
  cat("  Buckets: A=", m$n_a, " B=", m$n_b,
      " C=", m$n_c, " D=", m$n_d,
      " unknown=", m$n_unknown, "\n", sep = "")
  cat("  Audit-log rows:    ", nrow(x$reconciliation_log), "\n", sep = "")
  cat("  Reconciled at:     ", m$reconciled_at, "\n", sep = "")
  invisible(x)
}


# ============================================================================
# Internal helpers
# ============================================================================

#' Normalize a classroom / program name for matching
#' @keywords internal
#' @noRd
.normalize_name <- function(x) {
  x <- as.character(x)
  out <- tolower(trimws(x))
  out[is.na(x) | !nzchar(out)] <- NA_character_
  # "Pre-K 1" / "Pre K 1" / "PreK 1" / "Pre-K #1" -> "prek 1"
  out <- gsub("pre[\\s\\-_]*k", "prek", out, perl = TRUE)
  out <- gsub("#+", "", out, perl = TRUE)
  out <- gsub("[[:punct:]]", " ", out, perl = TRUE)
  out <- gsub("\\s+", " ", out, perl = TRUE)
  out <- trimws(out)
  out[!nzchar(out)] <- NA_character_
  out
}


#' Compose a "Org | Project" display string for the audit log
#' @keywords internal
#' @noRd
.compose_name <- function(org, proj) {
  paste(ifelse(is.na(org), "", org),
        ifelse(is.na(proj), "", proj),
        sep = " | ")
}


#' Empty log tibble template
#' @keywords internal
#' @noRd
.reconcile_log_empty <- function() {
  tibble::tibble(
    application_id     = character(0),
    source_sheet       = character(0),
    name_raw           = character(0),
    name_matched       = character(0),
    match_method       = character(0),
    score              = numeric(0),
    threshold_used     = numeric(0),
	    decision_source    = character(0),
	    decision_timestamp = character(0),
	    decision_seed      = integer(0),
	    candidate_classroom_code = character(0),
	    candidate_site_code = character(0),
	    candidate_rank     = integer(0),
	    score_margin       = numeric(0),
	    note               = character(0)
	  )
	}


#' Build a batch of audit-log rows
#' @keywords internal
#' @noRd
.reconcile_log_row_batch <- function(application_id, source_sheet,
                                      name_raw, name_matched,
	                                      match_method, score, threshold_used,
	                                      decision_source, decision_timestamp,
	                                      decision_seed, note,
	                                      candidate_classroom_code = NA_character_,
	                                      candidate_site_code = NA_character_,
	                                      candidate_rank = NA_integer_,
	                                      score_margin = NA_real_) {
	  tibble::tibble(
    application_id     = as.character(application_id),
    source_sheet       = as.character(source_sheet),
    name_raw           = as.character(name_raw),
    name_matched       = as.character(name_matched),
    match_method       = as.character(match_method),
    score              = as.numeric(score),
    threshold_used     = as.numeric(threshold_used),
	    decision_source    = as.character(decision_source),
	    decision_timestamp = as.character(decision_timestamp),
	    decision_seed      = as.integer(decision_seed),
	    candidate_classroom_code = as.character(candidate_classroom_code),
	    candidate_site_code = as.character(candidate_site_code),
	    candidate_rank     = as.integer(candidate_rank),
	    score_margin       = as.numeric(score_margin),
	    note               = as.character(note)
	  )
	}


#' County-blocked Jaro-Winkler fuzzy match
#'
#' Returns `decisions` (one row per input candidate with chosen match) and
#' `log` (one row per chosen decision + up to 3 runner-up candidates).
#'
#' @keywords internal
#' @noRd
.fuzzy_match_county_blocked <- function(candidates_df, pool, threshold, seed) {
	  n <- nrow(candidates_df)
	  reconciled_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

	  match_method          <- character(n)
  matched_classroom_code <- character(n)
  matched_site_code     <- character(n)
  score                 <- numeric(n)
  matched_display       <- character(n)
  log_chunks <- list()

	  pool_text <- .compose_name(pool$program_name, pool$classroom_name)
	  pool_text_norm <- .compose_name(pool$program_norm, pool$classroom_norm)
  pool_county <- pool$county_norm
  unique_counties <- unique(candidates_df$county_norm)

  # Process per-county block
  for (cty in unique_counties) {
    row_idx <- which(candidates_df$county_norm %in% cty)
    if (length(row_idx) == 0L) next
    if (is.na(cty)) {
      # No county info -> no candidates -> no_match
      match_method[row_idx]           <- "no_match"
      matched_classroom_code[row_idx] <- NA_character_
      matched_site_code[row_idx]      <- NA_character_
      score[row_idx]                  <- NA_real_
      matched_display[row_idx]        <- NA_character_
      log_chunks[[length(log_chunks) + 1L]] <- .reconcile_log_row_batch(
        application_id     = candidates_df$application_id[row_idx],
        source_sheet       = "fuzzy_blocked",
        name_raw           = candidates_df$fuzzy_text[row_idx],
        name_matched       = NA_character_,
        match_method       = "no_match",
        score              = NA_real_,
        threshold_used     = threshold,
        decision_source    = "code",
        decision_timestamp = reconciled_at,
	        decision_seed      = seed,
        note               = "county is NA - no candidates"
      )
      next
    }

    pool_block_idx <- which(pool_county %in% cty)
    if (length(pool_block_idx) == 0L) {
      # No pool candidates in this county
      match_method[row_idx]           <- "no_match"
      matched_classroom_code[row_idx] <- NA_character_
      matched_site_code[row_idx]      <- NA_character_
      score[row_idx]                  <- 0
      matched_display[row_idx]        <- NA_character_
      log_chunks[[length(log_chunks) + 1L]] <- .reconcile_log_row_batch(
        application_id     = candidates_df$application_id[row_idx],
        source_sheet       = "fuzzy_blocked",
        name_raw           = candidates_df$fuzzy_text[row_idx],
        name_matched       = NA_character_,
        match_method       = "no_match",
        score              = 0,
        threshold_used     = threshold,
        decision_source    = "code",
        decision_timestamp = reconciled_at,
	        decision_seed      = seed,
        note               = sprintf("no pool candidates in county='%s'", cty)
      )
      next
    }

    # Vectorized similarity: input rows x pool rows
    pool_block_text <- pool_text[pool_block_idx]
    pool_block_cls  <- pool$classroom_code[pool_block_idx]
    pool_block_site <- pool$site_code[pool_block_idx]
    for (i in row_idx) {
	      raw_text <- candidates_df$fuzzy_text[i]
	      raw_text_norm <- candidates_df$fuzzy_text_norm[i]
	      if (is.na(raw_text_norm) || !nzchar(raw_text_norm)) {
        match_method[i]           <- "no_match"
        matched_classroom_code[i] <- NA_character_
        matched_site_code[i]      <- NA_character_
        score[i]                  <- NA_real_
        matched_display[i]        <- NA_character_
        log_chunks[[length(log_chunks) + 1L]] <- .reconcile_log_row_batch(
          application_id     = candidates_df$application_id[i],
          source_sheet       = "fuzzy_blocked",
          name_raw           = NA_character_,
          name_matched       = NA_character_,
          match_method       = "no_match",
          score              = NA_real_,
          threshold_used     = threshold,
          decision_source    = "code",
          decision_timestamp = reconciled_at,
	          decision_seed      = seed,
          note               = "input name is NA"
        )
        next
      }
	      pool_block_text <- pool_text[pool_block_idx]
	      pool_block_text_norm <- pool_text_norm[pool_block_idx]
	      sims <- stringdist::stringsim(raw_text_norm, pool_block_text_norm,
	                                      method = "jw", p = 0.1)
      ord <- order(-sims, pool_block_cls)  # tie-break: lex by classroom_code
	      best_idx <- ord[1L]
	      best_score <- sims[best_idx]
	      score_margin <- if (length(ord) > 1L) best_score - sims[ord[2L]] else NA_real_
      method_here <- if (!is.na(best_score) && best_score >= threshold)
                       "fuzzy_auto" else "no_match"
      if (method_here == "fuzzy_auto") {
        matched_classroom_code[i] <- pool_block_cls[best_idx]
        matched_site_code[i]      <- pool_block_site[best_idx]
        matched_display[i]        <- pool_block_text[best_idx]
      } else {
        matched_classroom_code[i] <- NA_character_
        matched_site_code[i]      <- NA_character_
        matched_display[i]        <- NA_character_
      }
      match_method[i] <- method_here
      score[i]        <- best_score

      # Chosen decision row
      log_chunks[[length(log_chunks) + 1L]] <- .reconcile_log_row_batch(
        application_id     = candidates_df$application_id[i],
        source_sheet       = "fuzzy_blocked",
        name_raw           = raw_text,
        name_matched       = if (method_here == "fuzzy_auto")
                                pool_block_text[best_idx] else NA_character_,
        match_method       = method_here,
        score              = best_score,
	        threshold_used     = threshold,
	        decision_source    = "code",
	        decision_timestamp = reconciled_at,
	        decision_seed      = seed,
	        candidate_classroom_code = if (method_here == "fuzzy_auto")
	                                     pool_block_cls[best_idx] else NA_character_,
	        candidate_site_code = if (method_here == "fuzzy_auto")
	                                pool_block_site[best_idx] else NA_character_,
	        candidate_rank     = if (method_here == "fuzzy_auto") 1L else NA_integer_,
	        score_margin       = score_margin,
	        note               = sprintf("jw; n_candidates=%d; county='%s'",
	                                      length(pool_block_idx), cty)
      )
	      # Up to three runner-up rows for fuzzy_auto, or top three candidates for
	      # no_match. The chosen decision row is logged separately above.
	      top_k <- min(if (method_here == "fuzzy_auto") 4L else 3L, length(ord))
	      runner_idx <- if (method_here == "fuzzy_auto" && top_k > 1L)
	                      ord[2L:top_k] else
	                      if (method_here == "no_match") ord[seq_len(top_k)] else integer(0)
	      if (length(runner_idx) > 0L) {
	        runner_rank <- if (method_here == "fuzzy_auto") {
	          seq_along(runner_idx) + 1L
	        } else {
	          seq_along(runner_idx)
	        }
	        log_chunks[[length(log_chunks) + 1L]] <- .reconcile_log_row_batch(
	          application_id     = rep(candidates_df$application_id[i],
	                                    length(runner_idx)),
          source_sheet       = "fuzzy_blocked",
          name_raw           = rep(raw_text, length(runner_idx)),
          name_matched       = pool_block_text[runner_idx],
          match_method       = rep("fuzzy_candidate", length(runner_idx)),
	          score              = sims[runner_idx],
	          threshold_used     = threshold,
	          decision_source    = "code",
	          decision_timestamp = reconciled_at,
	          decision_seed      = seed,
	          candidate_classroom_code = pool_block_cls[runner_idx],
	          candidate_site_code = pool_block_site[runner_idx],
		          candidate_rank     = runner_rank,
		          score_margin       = NA_real_,
		          note               = sprintf("runner-up rank=%d", runner_rank)
		        )
	      }
    }
  }

  decisions <- tibble::tibble(
    match_method          = match_method,
    score                 = score,
    matched_classroom_code = matched_classroom_code,
    matched_site_code     = matched_site_code,
    matched_display       = matched_display
  )
  log_df <- if (length(log_chunks) > 0L) dplyr::bind_rows(log_chunks)
            else .reconcile_log_empty()
  list(decisions = decisions, log = log_df)
}


#' Wrap reconciled + log + summary + meta into final S3 object
#' @keywords internal
#' @noRd
.build_reconcile_result <- function(reconciled, reconciliation_log, meta) {
  bucket_counts <- table(factor(reconciled$bucket, levels = c("A", "B", "C", "D", "unknown")))
  n_a <- as.integer(bucket_counts["A"]); n_a[is.na(n_a)] <- 0L
  n_b <- as.integer(bucket_counts["B"]); n_b[is.na(n_b)] <- 0L
  n_c <- as.integer(bucket_counts["C"]); n_c[is.na(n_c)] <- 0L
  n_d <- as.integer(bucket_counts["D"]); n_d[is.na(n_d)] <- 0L
  n_unknown <- as.integer(bucket_counts["unknown"]); n_unknown[is.na(n_unknown)] <- 0L
  summary_df <- tibble::tibble(
    bucket = c("A", "B", "C", "D", "unknown"),
    label  = c("Renewal, exact match",
                "Renewal, fuzzy recovered",
                "New app, fuzzy matched",
                "Truly new",
                "Not reconciled (degraded mode only)"),
    n      = c(n_a, n_b, n_c, n_d, n_unknown)
  )
  meta <- c(meta, list(n_a = n_a, n_b = n_b, n_c = n_c, n_d = n_d,
                       n_unknown = n_unknown))
  structure(list(
    reconciled         = tibble::as_tibble(reconciled),
    reconciliation_log = tibble::as_tibble(reconciliation_log),
    summary            = summary_df,
    meta               = meta
  ), class = "alprek_applications_reconciled")
}


#' First nonmissing scalar helper
#' @keywords internal
#' @noRd
.ap_first_nonmissing <- function(...) {
  vals <- list(...)
  for (val in vals) {
    if (!is.null(val) && length(val) > 0L && !is.na(val[1]) && nzchar(as.character(val[1]))) {
      return(as.character(val[1]))
    }
  }
  NA_character_
}


#' Collapse unique nonmissing scalar metadata values
#' @keywords internal
#' @noRd
.ap_collapse_nonmissing <- function(...) {
  vals <- unlist(list(...), use.names = FALSE)
  if (length(vals) == 0L) return(NA_character_)
  vals <- as.character(vals)
  vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
  vals <- unique(vals)
  if (length(vals) == 0L) NA_character_ else paste(vals, collapse = ";")
}
