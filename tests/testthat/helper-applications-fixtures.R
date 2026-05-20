# tests/testthat/helper-applications-fixtures.R
#
# Synthetic fixture generators for applications edge-case testing.
# Each fixture deliberately injects ONE edge case into a baseline synthetic
# dataset (alprek_synthetic_applications()), so that tests can assert
# `applications_validate()` trips the expected check at the expected severity.
#
# Used by test-applications-validate.R (Step 4.3) and any future audit suite.

# ---------------------------------------------------------------------------
# Build a baseline alprek_applications_clean S3 wrapper around a tibble
# (synthetic data already conforms to standardized cycle-1 schema)
# ---------------------------------------------------------------------------
.fixture_wrap_clean <- function(df, kind) {
  df <- tibble::as_tibble(df)
  if (!"raw_row_index" %in% names(df)) {
    df$raw_row_index <- seq_len(nrow(df))
  }
  if (!"lineage_id" %in% names(df)) {
    df$lineage_id <- sprintf("fixture-%s-%04d", kind, df$raw_row_index)
  }
  structure(list(
    data         = df,
    cleaning_log = tibble::tibble(),
    meta = list(
      kind         = kind,
      cycle_year   = "2026-2027",
      cycle        = "cycle1",
      n_rows_in    = nrow(df),
      n_rows_out   = nrow(df),
	      n_rows_dropped = 0L,
	      file_sha256  = "fixture-deadbeef",
	      git_sha      = "fixture-git-sha",
	      receipt_date = "2026-04-20",
      data_source  = sprintf("ADECE-%s-sheet", switch(kind,
        renewals     = "renewals",
        new_apps     = "new",
        non_renewals = "nonrenewals",
        capacity     = "capacity",
        kind)),
      cleaned_at   = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    )
  ), class = "alprek_applications_clean")
}


# ---------------------------------------------------------------------------
# Add data_source column if missing (synthetic generator omits it)
# ---------------------------------------------------------------------------
.fixture_ensure_data_source <- function(df, kind) {
  if (!"data_source" %in% names(df)) {
    df$data_source <- sprintf("ADECE-%s-sheet", switch(kind,
      renewals     = "renewals",
      new_apps     = "new",
      non_renewals = "nonrenewals",
      capacity     = "capacity",
      kind))
  }
  df
}


#' Build a synthetic fixture demonstrating a specific edge case.
#'
#' @param case_id Character. One of "E01".."E17".
#' @param n_rows Integer. Baseline number of rows (case-injection adds noise on top).
#' @param seed Integer. RNG seed.
#' @return A list with named slots:
#'   * `case_id`, `kind` (which kind the fixture targets), `clean`
#'     (alprek_applications_clean S3 ready for applications_validate),
#'     `bad_row_index` (which rows demonstrate the case),
#'     `expected_check` (validate check name expected to trip),
#'     `expected_severity` ("ERROR"/"WARN"/"INFO").
#' @keywords internal
#' @noRd
make_edge_case_fixture <- function(case_id, n_rows = 12L, seed = 42L) {
  case_id <- toupper(case_id)
  set.seed(seed)
  base <- alprek_synthetic_applications(n_renewals = n_rows,
                                           n_new = max(4L, n_rows %/% 3L),
                                           n_capacity_sites = n_rows,
                                           seed = seed)

  switch(case_id,
    "E01" = {
      df <- .fixture_ensure_data_source(base$renewals, "renewals")
      # Inject two rows with same county + identical project_name but distinct orgs
      df$county[1:2]            <- "Madison"
      df$project_name[1:2]      <- "Shared Pre-K Classroom 1"
      df$organization_name[1]   <- "Madison School District"
      df$organization_name[2]   <- "Madison Private Academy"
      df$project_name_prior[1:2] <- df$project_name[1:2]
      list(case_id = case_id, kind = "renewals",
            clean = .fixture_wrap_clean(df, "renewals"),
            bad_row_index = 1:2,
            expected_check = "fuzzy_audit (downstream)", expected_severity = "WARN")
    },
    "E03" = {
      df <- .fixture_ensure_data_source(base$renewals, "renewals")
      df$tier_adjustment <- NULL
      list(case_id = case_id, kind = "renewals",
            clean = .fixture_wrap_clean(df, "renewals"),
            bad_row_index = integer(0),
            expected_check = "required_columns_renewals", expected_severity = "ERROR")
    },
    "E04" = {
      df <- .fixture_ensure_data_source(base$capacity, "capacity")
      df$capacity[1] <- 0L
      df$enrollment[1] <- 0L
      list(case_id = case_id, kind = "capacity",
            clean = .fixture_wrap_clean(df, "capacity"),
            bad_row_index = 1L,
            expected_check = "capacity_positive", expected_severity = "ERROR")
    },
    "E05" = {
      df <- .fixture_ensure_data_source(base$capacity, "capacity")
      df$enrollment[1] <- df$capacity[1] + 5L
      list(case_id = case_id, kind = "capacity",
            clean = .fixture_wrap_clean(df, "capacity"),
            bad_row_index = 1L,
            expected_check = "enrollment_le_capacity", expected_severity = "WARN")
    },
    "E06" = {
      df <- .fixture_ensure_data_source(base$capacity, "capacity")
      df$capacity[1] <- 10L
      df$waitlist[1] <- 50L
      list(case_id = case_id, kind = "capacity",
            clean = .fixture_wrap_clean(df, "capacity"),
            bad_row_index = 1L,
            expected_check = "waitlist_extreme (downstream)", expected_severity = "WARN")
    },
    "E11" = {
      df_ren <- .fixture_ensure_data_source(base$renewals, "renewals")
      df_new <- .fixture_ensure_data_source(base$new_apps, "new_apps")
      df_new$organization_name[1] <- df_ren$organization_name[1]
      df_new$project_name[1]      <- df_ren$project_name[1]
      df_new$county[1]            <- df_ren$county[1]
      list(case_id = case_id, kind = "both",
            renewals_clean = .fixture_wrap_clean(df_ren, "renewals"),
            new_apps_clean = .fixture_wrap_clean(df_new, "new_apps"),
            bad_row_index = 1L,
            expected_check = "sheet_contradiction (cross-sheet)",
            expected_severity = "ERROR")
    },
    "E12" = {
      df <- .fixture_ensure_data_source(base$renewals, "renewals")
      df$funding_type[1] <- "Mystery Funding (not in codebook)"
      list(case_id = case_id, kind = "renewals",
            clean = .fixture_wrap_clean(df, "renewals"),
            bad_row_index = 1L,
            expected_check = "funding_type_in_codebook", expected_severity = "WARN")
    },
    "E13" = {
      df <- .fixture_ensure_data_source(base$renewals, "renewals")
      df$county[1] <- "Cook"  # Illinois county
      list(case_id = case_id, kind = "renewals",
            clean = .fixture_wrap_clean(df, "renewals"),
            bad_row_index = 1L,
            expected_check = "county_in_alabama", expected_severity = "ERROR")
    },
	    "E14" = {
	      df <- .fixture_ensure_data_source(base$new_apps, "new_apps")
	      df$process_name[1] <- "2026 - 2027 First Class Pre-K Special Review Application"
	      list(case_id = case_id, kind = "new_apps",
	            clean = .fixture_wrap_clean(df, "new_apps"),
	            bad_row_index = 1L,
            expected_check = "process_name_in_codebook", expected_severity = "WARN")
    },
    "E15" = {
      df <- .fixture_ensure_data_source(base$renewals, "renewals")
      # Add a fully-blank row (NA everywhere except data_source)
      blank <- df[1, ]
      for (col in setdiff(names(blank), "data_source")) {
        if (is.numeric(blank[[col]])) blank[[col]] <- NA_real_
        else blank[[col]] <- NA_character_
      }
      df <- dplyr::bind_rows(df, blank)
      list(case_id = case_id, kind = "renewals",
            clean = .fixture_wrap_clean(df, "renewals"),
            bad_row_index = nrow(df),
            expected_check = "drop_fully_na (clean stage)", expected_severity = "INFO")
    },
    "E16" = {
      df <- .fixture_ensure_data_source(base$renewals, "renewals")
      df$region[1] <- "REGION TEN"
      list(case_id = case_id, kind = "renewals",
            clean = .fixture_wrap_clean(df, "renewals"),
            bad_row_index = 1L,
            expected_check = "region_format_valid", expected_severity = "WARN")
    },
    "E17" = {
      df <- .fixture_ensure_data_source(base$renewals, "renewals")
      df$draft_award[1] <- df$draft_base_award[1] + df$tier_adjustment[1] + 100
      list(case_id = case_id, kind = "renewals",
            clean = .fixture_wrap_clean(df, "renewals"),
            bad_row_index = 1L,
            expected_check = "draft_award_reconciles", expected_severity = "WARN")
    },
    stop(sprintf("Unknown case_id '%s'. Supported: E01, E03-E06, E11-E17.", case_id),
          call. = FALSE)
  )
}


#' Load the edge-cases codebook (CSV)
#' @keywords internal
#' @noRd
load_edge_cases_codebook <- function() {
  path <- system.file("extdata", "codebooks",
                       "applications_edge_cases.csv",
                       package = "ALprekDB", mustWork = TRUE)
  suppressMessages(readr::read_csv(path, show_col_types = FALSE,
                                     progress = FALSE))
}
