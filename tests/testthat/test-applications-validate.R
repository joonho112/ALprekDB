# tests/testthat/test-applications-validate.R
#
# Tests for applications_validate() across 4 cleaned kinds + reconciled data.
# Uses helper-applications-fixtures.R for synthetic edge-case data.

# ---------------------------------------------------------------------------
# Helper: build a clean alprek_applications_clean wrapper from a tibble
# ---------------------------------------------------------------------------
.test_wrap_clean <- function(df, kind) {
  df <- tibble::as_tibble(df)
  if (!"raw_row_index" %in% names(df)) {
    df$raw_row_index <- seq_len(nrow(df))
  }
  if (!"lineage_id" %in% names(df)) {
    df$lineage_id <- sprintf("fixture-%s-%04d", kind, df$raw_row_index)
  }
  structure(list(
    data = df,
    cleaning_log = tibble::tibble(),
    meta = list(kind = kind, cycle_year = "2026-2027", cycle = "cycle1",
                  n_rows_in = nrow(df), n_rows_out = nrow(df),
	                  n_rows_dropped = 0L,
	                  file_sha256 = "test-deadbeef",
	                  git_sha = "test-git-sha",
	                  cleaned_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  ), class = "alprek_applications_clean")
}

.test_synth <- function(seed = 11L) {
  s <- alprek_synthetic_applications(n_renewals = 12, n_new = 5,
                                       n_non_renewals = 3, n_capacity_sites = 12,
                                       seed = seed)
  # add data_source columns (synthetic generator omits)
  s$renewals$data_source     <- "ADECE-renewals-sheet"
  s$new_apps$data_source     <- "ADECE-new-sheet"
  s$non_renewals$data_source <- "ADECE-nonrenewals-sheet"
  s$capacity$data_source     <- "ADECE-capacity-sheet"
  s
}

.test_prior_panel <- function(renewals_df, school_year = "2025-2026") {
  project_prior <- if ("project_name_prior" %in% names(renewals_df)) {
    renewals_df$project_name_prior
  } else {
    renewals_df$project_name
  }
  panel <- tibble::tibble(
    school_year = rep(school_year, nrow(renewals_df)),
    program_name = renewals_df$organization_name,
    classroom_name = project_prior,
    county_name = renewals_df$county,
    classroom_code = sprintf("900T%04d.01", seq_len(nrow(renewals_df))),
    site_code = sprintf("900T%04d", seq_len(nrow(renewals_df)))
  )
  structure(list(
    data = panel,
    years = school_year,
    meta = list(n_rows = nrow(panel))
  ), class = "alprek_classroom_panel")
}


# ===========================================================================
# Category A: Required column existence (checks 1-4)
# ===========================================================================
test_that("missing required columns trip ERROR — renewals", {
  s <- .test_synth()
  s$renewals$tier_adjustment <- NULL
  v <- applications_validate(.test_wrap_clean(s$renewals, "renewals"))
  ck <- v$checks[v$checks$check_name == "required_columns_renewals", ]
  expect_equal(ck$status, "ERROR")
  expect_true(grepl("tier_adjustment", ck$details))
  expect_false(v$passed)
})

test_that("missing required columns trip ERROR — new_apps", {
  s <- .test_synth()
  s$new_apps$total_award <- NULL
  v <- applications_validate(.test_wrap_clean(s$new_apps, "new_apps"))
  ck <- v$checks[v$checks$check_name == "required_columns_new_apps", ]
  expect_equal(ck$status, "ERROR")
  expect_true(grepl("total_award", ck$details))
})

test_that("missing required columns trip ERROR — non_renewals", {
  s <- .test_synth()
  s$non_renewals$prior_funding_amount <- NULL
  v <- applications_validate(.test_wrap_clean(s$non_renewals, "non_renewals"))
  ck <- v$checks[v$checks$check_name == "required_columns_non_renewals", ]
  expect_equal(ck$status, "ERROR")
})

test_that("missing required columns trip ERROR — capacity", {
  s <- .test_synth()
  s$capacity$waitlist <- NULL
  v <- applications_validate(.test_wrap_clean(s$capacity, "capacity"))
  ck <- v$checks[v$checks$check_name == "required_columns_capacity", ]
  expect_equal(ck$status, "ERROR")
})


# ===========================================================================
# Category B: Value ranges (checks 5-9)
# ===========================================================================
test_that("negative funding values trip ERROR — renewals", {
  s <- .test_synth()
  s$renewals$award_prior[1] <- -1000
  s$renewals$total_funding_request[3] <- -50
  v <- applications_validate(.test_wrap_clean(s$renewals, "renewals"))
  ck <- v$checks[v$checks$check_name == "no_negative_funding", ]
  expect_equal(ck$status, "ERROR")
  expect_equal(ck$n_issues, 2L)
  expect_true(any(v$issues$issue_type == "negative_funding"))
})

test_that("clean funding passes no_negative_funding", {
  s <- .test_synth()
  v <- applications_validate(.test_wrap_clean(s$renewals, "renewals"))
  ck <- v$checks[v$checks$check_name == "no_negative_funding", ]
  expect_equal(ck$status, "PASS")
  expect_true("issue_type" %in% names(v$issues))
  expect_identical(nrow(v$issues), 0L)
})

test_that("tier_adjustment outside +/- $50k trips WARN", {
  s <- .test_synth()
  s$renewals$tier_adjustment[1] <- 60000
  s$renewals$tier_adjustment[2] <- -60000
  v <- applications_validate(.test_wrap_clean(s$renewals, "renewals"))
  ck <- v$checks[v$checks$check_name == "tier_adjustment_range", ]
  expect_equal(ck$status, "WARN")
  expect_equal(ck$n_issues, 2L)
})

test_that("capacity <= 0 trips ERROR — capacity", {
  s <- .test_synth()
  s$capacity$capacity[1] <- 0L
  s$capacity$enrollment[1] <- 0L  # to avoid enrollment > capacity
  v <- applications_validate(.test_wrap_clean(s$capacity, "capacity"))
  ck <- v$checks[v$checks$check_name == "capacity_positive", ]
  expect_equal(ck$status, "ERROR")
  expect_equal(ck$n_issues, 1L)
})

test_that("capacity rows missing site_code trip ERROR", {
  s <- .test_synth()
  s$capacity$site_code[1] <- NA_character_
  v <- applications_validate(.test_wrap_clean(s$capacity, "capacity"))
  ck <- v$checks[v$checks$check_name == "capacity_site_code_present", ]
  expect_equal(ck$status, "ERROR")
  expect_equal(ck$n_issues, 1L)
})

test_that("enrollment > capacity trips WARN — capacity", {
  s <- .test_synth()
  s$capacity$enrollment[1] <- s$capacity$capacity[1] + 5L
  v <- applications_validate(.test_wrap_clean(s$capacity, "capacity"))
  ck <- v$checks[v$checks$check_name == "enrollment_le_capacity", ]
  expect_equal(ck$status, "WARN")
  expect_gte(ck$n_issues, 1L)
})

test_that("negative waitlist trips ERROR — capacity", {
  s <- .test_synth()
  s$capacity$waitlist[1] <- -1L
  v <- applications_validate(.test_wrap_clean(s$capacity, "capacity"))
  ck <- v$checks[v$checks$check_name == "waitlist_non_negative", ]
  expect_equal(ck$status, "ERROR")
})


# ===========================================================================
# Category C: Cross-field consistency (checks 10-11)
# ===========================================================================
test_that("draft_award reconciliation — clean data PASS", {
  s <- .test_synth()
  v <- applications_validate(.test_wrap_clean(s$renewals, "renewals"))
  ck <- v$checks[v$checks$check_name == "draft_award_reconciles", ]
  expect_equal(ck$status, "PASS")
})

test_that("draft_award != base + adj trips WARN", {
  s <- .test_synth()
  s$renewals$draft_award[1] <- s$renewals$draft_base_award[1] +
                                s$renewals$tier_adjustment[1] + 100
  v <- applications_validate(.test_wrap_clean(s$renewals, "renewals"))
  ck <- v$checks[v$checks$check_name == "draft_award_reconciles", ]
  expect_equal(ck$status, "WARN")
})

test_that("total_award reconciliation — clean PASS", {
  s <- .test_synth()
  v <- applications_validate(.test_wrap_clean(s$new_apps, "new_apps"))
  ck <- v$checks[v$checks$check_name == "total_award_reconciles", ]
  expect_equal(ck$status, "PASS")
})

test_that("total_award != other + new trips WARN", {
  s <- .test_synth()
  s$new_apps$total_award[1] <- 9999999
  v <- applications_validate(.test_wrap_clean(s$new_apps, "new_apps"))
  ck <- v$checks[v$checks$check_name == "total_award_reconciles", ]
  expect_equal(ck$status, "WARN")
})


# ===========================================================================
# Category D: Codebook membership (checks 12-15)
# ===========================================================================
test_that("non-Alabama county trips ERROR", {
  s <- .test_synth()
  s$renewals$county[1] <- "Cook"      # Illinois
  s$renewals$county[2] <- "Travis"    # Texas
  v <- applications_validate(.test_wrap_clean(s$renewals, "renewals"))
  ck <- v$checks[v$checks$check_name == "county_in_alabama", ]
  expect_equal(ck$status, "ERROR")
  expect_equal(ck$n_issues, 2L)
})

test_that("non-codebook funding_type trips WARN", {
  s <- .test_synth()
  s$renewals$funding_type[1] <- "Phantom Funding"
  v <- applications_validate(.test_wrap_clean(s$renewals, "renewals"))
  ck <- v$checks[v$checks$check_name == "funding_type_in_codebook", ]
  expect_equal(ck$status, "WARN")
  expect_gte(ck$n_issues, 1L)
})

test_that("non-canonical region trips WARN", {
  s <- .test_synth()
  s$renewals$region[1] <- "REGION TEN"
  s$renewals$region[2] <- "REG 1"
  v <- applications_validate(.test_wrap_clean(s$renewals, "renewals"))
  ck <- v$checks[v$checks$check_name == "region_format_valid", ]
  expect_equal(ck$status, "WARN")
  expect_equal(ck$n_issues, 2L)
})

test_that("canonical region 'Region [1-9]' PASS", {
  s <- .test_synth()
  s$renewals$region[1] <- "Region 7"
  v <- applications_validate(.test_wrap_clean(s$renewals, "renewals"))
  ck <- v$checks[v$checks$check_name == "region_format_valid", ]
  expect_equal(ck$status, "PASS")
})

test_that("non-codebook process_name trips WARN", {
  s <- .test_synth()
  s$new_apps$process_name[1] <- "2026 - 2027 First Class Pre-K Mystery Application"
  v <- applications_validate(.test_wrap_clean(s$new_apps, "new_apps"))
  ck <- v$checks[v$checks$check_name == "process_name_in_codebook", ]
  expect_equal(ck$status, "WARN")
})


# ===========================================================================
# Category E: Provenance & sanity (checks 16-18)
# ===========================================================================
test_that("missing file_sha256 trips ERROR for provenance", {
  s <- .test_synth()
  bad <- .test_wrap_clean(s$renewals, "renewals")
  bad$meta$file_sha256 <- NULL
  v <- applications_validate(bad)
  ck <- v$checks[v$checks$check_name == "provenance_recorded", ]
  expect_equal(ck$status, "ERROR")
})

test_that("missing data_source on rows trips ERROR for provenance", {
  s <- .test_synth()
  s$renewals$data_source[1] <- NA_character_
  v <- applications_validate(.test_wrap_clean(s$renewals, "renewals"))
  ck <- v$checks[v$checks$check_name == "provenance_recorded", ]
  expect_equal(ck$status, "ERROR")
})

test_that("missing row lineage trips ERROR", {
  s <- .test_synth()
  bad <- .test_wrap_clean(s$renewals, "renewals")
  bad$data$lineage_id[1] <- NA_character_
  v <- applications_validate(bad)
  ck <- v$checks[v$checks$check_name == "row_lineage_recorded", ]
  expect_equal(ck$status, "ERROR")
})

test_that("zero rows trip ERROR for row_count_positive", {
  s <- .test_synth()
  empty <- s$renewals[0, ]
  v <- applications_validate(.test_wrap_clean(empty, "renewals"))
  ck <- v$checks[v$checks$check_name == "row_count_positive", ]
  expect_equal(ck$status, "ERROR")
})

test_that("few counties tripped INFO for coverage", {
  s <- .test_synth()
  s$renewals$county <- "Madison"  # all one county
  v <- applications_validate(.test_wrap_clean(s$renewals, "renewals"))
  ck <- v$checks[v$checks$check_name == "county_coverage_breadth", ]
  expect_equal(ck$status, "INFO")
})


# ===========================================================================
# Category F: Reconciled-only checks (R1, R2)
# ===========================================================================
test_that("reconciled validation passes on clean reconciled object", {
  s <- .test_synth()
  ren <- .test_wrap_clean(s$renewals, "renewals")
  new <- .test_wrap_clean(s$new_apps, "new_apps")
  panel <- .test_prior_panel(ren$data)
  rec <- applications_reconcile(ren, new, prior_classroom_panel = panel)
  v <- applications_validate(rec)
  expect_true(v$passed)
  expect_equal(v$kind, "reconciled")
  expect_equal(v$checks$status[v$checks$check_name == "every_row_has_bucket"], "PASS")
  expect_equal(v$checks$status[v$checks$check_name == "match_score_in_unit_interval"], "PASS")
  expect_equal(v$checks$status[v$checks$check_name == "reconciliation_log_coverage"], "PASS")
})

test_that("bucket NA trips ERROR — reconciled", {
  s <- .test_synth()
  ren <- .test_wrap_clean(s$renewals, "renewals")
  new <- .test_wrap_clean(s$new_apps, "new_apps")
  panel <- .test_prior_panel(ren$data)
  rec <- applications_reconcile(ren, new, prior_classroom_panel = panel)
  rec$reconciled$bucket[1] <- NA_character_
  v <- applications_validate(rec)
  ck <- v$checks[v$checks$check_name == "every_row_has_bucket", ]
  expect_equal(ck$status, "ERROR")
})

test_that("match_score outside [0,1] trips ERROR — reconciled", {
  s <- .test_synth()
  ren <- .test_wrap_clean(s$renewals, "renewals")
  new <- .test_wrap_clean(s$new_apps, "new_apps")
  panel <- .test_prior_panel(ren$data)
  rec <- applications_reconcile(ren, new, prior_classroom_panel = panel)
  rec$reconciled$match_score[1] <- 1.5
  v <- applications_validate(rec)
  ck <- v$checks[v$checks$check_name == "match_score_in_unit_interval", ]
  expect_equal(ck$status, "ERROR")
})

test_that("prior panel is required unless degraded mode is explicit", {
  s <- .test_synth()
  ren <- .test_wrap_clean(s$renewals, "renewals")
  new <- .test_wrap_clean(s$new_apps, "new_apps")
  expect_error(applications_reconcile(ren, new),
               "prior_classroom_panel is required")

  rec <- applications_reconcile(ren, new, allow_degraded = TRUE)
  expect_true(all(rec$reconciled$bucket == "unknown"))
  v <- applications_validate(rec)
  ck <- v$checks[v$checks$check_name == "every_row_has_bucket", ]
  expect_equal(ck$status, "ERROR")
})

test_that("reconciliation log coverage is enforced", {
  s <- .test_synth()
  ren <- .test_wrap_clean(s$renewals, "renewals")
  new <- .test_wrap_clean(s$new_apps, "new_apps")
  panel <- .test_prior_panel(ren$data)
  rec <- applications_reconcile(ren, new, prior_classroom_panel = panel)
  rec$reconciliation_log <- rec$reconciliation_log[-1, ]
  v <- applications_validate(rec)
  ck <- v$checks[v$checks$check_name == "reconciliation_log_coverage", ]
  expect_equal(ck$status, "ERROR")
})

test_that("normalized fuzzy text recovers case and punctuation-only variants", {
  s <- .test_synth()
  ren_empty <- .test_wrap_clean(s$renewals[0, ], "renewals")
  panel <- .test_prior_panel(s$renewals[1, ])
  new_df <- s$new_apps[1, ]
  new_df$organization_name <- toupper(panel$data$program_name[1])
  new_df$project_name <- gsub("Pre-K", "Pre K #", panel$data$classroom_name[1],
                              fixed = TRUE)
  new_df$county <- panel$data$county_name[1]
  new <- .test_wrap_clean(new_df, "new_apps")
  rec <- applications_reconcile(ren_empty, new, prior_classroom_panel = panel)
  expect_equal(rec$reconciled$bucket[1], "C")
  expect_equal(rec$reconciled$match_method[1], "fuzzy_auto")
})


# ===========================================================================
# Category G: strict mode + S3 + print
# ===========================================================================
test_that("strict=TRUE promotes WARN to fail", {
  s <- .test_synth()
  s$renewals$region[1] <- "REGION TEN"  # WARN
  cln <- .test_wrap_clean(s$renewals, "renewals")
  v_lax <- applications_validate(cln, strict = FALSE)
  v_strict <- applications_validate(cln, strict = TRUE)
  expect_true(v_lax$passed)
  expect_false(v_strict$passed)
})

test_that("invalid input class raises clear error", {
  expect_error(applications_validate(data.frame(x = 1)),
                "alprek_applications_clean")
  expect_error(applications_validate(list()),
                "alprek_applications_clean")
})

test_that("S3 class and required slots present", {
  s <- .test_synth()
  v <- applications_validate(.test_wrap_clean(s$renewals, "renewals"))
  expect_s3_class(v, "alprek_applications_validation")
  expect_true(all(c("passed", "n_errors", "n_warnings", "n_info",
                      "kind", "checks", "issues") %in% names(v)))
  expect_s3_class(v$checks, "tbl_df")
  expect_s3_class(v$issues, "tbl_df")
})

test_that("print method runs without error", {
  s <- .test_synth()
  v <- applications_validate(.test_wrap_clean(s$renewals, "renewals"))
  expect_output(print(v), "alprek_applications_validation")
  expect_output(print(v), "Checks:")
})

test_that("tolerance argument controls draft_award check", {
  s <- .test_synth()
  s$renewals$draft_award[1] <- s$renewals$draft_base_award[1] +
                                s$renewals$tier_adjustment[1] + 0.50
  cln <- .test_wrap_clean(s$renewals, "renewals")
  v_tight <- applications_validate(cln, tolerance = 0.10)
  v_loose <- applications_validate(cln, tolerance = 1.00)
  expect_equal(v_tight$checks$status[v_tight$checks$check_name == "draft_award_reconciles"], "WARN")
  expect_equal(v_loose$checks$status[v_loose$checks$check_name == "draft_award_reconciles"], "PASS")
})


# ===========================================================================
# Category H: Fixture-based smoke (drives helper-applications-fixtures.R)
# ===========================================================================
test_that("fixture E03 (missing required column) trips ERROR", {
  fx <- make_edge_case_fixture("E03")
  v <- applications_validate(fx$clean)
  ck <- v$checks[v$checks$check_name == "required_columns_renewals", ]
  expect_equal(ck$status, "ERROR")
})

test_that("fixture E04 (capacity = 0) trips ERROR", {
  fx <- make_edge_case_fixture("E04")
  v <- applications_validate(fx$clean)
  ck <- v$checks[v$checks$check_name == "capacity_positive", ]
  expect_equal(ck$status, "ERROR")
})

test_that("fixture E05 (enrollment > capacity) trips WARN", {
  fx <- make_edge_case_fixture("E05")
  v <- applications_validate(fx$clean)
  ck <- v$checks[v$checks$check_name == "enrollment_le_capacity", ]
  expect_equal(ck$status, "WARN")
})

test_that("fixture E12 (funding_type non-codebook) trips WARN", {
  fx <- make_edge_case_fixture("E12")
  v <- applications_validate(fx$clean)
  ck <- v$checks[v$checks$check_name == "funding_type_in_codebook", ]
  expect_equal(ck$status, "WARN")
})

test_that("fixture E13 (non-Alabama county) trips ERROR", {
  fx <- make_edge_case_fixture("E13")
  v <- applications_validate(fx$clean)
  ck <- v$checks[v$checks$check_name == "county_in_alabama", ]
  expect_equal(ck$status, "ERROR")
})

test_that("fixture E14 (unknown process_name variant) trips WARN", {
  fx <- make_edge_case_fixture("E14")
  v <- applications_validate(fx$clean)
  ck <- v$checks[v$checks$check_name == "process_name_in_codebook", ]
  expect_equal(ck$status, "WARN")
})

test_that("fixture E16 (region format) trips WARN", {
  fx <- make_edge_case_fixture("E16")
  v <- applications_validate(fx$clean)
  ck <- v$checks[v$checks$check_name == "region_format_valid", ]
  expect_equal(ck$status, "WARN")
})

test_that("fixture E17 (draft_award mismatch) trips WARN", {
  fx <- make_edge_case_fixture("E17")
  v <- applications_validate(fx$clean)
  ck <- v$checks[v$checks$check_name == "draft_award_reconciles", ]
  expect_equal(ck$status, "WARN")
})


# ===========================================================================
# Category I: Edge cases codebook loader
# ===========================================================================
test_that("edge cases codebook loads with expected schema", {
  ec <- load_edge_cases_codebook()
  expect_s3_class(ec, "tbl_df")
  expect_gte(nrow(ec), 15L)
  expected_cols <- c("case_id", "label", "description", "detection_rule",
                       "policy", "severity", "validate_check_name")
  expect_true(all(expected_cols %in% names(ec)))
  expect_true(all(ec$severity %in% c("ERROR", "WARN", "INFO")))
  expect_true(all(grepl("^E\\d{2}$", ec$case_id)))
})


# ===========================================================================
# Category J: Clean baseline — all checks PASS on synthetic-clean data
# ===========================================================================
test_that("clean synthetic renewals data — overall PASSED", {
  s <- .test_synth()
  v <- applications_validate(.test_wrap_clean(s$renewals, "renewals"))
  expect_true(v$passed)
  expect_equal(v$n_errors, 0L)
})

test_that("clean synthetic non_renewals data — overall PASSED", {
  s <- .test_synth()
  v <- applications_validate(.test_wrap_clean(s$non_renewals, "non_renewals"))
  expect_true(v$passed)
  expect_equal(v$n_errors, 0L)
})

test_that("clean synthetic capacity data — overall PASSED", {
  s <- .test_synth()
  v <- applications_validate(.test_wrap_clean(s$capacity, "capacity"))
  expect_true(v$passed)
  expect_equal(v$n_errors, 0L)
})
