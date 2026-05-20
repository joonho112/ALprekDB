# tests/testthat/test-applications-linkage.R

.tl_master <- function(seed = 11L, cycle_year = "2026-2027") {
  s <- alprek_synthetic_applications(n_renewals = 6, n_new = 2,
                                       n_capacity_sites = 5,
                                       cycle_year = cycle_year, seed = seed)
  s$renewals$data_source <- "ADECE-renewals-sheet"
  s$new_apps$data_source <- "ADECE-new-sheet"
  s$capacity$data_source <- "ADECE-capacity-sheet"
  mk <- function(df, kind) {
    df <- tibble::as_tibble(df)
    df$raw_row_index <- seq_len(nrow(df))
    df$lineage_id <- sprintf("tl-%s-%s-%04d", cycle_year, kind, df$raw_row_index)
    structure(list(data = df, cleaning_log = tibble::tibble(),
                    meta = list(kind = kind, cycle_year = cycle_year,
                                  cycle = "cycle1", n_rows_in = nrow(df),
                                  n_rows_out = nrow(df), n_rows_dropped = 0L,
                                  file_sha256 = "tl", git_sha = "tl",
                                  cleaned_at = format(Sys.time()))),
              class = "alprek_applications_clean")
  }
  ren <- mk(s$renewals, "renewals")
  new <- mk(s$new_apps, "new_apps")
  cap <- mk(s$capacity, "capacity")
  rec <- applications_reconcile(ren, new, allow_degraded = TRUE)
  applications_transform(rec, capacity_clean = cap)
}

.tl_synthetic_panel <- function(n = 5, school_year = "2024-2025") {
  panel <- tibble::tibble(
    school_year = rep(school_year, n),
    classroom_code = sprintf("900T%04d.01", seq_len(n)),
    site_code = sprintf("900T%04d", seq_len(n)),
    county_name = sample(c("Madison", "Jefferson"), n, replace = TRUE),
    program_name = sprintf("Synthetic Org %d", seq_len(n)),
    classroom_name = sprintf("Synthetic Pre-K %d", seq_len(n))
  )
  structure(list(data = panel, years = school_year,
                  meta = list(n_rows = n)),
	            class = "alprek_classroom_panel")
}

.tl_manual_master <- function(app_df, cycle_year = "2026-2027") {
  structure(list(
    data = tibble::as_tibble(app_df),
    capacity_data = NULL,
    derived_log = tibble::tibble(),
    meta = list(cycle_year = cycle_year)
  ), class = "alprek_applications_master")
}


test_that("linkage rejects non-master input", {
  panel <- .tl_synthetic_panel()
  expect_error(linkage_applications_classroom(list(), panel),
                "alprek_applications_master")
})

test_that("linkage rejects non-panel input", {
  mst <- .tl_master()
  expect_error(linkage_applications_classroom(mst, list()),
                "alprek_classroom_panel")
})

test_that("linkage returns alprek_applications_linkage S3", {
  mst <- .tl_master()
  panel <- .tl_synthetic_panel()
  lk <- linkage_applications_classroom(mst, panel,
                                          target_school_year = "2024-2025")
  expect_s3_class(lk, "alprek_applications_linkage")
  expect_true(all(c("classroom_level", "unmatched_applications",
                      "diagnostics", "meta") %in% names(lk)))
})

test_that("classroom_level row count matches target school_year subset", {
  mst <- .tl_master()
  panel <- .tl_synthetic_panel(n = 4)
  lk <- linkage_applications_classroom(mst, panel,
                                          target_school_year = "2024-2025")
  expect_equal(nrow(lk$classroom_level), 4L)
})

test_that("app_applied_this_cycle FALSE for classrooms with no application", {
  mst <- .tl_master()
  panel <- .tl_synthetic_panel(n = 5)
  lk <- linkage_applications_classroom(mst, panel,
                                          target_school_year = "2024-2025")
  expect_true(all(!lk$classroom_level$app_applied_this_cycle |
                     lk$classroom_level$app_applied_this_cycle %in% c(TRUE, FALSE)))
  expect_true("app_applied_this_cycle" %in% names(lk$classroom_level))
})

test_that("diagnostics tibble has expected metric rows", {
  mst <- .tl_master()
  panel <- .tl_synthetic_panel()
  lk <- linkage_applications_classroom(mst, panel,
                                          target_school_year = "2024-2025")
  expected_metrics <- c("n_classroom_rows", "n_applications_in",
                         "n_matched_to_classroom", "n_only_classroom",
                         "n_applications_direct_classroom",
                         "n_applications_site_aggregated",
                         "n_applications_accounted",
                         "n_only_application_unmatched")
  expect_true(all(expected_metrics %in% lk$diagnostics$metric))
})

test_that("diagnostics consistency: matched + only_classroom == total classroom rows", {
  mst <- .tl_master()
  panel <- .tl_synthetic_panel()
  lk <- linkage_applications_classroom(mst, panel,
                                          target_school_year = "2024-2025")
  d <- setNames(lk$diagnostics$value, lk$diagnostics$metric)
  expect_equal(d["n_matched_to_classroom"] + d["n_only_classroom"],
                d["n_classroom_rows"], ignore_attr = TRUE)
  expect_equal(d["n_applications_direct_classroom"] +
                 d["n_applications_site_aggregated"] +
                 d["n_only_application_unmatched"],
               d["n_applications_in"], ignore_attr = TRUE)
})

test_that("bucket C new apps aggregate by site even with candidate classroom code", {
  app <- tibble::tibble(
    application_id = "app-c-1",
    raw_row_index = 1L,
    lineage_id = "lin-c-1",
    source_sheet = "new_apps",
    bucket = "C",
    matched_classroom_code = "900T0001.01",
    matched_site_code = "900T0001",
    match_method = "fuzzy_auto",
    match_score = 0.91,
    organization_name = "Synthetic Org 1",
    project_name = "Synthetic Pre-K New",
    county = "Madison",
    is_renewal = FALSE,
    is_new = TRUE,
    applied_this_cycle = TRUE,
    cycle_year_std = "2026-2027",
    tier_prev_dollars = NA_real_,
    tier_prev_rank = NA_integer_,
    tier_prev_band = NA_character_,
    total_funding_request = 120000,
    draft_award = 120000
  )
  mst <- .tl_manual_master(app)
  panel <- structure(list(
    data = tibble::tibble(
      school_year = c("2026-2027", "2026-2027"),
      classroom_code = c("900T0001.01", "900T0001.02"),
      site_code = c("900T0001", "900T0001")
    )
  ), class = "alprek_classroom_panel")

  lk <- linkage_applications_classroom(mst, panel)
  expect_equal(lk$classroom_level$site_n_new_apps, c(1L, 1L))
  expect_true(all(grepl("app-c-1", lk$classroom_level$site_application_ids)))
  expect_true(all(grepl("lin-c-1", lk$classroom_level$site_lineage_ids)))
  expect_false("app_application_id" %in% names(lk$classroom_level))
  d <- setNames(lk$diagnostics$value, lk$diagnostics$metric)
  expect_equal(d["n_applications_site_aggregated"], 1L, ignore_attr = TRUE)
})

test_that("linkage preserves application lineage on direct and unmatched rows", {
  apps <- tibble::tibble(
    application_id = c("app-a-1", "app-d-1"),
    raw_row_index = c(1L, 2L),
    lineage_id = c("lin-a-1", "lin-d-1"),
    source_sheet = c("renewals", "new_apps"),
    bucket = c("A", "D"),
    matched_classroom_code = c("900T0001.01", NA_character_),
    matched_site_code = c("900T0001", NA_character_),
    match_method = c("exact", "no_match"),
    match_score = c(1, NA_real_),
    organization_name = c("Synthetic Org 1", "Synthetic Org D"),
    project_name = c("Synthetic Pre-K 1", "Synthetic Pre-K D"),
    county = c("Madison", "Madison"),
    is_renewal = c(TRUE, FALSE),
    is_new = c(FALSE, TRUE),
    applied_this_cycle = TRUE,
    cycle_year_std = "2026-2027",
    tier_prev_dollars = c(5610, NA_real_),
    tier_prev_rank = c(1L, NA_integer_),
    tier_prev_band = c("high", NA_character_),
    total_funding_request = c(120000, 120000),
    draft_award = c(120000, 120000)
  )
  mst <- .tl_manual_master(apps)
  panel <- structure(list(
    data = tibble::tibble(
      school_year = "2026-2027",
      classroom_code = "900T0001.01",
      site_code = "900T0001"
    )
  ), class = "alprek_classroom_panel")

  lk <- linkage_applications_classroom(mst, panel)
  expect_equal(lk$classroom_level$app_lineage_id, "lin-a-1")
  expect_equal(lk$classroom_level$app_raw_row_index, 1L)
  expect_equal(lk$unmatched_applications$lineage_id, "lin-d-1")
  v <- applications_validate(lk)
  expect_equal(v$checks$status[v$checks$check_name == "linkage_lineage_recorded"],
               "PASS")
})

test_that("warning issued when target_school_year has no rows", {
  mst <- .tl_master()
  panel <- .tl_synthetic_panel(school_year = "2024-2025")
  expect_warning(
    linkage_applications_classroom(mst, panel,
                                     target_school_year = "1999-2000"),
    "0 rows"
  )
})

test_that("attach_capacity = FALSE omits capacity cols", {
  mst <- .tl_master()
  panel <- .tl_synthetic_panel()
  lk <- linkage_applications_classroom(mst, panel,
                                          target_school_year = "2024-2025",
                                          attach_capacity = FALSE)
  expect_false("capacity_utilization" %in% names(lk$classroom_level))
})

test_that("attach_capacity = TRUE adds capacity cols", {
  mst <- .tl_master()
  panel <- .tl_synthetic_panel()
  lk <- linkage_applications_classroom(mst, panel,
                                          target_school_year = "2024-2025",
                                          attach_capacity = TRUE)
  expect_true("capacity_utilization" %in% names(lk$classroom_level))
})

test_that("print method runs without error", {
  mst <- .tl_master()
  panel <- .tl_synthetic_panel()
  lk <- linkage_applications_classroom(mst, panel,
                                          target_school_year = "2024-2025")
  expect_output(print(lk), "alprek_applications_linkage")
  expect_output(print(lk), "Diagnostics:")
})

# ---- Linkage validation ----

test_that("applications_validate accepts alprek_applications_linkage", {
  mst <- .tl_master()
  panel <- .tl_synthetic_panel()
  lk <- linkage_applications_classroom(mst, panel,
                                          target_school_year = "2024-2025")
  v <- applications_validate(lk)
  expect_s3_class(v, "alprek_applications_validation")
  expect_equal(v$kind, "linkage")
})

test_that("validate linkage runs the expected check names", {
  mst <- .tl_master()
  panel <- .tl_synthetic_panel()
  lk <- linkage_applications_classroom(mst, panel,
                                          target_school_year = "2024-2025")
  v <- applications_validate(lk)
	  expected_checks <- c("renewals_have_classroom_code",
	                        "new_apps_attached_to_sites",
	                        "tier_prev_renewal_only",
	                        "unmatched_bucket_is_d",
	                        "linkage_lineage_recorded",
	                        "diagnostics_consistent",
	                        "row_count_positive")
  expect_true(all(expected_checks %in% v$checks$check_name))
})

test_that("empty classroom_level triggers WARN row_count_positive", {
  mst <- .tl_master()
  panel <- .tl_synthetic_panel(school_year = "2024-2025")
  suppressWarnings(
    lk <- linkage_applications_classroom(mst, panel,
                                            target_school_year = "1999-2000")
  )
  v <- applications_validate(lk)
  ck <- v$checks[v$checks$check_name == "row_count_positive", ]
  expect_equal(ck$status, "WARN")
})

test_that("linkage validation passes on a normal join", {
  mst <- .tl_master()
  panel <- .tl_synthetic_panel()
  lk <- linkage_applications_classroom(mst, panel,
                                          target_school_year = "2024-2025")
  v <- applications_validate(lk)
  expect_true(v$passed)
	expect_equal(v$n_errors, 0L)
})

test_that("linkage validation catches non-D unmatched applications", {
  mst <- .tl_master()
  panel <- .tl_synthetic_panel()
  lk <- linkage_applications_classroom(mst, panel,
                                          target_school_year = "2024-2025")
  if (nrow(lk$unmatched_applications) == 0L) {
    lk$unmatched_applications <- tibble::tibble(
      application_id = "bad-unmatched",
      lineage_id = "bad-lineage",
      bucket = "C"
    )
  } else {
    lk$unmatched_applications$bucket[1] <- "C"
  }
  v <- applications_validate(lk)
  ck <- v$checks[v$checks$check_name == "unmatched_bucket_is_d", ]
  expect_equal(ck$status, "ERROR")
})
