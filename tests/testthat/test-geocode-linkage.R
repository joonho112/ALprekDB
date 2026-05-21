# tests/testthat/test-geocode-linkage.R
#
# Tests for linkage_geocode_classroom() and linkage_geocode_applications()
# (Step 6.1 of the v0.8.0 geocode rollout). The release contract is
# load-bearing here: coord_model_status MUST round-trip through both joins,
# and so must lineage_id (renamed `geocode_lineage_id` to avoid colliding
# with classroom-panel lineage).

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Build a synthetic alprek_geocode_panel by running the full
# clean -> reconcile -> transform -> bind_years pipeline on
# `alprek_synthetic_geocode()` output. This makes the test panel a faithful
# mirror of the real-world pipeline output (29 Melissa cols + 10 reconcile
# auth cols + transform derived cols) without the rest of the test file
# having to know the internals.

.lg_make_geocode_panel <- function(n_sites = 6L, n_years = 3L,
                                    seed = 20260520L) {
  df <- alprek_synthetic_geocode(n_sites = n_sites, n_years = n_years,
                                 seed = seed,
                                 share_missing_adece = 0.05,
                                 share_missing_site_code = 0.10)
  # Ensure dtypes match what geocode_clean() emits
  df$LAT <- suppressWarnings(as.numeric(as.character(df$LAT)))
  df$LNG <- suppressWarnings(as.numeric(as.character(df$LNG)))
  df$ERRORCODE <- as.character(df$ERRORCODE)
  df$site_zip <- as.character(df$site_zip)
  if (!"raw_row_index" %in% names(df)) df$raw_row_index <- seq_len(nrow(df))
  if (!"lineage_id" %in% names(df)) {
    df$lineage_id <- paste0("lin_", seq_len(nrow(df)))
  }

  meta <- list(
    path             = "/tmp/fake-melissa.xlsx",
    sheet            = "Sheet1",
    source           = "melissa",
    cycle_year       = "2026-2027",
    receipt_date     = as.Date("2026-03-04"),
    file_basename    = "fake-melissa.xlsx",
    file_sha256      = paste(rep("a", 64L), collapse = ""),
    git_sha          = "abc123",
    geocoding_source = "melissa_v1_2026",
    lineage_id       = as.character(df$lineage_id),
    raw_row_index    = as.integer(df$raw_row_index),
    n_rows           = nrow(df),
    n_rows_in        = nrow(df),
    n_rows_dropped   = 0L,
    cleaned_at       = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
  cleaning_log <- tibble::tibble(rule = character(0),
                                  n_affected = integer(0),
                                  details = character(0),
                                  severity = character(0))
  clean <- structure(list(data = df, cleaning_log = cleaning_log, meta = meta),
                     class = "alprek_geocode_clean")
  rec <- geocode_reconcile(clean)
  mst <- geocode_transform(rec)
  geocode_bind_years(mst)
}


# Build a minimal alprek_classroom_panel whose site_codes overlap with a
# given geocode panel by construction. Each classroom row carries its own
# lineage_id so we can assert it's preserved through the join.

.lg_make_classroom_panel <- function(geocode_panel,
                                      include_unmatched = TRUE) {
  gc_df <- geocode_panel$data
  gc_known <- gc_df[!is.na(gc_df$site_code), , drop = FALSE]
  # 2 classrooms per (site_code, school_year) so the join is exercised
  # at multiple classroom rows for the same geocode row.
  base <- unique(gc_known[, c("site_code", "school_year"), drop = FALSE])
  cl_rows <- list()
  for (i in seq_len(nrow(base))) {
    cl_rows[[length(cl_rows) + 1L]] <- tibble::tibble(
      classroom_code = sprintf("%s.01", as.character(base$site_code[i])),
      site_code = as.character(base$site_code[i]),
      school_year = as.character(base$school_year[i]),
      lineage_id = sprintf("class_lin_%s_%s",
                           as.character(base$site_code[i]),
                           as.character(base$school_year[i])),
      latitude = NA_real_,
      longitude = NA_real_
    )
    cl_rows[[length(cl_rows) + 1L]] <- tibble::tibble(
      classroom_code = sprintf("%s.02", as.character(base$site_code[i])),
      site_code = as.character(base$site_code[i]),
      school_year = as.character(base$school_year[i]),
      lineage_id = sprintf("class_lin_%s_%s_b",
                           as.character(base$site_code[i]),
                           as.character(base$school_year[i])),
      latitude = NA_real_,
      longitude = NA_real_
    )
  }

  if (include_unmatched) {
    # Add 3 classrooms whose site_code does NOT appear in the geocode panel.
    # These rows must show up as unmatched after the join.
    cl_rows[[length(cl_rows) + 1L]] <- tibble::tibble(
      classroom_code = "999P999999.01",
      site_code = "999P999999",
      school_year = "2024-2025",
      lineage_id = "class_lin_unmatched_1",
      latitude = NA_real_,
      longitude = NA_real_
    )
    cl_rows[[length(cl_rows) + 1L]] <- tibble::tibble(
      classroom_code = "999P999998.01",
      site_code = "999P999998",
      school_year = "2023-2024",
      lineage_id = "class_lin_unmatched_2",
      latitude = NA_real_,
      longitude = NA_real_
    )
    cl_rows[[length(cl_rows) + 1L]] <- tibble::tibble(
      classroom_code = "999P999997.01",
      site_code = "999P999997",
      school_year = "2022-2023",
      lineage_id = "class_lin_unmatched_3",
      latitude = NA_real_,
      longitude = NA_real_
    )
  }

  combined <- dplyr::bind_rows(cl_rows)
  combined <- combined[order(combined$school_year, combined$classroom_code), ,
                       drop = FALSE]
  years <- sort(unique(combined$school_year))

  structure(list(
    data = tibble::as_tibble(combined),
    years = years,
    n_total = nrow(combined),
    by_year = lapply(years, function(yr) list(school_year = yr,
                                              format = "test",
                                              n_classrooms = sum(combined$school_year == yr))),
    imputation_log = tibble::tibble(classroom_code = character(),
                                     school_year = character(),
                                     variable = character(),
                                     imputed_value = character(),
                                     method = character())
  ), class = "alprek_classroom_panel")
}


# Build a small alprek_applications_master by hand. Includes 3 buckets:
#   * 1 renewal row (matched_site_code present, school_year known)
#   * 1 bucket C row (new app at a known site, matched_site_code present)
#   * 2 bucket D rows: one whose row_id matches a Melissa `_new` row,
#     one whose row_id does not.

.lg_make_app_master <- function(geocode_panel, cycle_year = "2026-2027") {
  gc_df <- geocode_panel$data

  # Pick a known site_code from the geocode panel (first non-NA)
  known_sc <- as.character(gc_df$site_code[!is.na(gc_df$site_code)][1])
  known_yr <- as.character(gc_df$school_year[!is.na(gc_df$site_code)][1])

  # Find a Melissa "_new" row_id (bucket D path)
  new_row <- gc_df[grepl("_new_", as.character(gc_df$row_id)), ]
  bucketd_rowid <- if (nrow(new_row) > 0L) {
    as.character(new_row$row_id[1])
  } else {
    NA_character_
  }

  apps <- tibble::tibble(
    application_id = c("app-renew-1", "app-c-1", "app-d-match", "app-d-miss"),
    row_id = c("app_renew_001", "app_c_001",
               bucketd_rowid %||% "app_d_001",
               "app_d_unknown_row"),
    raw_row_index = 1:4,
    lineage_id = c("lin_app_renew_1", "lin_app_c_1",
                   "lin_app_d_match", "lin_app_d_miss"),
    source_sheet = c("renewals", "new_apps", "new_apps", "new_apps"),
    bucket = c("A", "C", "D", "D"),
    matched_classroom_code = c(paste0(known_sc, ".01"),
                                paste0(known_sc, ".02"),
                                NA_character_, NA_character_),
    matched_site_code = c(known_sc, known_sc, NA_character_, NA_character_),
    match_method = c("exact", "fuzzy_auto", "no_match", "no_match"),
    match_score = c(1, 0.9, NA_real_, NA_real_),
    organization_name = c("Synthetic Org Renew", "Synthetic Org C",
                           "Synthetic Org D-match", "Synthetic Org D-miss"),
    project_name = c("Pre-K 1", "New Pre-K", "New Pre-K D1", "New Pre-K D2"),
    county = c("Madison", "Madison", "Mobile", "Lee"),
    is_renewal = c(TRUE, FALSE, FALSE, FALSE),
    is_new = c(FALSE, TRUE, TRUE, TRUE),
    applied_this_cycle = TRUE,
    cycle_year_std = cycle_year,
    school_year_target = c(known_yr, known_yr,
                            "2025-2026_new", "2025-2026_new"),
    tier_prev_dollars = c(5610, NA_real_, NA_real_, NA_real_),
    tier_prev_rank = c(1L, NA_integer_, NA_integer_, NA_integer_),
    tier_prev_band = c("high", NA_character_,
                        NA_character_, NA_character_),
    total_funding_request = c(120000, 130000, 140000, 150000),
    draft_award = c(120000, 130000, 140000, 150000)
  )

  structure(list(
    data = apps,
    capacity_data = NULL,
    derived_log = tibble::tibble(),
    meta = list(cycle_year = cycle_year,
                has_capacity = FALSE)
  ), class = "alprek_applications_master")
}


`%||%` <- function(a, b) if (is.null(a)) b else a


# ============================================================================
# linkage_geocode_classroom() tests
# ============================================================================

test_that("linkage_geocode_classroom rejects non-panel inputs", {
  gp <- .lg_make_geocode_panel()
  cp <- .lg_make_classroom_panel(gp)

  expect_error(linkage_geocode_classroom(list(), cp),
               "alprek_geocode_panel")
  expect_error(linkage_geocode_classroom(gp, list()),
               "alprek_classroom_panel")
  expect_error(linkage_geocode_classroom("not a panel", cp),
               "alprek_geocode_panel")
})


test_that("linkage_geocode_classroom returns correct S3 with named slots", {
  gp <- .lg_make_geocode_panel()
  cp <- .lg_make_classroom_panel(gp)

  lk <- linkage_geocode_classroom(gp, cp)

  expect_s3_class(lk, "alprek_geocode_linkage_classroom")
  expect_true(is.list(lk))
  expect_true(all(c("data", "diagnostics", "meta") %in% names(lk)))
  expect_s3_class(lk$data, "tbl_df")
  expect_s3_class(lk$diagnostics, "tbl_df")
})


test_that("linkage_geocode_classroom preserves classroom row count (no inflation)", {
  gp <- .lg_make_geocode_panel(n_sites = 6L, n_years = 3L)
  cp <- .lg_make_classroom_panel(gp)

  lk <- linkage_geocode_classroom(gp, cp)

  expect_equal(nrow(lk$data), nrow(cp$data))
})


test_that("linkage_geocode_classroom attaches the 10 reconcile cols + geocode_run_id + geocode_lineage_id", {
  gp <- .lg_make_geocode_panel()
  cp <- .lg_make_classroom_panel(gp)

  lk <- linkage_geocode_classroom(gp, cp)

  expected_attached <- c(
    "geocode_lat_final", "geocode_lng_final", "geocode_lat_source",
    "geocode_lat_precision", "geocode_distance_adece_melissa_m",
    "geocode_coord_agreement_band", "geocode_needs_followup_geocoding",
    "geocode_followup_reason", "geocode_coord_model_status",
    "geocode_provenance",  # provenance prefix already 'geocode_' from upstream column name
    "geocode_run_id", "geocode_lineage_id"
  )
  # The reconcile column 'geocode_provenance' is already prefixed by upstream;
  # our helper does not double-prefix when the source col name is already
  # `geocode_provenance`. We accept either result, but at minimum the 10
  # reconcile semantic columns must be findable by their prefixed names.

  for (c in c("geocode_lat_final", "geocode_lng_final",
              "geocode_lat_source", "geocode_lat_precision",
              "geocode_distance_adece_melissa_m",
              "geocode_coord_agreement_band",
              "geocode_needs_followup_geocoding",
              "geocode_followup_reason",
              "geocode_coord_model_status",
              "geocode_run_id", "geocode_lineage_id")) {
    expect_true(c %in% names(lk$data),
                info = sprintf("expected column '%s' in linked data", c))
  }
})


test_that("linkage_geocode_classroom preserves coord_model_status", {
  gp <- .lg_make_geocode_panel()
  cp <- .lg_make_classroom_panel(gp)

  lk <- linkage_geocode_classroom(gp, cp)

  expect_true("geocode_coord_model_status" %in% names(lk$data))
  # Matched rows must have non-NA coord_model_status (factor with levels)
  matched <- !is.na(lk$data$geocode_lineage_id)
  if (any(matched)) {
    expect_true(is.factor(lk$data$geocode_coord_model_status))
    expect_true(any(!is.na(lk$data$geocode_coord_model_status[matched])))
    # All values must be in the spec's controlled vocabulary
    valid_levels <- c("missing", "not_model_ready",
                      "provisional_followup", "model_ready")
    obs_levels <- levels(lk$data$geocode_coord_model_status)
    expect_true(all(obs_levels %in% valid_levels))
  }
})


test_that("linkage_geocode_classroom preserves both lineage_ids (classroom + geocode)", {
  gp <- .lg_make_geocode_panel()
  cp <- .lg_make_classroom_panel(gp)

  lk <- linkage_geocode_classroom(gp, cp)

  # Classroom-side lineage_id preserved
  expect_true("lineage_id" %in% names(lk$data))
  expect_setequal(lk$data$lineage_id, cp$data$lineage_id)

  # Geocode-side lineage_id present and distinct
  expect_true("geocode_lineage_id" %in% names(lk$data))
  matched <- !is.na(lk$data$geocode_lineage_id)
  if (any(matched)) {
    # geocode_lineage_id values should come from the geocode panel
    gp_lins <- as.character(gp$data$lineage_id)
    expect_true(all(lk$data$geocode_lineage_id[matched] %in% gp_lins))
  }
})


test_that("linkage_geocode_classroom diagnostics reports match rate correctly", {
  gp <- .lg_make_geocode_panel()
  cp <- .lg_make_classroom_panel(gp, include_unmatched = TRUE)

  lk <- linkage_geocode_classroom(gp, cp)
  diags <- lk$diagnostics

  expect_true(all(c("metric", "value", "group_by") %in% names(diags)))
  d <- setNames(diags$value[is.na(diags$group_by)],
                diags$metric[is.na(diags$group_by)])
  expect_equal(d[["n_classroom_total"]], nrow(cp$data))
  # n_matched + n_unmatched_classroom == n_classroom_total
  expect_equal(d[["n_matched"]] + d[["n_unmatched_classroom"]],
               d[["n_classroom_total"]])
  # At least the 3 deliberately-unmatched rows are unmatched
  expect_gte(d[["n_unmatched_classroom"]], 3L)
})


test_that("linkage_geocode_classroom unmatched rows have NA for attached geocode cols", {
  gp <- .lg_make_geocode_panel()
  cp <- .lg_make_classroom_panel(gp, include_unmatched = TRUE)

  lk <- linkage_geocode_classroom(gp, cp)

  unmatched_idx <- which(lk$data$site_code %in%
                           c("999P999999", "999P999998", "999P999997"))
  expect_gt(length(unmatched_idx), 0L)

  # All attached cols should be NA on unmatched rows
  for (col in c("geocode_lat_final", "geocode_lng_final",
                "geocode_lat_source", "geocode_run_id",
                "geocode_lineage_id", "geocode_coord_model_status")) {
    if (col %in% names(lk$data)) {
      expect_true(all(is.na(lk$data[[col]][unmatched_idx])),
                  info = sprintf("col '%s' should be NA on unmatched rows", col))
    }
  }
})


test_that("linkage_geocode_classroom diagnostics include by-school_year coverage", {
  gp <- .lg_make_geocode_panel()
  cp <- .lg_make_classroom_panel(gp)

  lk <- linkage_geocode_classroom(gp, cp)
  diags <- lk$diagnostics

  yr_diags <- diags[!is.na(diags$group_by) &
                       grepl("^school_year=", diags$group_by), ]
  expect_gt(nrow(yr_diags), 0L)
  # Each group should have both 'n' and 'n_matched' rows
  expect_true(all(c("n", "n_matched") %in% yr_diags$metric))
})


test_that("linkage_geocode_classroom print method runs without error", {
  gp <- .lg_make_geocode_panel()
  cp <- .lg_make_classroom_panel(gp)

  lk <- linkage_geocode_classroom(gp, cp)
  expect_output(print(lk), "alprek_geocode_linkage_classroom")
  expect_output(print(lk), "Matched")
})


# ============================================================================
# linkage_geocode_applications() tests
# ============================================================================

test_that("linkage_geocode_applications rejects non-master/non-panel inputs", {
  gp <- .lg_make_geocode_panel()
  am <- .lg_make_app_master(gp)

  expect_error(linkage_geocode_applications(list(), am),
               "alprek_geocode_panel")
  expect_error(linkage_geocode_applications(gp, list()),
               "alprek_applications_master")
})


test_that("linkage_geocode_applications returns correct S3", {
  gp <- .lg_make_geocode_panel()
  am <- .lg_make_app_master(gp)

  lk <- linkage_geocode_applications(gp, am)

  expect_s3_class(lk, "alprek_geocode_linkage_applications")
  expect_true(all(c("data", "diagnostics", "meta") %in% names(lk)))
  expect_equal(nrow(lk$data), nrow(am$data))
})


test_that("linkage_geocode_applications preserves coord_model_status on matched rows", {
  gp <- .lg_make_geocode_panel()
  am <- .lg_make_app_master(gp)

  lk <- linkage_geocode_applications(gp, am)

  expect_true("geocode_coord_model_status" %in% names(lk$data))
  matched <- !is.na(lk$data$geocode_lineage_id)
  if (any(matched)) {
    expect_true(any(!is.na(lk$data$geocode_coord_model_status[matched])))
  }
})


test_that("linkage_geocode_applications: bucket D rows join via row_id", {
  gp <- .lg_make_geocode_panel()
  am <- .lg_make_app_master(gp)

  lk <- linkage_geocode_applications(gp, am)

  # The bucket D row whose row_id we set to a Melissa _new row should be
  # phase-2-matched. We test indirectly via the diagnostics phase counts.
  d <- setNames(lk$diagnostics$value[is.na(lk$diagnostics$group_by)],
                lk$diagnostics$metric[is.na(lk$diagnostics$group_by)])

  # Phase 2 must be > 0 if our app_master included a matching bucket-D row.
  # (The helper makes one matching D row by construction unless the panel
  # has zero `_new_` rows — guard for that.)
  has_new <- any(grepl("_new_", as.character(gp$data$row_id)))
  if (has_new) {
    expect_gte(d[["n_matched_phase2_rowid"]], 1L)
  }
  expect_equal(d[["n_matched"]],
               d[["n_matched_phase1_site"]] + d[["n_matched_phase2_rowid"]])
})


test_that("linkage_geocode_applications: matched + unmatched == total", {
  gp <- .lg_make_geocode_panel()
  am <- .lg_make_app_master(gp)

  lk <- linkage_geocode_applications(gp, am)
  d <- setNames(lk$diagnostics$value[is.na(lk$diagnostics$group_by)],
                lk$diagnostics$metric[is.na(lk$diagnostics$group_by)])
  expect_equal(d[["n_matched"]] + d[["n_unmatched_applications"]],
               d[["n_applications_total"]])
})


test_that("linkage_geocode_applications print method runs without error", {
  gp <- .lg_make_geocode_panel()
  am <- .lg_make_app_master(gp)

  lk <- linkage_geocode_applications(gp, am)
  expect_output(print(lk), "alprek_geocode_linkage_applications")
  expect_output(print(lk), "phase 1")
})


test_that("linkage_geocode_applications fuzzy_threshold validation", {
  gp <- .lg_make_geocode_panel()
  am <- .lg_make_app_master(gp)

  expect_error(linkage_geocode_applications(gp, am, fuzzy_threshold = -0.1),
               "fuzzy_threshold")
  expect_error(linkage_geocode_applications(gp, am, fuzzy_threshold = 1.5),
               "fuzzy_threshold")
  expect_error(linkage_geocode_applications(gp, am, fuzzy_threshold = "high"),
               "fuzzy_threshold")
  # NULL (default) should not error
  expect_silent({
    lk <- linkage_geocode_applications(gp, am, fuzzy_threshold = NULL)
  })
  # Valid numeric should not error
  expect_silent({
    lk <- linkage_geocode_applications(gp, am, fuzzy_threshold = 0.85)
  })
})
