# tests/testthat/test-linkage-master-geocode.R
#
# Tests for the v0.8.0 extension of `linkage_create_master()` that wires
# the reconciled-geocode panel and the applications master into the master
# classroom-level surface. The release contract is load-bearing:
#  * the 12 prefixed `geocode_*` columns MUST be present when a geocode
#    panel is supplied,
#  * `coord_model_status` MUST round-trip as an ordered/character factor,
#  * `lineage_id` (classroom + geocode) MUST both be visible,
#  * `nrow(classroom_level)` MUST be invariant across 3-arg / 4-arg /
#    5-arg invocations (backward compatibility).

# ---------------------------------------------------------------------------
# Synthetic fixture builders
# ---------------------------------------------------------------------------

# Build a geocode panel whose (site_code, school_year) keys align with the
# classroom panel in `make_linkage_fixtures()` (default school_year =
# "2023-2024"). Anchors `cycle_year_anchor = 2024` so the 3-year synthetic
# panel covers 2022-2023, 2023-2024, 2024-2025.

.lmg_make_geocode_panel <- function(classroom_panel,
                                     n_extra_sites = 4L,
                                     seed = 20260520L) {
  cl_df <- classroom_panel$data

  # Build raw rows whose site_code/school_year matches the classroom panel.
  cl_keys <- unique(cl_df[, c("site_code", "school_year"), drop = FALSE])
  cl_keys$site_code <- as.character(cl_keys$site_code)
  cl_keys$school_year <- as.character(cl_keys$school_year)

  set.seed(seed)
  n_keys <- nrow(cl_keys)

  # AL anchors for synthesizing plausible coords near the classroom panel's
  # ADECE points.
  anchor_lat <- runif(n_keys, 30.5, 34.5)
  anchor_lng <- runif(n_keys, -88.0, -85.5)
  md_lat <- anchor_lat + runif(n_keys, -0.0003, 0.0003)
  md_lng <- anchor_lng + runif(n_keys, -0.0003, 0.0003)

  base_rows <- tibble::tibble(
    row_id          = sprintf("%s_%s",
                              cl_keys$school_year, cl_keys$site_code),
    school_year     = cl_keys$school_year,
    site_code       = cl_keys$site_code,
    site_name       = sprintf("Synthetic Site %s", cl_keys$site_code),
    geocode_address = sprintf("%d MAIN ST, AL", seq_len(n_keys)),
    site_street     = sprintf("%d MAIN ST", seq_len(n_keys)),
    site_city       = "Birmingham",
    site_state      = "AL",
    site_zip        = "35203",
    latitude        = anchor_lat,
    longitude       = anchor_lng,
    has_latlon      = TRUE,
    md_street       = sprintf("%d Main St", seq_len(n_keys)),
    md_city         = "Birmingham",
    md_state        = "AL",
    GEOZIP          = "35203",
    PLUS4           = sprintf("%04d", seq.int(1234L, by = 1L,
                                              length.out = n_keys)),
    DPB             = sprintf("%02d", seq.int(10L, by = 1L,
                                              length.out = n_keys)),
    LAT             = sprintf("%.6f", md_lat),
    LNG             = sprintf("%.6f", md_lng),
    CT              = sprintf("0%05d", seq.int(100100L, by = 1L,
                                               length.out = n_keys)),
    CENSUSBLOC      = sprintf("%04d", seq.int(1001L, by = 1L,
                                              length.out = n_keys)),
    FIPS            = "01073",
    COUNTYNAME      = "Jefferson",
    PLACENAME       = "Birmingham",
    PLACECODE       = sprintf("01%05d", seq.int(50000L, by = 1L,
                                                length.out = n_keys)),
    RESULTCODE      = "GS05",
    STATUSCODE      = "B",
    ERRORCODE       = NA_character_
  )

  # Add a few "extra" geocode rows that don't appear in the classroom panel
  # (to exercise the n_unmatched_geocode path). They use a fake site_code
  # prefix `999P9999XX`. Use the same school_year for simplicity.
  if (n_extra_sites > 0L) {
    extra <- base_rows[1L, , drop = FALSE]
    extra <- extra[rep(1L, n_extra_sites), , drop = FALSE]
    extra$site_code <- sprintf("999P9999%02d", seq_len(n_extra_sites))
    extra$row_id    <- sprintf("%s_%s", extra$school_year, extra$site_code)
    extra$site_name <- sprintf("Extra Site %s", extra$site_code)
    base_rows <- dplyr::bind_rows(base_rows, extra)
  }

  base_rows$LAT <- suppressWarnings(as.numeric(base_rows$LAT))
  base_rows$LNG <- suppressWarnings(as.numeric(base_rows$LNG))
  base_rows$site_zip <- as.character(base_rows$site_zip)
  base_rows$ERRORCODE <- as.character(base_rows$ERRORCODE)
  base_rows$raw_row_index <- seq_len(nrow(base_rows))
  base_rows$lineage_id <- paste0("lin_", seq_len(nrow(base_rows)))

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
    lineage_id       = base_rows$lineage_id,
    raw_row_index    = base_rows$raw_row_index,
    n_rows           = nrow(base_rows),
    n_rows_in        = nrow(base_rows),
    n_rows_dropped   = 0L,
    cleaned_at       = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
  cleaning_log <- tibble::tibble(rule = character(0),
                                  n_affected = integer(0),
                                  details = character(0),
                                  severity = character(0))
  clean <- structure(list(data = base_rows,
                          cleaning_log = cleaning_log,
                          meta = meta),
                     class = "alprek_geocode_clean")
  rec <- geocode_reconcile(clean)
  mst <- geocode_transform(rec)
  geocode_bind_years(mst)
}


# Build a minimal alprek_applications_master whose rows reference the
# classroom panel's classroom codes / site codes for the `target_school_year`
# (== fixtures$classroom_panel default of "2023-2024"). We deliberately
# keep this minimal: 1 renewal row (bucket A) and 1 bucket C row.

.lmg_make_app_master <- function(classroom_panel, cycle_year = "2023-2024") {
  cl_df <- classroom_panel$data
  cl_yr <- cl_df[cl_df$school_year == cycle_year, , drop = FALSE]
  stopifnot(nrow(cl_yr) >= 2L)

  apps <- tibble::tibble(
    application_id = c("app-renew-1", "app-c-1"),
    row_id         = c("app_renew_001", "app_c_001"),
    raw_row_index  = 1:2,
    lineage_id     = c("lin_app_renew_1", "lin_app_c_1"),
    source_sheet   = c("renewals", "new_apps"),
    bucket         = c("A", "C"),
    matched_classroom_code = c(cl_yr$classroom_code[1], NA_character_),
    matched_site_code      = c(cl_yr$site_code[1], cl_yr$site_code[2]),
    match_method   = c("exact", "fuzzy_auto"),
    match_score    = c(1, 0.92),
    organization_name = c("Renew Org", "New Org C"),
    project_name   = c("Pre-K Renew", "Pre-K New C"),
    county         = c("Jefferson", "Jefferson"),
    is_renewal     = c(TRUE, FALSE),
    is_new         = c(FALSE, TRUE),
    applied_this_cycle = TRUE,
    cycle_year_std = cycle_year,
    school_year_target = cycle_year,
    tier_prev_dollars = c(5610, NA_real_),
    tier_prev_rank = c(1L, NA_integer_),
    tier_prev_band = c("high", NA_character_),
    total_funding_request = c(120000, 130000),
    draft_award    = c(120000, 130000)
  )

  structure(list(
    data = apps,
    capacity_data = NULL,
    derived_log = tibble::tibble(),
    meta = list(cycle_year = cycle_year, has_capacity = FALSE)
  ), class = "alprek_applications_master")
}


# ===========================================================================
# Tests
# ===========================================================================

# ---- 1. Backward compatibility (3-arg call) -------------------------------

test_that("linkage_create_master 3-arg call still works (backward compat)", {
  fixtures <- make_linkage_fixtures(n_classrooms = 20)

  # MUST not error and MUST produce v0.7.0 schema -- no geocode_* columns.
  master3 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel
  )

  expect_s3_class(master3, "alprek_linkage_master")
  expect_equal(nrow(master3$classroom_level), nrow(fixtures$classroom_panel$data))

  # Zero geocode_* columns in pure 3-arg mode
  geocode_cols <- grep("^geocode_", names(master3$classroom_level), value = TRUE)
  expect_length(geocode_cols, 0L)

  # No app_* columns either
  app_cols <- grep("^app_", names(master3$classroom_level), value = TRUE)
  expect_length(app_cols, 0L)

  # has_geocode / has_applications meta flags reflect the call
  expect_false(isTRUE(master3$meta$has_geocode))
  expect_false(isTRUE(master3$meta$has_applications))
})


test_that("linkage_create_master 3-arg result matches v0.7.0 schema cols", {
  fixtures <- make_linkage_fixtures(n_classrooms = 10)
  master3 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel
  )

  # Spot-check the v0.7.0-era expected columns are present
  expected_v070 <- c("school_year", "classroom_code", "grand_total",
                     "per_child_budget", "n_children", "latitude", "longitude")
  for (c in expected_v070) {
    expect_true(c %in% names(master3$classroom_level),
                info = sprintf("v0.7.0 col '%s' missing", c))
  }
})


# ---- 2. 4-arg call (with geocode) -----------------------------------------

test_that("linkage_create_master 4-arg call attaches 12 prefixed geocode_* cols", {
  fixtures <- make_linkage_fixtures(n_classrooms = 10)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)

  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )

  expect_s3_class(master4, "alprek_linkage_master")
  expect_true(isTRUE(master4$meta$has_geocode))

  # All 12 prefixed columns must be present
  expected_attached <- c(
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
  cl_names <- names(master4$classroom_level)
  for (c in expected_attached) {
    expect_true(c %in% cl_names,
                info = sprintf("expected col '%s' on master classroom_level", c))
  }

  # The ADECE coords stay as their own columns (Decision §11.4)
  expect_true("latitude" %in% cl_names)
  expect_true("longitude" %in% cl_names)
})


test_that("linkage_create_master 4-arg call preserves classroom row count (no inflation)", {
  fixtures <- make_linkage_fixtures(n_classrooms = 10)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)

  master3 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel
  )
  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )

  expect_equal(nrow(master4$classroom_level),
               nrow(master3$classroom_level))
  expect_equal(nrow(master4$classroom_level),
               nrow(fixtures$classroom_panel$data))
})


test_that("linkage_create_master 4-arg coord_model_status preserved as factor", {
  fixtures <- make_linkage_fixtures(n_classrooms = 10)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)

  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )

  expect_true("geocode_coord_model_status" %in% names(master4$classroom_level))
  vals <- master4$classroom_level$geocode_coord_model_status
  # Matched rows must have a factor value (possibly with NAs on unmatched)
  expect_true(is.factor(vals))

  valid_levels <- c("missing", "not_model_ready",
                    "provisional_followup", "model_ready")
  obs_levels <- levels(vals)
  expect_true(all(obs_levels %in% valid_levels))

  # At least one matched row should have a non-NA model status
  non_na <- !is.na(vals)
  expect_gt(sum(non_na), 0L)
})


test_that("linkage_create_master 4-arg preserves both lineage_ids (classroom + geocode)", {
  fixtures <- make_linkage_fixtures(n_classrooms = 10)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)

  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )

  cl <- master4$classroom_level
  # geocode-side lineage visible
  expect_true("geocode_lineage_id" %in% names(cl))
  matched_geo <- !is.na(cl$geocode_lineage_id)
  expect_gt(sum(matched_geo), 0L)
  # Geocode lineage values originate from the geocode panel
  gp_lineages <- as.character(gp$data$lineage_id)
  expect_true(all(cl$geocode_lineage_id[matched_geo] %in% gp_lineages))
})


# ---- 3. diagnostics$geocode_coverage --------------------------------------

test_that("linkage_create_master diagnostics$geocode_coverage tibble present + populated", {
  fixtures <- make_linkage_fixtures(n_classrooms = 10)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)

  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )

  cov <- master4$diagnostics$geocode_coverage
  expect_true(is.list(cov))

  expected_metrics <- c("n_classroom_total", "n_classroom_with_coord",
                        "n_needing_followup", "n_model_ready",
                        "pct_with_coord", "pct_followup",
                        "pct_model_ready")
  for (m in expected_metrics) {
    expect_true(m %in% names(cov),
                info = sprintf("expected geocode_coverage metric '%s'", m))
  }

  expect_equal(cov$n_classroom_total, nrow(master4$classroom_level))
  expect_true(cov$n_classroom_with_coord >= 0L)
  expect_true(cov$n_needing_followup >= 0L)
  expect_true(is.numeric(cov$pct_with_coord))
  expect_true(cov$pct_with_coord >= 0 && cov$pct_with_coord <= 100)

  # 3-arg call's geocode_coverage should be the empty shell
  master3 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel
  )
  cov3 <- master3$diagnostics$geocode_coverage
  expect_equal(cov3$n_classroom_total, 0L)
  expect_equal(cov3$n_classroom_with_coord, 0L)
})


# ---- 4. Print method shows geocode coverage line --------------------------

test_that("print.alprek_linkage_master shows geocode coverage line when geocode supplied", {
  fixtures <- make_linkage_fixtures(n_classrooms = 10)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)

  master3 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel
  )
  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )

  # 3-arg print must NOT mention geocode
  out3 <- capture.output(print(master3))
  expect_false(any(grepl("Geocode coverage", out3)))

  # 4-arg print SHOULD mention geocode coverage
  out4 <- capture.output(print(master4))
  expect_true(any(grepl("Geocode coverage", out4)),
              info = "Expected 'Geocode coverage' line in 4-arg print output")
  # "model_ready coord" phrase appears
  expect_true(any(grepl("model_ready coord", out4)))
})


# ---- 5. 5-arg call (with geocode + applications) --------------------------

test_that("linkage_create_master 5-arg call wires applications context", {
  fixtures <- make_linkage_fixtures(n_classrooms = 10)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)
  am <- .lmg_make_app_master(fixtures$classroom_panel,
                              cycle_year = "2023-2024")

  master5 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode      = gp,
    applications = am
  )

  expect_s3_class(master5, "alprek_linkage_master")
  expect_true(isTRUE(master5$meta$has_geocode))
  expect_true(isTRUE(master5$meta$has_applications))

  # Some applications-context columns should be present (v0.7.0 applications
  # linkage prefixes with `app_` and adds `site_n_new_apps`).
  app_or_site_cols <- grep("^(app_|site_n_new_apps$)",
                            names(master5$classroom_level), value = TRUE)
  expect_gt(length(app_or_site_cols), 0L)

  # `app_applied_this_cycle` SHOULD exist and be logical
  expect_true("app_applied_this_cycle" %in% names(master5$classroom_level))
  expect_true(is.logical(master5$classroom_level$app_applied_this_cycle))

  # At least one TRUE expected from our renewal row
  expect_gte(sum(master5$classroom_level$app_applied_this_cycle, na.rm = TRUE),
             1L)

  # Geocode cols still present too
  expect_true("geocode_lat_final" %in% names(master5$classroom_level))
  expect_true("geocode_coord_model_status" %in% names(master5$classroom_level))
})


# ---- 6. Row-count invariant (3 vs 4 vs 5 arg) -----------------------------

test_that("linkage_create_master row count invariant: 3-arg == 4-arg == 5-arg", {
  fixtures <- make_linkage_fixtures(n_classrooms = 10)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)
  am <- .lmg_make_app_master(fixtures$classroom_panel,
                              cycle_year = "2023-2024")

  master3 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel
  )
  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )
  master5 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode      = gp,
    applications = am
  )

  n3 <- nrow(master3$classroom_level)
  n4 <- nrow(master4$classroom_level)
  n5 <- nrow(master5$classroom_level)
  expect_equal(n3, n4)
  expect_equal(n4, n5)

  # Student-level also invariant (geocode + apps only touch classroom_level)
  expect_equal(nrow(master3$student_level), nrow(master4$student_level))
  expect_equal(nrow(master4$student_level), nrow(master5$student_level))
})


# ---- 7. Argument validation -----------------------------------------------

test_that("linkage_create_master rejects non-panel geocode argument", {
  fixtures <- make_linkage_fixtures(n_classrooms = 5)
  expect_error(
    linkage_create_master(
      fixtures$budget_panel,
      fixtures$classroom_panel,
      fixtures$student_panel,
      geocode = list()  # not an alprek_geocode_panel
    ),
    "alprek_geocode_panel"
  )
  expect_error(
    linkage_create_master(
      fixtures$budget_panel,
      fixtures$classroom_panel,
      fixtures$student_panel,
      geocode = "not_a_panel"
    ),
    "alprek_geocode_panel"
  )
})


test_that("linkage_create_master rejects non-master applications argument", {
  fixtures <- make_linkage_fixtures(n_classrooms = 5)
  expect_error(
    linkage_create_master(
      fixtures$budget_panel,
      fixtures$classroom_panel,
      fixtures$student_panel,
      applications = list()
    ),
    "alprek_applications_master"
  )
})


# ---- 8. Match-rate diagnostics surfaced into master ----------------------

test_that("linkage_create_master diagnostics$geocode_linkage carries the link metrics", {
  fixtures <- make_linkage_fixtures(n_classrooms = 10)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)

  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )

  link_diag <- master4$diagnostics$geocode_linkage
  expect_true(is.data.frame(link_diag))
  expect_true(all(c("metric", "value") %in% names(link_diag)))

  overall <- setNames(link_diag$value[is.na(link_diag$group_by)],
                      link_diag$metric[is.na(link_diag$group_by)])
  # Required metrics from the underlying linkage_geocode_classroom()
  expect_true(all(c("n_classroom_total", "n_matched") %in% names(overall)))
  expect_equal(overall[["n_classroom_total"]], nrow(fixtures$classroom_panel$data))
})


# ---- 9. coord_model_status status pct sanity -------------------------------

test_that("linkage_coverage_geocode pct math sums to ~100 across observed statuses", {
  fixtures <- make_linkage_fixtures(n_classrooms = 10)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)

  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )

  cov <- master4$diagnostics$geocode_coverage
  # by_coord_model_status (if present) should have pct values that sum to
  # less than or equal to 100 (NAs don't contribute)
  if (!is.null(cov$by_coord_model_status) &&
      nrow(cov$by_coord_model_status) > 0L) {
    expect_true(sum(cov$by_coord_model_status$pct) <= 100 + 1e-6)
    expect_true(all(cov$by_coord_model_status$n >= 0L))
  }
})


# ---- 10. linkage_summary_stats still works with master from geocode -------

test_that("linkage_summary_stats works on geocode-augmented master", {
  fixtures <- make_linkage_fixtures(n_classrooms = 10)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)

  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )

  stats <- linkage_summary_stats(master4)
  expect_true(is.data.frame(stats))
  expect_true("n" %in% names(stats))
})


# ===========================================================================
# Step 6.4: linkage_validate() geocode-specific checks
# ===========================================================================
#
# These tests cover the five new geocode checks plugged into the existing
# `linkage_validate()` framework:
#   1. geocode_coverage_classroom    (WARN if < 95% coord coverage)
#   2. followup_reason_completeness  (ERROR if reason missing)
#   3. county_check_agreement        (WARN if < 95% agreement; skips OK)
#   4. new_site_followup_visibility  (INFO surfacing bucket-D rows)
#   5. model_ready_threshold         (WARN if < 70% model-ready)


# ---- 11. Backward compatibility (3-arg master) --------------------------

test_that("linkage_validate gracefully skips geocode checks on 3-arg master", {
  fixtures <- make_linkage_fixtures(n_classrooms = 20)
  master3 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel
  )
  val <- linkage_validate(master3)

  expect_s3_class(val, "alprek_linkage_validation")
  checks <- val$checks
  geocode_checks <- c("geocode_coverage_classroom",
                      "followup_reason_completeness",
                      "county_check_agreement",
                      "new_site_followup_visibility",
                      "model_ready_threshold")
  expect_false(any(checks$check_name %in% geocode_checks),
               info = "3-arg master should not produce any geocode check rows")
})


test_that("linkage_validate skips geocode checks on classroom/student linkages", {
  fixtures <- make_linkage_fixtures(n_classrooms = 20)
  cb <- linkage_classroom_budget(fixtures$classroom_panel,
                                  fixtures$budget_panel)
  val_cb <- linkage_validate(cb)
  expect_s3_class(val_cb, "alprek_linkage_validation")
  expect_false(any(val_cb$checks$check_name == "geocode_coverage_classroom"))

  sc <- linkage_student_classroom(fixtures$student_panel,
                                   fixtures$classroom_panel)
  val_sc <- linkage_validate(sc)
  expect_s3_class(val_sc, "alprek_linkage_validation")
  expect_false(any(val_sc$checks$check_name == "model_ready_threshold"))
})


# ---- 12. Geocode-extended master surfaces all expected check rows --------

test_that("linkage_validate on 4-arg master surfaces geocode-specific checks", {
  fixtures <- make_linkage_fixtures(n_classrooms = 20)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)

  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )
  val <- linkage_validate(master4)
  expect_s3_class(val, "alprek_linkage_validation")

  # The three universal geocode checks (coverage, followup, model_ready)
  # must always fire when geocode_* cols are present.
  for (nm in c("geocode_coverage_classroom",
               "followup_reason_completeness",
               "model_ready_threshold")) {
    expect_true(any(val$checks$check_name == nm),
                info = sprintf("expected check '%s' in 4-arg master validate", nm))
  }
})


test_that("linkage_validate 4-arg coverage check passes on clean fixture", {
  # Clean fixture: every classroom_panel site is covered by the geocode panel.
  fixtures <- make_linkage_fixtures(n_classrooms = 20)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)
  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )
  val <- linkage_validate(master4)

  coverage_row <- val$checks[val$checks$check_name == "geocode_coverage_classroom", ]
  expect_equal(nrow(coverage_row), 1L)
  expect_equal(coverage_row$status, "PASS",
               info = "Clean synthetic geocode fixture must give >= 95% coverage")

  followup_row <- val$checks[val$checks$check_name == "followup_reason_completeness", ]
  expect_equal(nrow(followup_row), 1L)
  # Synthetic reconciler should never produce followup-without-reason.
  expect_true(followup_row$status %in% c("PASS"),
              info = "Clean reconciler must not yield followup rows missing reason")
})


# ---- 13. Each check trips on its synthetic fixture -----------------------

test_that("geocode_coverage_classroom WARNs when coord coverage < 95%", {
  # Strategy: build a 4-arg master, then directly mutate classroom_level so
  # >= 6% of geocode_lat_final values become NA. This bypasses the
  # row-inflation guard inside linkage_create_master() since we mutate
  # AFTER the master has been built.
  fixtures <- make_linkage_fixtures(n_classrooms = 20)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)
  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )
  # Knock out ~30% of the geocode coords (way below 95% coverage).
  n <- nrow(master4$classroom_level)
  n_zap <- max(2L, floor(n * 0.30))
  master4$classroom_level$geocode_lat_final[seq_len(n_zap)] <- NA_real_

  val <- linkage_validate(master4)
  coverage_row <- val$checks[val$checks$check_name == "geocode_coverage_classroom", ]
  expect_equal(nrow(coverage_row), 1L)
  expect_equal(coverage_row$status, "WARN")
  expect_match(coverage_row$details, "Coverage:")
})


test_that("followup_reason_completeness ERRORs when reason is missing", {
  fixtures <- make_linkage_fixtures(n_classrooms = 20)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)
  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )
  # Construct an explicit violation: one row needs followup but has NA reason.
  cl <- master4$classroom_level
  if (is.factor(cl$geocode_followup_reason)) {
    cl$geocode_followup_reason <- as.character(cl$geocode_followup_reason)
  }
  cl$geocode_needs_followup_geocoding[1L] <- TRUE
  cl$geocode_followup_reason[1L]           <- NA_character_
  master4$classroom_level <- cl

  val <- linkage_validate(master4)
  fu_row <- val$checks[val$checks$check_name == "followup_reason_completeness", ]
  expect_equal(nrow(fu_row), 1L)
  expect_equal(fu_row$status, "ERROR")
  expect_equal(as.integer(fu_row$n_issues), 1L)
})


test_that("model_ready_threshold WARNs when model-ready pct < 70%", {
  # Force the coverage rollup into a low model-ready scenario.
  fixtures <- make_linkage_fixtures(n_classrooms = 20)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)
  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )
  # Override the geocode coverage rollup to simulate a sparsely model-ready
  # delivery (50% model-ready).
  master4$diagnostics$geocode_coverage$n_model_ready <-
    as.integer(master4$diagnostics$geocode_coverage$n_classroom_total / 2L)
  master4$diagnostics$geocode_coverage$pct_model_ready <- 50.0

  val <- linkage_validate(master4)
  mr_row <- val$checks[val$checks$check_name == "model_ready_threshold", ]
  expect_equal(nrow(mr_row), 1L)
  expect_equal(mr_row$status, "WARN")
  expect_match(mr_row$details, "Model-ready: 50.0%")
})


test_that("county_check_agreement gracefully skips when no county source", {
  # The synthetic 4-arg master should have NO Melissa-vs-classroom county
  # comparison materialized -> the county check should simply NOT appear.
  fixtures <- make_linkage_fixtures(n_classrooms = 20)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)
  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )

  val <- linkage_validate(master4)
  # county_check_agreement is OPTIONAL. The graceful skip means no row.
  expect_false(any(val$checks$check_name == "county_check_agreement"),
               info = "No county column attached => check should not appear")
})


test_that("county_check_agreement WARNs when agreement < 95%", {
  # Inject a county comparison column directly. Choose 80% match rate so
  # the check resolves to WARN.
  fixtures <- make_linkage_fixtures(n_classrooms = 20)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)
  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )
  n <- nrow(master4$classroom_level)
  ag <- rep(TRUE, n)
  n_mismatch <- max(1L, floor(n * 0.20))  # 20% mismatch => 80% agreement
  ag[seq_len(n_mismatch)] <- FALSE
  master4$classroom_level$geocode_county_check_match <- ag

  val <- linkage_validate(master4)
  ag_row <- val$checks[val$checks$check_name == "county_check_agreement", ]
  expect_equal(nrow(ag_row), 1L)
  expect_equal(ag_row$status, "WARN")
  expect_match(ag_row$details, "Agreement: 80.0%")
})


test_that("county_check_agreement PASSes at >= 95% via string pair", {
  # Materialize the alternative source: a Melissa-side string + classroom
  # county_name match, with > 95% agreement.
  fixtures <- make_linkage_fixtures(n_classrooms = 20)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)
  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )
  # Inject perfect match: melissa_county_name == classroom-side county_name.
  master4$classroom_level$melissa_county_name <-
    master4$classroom_level$county_name

  val <- linkage_validate(master4)
  ag_row <- val$checks[val$checks$check_name == "county_check_agreement", ]
  expect_equal(nrow(ag_row), 1L)
  expect_equal(ag_row$status, "PASS")
  expect_match(ag_row$details, "Agreement: 100.0%")
})


# ---- 14. new_site_followup_visibility INFO ------------------------------

test_that("new_site_followup_visibility fires only when applications branch active", {
  fixtures <- make_linkage_fixtures(n_classrooms = 20)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)
  am <- .lmg_make_app_master(fixtures$classroom_panel,
                              cycle_year = "2023-2024")

  # 4-arg (no apps) -> visibility check should NOT appear.
  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )
  val4 <- linkage_validate(master4)
  expect_false(any(val4$checks$check_name == "new_site_followup_visibility"))

  # 5-arg (with apps) -> visibility check MUST appear with INFO status.
  master5 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode      = gp,
    applications = am
  )
  val5 <- linkage_validate(master5)
  vis_row <- val5$checks[val5$checks$check_name == "new_site_followup_visibility", ]
  expect_equal(nrow(vis_row), 1L)
  expect_equal(vis_row$status, "INFO")
})


# ---- 15. Severity ladder rolls up into n_errors / n_warnings / n_info ----

test_that("linkage_validate severity ladder rolls up the new geocode checks", {
  fixtures <- make_linkage_fixtures(n_classrooms = 20)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)
  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )
  # Make followup_reason_completeness trip ERROR + coverage_classroom WARN.
  cl <- master4$classroom_level
  if (is.factor(cl$geocode_followup_reason)) {
    cl$geocode_followup_reason <- as.character(cl$geocode_followup_reason)
  }
  cl$geocode_needs_followup_geocoding[1L] <- TRUE
  cl$geocode_followup_reason[1L]           <- NA_character_

  n <- nrow(cl)
  n_zap <- max(2L, floor(n * 0.30))
  cl$geocode_lat_final[seq_len(n_zap)] <- NA_real_
  master4$classroom_level <- cl

  val <- linkage_validate(master4)
  expect_gte(val$n_errors, 1L)
  expect_gte(val$n_warnings, 1L)
  expect_false(val$passed)
})


# ---- 16. Custom thresholds via linkage_validate() arguments -------------

test_that("linkage_validate honors custom geocode_coverage_min / model_ready_min", {
  fixtures <- make_linkage_fixtures(n_classrooms = 20)
  gp <- .lmg_make_geocode_panel(fixtures$classroom_panel)
  master4 <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel,
    geocode = gp
  )
  # Drop coverage to 80% (would WARN under default 95% threshold).
  n <- nrow(master4$classroom_level)
  n_zap <- floor(n * 0.20)
  master4$classroom_level$geocode_lat_final[seq_len(n_zap)] <- NA_real_

  # With default threshold (0.95): expect WARN
  val_default <- linkage_validate(master4)
  cov_def <- val_default$checks[val_default$checks$check_name == "geocode_coverage_classroom", ]
  expect_equal(cov_def$status, "WARN")

  # With lowered threshold (0.70): expect PASS
  val_lo <- linkage_validate(master4, geocode_coverage_min = 0.70)
  cov_lo <- val_lo$checks[val_lo$checks$check_name == "geocode_coverage_classroom", ]
  expect_equal(cov_lo$status, "PASS")
})
