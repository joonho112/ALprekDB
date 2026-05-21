# tests/testthat/test-geocode-followup.R
#
# Tests for geocode_followup_queue(): the consumer-facing helper that
# surfaces sites needing follow-up geocoding from a reconciled object.
#
# Coverage strategy:
#   - Argument validation (non-reconciled input, non-logical
#     include_disputed).
#   - Return type (tibble, not S3).
#   - Filter: all returned rows have needs_followup_geocoding == TRUE in
#     the upstream reconciled object.
#   - Sort: school_year DESC, then distance DESC, NAs in distance to end.
#   - include_disputed = FALSE excludes lat_source == "disputed_melissa".
#   - include_disputed = TRUE (default) preserves disputed rows.
#   - suggested_action: character, all values in controlled vocab,
#     mapping is correct for every documented followup_reason.
#   - Column set + order matches spec exactly.
#   - Empty input (0 followup rows) returns 0-row tibble with same
#     schema and dtypes.
#
# Fixtures reuse the .row_decision() / .wrap_clean() helpers loaded by
# test-geocode-reconcile.R via testthat's source pre-loading of files in
# the same directory; we re-define minimal local helpers here to keep
# this file standalone and order-independent.


# ---------------------------------------------------------------------------
# Local helpers (mirror those in test-geocode-reconcile.R)
# ---------------------------------------------------------------------------

.wrap_clean_fq <- function(df) {
  df <- tibble::as_tibble(df)
  if ("LAT" %in% names(df) && !is.numeric(df$LAT)) {
    df$LAT <- suppressWarnings(as.numeric(as.character(df$LAT)))
  }
  if ("LNG" %in% names(df) && !is.numeric(df$LNG)) {
    df$LNG <- suppressWarnings(as.numeric(as.character(df$LNG)))
  }
  if ("ERRORCODE" %in% names(df) && !is.character(df$ERRORCODE)) {
    df$ERRORCODE <- as.character(df$ERRORCODE)
  }
  if ("site_zip" %in% names(df) && !is.character(df$site_zip)) {
    df$site_zip <- as.character(df$site_zip)
  }
  if (!"raw_row_index" %in% names(df)) df$raw_row_index <- seq_len(nrow(df))
  if (!"lineage_id" %in% names(df)) df$lineage_id <- paste0("lin_", seq_len(nrow(df)))

  meta <- list(
    path             = "/tmp/fake-melissa.xlsx",
    sheet            = "Sheet1",
    source           = "melissa",
    cycle_year       = "2024-2025",
    receipt_date     = "2026-03-04",
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
  structure(list(data = df, cleaning_log = cleaning_log, meta = meta),
            class = "alprek_geocode_clean")
}


.row_decision_fq <- function(adece_lat = NA_real_, adece_lng = NA_real_,
                              melissa_lat = NA_real_, melissa_lng = NA_real_,
                              result_code = "GS05",
                              site_code = "999P000001",
                              school_year = "2024-2025") {
  tibble::tibble(
    row_id          = sprintf("%s_%s", school_year, site_code),
    school_year     = school_year,
    site_name       = "Test Site",
    site_code       = site_code,
    geocode_address = "100 MAIN ST, Birmingham, AL, 35203",
    site_street     = "100 MAIN ST",
    site_city       = "Birmingham",
    site_state      = "AL",
    site_zip        = "35203",
    latitude        = adece_lat,
    longitude       = adece_lng,
    has_latlon      = !is.na(adece_lat),
    md_street       = "100 Main St",
    md_city         = "Birmingham",
    md_state        = "AL",
    GEOZIP          = "35203",
    PLUS4           = "1234",
    DPB             = "10",
    LAT             = if (is.na(melissa_lat)) NA_real_
                       else as.numeric(melissa_lat),
    LNG             = if (is.na(melissa_lng)) NA_real_
                       else as.numeric(melissa_lng),
    CT              = "0100100",
    CENSUSBLOC      = "1001",
    FIPS            = "01073",
    COUNTYNAME      = "Jefferson",
    PLACENAME       = "Birmingham",
    PLACECODE       = "0150000",
    RESULTCODE      = result_code,
    STATUSCODE      = "B",
    ERRORCODE       = NA_character_
  )
}


.lng_offset_for_m_fq <- function(meters, lat_deg) {
  R <- 6371000
  d_rad <- meters / R
  dlon <- d_rad / cos(lat_deg * pi / 180)
  dlon * 180 / pi
}


# Build a multi-row reconciled fixture that exercises a mix of cells:
#  - D4 (GS05 within 250m): no followup
#  - D5 (GS05 500m): followup, disagreement_above_threshold
#  - D6 (GS05 30km): followup + disputed_melissa, disagreement_gross
#  - D10 (GS03 both): followup + disputed_melissa, resultcode_gs03_always_flag
#  - D11 (ADECE-only): followup, melissa_unexpectedly_missing
#  - D13 (Melissa-only GS06): followup, melissa_only_interpolated
#  - D14 (Melissa-only GS03): followup, melissa_only_gs03
#  - D15 (both missing): followup, both_missing
.mixed_reconciled_fixture <- function() {
  d500   <- .lng_offset_for_m_fq(500, 33.5)
  d30000 <- .lng_offset_for_m_fq(30000, 33.5)
  rows <- dplyr::bind_rows(
    # D4: ok, no followup (year 2025-2026)
    .row_decision_fq(adece_lat = 33.5, adece_lng = -86.8,
                      melissa_lat = 33.5, melissa_lng = -86.8,
                      result_code = "GS05",
                      site_code = "999P000010",
                      school_year = "2025-2026"),
    # D5: followup, disagreement_above_threshold, distance ~500m
    # (year 2024-2025)
    .row_decision_fq(adece_lat = 33.5, adece_lng = -86.8,
                      melissa_lat = 33.5, melissa_lng = -86.8 + d500,
                      result_code = "GS05",
                      site_code = "999P000020",
                      school_year = "2024-2025"),
    # D6: followup + disputed_melissa, disagreement_gross, ~30km
    # (year 2025-2026)
    .row_decision_fq(adece_lat = 33.5, adece_lng = -86.8,
                      melissa_lat = 33.5, melissa_lng = -86.8 + d30000,
                      result_code = "GS05",
                      site_code = "999P000030",
                      school_year = "2025-2026"),
    # D10: followup + disputed_melissa, GS03 always flag
    # (year 2024-2025)
    .row_decision_fq(adece_lat = 33.5, adece_lng = -86.8,
                      melissa_lat = 33.5, melissa_lng = -86.8,
                      result_code = "GS03",
                      site_code = "999P000040",
                      school_year = "2024-2025"),
    # D11: ADECE-only (NA distance)
    .row_decision_fq(adece_lat = 33.5, adece_lng = -86.8,
                      melissa_lat = NA_real_, melissa_lng = NA_real_,
                      result_code = NA_character_,
                      site_code = "999P000050",
                      school_year = "2025-2026"),
    # D13: Melissa-only GS06
    .row_decision_fq(adece_lat = NA_real_, adece_lng = NA_real_,
                      melissa_lat = 33.5, melissa_lng = -86.8,
                      result_code = "GS06",
                      site_code = "999P000060",
                      school_year = "2024-2025"),
    # D14: Melissa-only GS03
    .row_decision_fq(adece_lat = NA_real_, adece_lng = NA_real_,
                      melissa_lat = 33.5, melissa_lng = -86.8,
                      result_code = "GS03",
                      site_code = "999P000070",
                      school_year = "2024-2025"),
    # D15: both missing
    .row_decision_fq(adece_lat = NA_real_, adece_lng = NA_real_,
                      melissa_lat = NA_real_, melissa_lng = NA_real_,
                      result_code = NA_character_,
                      site_code = "999P000080",
                      school_year = "2025-2026")
  )
  geocode_reconcile(.wrap_clean_fq(rows))
}


# ===========================================================================
# Argument validation
# ===========================================================================
test_that("geocode_followup_queue() rejects non-reconciled input", {
  expect_error(geocode_followup_queue(list()),
               regexp = "alprek_geocode_reconciled")
  expect_error(geocode_followup_queue(tibble::tibble()),
               regexp = "alprek_geocode_reconciled")
})


test_that("geocode_followup_queue() rejects bad include_disputed", {
  rec <- .mixed_reconciled_fixture()
  expect_error(geocode_followup_queue(rec, include_disputed = NA),
               regexp = "include_disputed")
  expect_error(geocode_followup_queue(rec, include_disputed = "yes"),
               regexp = "include_disputed")
  expect_error(geocode_followup_queue(rec,
                                       include_disputed = c(TRUE, FALSE)),
               regexp = "include_disputed")
})


# ===========================================================================
# Return type
# ===========================================================================
test_that("geocode_followup_queue() returns a tibble (not S3)", {
  rec <- .mixed_reconciled_fixture()
  fq  <- geocode_followup_queue(rec)
  expect_s3_class(fq, "tbl_df")
  expect_false(inherits(fq, "alprek_geocode_followup_queue"))
})


# ===========================================================================
# Column set and order
# ===========================================================================
test_that("output columns match the spec exactly, in order", {
  rec <- .mixed_reconciled_fixture()
  fq  <- geocode_followup_queue(rec)
  expected <- c(
    "lineage_id", "row_id", "school_year", "site_code", "site_name",
    "site_street", "site_city", "site_state", "site_zip",
    "lat_source", "coord_agreement_band", "distance_adece_melissa_m",
    "melissa_result_code", "lat_precision", "followup_reason",
    "suggested_action"
  )
  expect_identical(names(fq), expected)
  expect_true(all(fq$lineage_id %in% rec$data$lineage_id))
})


# ===========================================================================
# Filter: all returned rows correspond to needs_followup_geocoding == TRUE
# ===========================================================================
test_that("every returned row was flagged needs_followup in reconciled$data", {
  rec <- .mixed_reconciled_fixture()
  fq  <- geocode_followup_queue(rec)
  # cross-reference by row_id back to the reconciled object
  flagged_ids <- as.character(rec$data$row_id[
    as.logical(rec$data$needs_followup_geocoding)
  ])
  expect_true(all(fq$row_id %in% flagged_ids))
  # And nothing from the unflagged set should appear
  unflagged_ids <- as.character(rec$data$row_id[
    !as.logical(rec$data$needs_followup_geocoding)
  ])
  expect_false(any(fq$row_id %in% unflagged_ids))
})


# ===========================================================================
# Sort: school_year DESC, then distance DESC, NAs in distance at the end
# ===========================================================================
test_that("rows are sorted by school_year DESC then distance DESC", {
  rec <- .mixed_reconciled_fixture()
  fq  <- geocode_followup_queue(rec)

  # 1) school_year is in non-increasing order
  sy_levels <- sort(unique(fq$school_year), na.last = TRUE)
  sy_rank   <- match(fq$school_year, sy_levels)
  expect_true(all(diff(sy_rank) <= 0L))

  # 2) Within each school_year, distance is non-increasing, with NAs at end
  for (sy in unique(fq$school_year)) {
    sub <- fq$distance_adece_melissa_m[fq$school_year == sy]
    if (length(sub) >= 2L) {
      # All non-NA values appear before any NAs
      non_na <- !is.na(sub)
      if (any(non_na) && any(!non_na)) {
        first_na_pos <- which(!non_na)[1]
        last_val_pos <- max(which(non_na))
        expect_true(last_val_pos < first_na_pos)
      }
      # Non-NA values are non-increasing
      non_na_vals <- sub[non_na]
      if (length(non_na_vals) >= 2L) {
        expect_true(all(diff(non_na_vals) <= 0))
      }
    }
  }
})


test_that("NA in distance sorts to the end within school_year", {
  # Build a reconciled object whose followup rows in the same school_year
  # include both finite distances and NA distances.
  d500 <- .lng_offset_for_m_fq(500, 33.5)
  rows <- dplyr::bind_rows(
    # D5 (followup, finite distance ~500m)
    .row_decision_fq(adece_lat = 33.5, adece_lng = -86.8,
                      melissa_lat = 33.5, melissa_lng = -86.8 + d500,
                      result_code = "GS05",
                      site_code = "999P000001",
                      school_year = "2024-2025"),
    # D11 (followup, ADECE-only, NA distance)
    .row_decision_fq(adece_lat = 33.5, adece_lng = -86.8,
                      melissa_lat = NA_real_, melissa_lng = NA_real_,
                      result_code = NA_character_,
                      site_code = "999P000002",
                      school_year = "2024-2025")
  )
  rec <- geocode_reconcile(.wrap_clean_fq(rows))
  fq  <- geocode_followup_queue(rec)
  expect_equal(nrow(fq), 2L)
  # First row should be the finite-distance one (D5);
  # second should be the NA-distance one (D11).
  expect_false(is.na(fq$distance_adece_melissa_m[1]))
  expect_true(is.na(fq$distance_adece_melissa_m[2]))
})


# ===========================================================================
# include_disputed parameter
# ===========================================================================
test_that("default invocation includes lat_source == 'disputed_melissa'", {
  rec <- .mixed_reconciled_fixture()
  fq  <- geocode_followup_queue(rec)
  expect_true("disputed_melissa" %in% fq$lat_source)
})


test_that("include_disputed = FALSE excludes lat_source == 'disputed_melissa'", {
  rec <- .mixed_reconciled_fixture()
  fq  <- geocode_followup_queue(rec, include_disputed = FALSE)
  expect_false("disputed_melissa" %in% fq$lat_source)
  # But other flagged rows are still present
  expect_true(nrow(fq) > 0L)
})


# ===========================================================================
# suggested_action: type, vocab, and mapping correctness
# ===========================================================================
test_that("suggested_action is character with values in controlled vocab", {
  rec <- .mixed_reconciled_fixture()
  fq  <- geocode_followup_queue(rec)
  expect_type(fq$suggested_action, "character")
  vocab <- c(
    "manual_source_adjudication",
    "verify_adece_address_and_request_recheck",
    "request_melissa_geocode",
    "obtain_coord",
    "request_higher_precision_geocode",
    "manual_review_gs03",
    "manual_review"
  )
  expect_true(all(fq$suggested_action %in% vocab))
})


test_that("suggested_action mapping is correct for every followup_reason", {
  rec <- .mixed_reconciled_fixture()
  fq  <- geocode_followup_queue(rec)

  # Build the reason -> action lookup table from spec.
  expected_map <- c(
    "disagreement_above_threshold" = "manual_source_adjudication",
    "disagreement_gross"           = "verify_adece_address_and_request_recheck",
    "both_missing"                 = "obtain_coord",
    "melissa_unexpectedly_missing" = "request_melissa_geocode",
    "melissa_only_interpolated"    = "request_higher_precision_geocode",
    "melissa_only_gs03"            = "request_higher_precision_geocode",
    "resultcode_gs03_always_flag"  = "manual_review_gs03"
  )

  # Every row's (reason -> action) must obey the mapping.
  for (i in seq_len(nrow(fq))) {
    reason <- fq$followup_reason[i]
    action <- fq$suggested_action[i]
    if (is.na(reason) || !(reason %in% names(expected_map))) {
      expect_equal(action, "manual_review")
    } else {
      expect_equal(action, unname(expected_map[reason]))
    }
  }
})


test_that("followup queue carries internal address privacy metadata", {
  rec <- .mixed_reconciled_fixture()
  fq <- geocode_followup_queue(rec)

  expect_equal(attr(fq, "privacy_level"), "internal_address_followup")
  expect_true(isTRUE(attr(fq, "contains_address_fields")))
})


test_that("unknown / NA followup_reason maps to 'manual_review'", {
  # Build a manual reconciled object where followup_reason is NA but
  # needs_followup is TRUE -- not natural via the reconciler, but the
  # helper must still cope.
  rec <- .mixed_reconciled_fixture()
  # Force one row to have NA followup_reason while keeping needs_followup
  # TRUE.
  fu_idx <- which(rec$data$needs_followup_geocoding)[1]
  rec$data$followup_reason <- as.character(rec$data$followup_reason)
  rec$data$followup_reason[fu_idx] <- NA_character_

  fq <- geocode_followup_queue(rec)
  na_rows <- fq[is.na(fq$followup_reason), ]
  if (nrow(na_rows) >= 1L) {
    expect_true(all(na_rows$suggested_action == "manual_review"))
  }
})


# ===========================================================================
# Empty / no-followup case
# ===========================================================================
test_that("zero followup rows returns 0-row tibble with same schema", {
  # A clean dataset with only D4 (within-threshold) rows -> no followup.
  rows <- dplyr::bind_rows(
    .row_decision_fq(adece_lat = 33.5, adece_lng = -86.8,
                      melissa_lat = 33.5, melissa_lng = -86.8,
                      result_code = "GS05",
                      site_code = "999P000001"),
    .row_decision_fq(adece_lat = 32.4, adece_lng = -86.3,
                      melissa_lat = 32.4, melissa_lng = -86.3,
                      result_code = "GS01",
                      site_code = "999P000002")
  )
  rec <- geocode_reconcile(.wrap_clean_fq(rows))
  expect_false(any(rec$data$needs_followup_geocoding))

  fq <- geocode_followup_queue(rec)
  expect_s3_class(fq, "tbl_df")
  expect_equal(nrow(fq), 0L)
  expected <- c(
    "lineage_id", "row_id", "school_year", "site_code", "site_name",
    "site_street", "site_city", "site_state", "site_zip",
    "lat_source", "coord_agreement_band", "distance_adece_melissa_m",
    "melissa_result_code", "lat_precision", "followup_reason",
    "suggested_action"
  )
  expect_identical(names(fq), expected)
  expect_type(fq$distance_adece_melissa_m, "double")
  expect_type(fq$row_id, "character")
  expect_type(fq$lineage_id, "character")
  expect_equal(attr(fq, "privacy_level"), "internal_address_followup")
  expect_true(isTRUE(attr(fq, "contains_address_fields")))
})


test_that("all-disputed followup with include_disputed=FALSE -> 0-row tibble", {
  # Just a single GS03+both row -> D10 only -> all followup rows are
  # disputed; with include_disputed=FALSE the queue is empty.
  rows <- .row_decision_fq(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8,
    result_code = "GS03")
  rec <- geocode_reconcile(.wrap_clean_fq(rows))
  expect_true(rec$data$needs_followup_geocoding[1])
  expect_equal(as.character(rec$data$lat_source[1]), "disputed_melissa")

  fq <- geocode_followup_queue(rec, include_disputed = FALSE)
  expect_equal(nrow(fq), 0L)
  expect_identical(
    names(fq),
    c("lineage_id", "row_id", "school_year", "site_code", "site_name",
      "site_street", "site_city", "site_state", "site_zip",
      "lat_source", "coord_agreement_band", "distance_adece_melissa_m",
      "melissa_result_code", "lat_precision", "followup_reason",
      "suggested_action")
  )
})


# ===========================================================================
# Column dtypes are stable and as documented
# ===========================================================================
test_that("output dtypes match spec (char + numeric only)", {
  rec <- .mixed_reconciled_fixture()
  fq  <- geocode_followup_queue(rec)

  char_cols <- c("lineage_id", "row_id", "school_year", "site_code", "site_name",
                 "site_street", "site_city", "site_state", "site_zip",
                 "lat_source", "coord_agreement_band",
                 "melissa_result_code", "lat_precision",
                 "followup_reason", "suggested_action")
  for (col in char_cols) {
    expect_type(fq[[col]], "character")
  }
  expect_type(fq$distance_adece_melissa_m, "double")
})
