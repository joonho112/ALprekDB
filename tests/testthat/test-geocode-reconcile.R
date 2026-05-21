# tests/testthat/test-geocode-reconcile.R
#
# Tests for geocode_reconcile(): LOCKED 15-cell decision matrix from
# Step 4.2 applied to a cleaned Melissa-returned geocoded dataset.
#
# Coverage strategy:
#   - Sanity: argument validation, S3 return shape, factor levels.
#   - 15 matrix cells D1..D15: each cell exercised via .wrap_clean_from_edge()
#     or hand-built synthetic rows (some cells share fixtures G01..G18).
#   - Multi-row behavior: log length, summary tibble, lat_source counts.
#   - GS03 always flags (no distance threshold), gross outlier wins over
#     tier threshold, both-missing -> none, ADECE-only -> adece.
#   - Provenance string format.
#   - Config swap (adece_first inverts D1/D4 outcome).
#
# All fixtures are deterministic (seeded 42L). Mirrors the
# .wrap_clean_from_edge() helper used by test-geocode-validate.R but adds
# helpers tailored to the reconciler's per-row inputs.


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Wrap a tibble of rows as an alprek_geocode_clean object (post-coercion
# dtypes: LAT/LNG numeric, ERRORCODE character, site_zip character).
.wrap_clean <- function(df) {
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


# Build a single-row "decision" fixture with explicit ADECE and Melissa
# lat/lng (numeric) and RESULTCODE. Caller chooses ADECE/Melissa to land
# in a target cell of the matrix.
.row_decision <- function(adece_lat = NA_real_, adece_lng = NA_real_,
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
    LAT             = if (is.na(melissa_lat)) NA_real_ else as.numeric(melissa_lat),
    LNG             = if (is.na(melissa_lng)) NA_real_ else as.numeric(melissa_lng),
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


# Approx longitude offset (degrees) that yields a given great-circle
# distance (meters) at latitude `lat_deg`. Useful for building precise
# "X meters away" fixtures.
.lng_offset_for_m <- function(meters, lat_deg) {
  R <- 6371000
  d_rad <- meters / R
  # along a parallel: dlon = d_rad / cos(lat)
  dlon <- d_rad / cos(lat_deg * pi / 180)
  dlon * 180 / pi
}


# ===========================================================================
# Return type, shape, and argument validation
# ===========================================================================
test_that("geocode_reconcile() returns alprek_geocode_reconciled S3 object", {
  clean <- .wrap_clean(.row_decision(
    adece_lat = 33.5207, adece_lng = -86.8025,
    melissa_lat = 33.5207, melissa_lng = -86.8025,
    result_code = "GS05"))
  rec <- geocode_reconcile(clean)

  expect_s3_class(rec, "alprek_geocode_reconciled")
  expect_true(is.list(rec))
  expect_named(rec, c("data", "reconciliation_log", "summary", "meta"),
               ignore.order = TRUE)
  expect_s3_class(rec$data, "tbl_df")
  expect_s3_class(rec$reconciliation_log, "tbl_df")
  expect_s3_class(rec$summary, "tbl_df")
  expect_true(is.list(rec$meta))
})


test_that("geocode_reconcile() rejects non-clean input", {
  expect_error(geocode_reconcile(list(data = tibble::tibble())),
               regexp = "alprek_geocode_clean")
})


test_that("geocode_reconcile() rejects bad config", {
  clean <- .wrap_clean(.row_decision(adece_lat = 33.5, adece_lng = -86.8,
                                       melissa_lat = 33.5, melissa_lng = -86.8))
  expect_error(geocode_reconcile(clean, config = list(foo = 1)),
                regexp = "alprek_geocode_config")
})


test_that("data has 10 new authoritative columns", {
  clean <- .wrap_clean(.row_decision(
    adece_lat = 33.5207, adece_lng = -86.8025,
    melissa_lat = 33.5207, melissa_lng = -86.8025))
  rec <- geocode_reconcile(clean)
  expected_new <- c("lat_final", "lng_final", "lat_source", "lat_precision",
                    "distance_adece_melissa_m", "coord_agreement_band",
                    "needs_followup_geocoding", "followup_reason",
                    "coord_model_status", "geocode_provenance")
  expect_true(all(expected_new %in% names(rec$data)))
})


test_that("lineage_id is preserved in reconciled data and log", {
  clean <- .wrap_clean(.row_decision(
    adece_lat = 33.5207, adece_lng = -86.8025,
    melissa_lat = 33.5207, melissa_lng = -86.8025))
  rec <- geocode_reconcile(clean)

  expect_equal(rec$data$lineage_id, clean$data$lineage_id)
  expect_equal(rec$reconciliation_log$lineage_id, clean$data$lineage_id)
})


# ===========================================================================
# Factor level completeness (every level is registered, even if unused)
# ===========================================================================
test_that("lat_source factor has the 4 documented levels", {
  clean <- .wrap_clean(.row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8))
  rec <- geocode_reconcile(clean)
  expect_true(is.factor(rec$data$lat_source))
  expect_setequal(levels(rec$data$lat_source),
                  c("melissa", "adece", "disputed_melissa", "none"))
})


test_that("coord_agreement_band factor has the 7 documented levels", {
  clean <- .wrap_clean(.row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8))
  rec <- geocode_reconcile(clean)
  expect_true(is.factor(rec$data$coord_agreement_band))
  expect_setequal(levels(rec$data$coord_agreement_band),
                  c("exact", "tight", "loose", "drift", "gross",
                    "one_source_only", "none"))
})


test_that("lat_precision is an ordered factor with all 8 levels", {
  clean <- .wrap_clean(.row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8))
  rec <- geocode_reconcile(clean)
  expect_true(is.factor(rec$data$lat_precision))
  expect_true(is.ordered(rec$data$lat_precision))
  expect_setequal(levels(rec$data$lat_precision),
                  c("none", "unknown", "centroid", "zip5", "zip4",
                    "area", "parcel", "rooftop"))
})


test_that("followup_reason factor uses the 8-value controlled vocab", {
  clean <- .wrap_clean(.row_decision(
    adece_lat = NA_real_, adece_lng = NA_real_,
    melissa_lat = NA_real_, melissa_lng = NA_real_))
  rec <- geocode_reconcile(clean)
  expect_true(is.factor(rec$data$followup_reason))
  expect_setequal(levels(rec$data$followup_reason),
                  c("both_missing",
                    "melissa_unexpectedly_missing",
                    "melissa_only_interpolated",
                    "melissa_only_gs03",
                    "disagreement_above_threshold",
                    "disagreement_gross",
                    "resultcode_not_acceptable_for_master",
                    "resultcode_gs03_always_flag"))
})


test_that("coord_model_status is an ordered factor with documented levels", {
  clean <- .wrap_clean(.row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8))
  rec <- geocode_reconcile(clean)
  expect_true(is.factor(rec$data$coord_model_status))
  expect_true(is.ordered(rec$data$coord_model_status))
  expect_setequal(levels(rec$data$coord_model_status),
                  c("missing", "not_model_ready",
                    "provisional_followup", "model_ready"))
})


# ===========================================================================
# Reconciliation log: 1:1 with input rows
# ===========================================================================
test_that("reconciliation_log has 1 row per input row", {
  df <- dplyr::bind_rows(
    .row_decision(adece_lat = 33.5, adece_lng = -86.8,
                  melissa_lat = 33.5, melissa_lng = -86.8,
                  result_code = "GS05", site_code = "999P000001"),
    .row_decision(adece_lat = 32.4, adece_lng = -86.3,
                  melissa_lat = 32.4, melissa_lng = -86.3,
                  result_code = "GS01", site_code = "999P000002"),
    .row_decision(adece_lat = NA_real_, adece_lng = NA_real_,
                  melissa_lat = 34.7, melissa_lng = -86.6,
                  result_code = "GS06", site_code = "999P000003")
  )
  clean <- .wrap_clean(df)
  rec <- geocode_reconcile(clean)
  expect_equal(nrow(rec$reconciliation_log), nrow(df))
  expect_equal(nrow(rec$data), nrow(df))
})


# ===========================================================================
# Cell D1: GS01, both present, distance <=50m
# ===========================================================================
test_that("D1: GS01 within 50m -> melissa, zip4, no followup, exact/tight", {
  # 30m east at lat=33.5 -> roughly 0.000324 deg lng
  dlng <- .lng_offset_for_m(30, 33.5)
  df <- .row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8 + dlng,
    result_code = "GS01")
  rec <- geocode_reconcile(.wrap_clean(df))
  expect_equal(as.character(rec$data$lat_source[1]), "melissa")
  expect_equal(as.character(rec$data$lat_precision[1]), "zip4")
  expect_false(rec$data$needs_followup_geocoding[1])
  expect_true(as.character(rec$data$coord_agreement_band[1]) %in%
                c("exact", "tight"))
  expect_equal(rec$reconciliation_log$matrix_cell[1], "D1")
})


# ===========================================================================
# Cell D2: GS01, both present, 50m < d < 10km -> melissa, flagged
# ===========================================================================
test_that("D2: GS01 60m disagreement -> melissa, zip4, flag disagreement", {
  dlng <- .lng_offset_for_m(60, 33.5)
  df <- .row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8 + dlng,
    result_code = "GS01")
  rec <- geocode_reconcile(.wrap_clean(df))
  expect_equal(as.character(rec$data$lat_source[1]), "melissa")
  expect_true(rec$data$needs_followup_geocoding[1])
  expect_equal(as.character(rec$data$followup_reason[1]),
               "disagreement_above_threshold")
  expect_equal(rec$reconciliation_log$matrix_cell[1], "D2")
})


# ===========================================================================
# Cell D3: GS01, both present, >=10km -> disputed_melissa, gross
# ===========================================================================
test_that("D3: GS01 50km disagreement -> disputed_melissa, gross", {
  dlng <- .lng_offset_for_m(50000, 33.5)
  df <- .row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8 + dlng,
    result_code = "GS01")
  rec <- geocode_reconcile(.wrap_clean(df))
  expect_equal(as.character(rec$data$lat_source[1]), "disputed_melissa")
  expect_equal(as.character(rec$data$coord_agreement_band[1]), "gross")
  expect_equal(as.character(rec$data$followup_reason[1]),
               "disagreement_gross")
  expect_equal(rec$reconciliation_log$matrix_cell[1], "D3")
})


# ===========================================================================
# Cell D4: GS05, both present, within 250m -> melissa, rooftop, no followup
# ===========================================================================
test_that("D4: GS05 within 250m -> melissa, rooftop, no followup", {
  dlng <- .lng_offset_for_m(80, 33.5)
  df <- .row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8 + dlng,
    result_code = "GS05")
  rec <- geocode_reconcile(.wrap_clean(df))
  expect_equal(as.character(rec$data$lat_source[1]), "melissa")
  expect_equal(as.character(rec$data$lat_precision[1]), "rooftop")
  expect_false(rec$data$needs_followup_geocoding[1])
  expect_equal(rec$reconciliation_log$matrix_cell[1], "D4")
})


# ===========================================================================
# Cell D5: GS05, both present, 250m < d < 10km -> melissa, flag
# ===========================================================================
test_that("D5: GS05 500m -> melissa, rooftop, flag", {
  dlng <- .lng_offset_for_m(500, 33.5)
  df <- .row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8 + dlng,
    result_code = "GS05")
  rec <- geocode_reconcile(.wrap_clean(df))
  expect_equal(as.character(rec$data$lat_source[1]), "melissa")
  expect_equal(as.character(rec$data$lat_precision[1]), "rooftop")
  expect_true(rec$data$needs_followup_geocoding[1])
  expect_equal(as.character(rec$data$followup_reason[1]),
               "disagreement_above_threshold")
  expect_equal(rec$reconciliation_log$matrix_cell[1], "D5")
})


# ===========================================================================
# Cell D6: GS05, both present, >=10km -> disputed_melissa, gross
# ===========================================================================
test_that("D6: GS05 30km -> disputed_melissa, gross", {
  dlng <- .lng_offset_for_m(30000, 33.5)
  df <- .row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8 + dlng,
    result_code = "GS05")
  rec <- geocode_reconcile(.wrap_clean(df))
  expect_equal(as.character(rec$data$lat_source[1]), "disputed_melissa")
  expect_equal(as.character(rec$data$coord_agreement_band[1]), "gross")
  expect_equal(as.character(rec$data$followup_reason[1]),
               "disagreement_gross")
  expect_equal(rec$reconciliation_log$matrix_cell[1], "D6")
})


# ===========================================================================
# Cell D7: GS06, both present, within 500m -> melissa, parcel, no followup
# ===========================================================================
test_that("D7: GS06 within 500m -> melissa, parcel, no followup", {
  dlng <- .lng_offset_for_m(200, 33.5)
  df <- .row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8 + dlng,
    result_code = "GS06")
  rec <- geocode_reconcile(.wrap_clean(df))
  expect_equal(as.character(rec$data$lat_source[1]), "melissa")
  expect_equal(as.character(rec$data$lat_precision[1]), "parcel")
  expect_false(rec$data$needs_followup_geocoding[1])
  expect_equal(rec$reconciliation_log$matrix_cell[1], "D7")
})


# ===========================================================================
# Cell D8: GS06, both present, 500m < d < 10km -> melissa, parcel, flag
# ===========================================================================
test_that("D8: GS06 800m -> melissa, parcel, flag", {
  dlng <- .lng_offset_for_m(800, 33.5)
  df <- .row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8 + dlng,
    result_code = "GS06")
  rec <- geocode_reconcile(.wrap_clean(df))
  expect_equal(as.character(rec$data$lat_source[1]), "melissa")
  expect_true(rec$data$needs_followup_geocoding[1])
  expect_equal(as.character(rec$data$followup_reason[1]),
               "disagreement_above_threshold")
  expect_equal(rec$reconciliation_log$matrix_cell[1], "D8")
})


# ===========================================================================
# Cell D9: GS06, both present, >=10km -> disputed_melissa, parcel, gross
# ===========================================================================
test_that("D9: GS06 20km -> disputed_melissa, gross", {
  dlng <- .lng_offset_for_m(20000, 33.5)
  df <- .row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8 + dlng,
    result_code = "GS06")
  rec <- geocode_reconcile(.wrap_clean(df))
  expect_equal(as.character(rec$data$lat_source[1]), "disputed_melissa")
  expect_equal(as.character(rec$data$coord_agreement_band[1]), "gross")
  expect_equal(as.character(rec$data$followup_reason[1]),
               "disagreement_gross")
  expect_equal(rec$reconciliation_log$matrix_cell[1], "D9")
})


# ===========================================================================
# Cell D10: GS03, both present, any distance -> disputed_melissa, gs03 flag
# ===========================================================================
test_that("D10a: GS03 with both present (tiny d) -> disputed_melissa, gs03 flag", {
  # zero distance: still flagged because GS03 always flags
  df <- .row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8,
    result_code = "GS03")
  rec <- geocode_reconcile(.wrap_clean(df))
  expect_equal(as.character(rec$data$lat_source[1]), "disputed_melissa")
  expect_equal(as.character(rec$data$lat_precision[1]), "zip5")
  expect_true(rec$data$needs_followup_geocoding[1])
  expect_equal(as.character(rec$data$followup_reason[1]),
               "resultcode_gs03_always_flag")
  expect_equal(rec$reconciliation_log$matrix_cell[1], "D10")
})


test_that("D10b: GS03 with non-trivial d -> disputed_melissa, gs03 flag", {
  dlng <- .lng_offset_for_m(2500, 33.5)
  df <- .row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8 + dlng,
    result_code = "GS03")
  rec <- geocode_reconcile(.wrap_clean(df))
  expect_true(rec$data$needs_followup_geocoding[1])
  expect_equal(as.character(rec$data$followup_reason[1]),
               "resultcode_gs03_always_flag")
  expect_equal(rec$reconciliation_log$matrix_cell[1], "D10")
})


# ===========================================================================
# Cell D11: ADECE only, Melissa missing
# ===========================================================================
test_that("D11: ADECE only -> lat_source=adece, one_source_only, flag", {
  df <- .row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = NA_real_, melissa_lng = NA_real_,
    result_code = NA_character_)
  rec <- geocode_reconcile(.wrap_clean(df))
  expect_equal(as.character(rec$data$lat_source[1]), "adece")
  expect_equal(as.character(rec$data$coord_agreement_band[1]),
               "one_source_only")
  expect_true(rec$data$needs_followup_geocoding[1])
  expect_equal(as.character(rec$data$followup_reason[1]),
               "melissa_unexpectedly_missing")
  expect_equal(rec$reconciliation_log$matrix_cell[1], "D11")
  expect_equal(rec$data$lat_final[1], 33.5)
  expect_equal(rec$data$lng_final[1], -86.8)
})


# ===========================================================================
# Cell D12: Melissa-only, GS01/GS05 -> melissa, OK
# ===========================================================================
test_that("D12a: Melissa-only GS01 -> melissa, zip4, no followup", {
  df <- .row_decision(
    adece_lat = NA_real_, adece_lng = NA_real_,
    melissa_lat = 33.5, melissa_lng = -86.8,
    result_code = "GS01")
  rec <- geocode_reconcile(.wrap_clean(df))
  expect_equal(as.character(rec$data$lat_source[1]), "melissa")
  expect_equal(as.character(rec$data$lat_precision[1]), "zip4")
  expect_false(rec$data$needs_followup_geocoding[1])
  expect_equal(rec$reconciliation_log$matrix_cell[1], "D12")
})


test_that("D12b: Melissa-only GS05 -> melissa, rooftop, no followup", {
  df <- .row_decision(
    adece_lat = NA_real_, adece_lng = NA_real_,
    melissa_lat = 33.5, melissa_lng = -86.8,
    result_code = "GS05")
  rec <- geocode_reconcile(.wrap_clean(df))
  expect_equal(as.character(rec$data$lat_source[1]), "melissa")
  expect_equal(as.character(rec$data$lat_precision[1]), "rooftop")
  expect_false(rec$data$needs_followup_geocoding[1])
  expect_equal(rec$reconciliation_log$matrix_cell[1], "D12")
})


# ===========================================================================
# Cell D13: Melissa-only, GS06 -> melissa, parcel, flag (centroid)
# ===========================================================================
test_that("D13: Melissa-only GS06 -> melissa, parcel, flag", {
  df <- .row_decision(
    adece_lat = NA_real_, adece_lng = NA_real_,
    melissa_lat = 33.5, melissa_lng = -86.8,
    result_code = "GS06")
  rec <- geocode_reconcile(.wrap_clean(df))
  expect_equal(as.character(rec$data$lat_source[1]), "melissa")
  expect_equal(as.character(rec$data$lat_precision[1]), "parcel")
  expect_true(rec$data$needs_followup_geocoding[1])
  expect_equal(as.character(rec$data$followup_reason[1]),
               "melissa_only_interpolated")
  expect_equal(rec$reconciliation_log$matrix_cell[1], "D13")
})


# ===========================================================================
# Cell D14: Melissa-only, GS03 -> melissa, zip5, flag (gs03)
# ===========================================================================
test_that("D14: Melissa-only GS03 -> melissa, zip5, flag", {
  df <- .row_decision(
    adece_lat = NA_real_, adece_lng = NA_real_,
    melissa_lat = 33.5, melissa_lng = -86.8,
    result_code = "GS03")
  rec <- geocode_reconcile(.wrap_clean(df))
  expect_equal(as.character(rec$data$lat_source[1]), "melissa")
  expect_equal(as.character(rec$data$lat_precision[1]), "zip5")
  expect_true(rec$data$needs_followup_geocoding[1])
  expect_equal(as.character(rec$data$followup_reason[1]),
               "melissa_only_gs03")
  expect_equal(rec$reconciliation_log$matrix_cell[1], "D14")
})


# ===========================================================================
# Cell D15: both missing
# ===========================================================================
test_that("D15: both missing -> lat_source=none, none, flag both_missing", {
  df <- .row_decision(
    adece_lat = NA_real_, adece_lng = NA_real_,
    melissa_lat = NA_real_, melissa_lng = NA_real_,
    result_code = NA_character_)
  rec <- geocode_reconcile(.wrap_clean(df))
  expect_equal(as.character(rec$data$lat_source[1]), "none")
  expect_equal(as.character(rec$data$coord_agreement_band[1]), "none")
  expect_equal(as.character(rec$data$lat_precision[1]), "none")
  expect_true(rec$data$needs_followup_geocoding[1])
  expect_equal(as.character(rec$data$followup_reason[1]), "both_missing")
  expect_equal(rec$reconciliation_log$matrix_cell[1], "D15")
  expect_true(is.na(rec$data$lat_final[1]))
  expect_true(is.na(rec$data$lng_final[1]))
})


# ===========================================================================
# Codebook acceptability and future RESULTCODE safety
# ===========================================================================
test_that("known unacceptable RESULTCODEs are retained but flagged", {
  rows <- dplyr::bind_rows(lapply(c("GS02", "GS04", "GS07", "GS08"), function(rc) {
    .row_decision(
      adece_lat = 33.5, adece_lng = -86.8,
      melissa_lat = 33.5, melissa_lng = -86.8,
      result_code = rc,
      site_code = paste0("999P", substr(rc, 3, 4), "0001"))
  }))

  rec <- geocode_reconcile(.wrap_clean(rows))

  expect_true(all(rec$data$needs_followup_geocoding))
  expect_true(all(as.character(rec$data$followup_reason) ==
                    "resultcode_not_acceptable_for_master"))
  expect_true(all(as.character(rec$data$coord_model_status) ==
                    "not_model_ready"))
  expect_true(all(as.character(rec$data$lat_source) == "melissa"))
})


test_that("unknown RESULTCODE does not crash and is flagged", {
  row <- .row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8,
    result_code = "GS99")

  expect_no_error(rec <- geocode_reconcile(.wrap_clean(row)))

  expect_equal(as.character(rec$data$lat_precision[1]), "unknown")
  expect_true(rec$data$needs_followup_geocoding[1])
  expect_equal(as.character(rec$data$followup_reason[1]),
               "resultcode_not_acceptable_for_master")
  expect_equal(as.character(rec$data$coord_model_status[1]),
               "not_model_ready")
})


test_that("Melissa-only unacceptable RESULTCODE is flagged", {
  row <- .row_decision(
    adece_lat = NA_real_, adece_lng = NA_real_,
    melissa_lat = 33.5, melissa_lng = -86.8,
    result_code = "GS02")

  rec <- geocode_reconcile(.wrap_clean(row))

  expect_equal(as.character(rec$data$lat_source[1]), "melissa")
  expect_true(rec$data$needs_followup_geocoding[1])
  expect_equal(as.character(rec$data$followup_reason[1]),
               "resultcode_not_acceptable_for_master")
  expect_equal(as.character(rec$data$coord_model_status[1]),
               "not_model_ready")
})


# ===========================================================================
# Synthetic "184 ADECE-missing equivalent": fixture G07 (Melissa rescues
# missing ADECE, GS05 -> D12 outcome, no followup).
# ===========================================================================
test_that("ADECE-missing GS05 batch -> all melissa, one_source_only, no followup", {
  # Build 10 rows like G07 (ADECE missing, Melissa GS05).
  rows <- lapply(seq_len(10L), function(i) {
    .row_decision(
      adece_lat = NA_real_, adece_lng = NA_real_,
      melissa_lat = 33.0 + i * 0.001,
      melissa_lng = -86.9 + i * 0.001,
      result_code = "GS05",
      site_code = sprintf("999P%06d", i))
  })
  df <- dplyr::bind_rows(rows)
  rec <- geocode_reconcile(.wrap_clean(df))

  expect_true(all(as.character(rec$data$lat_source) == "melissa"))
  expect_true(all(as.character(rec$data$coord_agreement_band) ==
                    "one_source_only"))
  expect_false(any(rec$data$needs_followup_geocoding))
  expect_true(all(rec$reconciliation_log$matrix_cell == "D12"))
})


# ===========================================================================
# Provenance string completeness
# ===========================================================================
test_that("geocode_provenance string is non-NA and well-formed for all rows", {
  df <- dplyr::bind_rows(
    .row_decision(adece_lat = 33.5, adece_lng = -86.8,
                  melissa_lat = 33.5, melissa_lng = -86.8,
                  result_code = "GS05", site_code = "999P000001"),
    .row_decision(adece_lat = NA_real_, adece_lng = NA_real_,
                  melissa_lat = 34.7, melissa_lng = -86.6,
                  result_code = "GS03", site_code = "999P000002"),
    .row_decision(adece_lat = 32.4, adece_lng = -86.3,
                  melissa_lat = NA_real_, melissa_lng = NA_real_,
                  result_code = NA_character_, site_code = "999P000003"),
    .row_decision(adece_lat = NA_real_, adece_lng = NA_real_,
                  melissa_lat = NA_real_, melissa_lng = NA_real_,
                  result_code = NA_character_, site_code = "999P000004")
  )
  rec <- geocode_reconcile(.wrap_clean(df))
  expect_true(all(!is.na(rec$data$geocode_provenance)))
  expect_true(all(nzchar(rec$data$geocode_provenance)))
  # Every provenance string contains the canonical key=value pieces
  for (s in rec$data$geocode_provenance) {
    expect_match(s, "melissa:")
    expect_match(s, "adece:")
    expect_match(s, "dist=")
    expect_match(s, "band=")
    expect_match(s, "source=")
    expect_match(s, "tier=")
  }
})


# ===========================================================================
# Config swap: adece_first inverts D1/D4 (lat_source -> adece)
# ===========================================================================
test_that("config adece_first inverts D1: lat_source becomes adece", {
  cfg <- geocode_config(path = "/tmp/x.xlsx",
                         cycle_year = "2024-2025",
                         delivery_date = "2026-03-04",
                         authoritative_priority = "adece_first")
  dlng <- .lng_offset_for_m(30, 33.5)
  df <- .row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8 + dlng,
    result_code = "GS01")
  rec <- geocode_reconcile(.wrap_clean(df), config = cfg)
  expect_equal(as.character(rec$data$lat_source[1]), "adece")
  expect_equal(rec$data$lat_final[1], 33.5)
  expect_equal(rec$reconciliation_log$matrix_cell[1], "D1")
})


test_that("config adece_first inverts D4: lat_source becomes adece", {
  cfg <- geocode_config(path = "/tmp/x.xlsx",
                         cycle_year = "2024-2025",
                         delivery_date = "2026-03-04",
                         authoritative_priority = "adece_first")
  dlng <- .lng_offset_for_m(80, 33.5)
  df <- .row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8 + dlng,
    result_code = "GS05")
  rec <- geocode_reconcile(.wrap_clean(df), config = cfg)
  expect_equal(as.character(rec$data$lat_source[1]), "adece")
  expect_equal(rec$data$lat_final[1], 33.5)
})


# ===========================================================================
# Summary tibble has 15 documented cells
# ===========================================================================
test_that("summary tibble enumerates all 15 matrix cells", {
  clean <- .wrap_clean(.row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8))
  rec <- geocode_reconcile(clean)
  expect_equal(nrow(rec$summary), 15L)
  expect_setequal(rec$summary$matrix_cell, sprintf("D%d", 1L:15L))
  # Sum of n's equals row count
  expect_equal(sum(rec$summary$n), nrow(rec$data))
})


# ===========================================================================
# Meta includes provenance from clean$meta and decision counters
# ===========================================================================
test_that("meta carries clean provenance + decision counters", {
  clean <- .wrap_clean(.row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8))
  rec <- geocode_reconcile(clean)
  expect_equal(rec$meta$authoritative_priority, "melissa_first")
  expect_equal(rec$meta$distance_threshold_rules, "by_resultcode")
  expect_equal(rec$meta$cycle_year, clean$meta$cycle_year)
  expect_equal(rec$meta$file_sha256, clean$meta$file_sha256)
  expect_equal(rec$meta$n_rows, 1L)
  expect_true(is.integer(rec$meta$n_needs_followup))
  expect_true(is.integer(rec$meta$n_disputed))
})


# ===========================================================================
# Print method does not error
# ===========================================================================
test_that("print() method does not error", {
  clean <- .wrap_clean(.row_decision(
    adece_lat = 33.5, adece_lng = -86.8,
    melissa_lat = 33.5, melissa_lng = -86.8))
  rec <- geocode_reconcile(clean)
  expect_invisible(print(rec))
})


# ===========================================================================
# Fixture-based regression: G07 (ADECE missing + Melissa GS05 rescue)
# ===========================================================================
test_that("fixture G07 matches expected reconciler outputs", {
  fx <- make_geocode_edge_case_fixture("G07", n_rows = 5L, seed = 42L)
  rec <- geocode_reconcile(.wrap_clean(fx$data))
  i <- fx$bad_row_index[1]
  expect_equal(as.character(rec$data$lat_source[i]),
               fx$expected_reconciler_lat_source)
  expect_equal(rec$data$needs_followup_geocoding[i],
               fx$expected_needs_followup)
})


test_that("fixture G06 (gross outlier) -> disputed_melissa, gross", {
  fx <- make_geocode_edge_case_fixture("G06", n_rows = 5L, seed = 42L)
  rec <- geocode_reconcile(.wrap_clean(fx$data))
  i <- fx$bad_row_index[1]
  expect_equal(as.character(rec$data$lat_source[i]), "disputed_melissa")
  expect_true(rec$data$needs_followup_geocoding[i])
  # band should be gross or drift depending on offset magnitude
  expect_true(as.character(rec$data$coord_agreement_band[i]) %in%
                c("drift", "gross"))
})


test_that("fixture G01 (both missing) -> none, both_missing", {
  fx <- make_geocode_edge_case_fixture("G01", n_rows = 5L, seed = 42L)
  rec <- geocode_reconcile(.wrap_clean(fx$data))
  i <- fx$bad_row_index[1]
  expect_equal(as.character(rec$data$lat_source[i]), "none")
  expect_equal(as.character(rec$data$followup_reason[i]), "both_missing")
})


test_that("fixture G09 (Melissa missing) -> adece, melissa_unexpectedly_missing", {
  fx <- make_geocode_edge_case_fixture("G09", n_rows = 5L, seed = 42L)
  rec <- geocode_reconcile(.wrap_clean(fx$data))
  i <- fx$bad_row_index[1]
  expect_equal(as.character(rec$data$lat_source[i]), "adece")
  expect_equal(as.character(rec$data$followup_reason[i]),
               "melissa_unexpectedly_missing")
})
