# tests/testthat/test-geocode-transform.R
#
# Tests for geocode_transform(): adds 5 derived analytical variables on top
# of the reconciled geocode output (Step 5.1).
#
# Coverage:
#   - S3 class and return shape
#   - 5 derived columns present in $data
#   - lineage_id and coord_model_status preserved 1:1
#   - precision_tier is an ordered factor with descending levels
#   - in_alabama logical TRUE/FALSE/NA semantics
#   - county_check_match defaults to NA in standalone runs;
#     respects adece_county sidecar (G16 path)
#   - coord_age_years arithmetic incl. "_new" school_year suffix
#   - geocode_run_id is a single, panel-stable character value
#   - Row-count invariance, transform_log shape, print() method
#   - Argument validation, config swap


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Wrap a tibble of rows as an alprek_geocode_clean object (post-coercion
# dtypes match what geocode_clean() emits).
.wrap_clean_tr <- function(df,
                            cycle_year = "2026-2027",
                            receipt_date = "2026-03-04") {
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
  if (!"lineage_id" %in% names(df)) {
    df$lineage_id <- paste0("lin_", seq_len(nrow(df)))
  }

  meta <- list(
    path             = "/tmp/fake-melissa.xlsx",
    sheet            = "Sheet1",
    source           = "melissa",
    cycle_year       = cycle_year,
    receipt_date     = as.Date(receipt_date),
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


# Build a single-row "decision" fixture (same as in test-geocode-reconcile.R)
.row_decision_tr <- function(adece_lat = NA_real_, adece_lng = NA_real_,
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


# Build a default reconciled fixture (5 well-formed rows + one missing) for
# generic shape/coverage tests.
.default_reconciled <- function(cycle_year = "2026-2027",
                                 receipt_date = "2026-03-04") {
  df <- dplyr::bind_rows(
    .row_decision_tr(adece_lat = 33.5207, adece_lng = -86.8025,
                     melissa_lat = 33.5207, melissa_lng = -86.8025,
                     result_code = "GS05", site_code = "999P000001",
                     school_year = "2024-2025"),
    .row_decision_tr(adece_lat = 32.3668, adece_lng = -86.2999,
                     melissa_lat = 32.3668, melissa_lng = -86.2999,
                     result_code = "GS01", site_code = "999P000002",
                     school_year = "2025-2026_new"),
    .row_decision_tr(adece_lat = NA_real_, adece_lng = NA_real_,
                     melissa_lat = 34.7304, melissa_lng = -86.5861,
                     result_code = "GS06", site_code = "999P000003",
                     school_year = "2021-2022"),
    .row_decision_tr(adece_lat = NA_real_, adece_lng = NA_real_,
                     melissa_lat = NA_real_, melissa_lng = NA_real_,
                     result_code = NA_character_, site_code = "999P000004",
                     school_year = "2022-2023"),
    .row_decision_tr(adece_lat = 30.6954, adece_lng = -88.0399,
                     melissa_lat = 30.6954, melissa_lng = -88.0399,
                     result_code = "GS01", site_code = "999P000005",
                     school_year = "2023-2024")
  )
  clean <- .wrap_clean_tr(df, cycle_year = cycle_year,
                          receipt_date = receipt_date)
  geocode_reconcile(clean)
}


# ===========================================================================
# 1. S3 class and return shape
# ===========================================================================
test_that("geocode_transform() returns alprek_geocode_master S3", {
  rec <- .default_reconciled()
  mst <- geocode_transform(rec)

  expect_s3_class(mst, "alprek_geocode_master")
  expect_true(is.list(mst))
  expect_named(mst, c("data", "transform_log", "meta"), ignore.order = TRUE)
  expect_s3_class(mst$data, "tbl_df")
  expect_s3_class(mst$transform_log, "tbl_df")
  expect_true(is.list(mst$meta))
})


# ===========================================================================
# 2. $data has all reconciled cols + 5 new derived cols
# ===========================================================================
test_that("$data has reconciled cols + 5 new derived cols", {
  rec <- .default_reconciled()
  mst <- geocode_transform(rec)

  expected_new <- c("precision_tier", "in_alabama", "county_check_match",
                    "coord_age_years", "geocode_run_id")
  expect_true(all(expected_new %in% names(mst$data)))
  # Every reconciled col must still be present
  expect_true(all(names(rec$data) %in% names(mst$data)))
  expect_equal(ncol(mst$data), ncol(rec$data) + length(expected_new))
})


# ===========================================================================
# 3. lineage_id is preserved 1:1
# ===========================================================================
test_that("lineage_id is preserved 1:1 from reconciled input", {
  rec <- .default_reconciled()
  mst <- geocode_transform(rec)
  expect_true("lineage_id" %in% names(mst$data))
  expect_equal(mst$data$lineage_id, rec$data$lineage_id)
})


# ===========================================================================
# 4. coord_model_status is preserved
# ===========================================================================
test_that("coord_model_status is preserved (Phase 5 contract)", {
  rec <- .default_reconciled()
  mst <- geocode_transform(rec)
  expect_true("coord_model_status" %in% names(mst$data))
  # Same values, same factor levels
  expect_equal(as.character(mst$data$coord_model_status),
               as.character(rec$data$coord_model_status))
  expect_true(is.factor(mst$data$coord_model_status))
  expect_true(is.ordered(mst$data$coord_model_status))
  expect_setequal(levels(mst$data$coord_model_status),
                  c("missing", "not_model_ready",
                    "provisional_followup", "model_ready"))
  # Verify non-model_ready rows are still present (not silently dropped)
  expect_equal(nrow(mst$data), nrow(rec$data))
})


# ===========================================================================
# 5. precision_tier is an ordered factor (descending levels)
# ===========================================================================
test_that("precision_tier is ordered factor with descending levels", {
  rec <- .default_reconciled()
  mst <- geocode_transform(rec)

  expect_true(is.factor(mst$data$precision_tier))
  expect_true(is.ordered(mst$data$precision_tier))
  # Spec: rooftop > parcel > zip4 > zip5 > centroid > area > unknown > none
  expect_equal(levels(mst$data$precision_tier),
               c("rooftop", "parcel", "zip4", "zip5",
                 "centroid", "area", "unknown", "none"))
  # All 5 rows should be in the level set
  expect_true(all(as.character(mst$data$precision_tier) %in%
                    levels(mst$data$precision_tier)))
})


# ===========================================================================
# 6. in_alabama logical: TRUE/FALSE/NA semantics
# ===========================================================================
test_that("in_alabama TRUE for valid AL coords", {
  rec <- .default_reconciled()
  mst <- geocode_transform(rec)
  expect_true(is.logical(mst$data$in_alabama))
  # Rows 1, 2, 3, 5 have AL coordinates (Birmingham/Montgomery/Huntsville/Mobile)
  expect_true(mst$data$in_alabama[1])
  expect_true(mst$data$in_alabama[2])
  expect_true(mst$data$in_alabama[3])
  expect_true(mst$data$in_alabama[5])
  # Row 4 has lat_final = NA (both missing) -> in_alabama is NA
  expect_true(is.na(mst$data$in_alabama[4]))
})


test_that("in_alabama FALSE for out-of-bounds coords (G11 fixture)", {
  # G11 fixture injects an out-of-bounds ADECE lat/lng (NY), Melissa stays AL.
  # The reconciler keeps Melissa (in AL) so the fixture's lat_final stays AL.
  # Use a direct hand-built row instead to force in_alabama = FALSE.
  df <- .row_decision_tr(
    adece_lat = 40.7128, adece_lng = -74.0060,  # NYC ADECE
    melissa_lat = 40.7128, melissa_lng = -74.0060,  # NYC Melissa too
    result_code = "GS05", site_code = "999P000099",
    school_year = "2024-2025")
  rec <- geocode_reconcile(.wrap_clean_tr(df))
  mst <- geocode_transform(rec)
  expect_false(mst$data$in_alabama[1])
})


test_that("in_alabama NA when lat_final is NA (none lat_source)", {
  df <- .row_decision_tr(
    adece_lat = NA_real_, adece_lng = NA_real_,
    melissa_lat = NA_real_, melissa_lng = NA_real_,
    result_code = NA_character_, site_code = "999P000099")
  rec <- geocode_reconcile(.wrap_clean_tr(df))
  mst <- geocode_transform(rec)
  expect_true(is.na(mst$data$in_alabama[1]))
  expect_equal(as.character(rec$data$lat_source[1]), "none")
})


# ===========================================================================
# 7. coord_age_years arithmetic (incl. "_new" suffix handling)
# ===========================================================================
test_that("coord_age_years is integer with correct values", {
  # cycle_year = "2026-2027" -> cycle_year_first = 2026
  rec <- .default_reconciled(cycle_year = "2026-2027")
  mst <- geocode_transform(rec)
  expect_true(is.integer(mst$data$coord_age_years))

  # school_year per row (in row order):
  #   1: "2024-2025"      -> 2026 - 2024 = 2
  #   2: "2025-2026_new"  -> 2026 - 2025 = 1   (strip _new suffix)
  #   3: "2021-2022"      -> 2026 - 2021 = 5
  #   4: "2022-2023"      -> 2026 - 2022 = 4
  #   5: "2023-2024"      -> 2026 - 2023 = 3
  expect_equal(mst$data$coord_age_years[1], 2L)
  expect_equal(mst$data$coord_age_years[2], 1L)
  expect_equal(mst$data$coord_age_years[3], 5L)
  expect_equal(mst$data$coord_age_years[4], 4L)
  expect_equal(mst$data$coord_age_years[5], 3L)
})


# ===========================================================================
# 8. geocode_run_id is character, single distinct value
# ===========================================================================
test_that("geocode_run_id is character + single panel-stable value", {
  rec <- .default_reconciled(cycle_year = "2026-2027",
                              receipt_date = "2026-03-04")
  mst <- geocode_transform(rec)

  expect_true(is.character(mst$data$geocode_run_id))
  expect_equal(length(unique(mst$data$geocode_run_id)), 1L)
  expect_equal(unique(mst$data$geocode_run_id), "melissa_v1_2026-03")
  # Also surfaced on meta
  expect_equal(mst$meta$geocode_run_id, "melissa_v1_2026-03")
})


# ===========================================================================
# 9. Row count is unchanged from input
# ===========================================================================
test_that("transform preserves row count and row order", {
  rec <- .default_reconciled()
  mst <- geocode_transform(rec)
  expect_equal(nrow(mst$data), nrow(rec$data))
  # Sanity: row_id preserved in order
  expect_equal(mst$data$row_id, rec$data$row_id)
})


# ===========================================================================
# 10. print() method works (invisible return)
# ===========================================================================
test_that("print() method works and returns invisible", {
  rec <- .default_reconciled()
  mst <- geocode_transform(rec)
  expect_invisible(print(mst))
  expect_output(print(mst), "alprek_geocode_master")
  expect_output(print(mst), "geocode_run_id")
})


# ===========================================================================
# 11. Argument validation (non-reconciled input rejected)
# ===========================================================================
test_that("geocode_transform rejects non-reconciled input", {
  expect_error(geocode_transform(list(data = tibble::tibble())),
               regexp = "alprek_geocode_reconciled")
  expect_error(geocode_transform(NULL),
               regexp = "alprek_geocode_reconciled")
  expect_error(geocode_transform(data.frame(x = 1)),
               regexp = "alprek_geocode_reconciled")
})


test_that("geocode_transform rejects bad config class", {
  rec <- .default_reconciled()
  expect_error(geocode_transform(rec, config = list(foo = 1)),
               regexp = "alprek_geocode_config")
})


# ===========================================================================
# 12. Transform log has 5 rows (one per rule)
# ===========================================================================
test_that("transform_log has 5 rows, one per derivation rule", {
  rec <- .default_reconciled()
  mst <- geocode_transform(rec)
  expect_equal(nrow(mst$transform_log), 5L)
  expect_setequal(mst$transform_log$rule,
                  c("precision_tier", "in_alabama", "county_check_match",
                    "coord_age_years", "geocode_run_id"))
  expect_true(all(c("rule", "n_affected", "details", "severity") %in%
                    names(mst$transform_log)))
})


# ===========================================================================
# 13. Config swap: different delivery_date -> different geocode_run_id
# ===========================================================================
test_that("config swap: different delivery_date yields different geocode_run_id", {
  rec <- .default_reconciled(receipt_date = "2026-03-04")

  cfg <- geocode_config(path = "/tmp/fake.xlsx",
                         cycle_year = "2026-2027",
                         delivery_date = "2027-09-15",
                         vendor = "melissa")
  mst <- geocode_transform(rec, config = cfg)

  expect_equal(unique(mst$data$geocode_run_id), "melissa_v1_2027-09")
  expect_equal(mst$meta$geocode_run_id, "melissa_v1_2027-09")
})


# ===========================================================================
# 14. county_check_match: NA in standalone runs (no adece_county sidecar)
# ===========================================================================
test_that("county_check_match is NA in standalone Step 5.1 (no adece_county)", {
  rec <- .default_reconciled()
  mst <- geocode_transform(rec)
  expect_true("county_check_match" %in% names(mst$data))
  expect_true(all(is.na(mst$data$county_check_match)))
})


# ===========================================================================
# 15. county_check_match: respects adece_county sidecar (G16-style fixture)
# ===========================================================================
test_that("county_check_match: TRUE when adece_county matches COUNTYNAME", {
  # Two rows: one matching, one mismatching adece_county vs COUNTYNAME.
  df <- dplyr::bind_rows(
    .row_decision_tr(adece_lat = 33.5207, adece_lng = -86.8025,
                     melissa_lat = 33.5207, melissa_lng = -86.8025,
                     result_code = "GS05", site_code = "999P000001"),
    .row_decision_tr(adece_lat = 32.3668, adece_lng = -86.2999,
                     melissa_lat = 32.3668, melissa_lng = -86.2999,
                     result_code = "GS05", site_code = "999P000002")
  )
  df$COUNTYNAME    <- c("Jefferson", "Montgomery")
  df$adece_county  <- c("Jefferson", "Lee")  # row 2 mismatches
  clean <- .wrap_clean_tr(df)
  rec <- geocode_reconcile(clean)

  # adece_county sidecar should be carried through reconcile to mst$data
  expect_true("adece_county" %in% names(rec$data))

  mst <- geocode_transform(rec)
  expect_true(is.logical(mst$data$county_check_match))
  expect_true(mst$data$county_check_match[1])
  expect_false(mst$data$county_check_match[2])
})


# ===========================================================================
# 16. meta carries transformed_at, geocode_run_id + inherited fields
# ===========================================================================
test_that("meta has transformed_at, geocode_run_id, and inherited fields", {
  rec <- .default_reconciled()
  mst <- geocode_transform(rec)
  expect_true(!is.null(mst$meta$transformed_at))
  expect_true(!is.null(mst$meta$geocode_run_id))
  # Inherited from reconciled meta
  expect_equal(mst$meta$cycle_year, rec$meta$cycle_year)
  expect_equal(mst$meta$file_sha256, rec$meta$file_sha256)
  expect_equal(mst$meta$source, rec$meta$source)
})
