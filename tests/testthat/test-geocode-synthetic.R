# tests/testthat/test-geocode-synthetic.R
# Tests for alprek_synthetic_geocode() parametric generator.
# Schema contract: 29 columns matching Melissa v1 column map
# (see inst/extdata/codebooks/geocode_column_map_melissa_v1.csv).

# Canonical 29-column schema (name + R dtype check fn pairs).
.expected_geocode_schema <- function() {
  list(
    # id (5)
    row_id          = is.character,
    school_year     = is.character,
    site_name       = is.character,
    site_code       = is.character,
    geocode_address = is.character,
    # adece (7)
    site_street     = is.character,
    site_city       = is.character,
    site_state      = is.character,
    site_zip        = is.numeric,
    latitude        = is.numeric,
    longitude       = is.numeric,
    has_latlon      = is.logical,
    # melissa_norm (6)
    md_street       = is.character,
    md_city         = is.character,
    md_state        = is.character,
    GEOZIP          = is.character,
    PLUS4           = is.character,
    DPB             = is.character,
    # melissa_out (11)
    LAT             = is.character,
    LNG             = is.character,
    CT              = is.character,
    CENSUSBLOC      = is.character,
    FIPS            = is.character,
    COUNTYNAME      = is.character,
    PLACENAME       = is.character,
    PLACECODE       = is.character,
    RESULTCODE      = is.character,
    STATUSCODE      = is.character,
    ERRORCODE       = is.logical
  )
}


# ==========================================================================
# Default invocation: shape + class
# ==========================================================================

test_that("default invocation returns tibble with 29 columns", {
  g <- alprek_synthetic_geocode()
  expect_s3_class(g, "tbl_df")
  expect_equal(ncol(g), 29L)
  expect_gt(nrow(g), 0L)
})


test_that("n_sites=10, n_years=2 returns ~20 rows", {
  g <- alprek_synthetic_geocode(n_sites = 10L, n_years = 2L, seed = 42L)
  # Expected: 10 sites x 2 years = 20 rows.
  # share_missing_site_code = 0.03 just NAs site_code in-place;
  # it does not change row count, so nrow should be exactly 20.
  expect_equal(nrow(g), 20L)
})


# ==========================================================================
# Schema: column names + dtypes
# ==========================================================================

test_that("schema: all 29 expected column names are present in order", {
  g <- alprek_synthetic_geocode(n_sites = 5L, n_years = 1L, seed = 1L)
  expected_names <- names(.expected_geocode_schema())
  expect_equal(ncol(g), length(expected_names))
  expect_equal(names(g), expected_names)
})


test_that("schema: every column has the contracted dtype", {
  g <- alprek_synthetic_geocode(n_sites = 8L, n_years = 2L, seed = 7L)
  schema <- .expected_geocode_schema()
  for (col in names(schema)) {
    expect_true(schema[[col]](g[[col]]),
                info = paste("Column", col, "failed dtype check;",
                             "got class:", paste(class(g[[col]]),
                                                  collapse = "/")))
  }
})


test_that("LAT and LNG are CHARACTER (Melissa contract, not numeric)", {
  g <- alprek_synthetic_geocode(n_sites = 5L, n_years = 1L, seed = 1L)
  expect_type(g$LAT, "character")
  expect_type(g$LNG, "character")
  expect_false(is.numeric(g$LAT))
  expect_false(is.numeric(g$LNG))
})


test_that("ERRORCODE is logical and 100% NA (v0.8.0 contract)", {
  g <- alprek_synthetic_geocode(n_sites = 30L, n_years = 2L, seed = 1L)
  expect_type(g$ERRORCODE, "logical")
  expect_true(all(is.na(g$ERRORCODE)))
})


# ==========================================================================
# Invariants: keys, derived columns
# ==========================================================================

test_that("row_id is 100% unique", {
  g <- alprek_synthetic_geocode(n_sites = 40L, n_years = 3L, seed = 11L)
  expect_equal(length(unique(g$row_id)), nrow(g))
  expect_equal(sum(is.na(g$row_id)), 0L)
})


test_that("has_latlon == !is.na(latitude) for every row", {
  g <- alprek_synthetic_geocode(n_sites = 50L, n_years = 2L,
                                  share_missing_adece = 0.2,
                                  seed = 13L)
  expect_identical(g$has_latlon, !is.na(g$latitude))
  # And NA pattern is paired across latitude/longitude:
  expect_identical(is.na(g$latitude), is.na(g$longitude))
})


# ==========================================================================
# Parametric distributions
# ==========================================================================

test_that("latitude NA share approximates share_missing_adece", {
  g <- alprek_synthetic_geocode(n_sites = 200L, n_years = 5L,
                                  share_missing_adece = 0.10,
                                  seed = 20260520L)
  observed_share <- mean(is.na(g$latitude))
  # With n = 1000, deterministic seed picks ~100 NA. We size the
  # tolerance generously (+/- 2%) to permit small rounding effects
  # from share_missing_site_code reassignments.
  expect_gt(observed_share, 0.08)
  expect_lt(observed_share, 0.12)
})


test_that("RESULTCODE values are inside canonical set {GS01,GS03,GS05,GS06}", {
  g <- alprek_synthetic_geocode(n_sites = 100L, n_years = 3L, seed = 5L)
  canonical <- c("GS01", "GS03", "GS05", "GS06")
  expect_true(all(g$RESULTCODE %in% canonical))
  # And at default share_high_resultcode_agreement = 0.7, GS05 majority
  expect_gt(mean(g$RESULTCODE == "GS05"), 0.5)
})


test_that("STATUSCODE is 1:1 with RESULTCODE (v0.8.0 pairing)", {
  g <- alprek_synthetic_geocode(n_sites = 100L, n_years = 3L, seed = 5L)
  pairs <- unique(g[, c("RESULTCODE", "STATUSCODE")])
  # n_distinct(RESULTCODE) == n_distinct(pair); enforces 1:1
  expect_equal(nrow(pairs), length(unique(g$RESULTCODE)))
})


# ==========================================================================
# Determinism (seed reproducibility)
# ==========================================================================

test_that("seed determinism: same seed -> identical output", {
  g1 <- alprek_synthetic_geocode(n_sites = 12L, n_years = 2L, seed = 99L)
  g2 <- alprek_synthetic_geocode(n_sites = 12L, n_years = 2L, seed = 99L)
  expect_identical(g1, g2)
})


test_that("seed determinism: different seeds -> different output", {
  g1 <- alprek_synthetic_geocode(n_sites = 12L, n_years = 2L, seed = 1L)
  g2 <- alprek_synthetic_geocode(n_sites = 12L, n_years = 2L, seed = 2L)
  # At minimum the (numeric, jittered) coordinate vectors must differ
  expect_false(identical(g1$latitude, g2$latitude))
})


# ==========================================================================
# AL geography bounds (sanity for non-NA coords)
# ==========================================================================

test_that("non-NA ADECE coords fall inside AL bounds", {
  g <- alprek_synthetic_geocode(n_sites = 100L, n_years = 3L, seed = 17L)
  lat <- g$latitude[!is.na(g$latitude)]
  lng <- g$longitude[!is.na(g$longitude)]
  # AL bounds per geocode_config defaults: lat [30, 36], lng [-89, -84]
  expect_true(all(lat >= 30 & lat <= 36))
  expect_true(all(lng >= -89 & lng <= -84))
})


test_that("non-NA Melissa LAT/LNG fall inside AL bounds when coerced", {
  g <- alprek_synthetic_geocode(n_sites = 100L, n_years = 3L, seed = 17L)
  lat_num <- suppressWarnings(as.numeric(g$LAT))
  lng_num <- suppressWarnings(as.numeric(g$LNG))
  expect_true(all(lat_num >= 30 & lat_num <= 36, na.rm = TRUE))
  expect_true(all(lng_num >= -89 & lng_num <= -84, na.rm = TRUE))
})


# ==========================================================================
# Edge-case delegation
# ==========================================================================

test_that("edge_case='G05' returns fixture-style mini tibble", {
  g <- alprek_synthetic_geocode(edge_case = "G05")
  expect_s3_class(g, "tbl_df")
  expect_equal(nrow(g), 5L)
  # G05 mini-fixture inherits the 29-col schema
  expect_true(all(names(.expected_geocode_schema()) %in% names(g)))
  # G05 mutation lives in row 1: RESULTCODE GS03, STATUSCODE 5,
  # PLUS4 and DPB NA, LAT drifted ~3km north of latitude.
  expect_equal(g$RESULTCODE[1], "GS03")
  expect_equal(g$STATUSCODE[1], "5")
  expect_true(is.na(g$PLUS4[1]))
  expect_true(is.na(g$DPB[1]))
})


test_that("edge_case is case-insensitive", {
  g_upper <- alprek_synthetic_geocode(edge_case = "G01")
  g_lower <- alprek_synthetic_geocode(edge_case = "g01")
  expect_identical(g_upper, g_lower)
})


test_that("edge_case rejects out-of-range case ids", {
  expect_error(alprek_synthetic_geocode(edge_case = "G99"),
                regexp = "G01.*G18")
  expect_error(alprek_synthetic_geocode(edge_case = "X05"),
                regexp = "G01.*G18")
  expect_error(alprek_synthetic_geocode(edge_case = ""),
                regexp = "non-empty")
})


# ==========================================================================
# Parameter validation
# ==========================================================================

test_that("invalid share parameters throw", {
  expect_error(alprek_synthetic_geocode(share_missing_adece = -0.1))
  expect_error(alprek_synthetic_geocode(share_missing_adece = 1.5))
  expect_error(alprek_synthetic_geocode(share_missing_site_code = 1.1))
  expect_error(
    alprek_synthetic_geocode(share_high_resultcode_agreement = -0.1)
  )
})


test_that("share_missing_site_code = 0 leaves site_code fully populated", {
  g <- alprek_synthetic_geocode(n_sites = 30L, n_years = 2L,
                                  share_missing_site_code = 0,
                                  seed = 3L)
  expect_equal(sum(is.na(g$site_code)), 0L)
  expect_false(any(g$school_year == "2025-2026_new"))
})


test_that("share_missing_adece = 0 leaves ADECE coords fully populated", {
  g <- alprek_synthetic_geocode(n_sites = 30L, n_years = 2L,
                                  share_missing_adece = 0,
                                  seed = 3L)
  expect_equal(sum(is.na(g$latitude)), 0L)
  expect_equal(sum(is.na(g$longitude)), 0L)
  expect_true(all(g$has_latlon))
})
