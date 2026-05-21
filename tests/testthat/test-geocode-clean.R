# tests/testthat/test-geocode-clean.R
#
# Tests for geocode_clean(): Melissa-returned geocoded data cleaner.
# Tests the 11-step cleaning pipeline (rename, type coercion, ZIP
# preservation, school_year validation, COUNTYNAME canonicalization,
# trimming, has_latlon coercion, data_source_map attribute, lockfile
# defense, raw_row_index/provenance preservation).
#
# Strategy: build a raw alprek_geocode_raw fixture by simulating
# geocode_read()'s output structure (a tibble with LAT/LNG character,
# ERRORCODE logical, site_zip numeric, COUNTYNAME ALL-CAPS, etc.) and a
# meta list with provenance fields.


# ---------------------------------------------------------------------------
# Helper: build a synthetic alprek_geocode_raw object inline so tests do not
# depend on a temp .xlsx round-trip (faster + more deterministic).
# Mirrors the dtype contract that readxl produces for a v1 Melissa file.
# ---------------------------------------------------------------------------
.fake_geocode_raw <- function(n = 5L,
                              school_year = "2024-2025",
                              extra_mutations = NULL) {
  # Lean on the synthetic generator for the column shape (matches v1).
  g <- alprek_synthetic_geocode(n_sites = n, n_years = 1L, seed = 42L,
                                cycle_year_anchor = as.integer(
                                  substr(school_year, 1, 4)
                                ))
  # Force the read-back dtypes that readxl produces on a v1 file:
  #   - LAT / LNG: character (Melissa contract)
  #   - ERRORCODE: logical (readxl all-NA quirk)
  #   - site_zip: numeric (ADECE source has it numeric)
  #   - COUNTYNAME: ALL-CAPS (Melissa returns ALL-CAPS like JEFFERSON)
  g$LAT <- as.character(g$LAT)
  g$LNG <- as.character(g$LNG)
  g$ERRORCODE <- as.logical(g$ERRORCODE)  # already logical NA
  g$site_zip <- as.numeric(g$site_zip)
  g$COUNTYNAME <- toupper(g$COUNTYNAME)
  g$school_year <- rep(school_year, nrow(g))

  if (is.function(extra_mutations)) g <- extra_mutations(g)

  g$raw_row_index <- seq_len(nrow(g))
  g$lineage_id <- paste0("lin_", seq_len(nrow(g)))

  meta <- list(
    path          = "/tmp/fake-melissa.xlsx",
    sheet         = "Sheet1",
    cycle_year    = "2024-2025",
    receipt_date  = "2026-03-04",
    source        = "melissa",
    file_sha256   = paste(rep("a", 64L), collapse = ""),
    file_basename = "fake-melissa.xlsx",
    git_sha       = "abc123",
    n_rows        = nrow(g),
    n_cols        = ncol(g) - 1L,  # excluding raw_row_index
    col_names     = setdiff(names(g), "raw_row_index"),
    read_at       = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    lineage_id    = g$lineage_id,
    raw_row_index = seq_len(nrow(g))
  )

  structure(list(data = tibble::as_tibble(g), meta = meta),
            class = "alprek_geocode_raw")
}


# ===========================================================================
# Assertion 1: Returns alprek_geocode_clean class
# ===========================================================================
test_that("geocode_clean() returns an alprek_geocode_clean S3 object", {
  raw <- .fake_geocode_raw(n = 5L)
  clean <- geocode_clean(raw)

  expect_s3_class(clean, "alprek_geocode_clean")
  expect_true(is.list(clean))
  expect_true(all(c("data", "cleaning_log", "meta") %in% names(clean)))
})


# ===========================================================================
# Assertion 2: LAT/LNG are numeric (not character) after clean
# ===========================================================================
test_that("LAT/LNG are coerced from character to numeric", {
  raw <- .fake_geocode_raw(n = 5L)
  expect_type(raw$data$LAT, "character")  # precondition
  expect_type(raw$data$LNG, "character")

  clean <- geocode_clean(raw)

  expect_type(clean$data$LAT, "double")
  expect_type(clean$data$LNG, "double")
  expect_true(is.numeric(clean$data$LAT))
  expect_true(is.numeric(clean$data$LNG))
  # The 6 d.p. character LATs should round-trip into a sensible AL lat range.
  expect_true(all(clean$data$LAT > 25 & clean$data$LAT < 40, na.rm = TRUE))
})


# ===========================================================================
# Assertion 3: ERRORCODE is character (not logical)
# ===========================================================================
test_that("ERRORCODE is coerced from logical to character", {
  raw <- .fake_geocode_raw(n = 5L)
  expect_type(raw$data$ERRORCODE, "logical")  # precondition

  clean <- geocode_clean(raw)

  expect_type(clean$data$ERRORCODE, "character")
})


# ===========================================================================
# Assertion 4: site_zip is character with leading zero preservation
# ===========================================================================
test_that("site_zip is character with 5-digit zero-padding", {
  raw <- .fake_geocode_raw(n = 5L, extra_mutations = function(g) {
    # Insert a fake leading-zero ZIP to demonstrate preservation.
    g$site_zip[1] <- 7401  # would print as "07401" when zero-padded
    g
  })
  expect_type(raw$data$site_zip, "double")  # precondition

  clean <- geocode_clean(raw)

  expect_type(clean$data$site_zip, "character")
  expect_equal(nchar(clean$data$site_zip[1]), 5L)
  expect_equal(clean$data$site_zip[1], "07401")
})


# ===========================================================================
# Assertion 5: school_year validated against canonical set
# ===========================================================================
test_that("school_year is validated against the canonical set", {
  raw <- .fake_geocode_raw(n = 5L, school_year = "2024-2025")
  clean <- geocode_clean(raw)

  canonical <- c("2021-2022", "2022-2023", "2023-2024",
                 "2024-2025", "2025-2026_new")
  expect_true(all(clean$data$school_year %in% canonical))
})


# ===========================================================================
# Assertion 6: COUNTYNAME is title-case (e.g., "Jefferson" not "JEFFERSON")
# ===========================================================================
test_that("COUNTYNAME is canonicalized to title case", {
  raw <- .fake_geocode_raw(n = 5L)
  expect_true(all(raw$data$COUNTYNAME == toupper(raw$data$COUNTYNAME)))

  clean <- geocode_clean(raw)

  # No longer all-caps
  expect_false(all(clean$data$COUNTYNAME ==
                     toupper(clean$data$COUNTYNAME)))
  # Expect "Jefferson", "Montgomery", etc.
  al_counties <- alprek_geocode_al_fips_counties()
  expect_true(all(clean$data$COUNTYNAME %in% al_counties$county_name))
})


# ===========================================================================
# Assertion 7: cleaning_log has at least 1 row for LAT/LNG coercion
# ===========================================================================
test_that("cleaning_log records LAT and LNG coercion", {
  raw <- .fake_geocode_raw(n = 5L)
  clean <- geocode_clean(raw)

  expect_true(any(clean$cleaning_log$rule == "coerce_LAT_to_numeric"))
  expect_true(any(clean$cleaning_log$rule == "coerce_LNG_to_numeric"))
})


# ===========================================================================
# Assertion 8: cleaning_log columns: rule, n_affected, details, severity
# ===========================================================================
test_that("cleaning_log has expected columns", {
  raw <- .fake_geocode_raw(n = 5L)
  clean <- geocode_clean(raw)

  expect_true(all(c("rule", "n_affected", "details", "severity") %in%
                    names(clean$cleaning_log)))
  expect_type(clean$cleaning_log$rule, "character")
  expect_type(clean$cleaning_log$details, "character")
  expect_type(clean$cleaning_log$severity, "character")
  # severity values must be one of {INFO, WARN, ERROR}
  expect_true(all(clean$cleaning_log$severity %in%
                    c("INFO", "WARN", "ERROR")))
})


# ===========================================================================
# Assertion 9: has_latlon is logical
# ===========================================================================
test_that("has_latlon is logical after clean", {
  raw <- .fake_geocode_raw(n = 5L)
  clean <- geocode_clean(raw)

  expect_type(clean$data$has_latlon, "logical")
})


# ===========================================================================
# Assertion 10: Provenance preserved (file_sha256, git_sha)
# ===========================================================================
test_that("meta preserves file_sha256, git_sha, and lineage provenance", {
  raw <- .fake_geocode_raw(n = 5L)
  clean <- geocode_clean(raw)

  expect_equal(clean$meta$file_sha256, raw$meta$file_sha256)
  expect_equal(clean$meta$git_sha, raw$meta$git_sha)
  expect_equal(clean$meta$geocoding_source, "melissa_v1_2026")
  expect_equal(clean$data$lineage_id, raw$data$lineage_id)
  expect_equal(clean$meta$lineage_id, raw$data$lineage_id)
})


# ===========================================================================
# Assertion 11: data_source_map attribute present
# ===========================================================================
test_that("data_source_map attribute is attached to clean$data", {
  raw <- .fake_geocode_raw(n = 5L)
  clean <- geocode_clean(raw)

  dsmap <- attr(clean$data, "data_source_map")
  expect_false(is.null(dsmap))
  expect_type(dsmap, "character")
  expect_true(length(dsmap) >= ncol(clean$data) - 1L)

  # id and adece groups -> "ADECE"
  expect_equal(unname(dsmap["row_id"]), "ADECE")
  expect_equal(unname(dsmap["site_street"]), "ADECE")
  # melissa_norm and melissa_out groups -> "Melissa-<date>"
  expect_true(grepl("^Melissa", dsmap["LAT"]))
  expect_true(grepl("^Melissa", dsmap["GEOZIP"]))
  # Receipt date should appear in the label
  expect_true(grepl("2026-03-04", dsmap["LAT"]))
})


# ===========================================================================
# Assertion 12: Unknown school_year value triggers WARN entry
# ===========================================================================
test_that("unknown school_year triggers a WARN cleaning_log entry", {
  raw <- .fake_geocode_raw(n = 5L, extra_mutations = function(g) {
    g$school_year[1] <- "1999-2000"  # not in canonical set
    g
  })
  clean <- geocode_clean(raw)

  bad_rows <- clean$cleaning_log[
    clean$cleaning_log$rule == "school_year_unknown", ]
  expect_gt(nrow(bad_rows), 0L)
  expect_true(all(bad_rows$severity == "WARN"))
  expect_gte(sum(bad_rows$n_affected), 1L)
})


# ===========================================================================
# Assertion 13: raw_row_index preserved unchanged
# ===========================================================================
test_that("raw_row_index is preserved unchanged from raw to clean", {
  raw <- .fake_geocode_raw(n = 7L)
  clean <- geocode_clean(raw)

  expect_true("raw_row_index" %in% names(clean$data))
  expect_equal(clean$data$raw_row_index, raw$data$raw_row_index)
  expect_equal(clean$data$raw_row_index, seq_len(nrow(clean$data)))
})


# ===========================================================================
# Assertion 14: n_rows unchanged from raw (when no lockfile rows leaked in)
# ===========================================================================
test_that("n_rows is unchanged from raw to clean (no lockfile rows)", {
  raw <- .fake_geocode_raw(n = 8L)
  clean <- geocode_clean(raw)

  expect_equal(clean$meta$n_rows, raw$meta$n_rows)
  expect_equal(nrow(clean$data), nrow(raw$data))
  expect_equal(clean$meta$n_rows_dropped, 0L)
})


# ===========================================================================
# Bonus: print() method runs cleanly
# ===========================================================================
test_that("print.alprek_geocode_clean() runs and returns invisible(x)", {
  raw <- .fake_geocode_raw(n = 5L)
  clean <- geocode_clean(raw)

  out <- capture.output(ret <- print(clean))
  expect_true(length(out) >= 1L)
  out_chr <- paste(out, collapse = "\n")
  expect_match(out_chr, "alprek_geocode_clean")
  expect_match(out_chr, "Geocoding source:")
  expect_match(out_chr, "Cleaning log:")
  expect_identical(ret, clean)
})


# ===========================================================================
# Bonus: invalid input errors
# ===========================================================================
test_that("geocode_clean() rejects non-alprek_geocode_raw input", {
  expect_error(geocode_clean(list(data = tibble::tibble())),
               regexp = "alprek_geocode_raw")
})
