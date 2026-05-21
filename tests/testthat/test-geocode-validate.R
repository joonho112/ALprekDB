# tests/testthat/test-geocode-validate.R
#
# Tests for geocode_validate(): 15 structured checks on an
# alprek_geocode_clean object (passing baseline + tripping fixtures via
# make_geocode_edge_case_fixture() and explicit per-check mutations).
#
# Strategy: build a "passing" alprek_geocode_clean fixture by simulating
# the geocode_clean() output structure (post-coercion: LAT/LNG numeric,
# COUNTYNAME title-case, has_latlon logical, ERRORCODE character, etc.)
# and a complete meta list. For each check, deliberately mutate one cell
# so that exactly that check trips with the expected severity.


# ---------------------------------------------------------------------------
# Helper: build a passing alprek_geocode_clean wrapper inline. Mirrors the
# post-coercion contract from geocode_clean() so the validator sees the
# same dtype layout it would in production.
# ---------------------------------------------------------------------------
.fake_geocode_clean <- function(n = 5L,
                                 school_year = "2024-2025",
                                 extra_mutations = NULL,
                                 extra_meta_mutations = NULL) {
  # Lean on the synthetic generator for the column shape (matches v1).
  cycle_anchor <- as.integer(substr(school_year, 1, 4))
  g <- alprek_synthetic_geocode(n_sites = n, n_years = 1L, seed = 42L,
                                cycle_year_anchor = cycle_anchor)
  # Coerce to post-clean dtypes:
  g$LAT <- as.numeric(g$LAT)
  g$LNG <- as.numeric(g$LNG)
  g$ERRORCODE <- as.character(g$ERRORCODE)
  g$site_zip <- as.character(g$site_zip)
  g$has_latlon <- as.logical(g$has_latlon)
  # COUNTYNAME comes out title-case from the synthetic generator already
  # ("Jefferson" etc.), so no re-case needed.
  # school_year override (force a single canonical value):
  g$school_year <- rep(school_year, nrow(g))
  # Synthetic generator sometimes flips a small share to "_new" cohort;
  # restore consistency by giving every row a non-NA site_code based on
  # row_id pattern, then re-derive row_id.
  g$site_code <- sprintf("999P%06d", seq_len(nrow(g)))
  g$row_id <- sprintf("%s_%s", g$school_year, g$site_code)

  if (is.function(extra_mutations)) g <- extra_mutations(g)

  g$raw_row_index <- seq_len(nrow(g))
  g$lineage_id <- paste0("lin_", seq_len(nrow(g)))

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
    lineage_id       = g$lineage_id,
    raw_row_index    = g$raw_row_index,
    n_rows           = nrow(g),
    n_rows_in        = nrow(g),
    n_rows_dropped   = 0L,
    cleaned_at       = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
  if (is.function(extra_meta_mutations)) meta <- extra_meta_mutations(meta)

  cleaning_log <- tibble::tibble(rule = character(0),
                                  n_affected = integer(0),
                                  details = character(0),
                                  severity = character(0))

  structure(list(data = tibble::as_tibble(g),
                  cleaning_log = cleaning_log,
                  meta = meta),
            class = "alprek_geocode_clean")
}


# Wraps a tibble (e.g. output of make_geocode_edge_case_fixture()$data) as
# an alprek_geocode_clean, applying the dtype coercions that geocode_clean()
# would have produced. Tolerates partial-tibble inputs.
.wrap_clean_from_edge <- function(df, extra_meta_mutations = NULL) {
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
  if (is.function(extra_meta_mutations)) meta <- extra_meta_mutations(meta)

  cleaning_log <- tibble::tibble(rule = character(0),
                                  n_affected = integer(0),
                                  details = character(0),
                                  severity = character(0))

  structure(list(data = df, cleaning_log = cleaning_log, meta = meta),
            class = "alprek_geocode_clean")
}


# Helper to fetch a single check row from a validation object.
.get_check <- function(v, check_id) {
  v$checks[v$checks$check_id == check_id, ]
}


# ===========================================================================
# Return type and shape
# ===========================================================================
test_that("geocode_validate() returns alprek_geocode_validation S3 object", {
  clean <- .fake_geocode_clean(n = 8L)
  v <- geocode_validate(clean)

  expect_s3_class(v, "alprek_geocode_validation")
  expect_true(is.list(v))
  expect_named(v, c("passed", "n_errors", "n_warnings", "n_info",
                    "checks", "issues"),
               ignore.order = TRUE)
  expect_true(is.logical(v$passed))
  expect_type(v$n_errors, "integer")
  expect_s3_class(v$checks, "tbl_df")
  expect_s3_class(v$issues, "tbl_df")
  # checks tibble has the documented columns:
  expect_true(all(c("check_id", "description", "status",
                    "n_issues", "details") %in% names(v$checks)))
  # issues tibble has the documented columns:
  expect_true(all(c("row_id", "check_id", "severity",
                    "value", "expected", "note") %in% names(v$issues)))
})


test_that("geocode_validate() rejects non-alprek_geocode_clean input", {
  expect_error(geocode_validate(list(data = tibble::tibble())),
                regexp = "alprek_geocode_clean")
})


test_that("geocode_validate() rejects bad strict / config args", {
  clean <- .fake_geocode_clean(n = 5L)
  expect_error(geocode_validate(clean, strict = "yes"))
  expect_error(geocode_validate(clean, config = list(foo = 1)))
})


# ===========================================================================
# Baseline: passing fixture passes ALL 15 checks at PASS or INFO
# ===========================================================================
test_that("passing fixture passes all 15 checks (n_errors == 0)", {
  clean <- .fake_geocode_clean(n = 10L)
  v <- geocode_validate(clean)

  expect_true(v$passed)
  expect_equal(v$n_errors, 0L)
  # 15 numbered checks + summary INFO -> 16 total
  expect_gte(nrow(v$checks), 15L)
  # No row-level issues at ERROR severity
  if (nrow(v$issues) > 0L) {
    expect_true(all(v$issues$severity != "ERROR"))
  }
})


# ===========================================================================
# Check 1: required_columns (ERROR)
# ===========================================================================
test_that("required_columns passes on baseline fixture", {
  clean <- .fake_geocode_clean(n = 5L)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "required_columns")
  expect_equal(ck$status, "PASS")
})

test_that("required_columns trips ERROR when a column is missing", {
  clean <- .fake_geocode_clean(n = 5L, extra_mutations = function(g) {
    g$FIPS <- NULL  # drop a required column
    g
  })
  v <- geocode_validate(clean)
  ck <- .get_check(v, "required_columns")
  expect_equal(ck$status, "ERROR")
  expect_match(ck$details, "FIPS")
  expect_false(v$passed)
})


# ===========================================================================
# Check 2: row_id_unique (ERROR)
# Tripping fixture: G15 (row_id duplicate)
# ===========================================================================
test_that("row_id_unique passes on baseline fixture", {
  clean <- .fake_geocode_clean(n = 5L)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "row_id_unique")
  expect_equal(ck$status, "PASS")
})

test_that("row_id_unique trips ERROR on G15 duplicate fixture", {
  fx <- make_geocode_edge_case_fixture("G15")
  clean <- .wrap_clean_from_edge(fx$data)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "row_id_unique")
  expect_equal(ck$status, "ERROR")
  expect_gt(ck$n_issues, 0L)
  expect_true(any(v$issues$check_id == "row_id_unique"))
  expect_false(v$passed)
})


# ===========================================================================
# Check 3: row_id_format (WARN)
# ===========================================================================
test_that("row_id_format passes on baseline fixture", {
  clean <- .fake_geocode_clean(n = 5L)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "row_id_format")
  expect_equal(ck$status, "PASS")
})

test_that("row_id_format trips WARN on malformed row_id", {
  clean <- .fake_geocode_clean(n = 5L, extra_mutations = function(g) {
    g$row_id[1] <- "not-a-valid-row-id"
    g
  })
  v <- geocode_validate(clean)
  ck <- .get_check(v, "row_id_format")
  expect_equal(ck$status, "WARN")
  expect_gte(ck$n_issues, 1L)
  expect_true(any(v$issues$check_id == "row_id_format"))
})


# ===========================================================================
# Check 4: school_year_canonical (ERROR)
# ===========================================================================
test_that("school_year_canonical passes on baseline fixture", {
  clean <- .fake_geocode_clean(n = 5L)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "school_year_canonical")
  expect_equal(ck$status, "PASS")
})

test_that("school_year_canonical trips ERROR on unknown school_year", {
  clean <- .fake_geocode_clean(n = 5L, extra_mutations = function(g) {
    g$school_year[1] <- "1999-2000"  # not in canonical set
    g
  })
  v <- geocode_validate(clean)
  ck <- .get_check(v, "school_year_canonical")
  expect_equal(ck$status, "ERROR")
  expect_gte(ck$n_issues, 1L)
  expect_false(v$passed)
})


# ===========================================================================
# Check 5: site_code_missingness_in_new_only (ERROR)
# Tripping fixture: site_code NA on a non-"_new" row
# ===========================================================================
test_that("site_code_missingness_in_new_only passes on baseline fixture", {
  clean <- .fake_geocode_clean(n = 5L)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "site_code_missingness_in_new_only")
  expect_equal(ck$status, "PASS")
})

test_that("site_code_missingness_in_new_only allows NA in _new cohort (G10 pattern)", {
  # G10 has site_code NA but school_year == "2025-2026_new" -> still PASS
  fx <- make_geocode_edge_case_fixture("G10")
  clean <- .wrap_clean_from_edge(fx$data)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "site_code_missingness_in_new_only")
  expect_equal(ck$status, "PASS")
})

test_that("site_code_missingness_in_new_only trips ERROR when site_code NA outside _new", {
  clean <- .fake_geocode_clean(n = 5L, extra_mutations = function(g) {
    g$site_code[1] <- NA_character_  # school_year stays "2024-2025"
    g
  })
  v <- geocode_validate(clean)
  ck <- .get_check(v, "site_code_missingness_in_new_only")
  expect_equal(ck$status, "ERROR")
  expect_gte(ck$n_issues, 1L)
})


# ===========================================================================
# Check 6: melissa_lat_lng_present (ERROR)
# Tripping fixture: G09 (Melissa missing)
# ===========================================================================
test_that("melissa_lat_lng_present passes on baseline fixture", {
  clean <- .fake_geocode_clean(n = 5L)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "melissa_lat_lng_present")
  expect_equal(ck$status, "PASS")
})

test_that("melissa_lat_lng_present trips ERROR on G09 Melissa-missing fixture", {
  fx <- make_geocode_edge_case_fixture("G09")
  clean <- .wrap_clean_from_edge(fx$data)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "melissa_lat_lng_present")
  expect_equal(ck$status, "ERROR")
  expect_gte(ck$n_issues, 1L)
  expect_false(v$passed)
})


# ===========================================================================
# Check 7: has_latlon_consistency (ERROR)
# Tripping fixture: G14
# ===========================================================================
test_that("has_latlon_consistency passes on baseline fixture", {
  clean <- .fake_geocode_clean(n = 5L)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "has_latlon_consistency")
  expect_equal(ck$status, "PASS")
})

test_that("has_latlon_consistency trips ERROR on G14 fixture", {
  fx <- make_geocode_edge_case_fixture("G14")
  clean <- .wrap_clean_from_edge(fx$data)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "has_latlon_consistency")
  expect_equal(ck$status, "ERROR")
  expect_gte(ck$n_issues, 1L)
  expect_false(v$passed)
})


# ===========================================================================
# Check 8: melissa_coord_in_al_bounds (ERROR)
# Tripping fixture: G12
# ===========================================================================
test_that("melissa_coord_in_al_bounds passes on baseline fixture", {
  clean <- .fake_geocode_clean(n = 5L)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "melissa_coord_in_al_bounds")
  expect_equal(ck$status, "PASS")
})

test_that("melissa_coord_in_al_bounds trips ERROR on G12 OOB fixture", {
  fx <- make_geocode_edge_case_fixture("G12")
  clean <- .wrap_clean_from_edge(fx$data)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "melissa_coord_in_al_bounds")
  expect_equal(ck$status, "ERROR")
  expect_gte(ck$n_issues, 1L)
  expect_false(v$passed)
})


# ===========================================================================
# Check 9: adece_coord_in_al_bounds (WARN)
# Tripping fixture: G11
# ===========================================================================
test_that("adece_coord_in_al_bounds passes on baseline fixture", {
  clean <- .fake_geocode_clean(n = 5L)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "adece_coord_in_al_bounds")
  expect_equal(ck$status, "PASS")
})

test_that("adece_coord_in_al_bounds trips WARN on G11 OOB fixture", {
  fx <- make_geocode_edge_case_fixture("G11")
  clean <- .wrap_clean_from_edge(fx$data)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "adece_coord_in_al_bounds")
  expect_equal(ck$status, "WARN")
  expect_gte(ck$n_issues, 1L)
})


# ===========================================================================
# Check 10: resultcode_canonical (WARN)
# Tripping fixture: G13
# ===========================================================================
test_that("resultcode_canonical passes on baseline fixture", {
  clean <- .fake_geocode_clean(n = 5L)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "resultcode_canonical")
  expect_equal(ck$status, "PASS")
})

test_that("resultcode_canonical trips WARN on G13 fixture (RESULTCODE = GS99)", {
  fx <- make_geocode_edge_case_fixture("G13")
  clean <- .wrap_clean_from_edge(fx$data)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "resultcode_canonical")
  expect_equal(ck$status, "WARN")
  expect_gte(ck$n_issues, 1L)
})


test_that("config acceptable_resultcodes does not suppress unknown RESULTCODE warning", {
  fx <- make_geocode_edge_case_fixture("G13")
  clean <- .wrap_clean_from_edge(fx$data)
  cfg <- structure(
    list(
      al_lat_bounds = c(30, 36),
      al_lng_bounds = c(-89, -84),
      acceptable_resultcodes = c("GS01", "GS05", "GS06", "GS99")
    ),
    class = "alprek_geocode_config"
  )
  v <- geocode_validate(clean, config = cfg)
  ck <- .get_check(v, "resultcode_canonical")
  expect_equal(ck$status, "WARN")
})


# ===========================================================================
# Check 11: statuscode_canonical (WARN)
# ===========================================================================
test_that("statuscode_canonical passes on baseline fixture", {
  clean <- .fake_geocode_clean(n = 5L)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "statuscode_canonical")
  expect_equal(ck$status, "PASS")
})

test_that("statuscode_canonical trips WARN on unknown STATUSCODE (G13 sets STATUSCODE = Z)", {
  fx <- make_geocode_edge_case_fixture("G13")
  clean <- .wrap_clean_from_edge(fx$data)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "statuscode_canonical")
  expect_equal(ck$status, "WARN")
  expect_gte(ck$n_issues, 1L)
})


# ===========================================================================
# Check 12: resultcode_statuscode_consistency (WARN)
# ===========================================================================
test_that("resultcode_statuscode_consistency passes on baseline fixture", {
  clean <- .fake_geocode_clean(n = 5L)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "resultcode_statuscode_consistency")
  expect_equal(ck$status, "PASS")
})

test_that("resultcode_statuscode_consistency pair map is codebook-driven", {
  sc <- alprek_geocode_statuscode_meaning()
  keep <- !is.na(sc$paired_resultcode_in_v080) &
    nzchar(sc$paired_resultcode_in_v080)
  expected <- stats::setNames(as.character(sc$code[keep]),
                              as.character(sc$paired_resultcode_in_v080[keep]))

  expect_equal(.gv_statuscode_pair_map(), expected)
  expect_setequal(names(.gv_statuscode_pair_map()),
                  c("GS01", "GS03", "GS05", "GS06"))
})


test_that("resultcode_statuscode_consistency trips WARN on broken pairing", {
  clean <- .fake_geocode_clean(n = 5L, extra_mutations = function(g) {
    # Force RC = GS05 (which should pair with STATUSCODE 'B') but assign
    # STATUSCODE '5' (which should pair with GS03). One row is enough.
    g$RESULTCODE[1] <- "GS05"
    g$STATUSCODE[1] <- "5"
    g
  })
  v <- geocode_validate(clean)
  ck <- .get_check(v, "resultcode_statuscode_consistency")
  expect_equal(ck$status, "WARN")
  expect_gte(ck$n_issues, 1L)
})


# ===========================================================================
# Check 13: errorcode_all_na_in_v080 (INFO; passes when all NA)
# ===========================================================================
test_that("errorcode_all_na_in_v080 passes when ERRORCODE is 100% NA", {
  clean <- .fake_geocode_clean(n = 5L)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "errorcode_all_na_in_v080")
  expect_equal(ck$status, "PASS")
})

test_that("errorcode_all_na_in_v080 returns INFO when ERRORCODE populated", {
  clean <- .fake_geocode_clean(n = 5L, extra_mutations = function(g) {
    g$ERRORCODE[1] <- "AE07"  # plausible Melissa error code
    g
  })
  v <- geocode_validate(clean)
  ck <- .get_check(v, "errorcode_all_na_in_v080")
  expect_equal(ck$status, "INFO")
  expect_gte(ck$n_issues, 1L)
  # INFO does not flip passed:
  expect_true(v$passed)
})


# ===========================================================================
# Check 14: provenance_complete (ERROR)
# ===========================================================================
test_that("provenance_complete passes on baseline fixture", {
  clean <- .fake_geocode_clean(n = 5L)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "provenance_complete")
  expect_equal(ck$status, "PASS")
})

test_that("provenance_complete trips ERROR when file_sha256 missing", {
  clean <- .fake_geocode_clean(
    n = 5L,
    extra_meta_mutations = function(m) {
      m$file_sha256 <- NULL
      m
    }
  )
  v <- geocode_validate(clean)
  ck <- .get_check(v, "provenance_complete")
  expect_equal(ck$status, "ERROR")
  expect_gte(ck$n_issues, 1L)
  expect_match(ck$details, "file_sha256")
  expect_false(v$passed)
})

test_that("provenance_complete trips ERROR when git_sha is NA", {
  clean <- .fake_geocode_clean(
    n = 5L,
    extra_meta_mutations = function(m) {
      m$git_sha <- NA_character_
      m
    }
  )
  v <- geocode_validate(clean)
  ck <- .get_check(v, "provenance_complete")
  expect_equal(ck$status, "ERROR")
  expect_match(ck$details, "git_sha")
})


# ===========================================================================
# Check 15: lineage_id_complete (ERROR)
# ===========================================================================
test_that("lineage_id_complete passes on baseline fixture", {
  clean <- .fake_geocode_clean(n = 5L)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "lineage_id_complete")
  expect_equal(ck$status, "PASS")
})

test_that("lineage_id_complete trips ERROR when lineage_id is missing", {
  clean <- .fake_geocode_clean(n = 5L)
  clean$data$lineage_id <- NULL
  v <- geocode_validate(clean)
  ck <- .get_check(v, "lineage_id_complete")
  expect_equal(ck$status, "ERROR")
  expect_gte(ck$n_issues, 1L)
  expect_false(v$passed)
})

test_that("lineage_id_complete trips ERROR when lineage_id is duplicated", {
  clean <- .fake_geocode_clean(n = 5L)
  clean$data$lineage_id[2] <- clean$data$lineage_id[1]
  v <- geocode_validate(clean)
  ck <- .get_check(v, "lineage_id_complete")
  expect_equal(ck$status, "ERROR")
  expect_gte(ck$n_issues, 2L)
  expect_false(v$passed)
})


# ===========================================================================
# Summary INFO check (coverage / followup / placename)
# ===========================================================================
test_that("summary_coverage is reported as INFO on baseline fixture", {
  clean <- .fake_geocode_clean(n = 10L)
  v <- geocode_validate(clean)
  ck <- .get_check(v, "summary_coverage")
  expect_equal(ck$status, "INFO")
  # details string should mention RESULTCODE coverage AND follow-up
  expect_match(ck$details, "RESULTCODE coverage")
  expect_match(ck$details, "follow-up queue")
})


# ===========================================================================
# strict mode: WARN flips passed to FALSE
# ===========================================================================
test_that("strict = TRUE treats WARN as overall failure", {
  fx <- make_geocode_edge_case_fixture("G13")  # WARN-only trip
  clean <- .wrap_clean_from_edge(fx$data)

  v_default <- geocode_validate(clean, strict = FALSE)
  expect_true(v_default$passed)  # WARN doesn't fail default mode
  expect_gt(v_default$n_warnings, 0L)

  v_strict <- geocode_validate(clean, strict = TRUE)
  expect_false(v_strict$passed)  # but does fail strict mode
})


# ===========================================================================
# config argument override
# ===========================================================================
test_that("config argument supplies AL bounds for OOB checks", {
  clean <- .fake_geocode_clean(n = 5L)
  # Use a config with VERY narrow bounds so the baseline fixture trips
  # the AL bounds check:
  fake_cfg <- structure(
    list(
      al_lat_bounds = c(31.0, 31.1),   # baseline lats are 32-34
      al_lng_bounds = c(-87.0, -86.9), # baseline lngs are -88 to -85
      acceptable_resultcodes = c("GS01", "GS02", "GS03", "GS04",
                                  "GS05", "GS06")
    ),
    class = "alprek_geocode_config"
  )
  v <- geocode_validate(clean, config = fake_cfg)
  ck <- .get_check(v, "melissa_coord_in_al_bounds")
  expect_equal(ck$status, "ERROR")  # narrow bounds force trip
  expect_gt(ck$n_issues, 0L)
})


# ===========================================================================
# Print method
# ===========================================================================
test_that("print.alprek_geocode_validation() runs and returns invisible(x)", {
  clean <- .fake_geocode_clean(n = 5L)
  v <- geocode_validate(clean)

  out <- capture.output(ret <- print(v))
  expect_true(length(out) >= 1L)
  out_chr <- paste(out, collapse = "\n")
  expect_match(out_chr, "alprek_geocode_validation")
  expect_match(out_chr, "PASSED|FAILED")
  expect_match(out_chr, "Errors:")
  expect_match(out_chr, "Checks:")
  expect_identical(ret, v)
})


test_that("print.alprek_geocode_validation() shows FAILED when errors present", {
  clean <- .fake_geocode_clean(n = 5L, extra_mutations = function(g) {
    g$LAT[1] <- NA_real_
    g
  })
  v <- geocode_validate(clean)
  out <- capture.output(print(v))
  out_chr <- paste(out, collapse = "\n")
  expect_match(out_chr, "FAILED")
})
