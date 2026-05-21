# tests/testthat/test-geocode-detect.R
#
# Tests for geocode_detect_format() and geocode_compare_deliveries().
#
# Strategy: build synthetic 29-col tibbles via alprek_synthetic_geocode(),
# write to temp .xlsx files via openxlsx, then exercise both functions
# across the 8 protocol scenarios in §7 of melissa_format_diff_protocol.md.


# ---------------------------------------------------------------------------
# Skip if openxlsx is not installed (Suggests-only)
# ---------------------------------------------------------------------------
skip_if_no_openxlsx <- function() {
  testthat::skip_if_not_installed("openxlsx")
  testthat::skip_if_not_installed("withr")
}


# ---------------------------------------------------------------------------
# Helper: write a synthetic 29-col geocode tibble to a temp .xlsx
# ---------------------------------------------------------------------------
.write_synthetic_geocode_xlsx <- function(path,
                                          df = NULL,
                                          sheet = "Sheet1",
                                          n_sites = 10L,
                                          n_years = 2L,
                                          seed = 42L) {
  if (is.null(df)) {
    df <- alprek_synthetic_geocode(n_sites = n_sites,
                                    n_years = n_years,
                                    seed = seed)
  }
  openxlsx::write.xlsx(df, file = path, sheetName = sheet,
                       overwrite = TRUE)
  invisible(path)
}


# ===========================================================================
# geocode_detect_format() — character vector input
# ===========================================================================

test_that("detect_format: exact 29-col v1 character vector -> v1, conf=1", {
  v1 <- alprek_geocode_column_map()$raw_col
  res <- geocode_detect_format(v1)
  expect_s3_class(res, "alprek_geocode_format_detection")
  expect_equal(res$format, "melissa_v1_2026")
  expect_equal(res$confidence, 1)
  expect_equal(length(res$unknown_columns), 0L)
  expect_equal(length(res$missing_v1_columns), 0L)
  expect_setequal(res$markers_found,
                   c("row_id", "LAT", "LNG", "RESULTCODE"))
})


test_that("detect_format: missing one marker -> unknown, conf=0", {
  v1 <- alprek_geocode_column_map()$raw_col
  # Drop the LAT marker
  trimmed <- setdiff(v1, "LAT")
  res <- geocode_detect_format(trimmed)
  expect_equal(res$format, "unknown")
  expect_equal(res$confidence, 0)
  expect_true("LAT" %in% res$missing_v1_columns)
})


test_that("detect_format: lowercased column names -> unknown (case-sensitive)", {
  v1 <- alprek_geocode_column_map()$raw_col
  lowered <- tolower(v1)
  res <- geocode_detect_format(lowered)
  # Markers ("LAT", "LNG", "RESULTCODE") all dropped -> unknown
  expect_equal(res$format, "unknown")
  expect_equal(res$confidence, 0)
})


test_that("detect_format: extra columns -> still v1 (conf < 1)", {
  v1 <- alprek_geocode_column_map()$raw_col
  with_extras <- c(v1, "vendor_internal_id", "audit_pass_3")
  res <- geocode_detect_format(with_extras)
  expect_equal(res$format, "melissa_v1_2026")
  expect_true(res$confidence < 1)
  expect_true(res$confidence > 0.5)
  expect_setequal(res$unknown_columns,
                   c("vendor_internal_id", "audit_pass_3"))
  expect_equal(length(res$missing_v1_columns), 0L)
})


# ===========================================================================
# geocode_detect_format() — alprek_geocode_raw object
# ===========================================================================

test_that("detect_format: alprek_geocode_raw input -> v1", {
  skip_if_no_openxlsx()
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  .write_synthetic_geocode_xlsx(tmp)
  raw <- geocode_read(path = tmp, cycle_year = "2024-2025",
                      verbose = FALSE)
  res <- geocode_detect_format(raw)
  expect_equal(res$format, "melissa_v1_2026")
  expect_equal(res$confidence, 1)
})


# ===========================================================================
# geocode_detect_format() — file path input
# ===========================================================================

test_that("detect_format: file path input reads header row", {
  skip_if_no_openxlsx()
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  .write_synthetic_geocode_xlsx(tmp)
  res <- geocode_detect_format(tmp)
  expect_equal(res$format, "melissa_v1_2026")
  expect_equal(res$confidence, 1)
})


test_that("detect_format: print method runs and returns invisible(x)", {
  v1 <- alprek_geocode_column_map()$raw_col
  res <- geocode_detect_format(v1)
  out <- capture.output(ret <- print(res))
  expect_true(length(out) >= 1L)
  expect_match(paste(out, collapse = "\n"),
                "alprek_geocode_format_detection")
  expect_identical(ret, res)
})


# ===========================================================================
# geocode_compare_deliveries() — protocol scenarios
#
# We exercise four canonical scenarios from
# log/2026-05-20_geocode-log/data/melissa_format_diff_protocol.md §7:
#   * Scenario 1: Identical file -> compatible
#   * Scenario 2: New rows added -> compatible_with_additions
#   * Scenario 3: 108 _new resolved -> compatible_with_additions
#                                       + row_id_replaced_pairs populated
#   * Scenario 7: Column renamed -> breaking
# ===========================================================================

test_that("compare: Scenario 1 — identical file -> compatible", {
  skip_if_no_openxlsx()
  tmp1 <- withr::local_tempfile(fileext = ".xlsx")
  df <- alprek_synthetic_geocode(n_sites = 6L, n_years = 2L,
                                  share_missing_site_code = 0,
                                  seed = 123L)
  .write_synthetic_geocode_xlsx(tmp1, df = df)
  # Make path_new a separate file with identical content so SHA-256s
  # may match (or differ if openxlsx randomises) — either way the
  # comparison should yield 'compatible'.
  tmp2 <- withr::local_tempfile(fileext = ".xlsx")
  .write_synthetic_geocode_xlsx(tmp2, df = df)

  diff <- geocode_compare_deliveries(tmp1, tmp2, verbose = FALSE)
  expect_s3_class(diff, "alprek_geocode_delivery_diff")
  expect_equal(diff$verdict, "compatible")
  expect_equal(nrow(diff$rows_only_old), 0L)
  expect_equal(nrow(diff$rows_only_new), 0L)
  expect_equal(nrow(diff$rows_changed), 0L)
  expect_equal(nrow(diff$row_id_replaced_pairs), 0L)
  # No schema-breaking statuses
  expect_false(any(diff$schema_diff$status %in%
                     c("added", "removed", "dtype_changed",
                        "possible_rename")))
})


test_that("compare: Scenario 2 — new rows added -> compatible_with_additions", {
  skip_if_no_openxlsx()
  tmp_old <- withr::local_tempfile(fileext = ".xlsx")
  tmp_new <- withr::local_tempfile(fileext = ".xlsx")
  df_old <- alprek_synthetic_geocode(n_sites = 6L, n_years = 2L,
                                       share_missing_site_code = 0,
                                       seed = 123L)
  # New file: append fresh rows from a separate seed (different sites)
  df_extra <- alprek_synthetic_geocode(n_sites = 3L, n_years = 1L,
                                        share_missing_site_code = 0,
                                        seed = 999L)
  # Make sure the appended row_ids are unique versus df_old
  df_extra$row_id <- paste0(df_extra$row_id, "_extra")
  df_extra$site_code <- paste0(df_extra$site_code, "_x")
  df_new <- dplyr::bind_rows(df_old, df_extra)

  .write_synthetic_geocode_xlsx(tmp_old, df = df_old)
  .write_synthetic_geocode_xlsx(tmp_new, df = df_new)

  diff <- geocode_compare_deliveries(tmp_old, tmp_new, verbose = FALSE)
  expect_equal(diff$verdict, "compatible_with_additions")
  expect_equal(nrow(diff$rows_only_old), 0L)
  expect_equal(nrow(diff$rows_only_new), nrow(df_extra))
  expect_equal(nrow(diff$rows_changed), 0L)
  # No row_id_replaced pairs (the appended rows don't reuse _new
  # placeholders)
  expect_equal(nrow(diff$row_id_replaced_pairs), 0L)
  # rows_only_new should carry a likely_replaces column (NA when no
  # pair detected)
  expect_true("likely_replaces" %in% colnames(diff$rows_only_new))
  expect_true(all(is.na(diff$rows_only_new$likely_replaces)))
})


test_that("compare: Scenario 3 — _new resolved to site_codes -> row_id_replaced_pairs", {
  skip_if_no_openxlsx()
  tmp_old <- withr::local_tempfile(fileext = ".xlsx")
  tmp_new <- withr::local_tempfile(fileext = ".xlsx")

  # OLD: include 3 _new rows for 2025-2026
  df_old <- alprek_synthetic_geocode(n_sites = 5L, n_years = 2L,
                                       share_missing_site_code = 0,
                                       seed = 321L)
  new_old_rows <- tibble::tibble(
    row_id          = sprintf("2025-2026_new_%04d", 1:3),
    school_year     = rep("2025-2026_new", 3L),
    site_name       = sprintf("New Site %d", 1:3),
    site_code       = rep(NA_character_, 3L),
    geocode_address = sprintf("%d ELM ST, Birmingham, AL, 35203",
                                100L * 1:3),
    site_street     = sprintf("%d ELM ST", 100L * 1:3),
    site_city       = rep("Birmingham", 3L),
    site_state      = rep("AL", 3L),
    site_zip        = rep(35203, 3L),
    latitude        = c(33.5207, 33.5210, 33.5215),
    longitude       = c(-86.8025, -86.8030, -86.8035),
    has_latlon      = rep(TRUE, 3L),
    md_street       = sprintf("%d Elm St", 100L * 1:3),
    md_city         = rep("Birmingham", 3L),
    md_state        = rep("AL", 3L),
    GEOZIP          = rep("35203", 3L),
    PLUS4           = c("0001", "0002", "0003"),
    DPB             = c("10", "11", "12"),
    LAT             = sprintf("%.6f", c(33.5207, 33.5210, 33.5215)),
    LNG             = sprintf("%.6f", c(-86.8025, -86.8030, -86.8035)),
    CT              = c("0100200", "0100201", "0100202"),
    CENSUSBLOC      = c("1001", "1002", "1003"),
    FIPS            = rep("01073", 3L),
    COUNTYNAME      = rep("Jefferson", 3L),
    PLACENAME       = rep("Birmingham", 3L),
    PLACECODE       = c("0150001", "0150002", "0150003"),
    RESULTCODE      = rep("GS05", 3L),
    STATUSCODE      = rep("B", 3L),
    ERRORCODE       = rep(NA, 3L)
  )
  df_old <- dplyr::bind_rows(df_old, new_old_rows)

  # NEW: 3 _new rows resolved to assigned site_codes (school_year
  # changes _new -> 2025-2026, site_code assigned, row_id rebuilt).
  resolved_rows <- new_old_rows
  resolved_rows$school_year <- rep("2025-2026", 3L)
  resolved_rows$site_code   <- sprintf("999P77%04d", 1:3)
  resolved_rows$row_id      <- sprintf("2025-2026_%s",
                                          resolved_rows$site_code)

  # Renewable rows in OLD also appear in NEW unchanged
  renew_old <- df_old[df_old$school_year != "2025-2026_new", ,
                       drop = FALSE]
  df_new <- dplyr::bind_rows(renew_old, resolved_rows)

  .write_synthetic_geocode_xlsx(tmp_old, df = df_old)
  .write_synthetic_geocode_xlsx(tmp_new, df = df_new)

  diff <- geocode_compare_deliveries(tmp_old, tmp_new, verbose = FALSE)
  expect_equal(diff$verdict, "compatible_with_additions")
  expect_equal(nrow(diff$row_id_replaced_pairs), 3L)
  expect_equal(sort(diff$row_id_replaced_pairs$old_row_id),
                sort(new_old_rows$row_id))
  expect_equal(sort(diff$row_id_replaced_pairs$new_row_id),
                sort(resolved_rows$row_id))
  expect_true(all(diff$row_id_replaced_pairs$school_year_old ==
                     "2025-2026_new"))
  expect_true(all(diff$row_id_replaced_pairs$school_year_new ==
                     "2025-2026"))
  expect_true(all(!is.na(diff$row_id_replaced_pairs$assigned_site_code)))
  # rows_only_old should be the 3 _new rows annotated with
  # likely_replaced_by; rows_only_new likewise with likely_replaces
  expect_equal(nrow(diff$rows_only_old), 3L)
  expect_equal(nrow(diff$rows_only_new), 3L)
  expect_true("likely_replaced_by" %in% colnames(diff$rows_only_old))
  expect_true(all(!is.na(diff$rows_only_old$likely_replaced_by)))
  expect_true("likely_replaces" %in% colnames(diff$rows_only_new))
  expect_true(all(!is.na(diff$rows_only_new$likely_replaces)))
})


test_that("compare: Scenario 7 — column renamed -> breaking", {
  skip_if_no_openxlsx()
  tmp_old <- withr::local_tempfile(fileext = ".xlsx")
  tmp_new <- withr::local_tempfile(fileext = ".xlsx")

  df_old <- alprek_synthetic_geocode(n_sites = 5L, n_years = 1L,
                                       share_missing_site_code = 0,
                                       seed = 11L)
  df_new <- df_old
  # Rename `site_zip` -> `zip` (per protocol §4.1, §7)
  names(df_new)[names(df_new) == "site_zip"] <- "zip"

  .write_synthetic_geocode_xlsx(tmp_old, df = df_old)
  .write_synthetic_geocode_xlsx(tmp_new, df = df_new)

  diff <- geocode_compare_deliveries(tmp_old, tmp_new, verbose = FALSE)
  expect_equal(diff$verdict, "breaking")
  # site_zip removed + zip added OR detected as possible_rename
  schema <- diff$schema_diff
  breaking <- schema$status %in% c("added", "removed",
                                     "dtype_changed", "possible_rename")
  expect_true(any(breaking))
  # At least one of site_zip / zip is reported
  reported_old <- na.omit(schema$column_old[breaking])
  reported_new <- na.omit(schema$column_new[breaking])
  expect_true("site_zip" %in% reported_old || "zip" %in% reported_new)
})


# ===========================================================================
# geocode_compare_deliveries() — additional edge cases
# ===========================================================================

test_that("compare: dtype change (LAT character -> numeric) -> breaking", {
  skip_if_no_openxlsx()
  tmp_old <- withr::local_tempfile(fileext = ".xlsx")
  tmp_new <- withr::local_tempfile(fileext = ".xlsx")

  df_old <- alprek_synthetic_geocode(n_sites = 5L, n_years = 1L,
                                       share_missing_site_code = 0,
                                       seed = 99L)
  df_new <- df_old
  # Convert LAT/LNG to numeric (a forbidden vendor-pipeline change)
  df_new$LAT <- as.numeric(df_new$LAT)
  df_new$LNG <- as.numeric(df_new$LNG)

  .write_synthetic_geocode_xlsx(tmp_old, df = df_old)
  .write_synthetic_geocode_xlsx(tmp_new, df = df_new)

  diff <- geocode_compare_deliveries(tmp_old, tmp_new, verbose = FALSE)
  expect_equal(diff$verdict, "breaking")
  # Expect a dtype_changed row for LAT or LNG (at least one)
  schema <- diff$schema_diff
  expect_true(any(schema$status == "dtype_changed"))
  changed_cols <- schema$column_new[schema$status == "dtype_changed"]
  expect_true(any(c("LAT", "LNG") %in% changed_cols))
})


test_that("compare: row_id collision in path_new -> breaking", {
  skip_if_no_openxlsx()
  tmp_old <- withr::local_tempfile(fileext = ".xlsx")
  tmp_new <- withr::local_tempfile(fileext = ".xlsx")

  df_old <- alprek_synthetic_geocode(n_sites = 5L, n_years = 1L,
                                       share_missing_site_code = 0,
                                       seed = 55L)
  df_new <- df_old
  # Force a duplicate row_id
  df_new$row_id[2] <- df_new$row_id[1]

  .write_synthetic_geocode_xlsx(tmp_old, df = df_old)
  .write_synthetic_geocode_xlsx(tmp_new, df = df_new)

  diff <- geocode_compare_deliveries(tmp_old, tmp_new, verbose = FALSE)
  expect_equal(diff$verdict, "breaking")
  expect_true(any(grepl("duplicate row_id",
                          diff$verdict_reasons)))
})


test_that("compare: address corrected on existing row -> rows_changed populated", {
  skip_if_no_openxlsx()
  tmp_old <- withr::local_tempfile(fileext = ".xlsx")
  tmp_new <- withr::local_tempfile(fileext = ".xlsx")

  df_old <- alprek_synthetic_geocode(n_sites = 5L, n_years = 1L,
                                       share_missing_site_code = 0,
                                       seed = 77L)
  df_new <- df_old
  # Correct site_street + geocode_address for row 1
  df_new$site_street[1]     <- "999 NEW ADDRESS ST"
  df_new$geocode_address[1] <- "999 NEW ADDRESS ST, Birmingham, AL, 35203"

  .write_synthetic_geocode_xlsx(tmp_old, df = df_old)
  .write_synthetic_geocode_xlsx(tmp_new, df = df_new)

  diff <- geocode_compare_deliveries(tmp_old, tmp_new, verbose = FALSE)
  expect_equal(diff$verdict, "compatible_with_additions")
  expect_gte(nrow(diff$rows_changed), 2L)  # 2 columns changed for 1 row
  changed_cols <- unique(diff$rows_changed$column)
  expect_true("site_street" %in% changed_cols)
  expect_true("geocode_address" %in% changed_cols)
})


test_that("compare: new RESULTCODE level (e.g., GS02) -> compatible_with_additions", {
  skip_if_no_openxlsx()
  tmp_old <- withr::local_tempfile(fileext = ".xlsx")
  tmp_new <- withr::local_tempfile(fileext = ".xlsx")

  df_old <- alprek_synthetic_geocode(n_sites = 5L, n_years = 1L,
                                       share_missing_site_code = 0,
                                       seed = 88L)
  df_new <- df_old
  df_new$RESULTCODE[1] <- "GS02"
  df_new$STATUSCODE[1] <- "X"  # new statuscode too

  .write_synthetic_geocode_xlsx(tmp_old, df = df_old)
  .write_synthetic_geocode_xlsx(tmp_new, df = df_new)

  diff <- geocode_compare_deliveries(tmp_old, tmp_new, verbose = FALSE)
  expect_equal(diff$verdict, "compatible_with_additions")
  added_levels <- diff$value_set_diff[
    diff$value_set_diff$status == "added", , drop = FALSE
  ]
  expect_true("RESULTCODE" %in% added_levels$column)
  expect_true("GS02" %in% added_levels$value)
})


test_that("compare: meta carries SHA-256, sheet, dims for both files", {
  skip_if_no_openxlsx()
  tmp_old <- withr::local_tempfile(fileext = ".xlsx")
  tmp_new <- withr::local_tempfile(fileext = ".xlsx")
  df <- alprek_synthetic_geocode(n_sites = 4L, n_years = 1L,
                                  share_missing_site_code = 0,
                                  seed = 7L)
  .write_synthetic_geocode_xlsx(tmp_old, df = df)
  .write_synthetic_geocode_xlsx(tmp_new, df = df)
  diff <- geocode_compare_deliveries(tmp_old, tmp_new, verbose = FALSE)

  expect_equal(diff$meta$path_old, tmp_old)
  expect_equal(diff$meta$path_new, tmp_new)
  expect_match(diff$meta$sha256_old, "^[0-9a-f]{64}$")
  expect_match(diff$meta$sha256_new, "^[0-9a-f]{64}$")
  expect_equal(diff$meta$sheet_old, "Sheet1")
  expect_equal(diff$meta$sheet_new, "Sheet1")
  expect_equal(diff$meta$n_rows_old, nrow(df))
  expect_equal(diff$meta$n_rows_new, nrow(df))
  expect_equal(diff$meta$n_cols_old, ncol(df))
  expect_equal(diff$meta$n_cols_new, ncol(df))
})


test_that("compare: summary tibble has expected metrics", {
  skip_if_no_openxlsx()
  tmp_old <- withr::local_tempfile(fileext = ".xlsx")
  tmp_new <- withr::local_tempfile(fileext = ".xlsx")
  df <- alprek_synthetic_geocode(n_sites = 4L, n_years = 1L,
                                  share_missing_site_code = 0,
                                  seed = 21L)
  .write_synthetic_geocode_xlsx(tmp_old, df = df)
  .write_synthetic_geocode_xlsx(tmp_new, df = df)
  diff <- geocode_compare_deliveries(tmp_old, tmp_new, verbose = FALSE)

  expect_s3_class(diff$summary, "tbl_df")
  expect_true(all(c("metric", "value") %in% colnames(diff$summary)))
  expect_true("n_rows_old" %in% diff$summary$metric)
  expect_true("n_rows_new" %in% diff$summary$metric)
  expect_true("n_schema_in_both" %in% diff$summary$metric)
  expect_true("n_row_id_replaced_pairs" %in% diff$summary$metric)
})


test_that("compare: print method returns invisible(x) and includes verdict", {
  skip_if_no_openxlsx()
  tmp_old <- withr::local_tempfile(fileext = ".xlsx")
  tmp_new <- withr::local_tempfile(fileext = ".xlsx")
  df <- alprek_synthetic_geocode(n_sites = 4L, n_years = 1L,
                                  share_missing_site_code = 0,
                                  seed = 33L)
  .write_synthetic_geocode_xlsx(tmp_old, df = df)
  .write_synthetic_geocode_xlsx(tmp_new, df = df)
  diff <- geocode_compare_deliveries(tmp_old, tmp_new, verbose = FALSE)

  out <- capture.output(ret <- print(diff))
  expect_identical(ret, diff)
  expect_match(paste(out, collapse = "\n"),
                "alprek_geocode_delivery_diff")
  expect_match(paste(out, collapse = "\n"), "verdict:")
})


test_that("compare: missing path raises informative error", {
  bogus_old <- file.path(tempdir(),
                          sprintf("alprekdb_missing_old_%s.xlsx",
                                  format(Sys.time(), "%Y%m%d%H%M%S")))
  bogus_new <- file.path(tempdir(),
                          sprintf("alprekdb_missing_new_%s.xlsx",
                                  format(Sys.time(), "%Y%m%d%H%M%S")))
  expect_false(file.exists(bogus_old))
  expect_error(
    geocode_compare_deliveries(bogus_old, bogus_new, verbose = FALSE),
    regexp = "File not found"
  )
})
