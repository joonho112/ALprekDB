# tests/testthat/test-geocode-read.R
#
# Tests for geocode_read(): Melissa-returned geocoded xlsx reader with
# provenance capture (file SHA-256, git SHA, lineage_id, raw_row_index).
#
# Strategy: write a synthetic 29-col tibble (from alprek_synthetic_geocode())
# to a temp .xlsx via openxlsx::write.xlsx(), then read it back with
# geocode_read() and verify shape, classes, provenance, and printing.


# ---------------------------------------------------------------------------
# Skip if openxlsx is not installed (Suggests-only)
# ---------------------------------------------------------------------------
skip_if_no_openxlsx <- function() {
  testthat::skip_if_not_installed("openxlsx")
  testthat::skip_if_not_installed("withr")
}


# ---------------------------------------------------------------------------
# Helper: build a synthetic 29-col tibble and write to a temp .xlsx file.
# Returns the path; caller is responsible for tempfile lifetime via withr.
# ---------------------------------------------------------------------------
.write_synthetic_geocode_xlsx <- function(path,
                                          sheet = "Sheet1",
                                          n_sites = 10L,
                                          n_years = 2L,
                                          seed = 42L) {
  g <- alprek_synthetic_geocode(n_sites = n_sites, n_years = n_years,
                                seed = seed)
  openxlsx::write.xlsx(g, file = path, sheetName = sheet,
                       overwrite = TRUE)
  invisible(path)
}


# ===========================================================================
# Assertion 1: Returns alprek_geocode_raw class
# ===========================================================================
test_that("geocode_read() returns an alprek_geocode_raw S3 object", {
  skip_if_no_openxlsx()
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  .write_synthetic_geocode_xlsx(tmp)

  raw <- geocode_read(path = tmp, sheet = "Sheet1",
                      cycle_year = "2024-2025",
                      receipt_date = "2026-03-04",
                      source = "melissa",
                      verbose = FALSE)

  expect_s3_class(raw, "alprek_geocode_raw")
  expect_true(is.list(raw))
  expect_true(all(c("data", "meta") %in% names(raw)))
})


# ===========================================================================
# Assertion 2: $data is a tibble with 29 source cols
# (read-back includes raw_row_index and lineage_id added by the reader)
# ===========================================================================
test_that("$data is a tibble carrying the 29 Melissa columns (+ reader keys)", {
  skip_if_no_openxlsx()
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  .write_synthetic_geocode_xlsx(tmp, n_sites = 10L, n_years = 2L)

  raw <- geocode_read(path = tmp,
                      cycle_year = "2024-2025",
                      receipt_date = "2026-03-04",
                      verbose = FALSE)

  expect_s3_class(raw$data, "tbl_df")
  # data carries 29 source cols + raw_row_index + lineage_id = 31
  expect_equal(ncol(raw$data), 31L)
  # the source-side n_cols (in meta) records the 29 columns
  expect_equal(raw$meta$n_cols, 29L)
  expect_true("raw_row_index" %in% names(raw$data))
  expect_true("lineage_id" %in% names(raw$data))
})


# ===========================================================================
# Assertion 3: $meta$file_sha256 is a 64-char hex string
# ===========================================================================
test_that("$meta$file_sha256 is a non-empty 64-char character", {
  skip_if_no_openxlsx()
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  .write_synthetic_geocode_xlsx(tmp)

  raw <- geocode_read(path = tmp, cycle_year = "2024-2025",
                      verbose = FALSE)

  expect_type(raw$meta$file_sha256, "character")
  expect_equal(length(raw$meta$file_sha256), 1L)
  expect_true(nzchar(raw$meta$file_sha256))
  expect_equal(nchar(raw$meta$file_sha256), 64L)
  expect_match(raw$meta$file_sha256, "^[0-9a-f]{64}$")
})


# ===========================================================================
# Assertion 4: $meta$cycle_year matches input
# ===========================================================================
test_that("$meta$cycle_year matches the input argument", {
  skip_if_no_openxlsx()
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  .write_synthetic_geocode_xlsx(tmp)

  raw <- geocode_read(path = tmp, cycle_year = "2026-2027",
                      verbose = FALSE)

  expect_equal(raw$meta$cycle_year, "2026-2027")
})


# ===========================================================================
# Assertion 5: $meta$source matches input
# ===========================================================================
test_that("$meta$source matches the input argument", {
  skip_if_no_openxlsx()
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  .write_synthetic_geocode_xlsx(tmp)

  raw <- geocode_read(path = tmp, cycle_year = "2026-2027",
                      source = "melissa",
                      verbose = FALSE)

  expect_equal(raw$meta$source, "melissa")
})


# ===========================================================================
# Assertion 6: $meta$n_rows / n_cols match data dimensions
# ===========================================================================
test_that("$meta$n_rows and n_cols match the source data dimensions", {
  skip_if_no_openxlsx()
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  .write_synthetic_geocode_xlsx(tmp, n_sites = 12L, n_years = 3L,
                                seed = 99L)

  raw <- geocode_read(path = tmp, cycle_year = "2024-2025",
                      verbose = FALSE)

  # rows: synthetic generator produces n_sites * n_years = 36 rows
  expect_equal(raw$meta$n_rows, 36L)
  expect_equal(raw$meta$n_rows, nrow(raw$data))

  # cols: meta records the 29 source cols (raw_row_index is reader-injected)
  expect_equal(raw$meta$n_cols, 29L)
  expect_equal(length(raw$meta$col_names), 29L)
})


# ===========================================================================
# Assertion 7: $data$raw_row_index is 1:nrow
# ===========================================================================
test_that("$data$raw_row_index is the sequence 1:nrow", {
  skip_if_no_openxlsx()
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  .write_synthetic_geocode_xlsx(tmp, n_sites = 8L, n_years = 2L)

  raw <- geocode_read(path = tmp, cycle_year = "2024-2025",
                      verbose = FALSE)

  expect_equal(raw$data$raw_row_index, seq_len(nrow(raw$data)))
  expect_equal(raw$meta$raw_row_index, seq_len(raw$meta$n_rows))
})


# ===========================================================================
# Assertion 8: $meta$lineage_id is unique per row (one ID per row)
# ===========================================================================
test_that("$data$lineage_id and $meta$lineage_id are unique row keys", {
  skip_if_no_openxlsx()
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  .write_synthetic_geocode_xlsx(tmp, n_sites = 15L, n_years = 2L)

  raw <- geocode_read(path = tmp, cycle_year = "2024-2025",
                      verbose = FALSE)

  expect_type(raw$meta$lineage_id, "character")
  expect_equal(length(raw$meta$lineage_id), raw$meta$n_rows)
  expect_equal(length(unique(raw$meta$lineage_id)), raw$meta$n_rows)
  expect_true(all(nzchar(raw$meta$lineage_id)))
  expect_true("lineage_id" %in% names(raw$data))
  expect_equal(raw$data$lineage_id, raw$meta$lineage_id)
  expect_equal(length(unique(raw$data$lineage_id)), nrow(raw$data))
})


# ===========================================================================
# Assertion 9: file-not-found error is informative
# ===========================================================================
test_that("geocode_read() raises an informative error when the file is missing", {
  bogus <- file.path(tempdir(),
                      sprintf("alprekdb_missing_%s.xlsx",
                              format(Sys.time(), "%Y%m%d%H%M%S")))
  expect_false(file.exists(bogus))
  expect_error(
    geocode_read(path = bogus, cycle_year = "2024-2025",
                 verbose = FALSE),
    regexp = "File not found"
  )
})


# ===========================================================================
# Assertion 10: print() runs without error and returns invisible(x)
# ===========================================================================
test_that("print.alprek_geocode_raw() runs and returns invisible(x)", {
  skip_if_no_openxlsx()
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  .write_synthetic_geocode_xlsx(tmp)

  raw <- geocode_read(path = tmp, cycle_year = "2024-2025",
                      verbose = FALSE)

  # Capture printed output (just to silence in test logs)
  out <- capture.output(ret <- print(raw))
  expect_true(length(out) >= 1L)
  # Output should mention source, sheet, rows, sha-256, cycle, receipt
  out_chr <- paste(out, collapse = "\n")
  expect_match(out_chr, "alprek_geocode_raw")
  expect_match(out_chr, "Source:")
  expect_match(out_chr, "Sheet:")
  expect_match(out_chr, "Cycle year:")
  expect_match(out_chr, "SHA-256:")
  # print() should return invisible(x)
  expect_identical(ret, raw)
})
