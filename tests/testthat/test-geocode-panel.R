# tests/testthat/test-geocode-panel.R
#
# Tests for geocode_bind_years() — Step 5.2 panel scaffolding.
#
# The Melissa file itself is already a 5-year long panel. So
# geocode_bind_years() exists to bind MULTIPLE Melissa runs across release
# cycles (future v0.9.0 + v0.8.0). For v0.8.0 the typical call is a
# degenerate identity on a single master. Tests exercise both paths.


# ---------------------------------------------------------------------------
# Helpers — construct alprek_geocode_master objects without going through
# the full clean -> reconcile pipeline each time. These mirror the helpers
# in test-geocode-transform.R but are duplicated here so the file is
# self-contained.
# ---------------------------------------------------------------------------

.wrap_clean_pn <- function(df,
                            cycle_year = "2026-2027",
                            receipt_date = "2026-03-04",
                            file_sha256 = paste(rep("a", 64L), collapse = "")) {
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
    file_sha256      = file_sha256,
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


.row_decision_pn <- function(adece_lat = 33.5207, adece_lng = -86.8025,
                              melissa_lat = 33.5207, melissa_lng = -86.8025,
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


# Build an alprek_geocode_master with `n_rows` distinct sites. Site codes
# are `999Pxxxxxx` so two masters with the same `start_idx` will share row_ids
# (renewal pattern) — by design.
.make_master_pn <- function(n_rows = 3L,
                             cycle_year = "2026-2027",
                             receipt_date = "2026-03-04",
                             vendor = "melissa",
                             start_idx = 1L,
                             school_year = "2024-2025",
                             file_sha256 =
                               paste(rep("a", 64L), collapse = "")) {
  rows <- lapply(seq_len(n_rows), function(i) {
    idx <- start_idx + i - 1L
    .row_decision_pn(
      adece_lat = 33.5207 + idx * 0.001,
      adece_lng = -86.8025 - idx * 0.001,
      melissa_lat = 33.5207 + idx * 0.001,
      melissa_lng = -86.8025 - idx * 0.001,
      result_code = "GS05",
      site_code = sprintf("999P%06d", idx),
      school_year = school_year
    )
  })
  df <- dplyr::bind_rows(rows)
  clean <- .wrap_clean_pn(df, cycle_year = cycle_year,
                          receipt_date = receipt_date,
                          file_sha256 = file_sha256)
  rec <- geocode_reconcile(clean)
  cfg <- geocode_config(
    path = "/tmp/fake.xlsx",
    cycle_year = cycle_year,
    delivery_date = receipt_date,
    vendor = vendor
  )
  geocode_transform(rec, config = cfg)
}


# ===========================================================================
# 1. Single-master input returns alprek_geocode_panel S3
# ===========================================================================
test_that("single master input returns alprek_geocode_panel S3", {
  m1 <- .make_master_pn(n_rows = 3L)
  pn <- geocode_bind_years(m1)

  expect_s3_class(pn, "alprek_geocode_panel")
  expect_true(is.list(pn))
  expect_named(pn, c("data", "meta", "binding_log"), ignore.order = TRUE)
  expect_s3_class(pn$data, "tbl_df")
  expect_s3_class(pn$binding_log, "tbl_df")
  expect_equal(pn$meta$n_runs, 1L)
})


# ===========================================================================
# 2. Single-input: nrow(panel$data) == nrow(master$data)
# ===========================================================================
test_that("single-input panel data == master data (degenerate identity)", {
  m1 <- .make_master_pn(n_rows = 5L)
  pn <- geocode_bind_years(m1)

  expect_equal(nrow(pn$data), nrow(m1$data))
  expect_equal(ncol(pn$data), ncol(m1$data))
  # data is byte-identical (not just same row count)
  expect_identical(pn$data, m1$data)
})


# ===========================================================================
# 3. List of 2: combined row count = sum
# ===========================================================================
test_that("list of 2 masters: panel row count = sum", {
  m1 <- .make_master_pn(n_rows = 3L,
                         cycle_year = "2026-2027",
                         receipt_date = "2026-03-04",
                         start_idx = 1L)
  m2 <- .make_master_pn(n_rows = 4L,
                         cycle_year = "2027-2028",
                         receipt_date = "2027-09-15",
                         start_idx = 100L)
  pn <- geocode_bind_years(list(m1, m2))

  expect_equal(pn$meta$n_runs, 2L)
  expect_equal(nrow(pn$data), nrow(m1$data) + nrow(m2$data))
  expect_equal(pn$meta$n_rows_total, nrow(m1$data) + nrow(m2$data))
})


# ===========================================================================
# 4. geocode_run_id distinguishes runs in bound panel
# ===========================================================================
test_that("geocode_run_id distinguishes runs in bound panel", {
  m1 <- .make_master_pn(receipt_date = "2026-03-04", start_idx = 1L)
  m2 <- .make_master_pn(receipt_date = "2027-09-15", start_idx = 50L)
  pn <- geocode_bind_years(list(m1, m2))

  expect_true("geocode_run_id" %in% names(pn$data))
  expect_setequal(unique(pn$data$geocode_run_id),
                   c("melissa_v1_2026-03", "melissa_v1_2027-09"))
  expect_setequal(pn$meta$run_ids,
                   c("melissa_v1_2026-03", "melissa_v1_2027-09"))
})


# ===========================================================================
# 5. (row_id, geocode_run_id) is unique in bound panel even when row_ids
#    overlap across runs (renewal pattern by design)
# ===========================================================================
test_that("(row_id, geocode_run_id) unique even with row_id overlap across runs", {
  # Same start_idx -> same site_codes -> same row_ids: renewal site
  # re-geocoded each year. Allowed.
  m1 <- .make_master_pn(n_rows = 3L, start_idx = 1L,
                         receipt_date = "2026-03-04")
  m2 <- .make_master_pn(n_rows = 3L, start_idx = 1L,
                         receipt_date = "2027-09-15")
  pn <- geocode_bind_years(list(m1, m2))

  # row_ids overlap
  shared <- intersect(m1$data$row_id, m2$data$row_id)
  expect_true(length(shared) > 0L)

  # But (row_id, geocode_run_id) is unique
  keys <- paste(pn$data$row_id, pn$data$geocode_run_id, sep = "||")
  expect_equal(length(unique(keys)), nrow(pn$data))
})


# ===========================================================================
# 6. lineage_id preserved in bound panel
# ===========================================================================
test_that("lineage_id is preserved in bound panel", {
  m1 <- .make_master_pn(n_rows = 3L, start_idx = 1L,
                         receipt_date = "2026-03-04")
  m2 <- .make_master_pn(n_rows = 3L, start_idx = 100L,
                         receipt_date = "2027-09-15")
  pn <- geocode_bind_years(list(m1, m2))

  expect_true("lineage_id" %in% names(pn$data))
  expect_equal(sum(!is.na(pn$data$lineage_id)),
                sum(!is.na(m1$data$lineage_id)) +
                  sum(!is.na(m2$data$lineage_id)))
  # All lineage_ids from inputs must be present
  expect_true(all(m1$data$lineage_id %in% pn$data$lineage_id))
  expect_true(all(m2$data$lineage_id %in% pn$data$lineage_id))
})


# ===========================================================================
# 7. coord_model_status preserved (Phase 5 contract)
# ===========================================================================
test_that("coord_model_status preserved in bound panel", {
  m1 <- .make_master_pn(n_rows = 3L, start_idx = 1L,
                         receipt_date = "2026-03-04")
  m2 <- .make_master_pn(n_rows = 3L, start_idx = 100L,
                         receipt_date = "2027-09-15")
  pn <- geocode_bind_years(list(m1, m2))

  expect_true("coord_model_status" %in% names(pn$data))
  expect_true(is.factor(pn$data$coord_model_status))
  # All values from both inputs must appear (as character) in panel
  combined_chr <- as.character(pn$data$coord_model_status)
  m1_chr <- as.character(m1$data$coord_model_status)
  m2_chr <- as.character(m2$data$coord_model_status)
  expect_setequal(sort(combined_chr), sort(c(m1_chr, m2_chr)))
})


# ===========================================================================
# 8. binding_log has one row per input run
# ===========================================================================
test_that("binding_log has one row per input run", {
  m1 <- .make_master_pn(n_rows = 3L, start_idx = 1L,
                         receipt_date = "2026-03-04")
  pn_single <- geocode_bind_years(m1)
  expect_equal(nrow(pn_single$binding_log), 1L)
  expect_true(all(c("geocode_run_id", "snapshot_date",
                      "file_sha256", "n_rows",
                      "n_columns", "severity", "details") %in%
                     names(pn_single$binding_log)))

  m2 <- .make_master_pn(n_rows = 4L, start_idx = 100L,
                         receipt_date = "2027-09-15")
  pn_double <- geocode_bind_years(list(m1, m2))
  expect_equal(nrow(pn_double$binding_log), 2L)
  expect_setequal(pn_double$binding_log$geocode_run_id,
                   c("melissa_v1_2026-03", "melissa_v1_2027-09"))
  expect_equal(pn_double$binding_log$n_rows[
    pn_double$binding_log$geocode_run_id == "melissa_v1_2026-03"], 3L)
  expect_equal(pn_double$binding_log$n_rows[
    pn_double$binding_log$geocode_run_id == "melissa_v1_2027-09"], 4L)
})


# ===========================================================================
# 9. meta has n_runs and run_ids + snapshot meta
# ===========================================================================
test_that("meta carries n_runs, run_ids, snapshot_dates, file_sha256s", {
  m1 <- .make_master_pn(receipt_date = "2026-03-04",
                         file_sha256 = paste(rep("b", 64L), collapse = ""))
  m2 <- .make_master_pn(receipt_date = "2027-09-15",
                         file_sha256 = paste(rep("c", 64L), collapse = ""),
                         start_idx = 100L)
  pn <- geocode_bind_years(list(m1, m2))

  expect_equal(pn$meta$n_runs, 2L)
  expect_setequal(pn$meta$run_ids,
                   c("melissa_v1_2026-03", "melissa_v1_2027-09"))
  expect_setequal(format(pn$meta$snapshot_dates, "%Y-%m-%d"),
                   c("2026-03-04", "2027-09-15"))
  expect_setequal(pn$meta$snapshot_file_sha256s,
                   c(paste(rep("b", 64L), collapse = ""),
                     paste(rep("c", 64L), collapse = "")))
  expect_true(!is.null(pn$meta$bound_at))
  expect_true(is.character(pn$meta$bound_at))
})


# ===========================================================================
# 10. print() method works
# ===========================================================================
test_that("print method works and returns invisibly", {
  m1 <- .make_master_pn(receipt_date = "2026-03-04")
  pn <- geocode_bind_years(m1)
  expect_invisible(print(pn))
  expect_output(print(pn), "alprek_geocode_panel")
  expect_output(print(pn), "n_runs")
  expect_output(print(pn), "melissa_v1_2026-03")
})


# ===========================================================================
# 11. Argument validation: non-master input + mixed-class list rejected
# ===========================================================================
test_that("non-master input rejected", {
  expect_error(geocode_bind_years(NULL),
                regexp = "alprek_geocode_master")
  expect_error(geocode_bind_years(data.frame(x = 1)),
                regexp = "alprek_geocode_master")
  # Empty list
  expect_error(geocode_bind_years(list()),
                regexp = "empty")
})


test_that("mixed-class list rejected (one element is not a master)", {
  m1 <- .make_master_pn(receipt_date = "2026-03-04")
  expect_error(geocode_bind_years(list(m1, data.frame(x = 1))),
                regexp = "alprek_geocode_master")
})


# ===========================================================================
# 12. Duplicate geocode_run_id (same physical run twice) errors
# ===========================================================================
test_that("duplicate geocode_run_id supplied twice errors", {
  m1 <- .make_master_pn(receipt_date = "2026-03-04")
  m1b <- .make_master_pn(receipt_date = "2026-03-04",
                          start_idx = 100L) # same run_id, different rows
  expect_error(geocode_bind_years(list(m1, m1b)),
                regexp = "Duplicate geocode_run_id")
})


# ===========================================================================
# 13. (Optional) Schema mismatch produces WARN entry in binding_log
# ===========================================================================
test_that("schema mismatch across runs produces WARN entry in binding_log", {
  m1 <- .make_master_pn(n_rows = 2L, start_idx = 1L,
                         receipt_date = "2026-03-04")
  m2 <- .make_master_pn(n_rows = 2L, start_idx = 100L,
                         receipt_date = "2027-09-15")
  # Mutate m2 schema: add a column m1 doesn't have
  m2$data$extra_future_col <- "v0.9.0_new"

  pn <- geocode_bind_years(list(m1, m2))
  # Should still bind, but binding_log should have a WARN entry for m2
  expect_true(any(pn$binding_log$severity == "WARN"))
  warn_row <- pn$binding_log[pn$binding_log$severity == "WARN", ]
  expect_true(grepl("extra_future_col", warn_row$details))
  expect_true(length(pn$meta$schema_warn) > 0L)
})
