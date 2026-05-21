# tests/testthat/test-geocode-export.R
#
# Tests for R/geocode-export.R (Step 5.3 - critical Goal #3 deliverable):
#   - 5 export formats (csv, parquet, xlsx, rds, dta) round-trip cleanly
#   - lineage_id and coord_model_status preserved across all formats
#   - geocode_export_followup_queue() writes a CSV + carries privacy attrs
#   - internal-use header comment present in the followup CSV
#   - Followup queue schema (lineage_id first, all 16 columns)
#   - Auto-path uses output/geocode/ and creates the directory
#   - Suggests-package missing -> informative error (parquet/excel/dta)
#   - Argument validation (non-master/panel/reconciled input rejected)
#   - include_summary = TRUE for excel adds a "Summary" sheet
#   - CSV is UTF-8 encoded (ASCII safe)


# ---------------------------------------------------------------------------
# Local helpers (mirror the pattern in test-geocode-transform.R /
# test-geocode-panel.R so this file is order-independent).
# ---------------------------------------------------------------------------

.wrap_clean_ex <- function(df,
                            cycle_year = "2026-2027",
                            receipt_date = "2026-03-04",
                            file_sha256 = paste(rep("a", 64L),
                                                collapse = "")) {
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
  structure(list(data = df,
                  cleaning_log = tibble::tibble(rule = character(0),
                                                n_affected = integer(0),
                                                details = character(0),
                                                severity = character(0)),
                  meta = meta),
             class = "alprek_geocode_clean")
}


.row_decision_ex <- function(adece_lat = 33.5207, adece_lng = -86.8025,
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


.lng_offset_for_m_ex <- function(meters, lat_deg) {
  R <- 6371000
  d_rad <- meters / R
  dlon <- d_rad / cos(lat_deg * pi / 180)
  dlon * 180 / pi
}


# Build an alprek_geocode_master with both followup and non-followup rows,
# spanning several school_years for sort tests.
.make_master_ex <- function(cycle_year = "2026-2027",
                             receipt_date = "2026-03-04") {
  d500 <- .lng_offset_for_m_ex(500, 33.5)
  d30000 <- .lng_offset_for_m_ex(30000, 33.5)
  rows <- dplyr::bind_rows(
    # D4 (within threshold) -> NO followup
    .row_decision_ex(adece_lat = 33.5, adece_lng = -86.8,
                     melissa_lat = 33.5, melissa_lng = -86.8,
                     result_code = "GS05", site_code = "999P000010",
                     school_year = "2025-2026"),
    # D5 -> followup, disagreement_above_threshold
    .row_decision_ex(adece_lat = 33.5, adece_lng = -86.8,
                     melissa_lat = 33.5, melissa_lng = -86.8 + d500,
                     result_code = "GS05", site_code = "999P000020",
                     school_year = "2024-2025"),
    # D6 -> followup + disputed_melissa, disagreement_gross
    .row_decision_ex(adece_lat = 33.5, adece_lng = -86.8,
                     melissa_lat = 33.5, melissa_lng = -86.8 + d30000,
                     result_code = "GS05", site_code = "999P000030",
                     school_year = "2025-2026"),
    # D10 -> followup + disputed_melissa, GS03 always flag
    .row_decision_ex(adece_lat = 33.5, adece_lng = -86.8,
                     melissa_lat = 33.5, melissa_lng = -86.8,
                     result_code = "GS03", site_code = "999P000040",
                     school_year = "2024-2025"),
    # D15 -> followup, both_missing
    .row_decision_ex(adece_lat = NA_real_, adece_lng = NA_real_,
                     melissa_lat = NA_real_, melissa_lng = NA_real_,
                     result_code = NA_character_, site_code = "999P000050",
                     school_year = "2025-2026")
  )
  clean <- .wrap_clean_ex(rows, cycle_year = cycle_year,
                          receipt_date = receipt_date)
  rec   <- geocode_reconcile(clean)
  cfg   <- geocode_config(path = "/tmp/fake.xlsx",
                          cycle_year = cycle_year,
                          delivery_date = receipt_date,
                          vendor = "melissa")
  geocode_transform(rec, config = cfg)
}


# ===========================================================================
# 1. CSV round-trip: row count + columns preserved
# ===========================================================================
test_that("CSV export round-trips row count and columns", {
  mst <- .make_master_ex()
  p <- withr::local_tempfile(fileext = ".csv")
  geocode_export_csv(mst, p)

  expect_true(file.exists(p))
  back <- read.csv(p, stringsAsFactors = FALSE, encoding = "UTF-8")
  expect_equal(nrow(back), nrow(mst$data))
  expect_equal(ncol(back), ncol(mst$data))
})


# ===========================================================================
# 2. CSV preserves lineage_id and coord_model_status
# ===========================================================================
test_that("CSV export preserves lineage_id and coord_model_status", {
  mst <- .make_master_ex()
  p <- withr::local_tempfile(fileext = ".csv")
  geocode_export_csv(mst, p)
  back <- read.csv(p, stringsAsFactors = FALSE, encoding = "UTF-8")

  expect_true("lineage_id" %in% names(back))
  expect_true("coord_model_status" %in% names(back))
  expect_setequal(back$lineage_id, mst$data$lineage_id)

  # All non-NA coord_model_status values from in-memory should survive in CSV
  expected_levels <- as.character(mst$data$coord_model_status)
  expect_setequal(unique(back$coord_model_status), unique(expected_levels))
})


# ===========================================================================
# 3. RDS export: full round-trip identical S3 object
# ===========================================================================
test_that("RDS export round-trips the full alprek_geocode_master S3", {
  mst <- .make_master_ex()
  p <- withr::local_tempfile(fileext = ".rds")
  geocode_export_rds(mst, p)
  back <- readRDS(p)

  expect_s3_class(back, "alprek_geocode_master")
  expect_equal(nrow(back$data), nrow(mst$data))
  expect_equal(back$data$lineage_id, mst$data$lineage_id)
  # Factor levels preserved (RDS is the canonical native R format)
  expect_identical(levels(back$data$coord_model_status),
                   levels(mst$data$coord_model_status))
  expect_true(is.ordered(back$data$coord_model_status))
})


# ===========================================================================
# 4. Parquet round-trip (if arrow available)
# ===========================================================================
test_that("Parquet export round-trips row count + lineage_id (if arrow available)", {
  skip_if_not_installed("arrow")
  mst <- .make_master_ex()
  p <- withr::local_tempfile(fileext = ".parquet")
  geocode_export_parquet(mst, p)

  back <- as.data.frame(arrow::read_parquet(p))
  expect_equal(nrow(back), nrow(mst$data))
  expect_true("lineage_id" %in% names(back))
  expect_setequal(back$lineage_id, mst$data$lineage_id)
  expect_true("coord_model_status" %in% names(back))
})


# ===========================================================================
# 5. Excel round-trip + include_summary
# ===========================================================================
test_that("Excel export round-trips row count (if openxlsx available)", {
  skip_if_not_installed("openxlsx")
  mst <- .make_master_ex()
  p <- withr::local_tempfile(fileext = ".xlsx")
  geocode_export_excel(mst, p)

  expect_true(file.exists(p))
  sheets <- openxlsx::getSheetNames(p)
  expect_true("Geocode" %in% sheets)
  expect_false("Summary" %in% sheets)  # default include_summary = FALSE

  back <- openxlsx::read.xlsx(p, "Geocode")
  expect_equal(nrow(back), nrow(mst$data))
  expect_true("lineage_id" %in% names(back))
  expect_true("coord_model_status" %in% names(back))
})


test_that("Excel export with include_summary = TRUE adds Summary sheet", {
  skip_if_not_installed("openxlsx")
  mst <- .make_master_ex()
  p <- withr::local_tempfile(fileext = ".xlsx")
  geocode_export_excel(mst, p, include_summary = TRUE)

  sheets <- openxlsx::getSheetNames(p)
  expect_true("Geocode" %in% sheets)
  expect_true("Summary" %in% sheets)

  summary_df <- openxlsx::read.xlsx(p, "Summary")
  expect_true(all(c("section", "value", "n") %in% names(summary_df)))
  expect_true("coord_model_status" %in% summary_df$section)
  expect_true("lat_source" %in% summary_df$section)
  expect_true("n_followup" %in% summary_df$section)
})


# ===========================================================================
# 6. Stata round-trip (if haven available)
# ===========================================================================
test_that("Stata export round-trips row count (if haven available)", {
  skip_if_not_installed("haven")
  mst <- .make_master_ex()
  p <- withr::local_tempfile(fileext = ".dta")
  geocode_export_stata(mst, p)

  back <- haven::read_dta(p)
  expect_equal(nrow(back), nrow(mst$data))
  expect_true("lineage_id" %in% names(back))
  # coord_model_status survives as character labels (factors coerced)
  expect_true("coord_model_status" %in% names(back))
  expect_setequal(unique(as.character(back$coord_model_status)),
                  unique(as.character(mst$data$coord_model_status)))
})


# ===========================================================================
# 7. Auto-path uses output/geocode/ and creates directory
# ===========================================================================
test_that("Auto-path creates output/geocode/ and uses geocode_run_id naming", {
  mst <- .make_master_ex(receipt_date = "2026-03-04")
  withr::with_dir(withr::local_tempdir(), {
    p <- geocode_export_csv(mst)
    expect_true(file.exists(p))
    expect_match(p, "^output/geocode/geocode_melissa_v1_2026-03\\.csv$")
    expect_true(dir.exists("output/geocode"))
  })
})


test_that("Auto-path uses panel run-id concatenation for panels", {
  m1 <- .make_master_ex(receipt_date = "2026-03-04")
  pn <- geocode_bind_years(m1)
  withr::with_dir(withr::local_tempdir(), {
    p <- geocode_export_csv(pn)
    expect_true(file.exists(p))
    expect_match(p, "^output/geocode/geocode_panel_.*\\.csv$")
  })
})


# ===========================================================================
# 8. Argument validation: rejects non-supported classes
# ===========================================================================
test_that("Argument validation rejects non-supported input classes", {
  p <- withr::local_tempfile(fileext = ".csv")
  expect_error(geocode_export_csv(data.frame(x = 1), p),
                regexp = "alprek_geocode_master")
  expect_error(geocode_export_csv(list(data = tibble::tibble()), p),
                regexp = "alprek_geocode_master")
  expect_error(geocode_export_csv(NULL, p),
                regexp = "alprek_geocode_master")

  p_rds <- withr::local_tempfile(fileext = ".rds")
  expect_error(geocode_export_rds(list(), p_rds),
                regexp = "alprek_geocode_master")
})


# ===========================================================================
# 9. geocode_export_followup_queue: writes CSV at auto-path
# ===========================================================================
test_that("geocode_export_followup_queue() writes CSV at auto-generated path", {
  mst <- .make_master_ex(cycle_year = "2026-2027")
  rec <- geocode_reconcile(.wrap_clean_ex(
    dplyr::bind_rows(
      .row_decision_ex(adece_lat = 33.5, adece_lng = -86.8,
                       melissa_lat = 33.5,
                       melissa_lng = -86.8 + .lng_offset_for_m_ex(500, 33.5),
                       result_code = "GS05", site_code = "999P000020",
                       school_year = "2024-2025"),
      .row_decision_ex(adece_lat = NA_real_, adece_lng = NA_real_,
                       melissa_lat = NA_real_, melissa_lng = NA_real_,
                       result_code = NA_character_, site_code = "999P000050",
                       school_year = "2024-2025")
    ),
    cycle_year = "2026-2027"))
  withr::with_dir(withr::local_tempdir(), {
    out <- geocode_export_followup_queue(rec)
    expect_true(file.exists("output/geocode/sites_needing_geocoding_2026-2027.csv"))
    expect_match(attr(out, "output_path"),
                  "sites_needing_geocoding_2026-2027\\.csv$")
    expect_true(dir.exists("output/geocode"))
  })
})


# ===========================================================================
# 10. Internal-use header comment present
# ===========================================================================
test_that("geocode_export_followup_queue() internal-use header is in the CSV", {
  d500 <- .lng_offset_for_m_ex(500, 33.5)
  rows <- .row_decision_ex(adece_lat = 33.5, adece_lng = -86.8,
                           melissa_lat = 33.5, melissa_lng = -86.8 + d500,
                           result_code = "GS05", site_code = "999P000020",
                           school_year = "2024-2025")
  rec <- geocode_reconcile(.wrap_clean_ex(rows, cycle_year = "2026-2027"))
  p <- withr::local_tempfile(fileext = ".csv")
  geocode_export_followup_queue(rec, path = p, internal_use = TRUE)

  lines <- readLines(p, encoding = "UTF-8")
  expect_match(lines[1], "INTERNAL USE", fixed = TRUE)
  expect_match(lines[1], "DO NOT REDISTRIBUTE", fixed = TRUE)
  # Should have a Generated-by line too
  expect_true(any(grepl("Generated by geocode_export_followup_queue", lines)))
  # Should have cycle_year line
  expect_true(any(grepl("cycle_year=2026-2027", lines, fixed = TRUE)))

  # With internal_use=FALSE no header is prepended
  p2 <- withr::local_tempfile(fileext = ".csv")
  geocode_export_followup_queue(rec, path = p2, internal_use = FALSE)
  lines2 <- readLines(p2, encoding = "UTF-8")
  expect_false(any(grepl("INTERNAL USE", lines2, fixed = TRUE)))
})


# ===========================================================================
# 11. Followup queue includes ALL documented columns with lineage_id FIRST
# ===========================================================================
test_that("geocode_export_followup_queue() CSV columns match spec, lineage_id first", {
  d500 <- .lng_offset_for_m_ex(500, 33.5)
  rows <- .row_decision_ex(adece_lat = 33.5, adece_lng = -86.8,
                           melissa_lat = 33.5, melissa_lng = -86.8 + d500,
                           result_code = "GS05", site_code = "999P000020",
                           school_year = "2024-2025")
  rec <- geocode_reconcile(.wrap_clean_ex(rows, cycle_year = "2026-2027"))
  p <- withr::local_tempfile(fileext = ".csv")
  geocode_export_followup_queue(rec, path = p, internal_use = FALSE)

  # Read just the CSV content (no header skipping needed)
  back <- read.csv(p, stringsAsFactors = FALSE, encoding = "UTF-8")
  expected_cols <- c(
    "lineage_id", "row_id", "school_year", "site_code", "site_name",
    "site_street", "site_city", "site_state", "site_zip",
    "lat_source", "coord_agreement_band", "distance_adece_melissa_m",
    "melissa_result_code", "lat_precision", "followup_reason",
    "suggested_action"
  )
  expect_identical(names(back), expected_cols)
  # lineage_id IS first
  expect_equal(names(back)[1], "lineage_id")
})


# ===========================================================================
# 12. Followup queue: privacy attribute carried on return value
# ===========================================================================
test_that("geocode_export_followup_queue() returns tibble with privacy attrs", {
  d500 <- .lng_offset_for_m_ex(500, 33.5)
  rows <- .row_decision_ex(adece_lat = 33.5, adece_lng = -86.8,
                           melissa_lat = 33.5, melissa_lng = -86.8 + d500,
                           result_code = "GS05", site_code = "999P000020",
                           school_year = "2024-2025")
  rec <- geocode_reconcile(.wrap_clean_ex(rows, cycle_year = "2026-2027"))
  p <- withr::local_tempfile(fileext = ".csv")
  out <- geocode_export_followup_queue(rec, path = p, internal_use = TRUE)

  expect_s3_class(out, "tbl_df")
  expect_equal(attr(out, "privacy_level"), "internal_address_followup")
  expect_true(isTRUE(attr(out, "contains_address_fields")))
  expect_true(isTRUE(attr(out, "internal_use")))

  # internal_use = FALSE -> no internal_use attribute set
  out2 <- geocode_export_followup_queue(rec,
                                         path = withr::local_tempfile(fileext = ".csv"),
                                         internal_use = FALSE)
  expect_null(attr(out2, "internal_use"))
})


# ===========================================================================
# 13. Followup queue: accepts an alprek_geocode_master (transformed)
# ===========================================================================
test_that("geocode_export_followup_queue() accepts an alprek_geocode_master", {
  mst <- .make_master_ex(cycle_year = "2026-2027")
  withr::with_dir(withr::local_tempdir(), {
    out <- geocode_export_followup_queue(mst)
    expect_s3_class(out, "tbl_df")
    expect_true(nrow(out) >= 1L)
    expect_true(file.exists("output/geocode/sites_needing_geocoding_2026-2027.csv"))
  })
})


# ===========================================================================
# 14. Argument validation on followup queue exporter
# ===========================================================================
test_that("geocode_export_followup_queue() validates arguments", {
  expect_error(geocode_export_followup_queue(NULL),
                regexp = "alprek_geocode")
  expect_error(geocode_export_followup_queue(data.frame(x = 1)),
                regexp = "alprek_geocode")

  d500 <- .lng_offset_for_m_ex(500, 33.5)
  rec <- geocode_reconcile(.wrap_clean_ex(
    .row_decision_ex(adece_lat = 33.5, adece_lng = -86.8,
                     melissa_lat = 33.5, melissa_lng = -86.8 + d500,
                     result_code = "GS05"),
    cycle_year = "2026-2027"))
  expect_error(geocode_export_followup_queue(rec, include_disputed = NA),
                regexp = "include_disputed")
  expect_error(geocode_export_followup_queue(rec, internal_use = NA),
                regexp = "internal_use")
  expect_error(geocode_export_followup_queue(rec, cycle_year = 2026),
                regexp = "cycle_year")
})


# ===========================================================================
# 15. cycle_year override changes the auto-path token
# ===========================================================================
test_that("cycle_year override changes the auto-path and header", {
  d500 <- .lng_offset_for_m_ex(500, 33.5)
  rec <- geocode_reconcile(.wrap_clean_ex(
    .row_decision_ex(adece_lat = 33.5, adece_lng = -86.8,
                     melissa_lat = 33.5, melissa_lng = -86.8 + d500,
                     result_code = "GS05"),
    cycle_year = "2026-2027"))
  withr::with_dir(withr::local_tempdir(), {
    out <- geocode_export_followup_queue(rec, cycle_year = "2099-2100")
    expect_true(file.exists("output/geocode/sites_needing_geocoding_2099-2100.csv"))
    lines <- readLines("output/geocode/sites_needing_geocoding_2099-2100.csv",
                        encoding = "UTF-8")
    expect_true(any(grepl("cycle_year=2099-2100", lines, fixed = TRUE)))
  })
})


# ===========================================================================
# 16. CSV output is UTF-8 (ASCII safe)
# ===========================================================================
test_that("CSV exports are UTF-8 encoded (ASCII safe)", {
  mst <- .make_master_ex()
  p <- withr::local_tempfile(fileext = ".csv")
  geocode_export_csv(mst, p)
  # Read raw bytes and verify there are no invalid UTF-8 sequences
  raw <- readBin(p, what = "raw", n = file.info(p)$size)
  # Round-trip through readLines with explicit UTF-8 must succeed without
  # the "input string is not valid UTF-8" warning.
  expect_silent({
    txt <- readLines(p, encoding = "UTF-8", warn = FALSE)
  })
  expect_true(length(txt) >= 1L)
  # All chars should be valid UTF-8
  expect_true(all(validUTF8(txt)))
})


# ===========================================================================
# 17. include_disputed = FALSE flows through to followup queue
# ===========================================================================
test_that("include_disputed = FALSE filters out disputed_melissa rows", {
  # Use the multi-row master that includes a disputed_melissa row (D6/D10).
  mst <- .make_master_ex()
  withr::with_dir(withr::local_tempdir(), {
    out_all <- geocode_export_followup_queue(mst, include_disputed = TRUE)
    out_no  <- geocode_export_followup_queue(
      mst,
      path = "output/geocode/no_disputed.csv",
      include_disputed = FALSE)
    expect_true("disputed_melissa" %in% out_all$lat_source)
    expect_false("disputed_melissa" %in% out_no$lat_source)
    expect_true(nrow(out_no) < nrow(out_all))
  })
})


# ===========================================================================
# 18. Suggests-package missing produces informative error
# ===========================================================================
test_that("Parquet exporter errors informatively when arrow unavailable", {
  # We can only verify the error MESSAGE not the actual missing-package
  # condition (we can't uninstall arrow in a test). Skip if arrow is
  # actually present and just sanity-check the message string.
  skip_if(requireNamespace("arrow", quietly = TRUE),
          "arrow is installed; cannot test missing-package path")
  mst <- .make_master_ex()
  expect_error(geocode_export_parquet(mst, "x.parquet"),
                regexp = "arrow", ignore.case = TRUE)
})


test_that("Excel exporter errors informatively when openxlsx unavailable", {
  skip_if(requireNamespace("openxlsx", quietly = TRUE),
          "openxlsx is installed; cannot test missing-package path")
  mst <- .make_master_ex()
  expect_error(geocode_export_excel(mst, "x.xlsx"),
                regexp = "openxlsx", ignore.case = TRUE)
})


test_that("Stata exporter errors informatively when haven unavailable", {
  skip_if(requireNamespace("haven", quietly = TRUE),
          "haven is installed; cannot test missing-package path")
  mst <- .make_master_ex()
  expect_error(geocode_export_stata(mst, "x.dta"),
                regexp = "haven", ignore.case = TRUE)
})


# ===========================================================================
# 19. Panel input: 5-format exporters accept panel
# ===========================================================================
test_that("geocode_export_csv accepts alprek_geocode_panel", {
  m1 <- .make_master_ex(receipt_date = "2026-03-04")
  pn <- geocode_bind_years(m1)
  p <- withr::local_tempfile(fileext = ".csv")
  geocode_export_csv(pn, p)
  back <- read.csv(p, stringsAsFactors = FALSE, encoding = "UTF-8")
  expect_equal(nrow(back), nrow(pn$data))
  expect_true("geocode_run_id" %in% names(back))
  expect_true("lineage_id" %in% names(back))
  expect_true("coord_model_status" %in% names(back))
})


# ===========================================================================
# 20. Reconciled input: 5-format exporters accept reconciled
# ===========================================================================
test_that("geocode_export_csv accepts alprek_geocode_reconciled", {
  d500 <- .lng_offset_for_m_ex(500, 33.5)
  rec <- geocode_reconcile(.wrap_clean_ex(
    .row_decision_ex(adece_lat = 33.5, adece_lng = -86.8,
                     melissa_lat = 33.5, melissa_lng = -86.8 + d500,
                     result_code = "GS05"),
    cycle_year = "2026-2027"))
  p <- withr::local_tempfile(fileext = ".csv")
  geocode_export_csv(rec, p)
  back <- read.csv(p, stringsAsFactors = FALSE, encoding = "UTF-8")
  expect_equal(nrow(back), nrow(rec$data))
  expect_true("lineage_id" %in% names(back))
  expect_true("coord_model_status" %in% names(back))
})
