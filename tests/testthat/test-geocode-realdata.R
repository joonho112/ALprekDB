# tests/testthat/test-geocode-realdata.R
#
# Opt-in real-data smoke for the Melissa geocode module.
# Run with:
#   ALPREK_RUN_REALDATA=1 Rscript -e 'devtools::test(filter="geocode-realdata")'
#
# Assertions are aggregate only; do not add row-level addresses or names here.

skip_if_not_geocode_realdata <- function() {
  enabled <- tolower(trimws(Sys.getenv("ALPREK_RUN_REALDATA", unset = "")))
  skip_if_not(
    enabled %in% c("1", "true", "t", "yes", "y"),
    "Set ALPREK_RUN_REALDATA=1 to run Melissa geocode real-data checks"
  )
}


geocode_realdata_path <- function() {
  file.path(
    test_path("../../"),
    "ORIGINAL-DATA",
    "2026-03-04_Pre-K Geocoding Melissa",
    "2026-03-04_geocoding_master_Final.xlsx"
  )
}


test_that("geocode real-data smoke is opt-in by default", {
  withr::local_envvar(c(ALPREK_RUN_REALDATA = NA))
  enabled <- tolower(trimws(Sys.getenv("ALPREK_RUN_REALDATA", unset = "")))
  expect_false(enabled %in% c("1", "true", "t", "yes", "y"))
})


test_that("env-gated Melissa geocode real-data smoke matches Step 4.5 anchors", {
  skip_if_not_geocode_realdata()

  path <- geocode_realdata_path()
  skip_if_not(file.exists(path), "Melissa geocode real-data xlsx not present")

  raw <- geocode_read(
    path,
    cycle_year = "2026-2027",
    receipt_date = as.Date("2026-03-04"),
    verbose = FALSE
  )
  clean <- geocode_clean(raw)
  validation <- geocode_validate(clean)
  expect_true(validation$passed)
  expect_equal(validation$n_errors, 0L)

  cfg <- geocode_config(
    path = raw$meta$path,
    cycle_year = "2026-2027",
    delivery_date = "2026-03-04",
    verbose = FALSE
  )
  recon <- geocode_reconcile(clean, config = cfg)
  queue <- geocode_followup_queue(recon)

  expect_equal(nrow(recon$data), 3396L)
  expect_equal(nrow(queue), 677L)
  expect_equal(length(unique(recon$data$lineage_id)), 3396L)
  expect_equal(unname(recon$reconciliation_log$lineage_id),
               unname(recon$data$lineage_id))

  expect_equal(
    as.integer(table(recon$data$lat_source)),
    c(3150L, 0L, 246L, 0L)
  )
  expect_equal(
    as.integer(table(recon$data$coord_agreement_band)),
    c(45L, 1540L, 1268L, 326L, 33L, 184L, 0L)
  )

  reason_tab <- table(as.character(queue$followup_reason))
  expect_equal(as.integer(reason_tab[c(
    "disagreement_above_threshold",
    "disagreement_gross",
    "melissa_only_interpolated",
    "melissa_only_gs03",
    "resultcode_gs03_always_flag"
  )]), c(390L, 9L, 25L, 16L, 237L))

  sourced <- as.character(recon$data$lat_source) != "none"
  expect_false(any(is.na(recon$data$lat_final[sourced])))
  expect_false(any(is.na(recon$data$lng_final[sourced])))
  expect_equal(attr(queue, "privacy_level"), "internal_address_followup")
  expect_true(isTRUE(attr(queue, "contains_address_fields")))
})
