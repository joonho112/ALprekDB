# tests/testthat/test-applications-panel.R

.tp_synth_master <- function(seed, cycle_year, with_cap = TRUE) {
  s <- alprek_synthetic_applications(n_renewals = 8, n_new = 3,
                                       n_capacity_sites = 6,
                                       cycle_year = cycle_year, seed = seed)
  s$renewals$data_source <- "ADECE-renewals-sheet"
  s$new_apps$data_source <- "ADECE-new-sheet"
  s$capacity$data_source <- "ADECE-capacity-sheet"
  mk <- function(df, kind) {
    df <- tibble::as_tibble(df)
    df$raw_row_index <- seq_len(nrow(df))
    df$lineage_id <- sprintf("tp-%s-%s-%04d", cycle_year, kind, df$raw_row_index)
    structure(list(data = df, cleaning_log = tibble::tibble(),
                    meta = list(kind = kind, cycle_year = cycle_year,
                                  cycle = "cycle1", n_rows_in = nrow(df),
                                  n_rows_out = nrow(df), n_rows_dropped = 0L,
                                  file_sha256 = "tp", git_sha = "tp",
                                  cleaned_at = format(Sys.time()))),
              class = "alprek_applications_clean")
  }
  ren <- mk(s$renewals, "renewals")
  new <- mk(s$new_apps, "new_apps")
  cap <- if (with_cap) mk(s$capacity, "capacity") else NULL
  rec <- applications_reconcile(ren, new, allow_degraded = TRUE)
  applications_transform(rec, capacity_clean = cap)
}


test_that("bind_years variadic + master_list are equivalent", {
  m1 <- .tp_synth_master(1L, "2025-2026")
  m2 <- .tp_synth_master(2L, "2026-2027")
  p1 <- applications_bind_years(m1, m2)
  p2 <- applications_bind_years(master_list = list(m1, m2))
  expect_identical(nrow(p1$data), nrow(p2$data))
  expect_identical(p1$cycle_years, p2$cycle_years)
})

test_that("bind_years returns alprek_applications_panel S3 with expected slots", {
  m1 <- .tp_synth_master(1L, "2025-2026")
  m2 <- .tp_synth_master(2L, "2026-2027")
  p <- applications_bind_years(m1, m2)
  expect_s3_class(p, "alprek_applications_panel")
  expect_true(all(c("data", "capacity_data", "cycle_years", "n_cycles",
                      "by_cycle", "meta") %in% names(p)))
})

test_that("bind_years preserves row counts (apps + capacity)", {
  m1 <- .tp_synth_master(1L, "2025-2026")
  m2 <- .tp_synth_master(2L, "2026-2027")
  p <- applications_bind_years(m1, m2)
  expect_equal(nrow(p$data), nrow(m1$data) + nrow(m2$data))
  expect_equal(nrow(p$capacity_data),
                nrow(m1$capacity_data) + nrow(m2$capacity_data))
})

test_that("bind_years adds cycle_year column to combined data", {
  m1 <- .tp_synth_master(1L, "2025-2026")
  m2 <- .tp_synth_master(2L, "2026-2027")
  p <- applications_bind_years(m1, m2)
  expect_true("cycle_year" %in% names(p$data))
  expect_setequal(unique(p$data$cycle_year), c("2025-2026", "2026-2027"))
})

test_that("bind_years sorts by cycle_year ascending", {
  m1 <- .tp_synth_master(1L, "2025-2026")
  m2 <- .tp_synth_master(2L, "2026-2027")
  p <- applications_bind_years(m2, m1)  # supplied in reverse order
  expect_identical(p$data$cycle_year[1], "2025-2026")
  expect_identical(p$data$cycle_year[nrow(p$data)], "2026-2027")
})

test_that("duplicate cycle_year rejected", {
  m1 <- .tp_synth_master(1L, "2025-2026")
  m2 <- .tp_synth_master(2L, "2025-2026")
  expect_error(applications_bind_years(m1, m2), "Duplicate cycle_year")
})

test_that("non-master input rejected", {
  expect_error(applications_bind_years(list(a = 1)),
                "not an alprek_applications_master")
  expect_error(applications_bind_years(),
                "No data to combine")
})

test_that("capacity_data NULL when no inputs have capacity", {
  m1 <- .tp_synth_master(1L, "2025-2026", with_cap = FALSE)
  m2 <- .tp_synth_master(2L, "2026-2027", with_cap = FALSE)
  p <- applications_bind_years(m1, m2)
  expect_null(p$capacity_data)
})

test_that("by_cycle has bucket counts per cycle", {
  m1 <- .tp_synth_master(1L, "2025-2026")
  m2 <- .tp_synth_master(2L, "2026-2027")
  p <- applications_bind_years(m1, m2)
  expect_equal(length(p$by_cycle), 2L)
  expect_true(all(c("cycle_year", "n_apps", "n_capacity", "n_buckets") %in%
                    names(p$by_cycle[[1L]])))
})

test_that("print method runs without error", {
  m1 <- .tp_synth_master(1L, "2025-2026")
  m2 <- .tp_synth_master(2L, "2026-2027")
  p <- applications_bind_years(m1, m2)
  expect_output(print(p), "alprek_applications_panel")
  expect_output(print(p), "Cycles:")
})

test_that("applications_track_classrooms returns expected columns", {
  m1 <- .tp_synth_master(1L, "2025-2026")
  m2 <- .tp_synth_master(2L, "2026-2027")
  p <- applications_bind_years(m1, m2)
  trk <- applications_track_classrooms(p)
  expect_s3_class(trk, "tbl_df")
  expected_cols <- c("classroom_key", "2025-2026", "2026-2027",
                       "n_cycles_present", "all_cycles",
                       "first_cycle", "last_cycle")
  expect_true(all(expected_cols %in% names(trk)))
})

test_that("applications_track_classrooms rejects non-panel input", {
  expect_error(applications_track_classrooms(list()),
                "alprek_applications_panel")
})
