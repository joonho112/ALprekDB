# tests/testthat/test-applications-transform.R

.tt_synth_master <- function(seed = 11L, cycle_year = "2026-2027",
                              add_capacity = TRUE) {
  s <- alprek_synthetic_applications(n_renewals = 10, n_new = 4,
                                       n_non_renewals = 3, n_capacity_sites = 10,
                                       cycle_year = cycle_year, seed = seed)
  s$renewals$data_source     <- "ADECE-renewals-sheet"
  s$new_apps$data_source     <- "ADECE-new-sheet"
  s$capacity$data_source     <- "ADECE-capacity-sheet"
  mk <- function(df, kind) {
    df <- tibble::as_tibble(df)
    df$raw_row_index <- seq_len(nrow(df))
    df$lineage_id <- sprintf("tt-%s-%s-%04d", cycle_year, kind, df$raw_row_index)
    structure(list(data = df, cleaning_log = tibble::tibble(),
                    meta = list(kind = kind, cycle_year = cycle_year,
                                  cycle = "cycle1", n_rows_in = nrow(df),
                                  n_rows_out = nrow(df), n_rows_dropped = 0L,
                                  file_sha256 = "tt", git_sha = "tt",
                                  cleaned_at = format(Sys.time()))),
              class = "alprek_applications_clean")
  }
  ren <- mk(s$renewals, "renewals")
  new <- mk(s$new_apps, "new_apps")
  cap <- if (add_capacity) mk(s$capacity, "capacity") else NULL
  rec <- applications_reconcile(ren, new, allow_degraded = TRUE)
  applications_transform(rec, capacity_clean = cap)
}


test_that("transform returns alprek_applications_master S3 with expected slots", {
  mst <- .tt_synth_master()
  expect_s3_class(mst, "alprek_applications_master")
  expect_true(all(c("data", "capacity_data", "derived_log", "meta") %in%
                    names(mst)))
})

test_that("derived applications cols are present", {
  mst <- .tt_synth_master()
  expected <- c("is_renewal", "is_new", "cycle_year_std", "applied_this_cycle",
                "tier_prev_dollars", "tier_prev_rank", "tier_prev_band")
  expect_true(all(expected %in% names(mst$data)))
})

test_that("is_renewal and is_new are mutually exclusive", {
  mst <- .tt_synth_master()
  expect_true(all(mst$data$is_renewal != mst$data$is_new))
})

test_that("cycle_year_std equals master meta cycle_year", {
  mst <- .tt_synth_master(cycle_year = "2026-2027")
  expect_true(all(mst$data$cycle_year_std == "2026-2027"))
})

test_that("applied_this_cycle is TRUE for every row", {
  mst <- .tt_synth_master()
  expect_true(all(mst$data$applied_this_cycle))
})

test_that("tier_prev_rank in 1..5 or NA", {
  mst <- .tt_synth_master()
  rks <- mst$data$tier_prev_rank
  expect_true(all(is.na(rks) | (rks >= 1L & rks <= 5L)))
})

test_that("tier_prev_band consistent with tier_prev_rank", {
  mst <- .tt_synth_master()
  d <- mst$data
  expect_true(all(d$tier_prev_band[d$tier_prev_rank %in% c(1L, 2L)] == "high",
                    na.rm = TRUE))
  expect_true(all(d$tier_prev_band[d$tier_prev_rank == 3L] == "medium",
                    na.rm = TRUE))
  expect_true(all(d$tier_prev_band[d$tier_prev_rank %in% c(4L, 5L)] == "low",
                    na.rm = TRUE))
})

test_that("capacity_data has derived cols when capacity_clean supplied", {
  mst <- .tt_synth_master(add_capacity = TRUE)
  expect_false(is.null(mst$capacity_data))
  expected <- c("capacity_utilization", "waitlist_ratio", "is_oversubscribed")
  expect_true(all(expected %in% names(mst$capacity_data)))
})

test_that("capacity_data is NULL when capacity_clean not supplied", {
  mst <- .tt_synth_master(add_capacity = FALSE)
  expect_null(mst$capacity_data)
})

test_that("capacity_utilization is NA when capacity = 0", {
  s <- alprek_synthetic_applications(n_capacity_sites = 6, seed = 5L)
  s$capacity$data_source <- "ADECE-capacity-sheet"
  s$capacity$capacity[1] <- 0L
  s$capacity$enrollment[1] <- 0L
  cap <- structure(list(data = tibble::as_tibble(s$capacity),
                          cleaning_log = tibble::tibble(),
                          meta = list(kind = "capacity",
                                        cycle_year = "2026-2027",
                                        cycle = "cycle1",
                                        n_rows_in = 6L, n_rows_out = 6L,
                                        n_rows_dropped = 0L,
                                        file_sha256 = "tt", git_sha = "tt",
                                        cleaned_at = format(Sys.time()))),
                     class = "alprek_applications_clean")
  cap$data$raw_row_index <- 1:6
  cap$data$lineage_id <- paste0("ln-", 1:6)
  mst <- .tt_synth_master()
  mst <- applications_transform(
    structure(list(reconciled = mst$data, reconciliation_log = tibble::tibble(),
                    summary = tibble::tibble(), meta = mst$meta),
              class = "alprek_applications_reconciled"),
    capacity_clean = cap)
  expect_true(is.na(mst$capacity_data$capacity_utilization[1]))
})

test_that("is_oversubscribed = TRUE for waitlist > 0", {
  s <- alprek_synthetic_applications(n_capacity_sites = 4, seed = 9L)
  s$capacity$data_source <- "ADECE-capacity-sheet"
  s$capacity$waitlist[1] <- 5L
  s$capacity$enrollment[1] <- 0L  # avoid over-enroll trigger; only waitlist
  cap <- structure(list(data = tibble::as_tibble(s$capacity),
                          cleaning_log = tibble::tibble(),
                          meta = list(kind = "capacity",
                                        cycle_year = "2026-2027",
                                        cycle = "cycle1",
                                        n_rows_in = 4L, n_rows_out = 4L,
                                        n_rows_dropped = 0L,
                                        file_sha256 = "tt", git_sha = "tt",
                                        cleaned_at = format(Sys.time()))),
                     class = "alprek_applications_clean")
  cap$data$raw_row_index <- 1:4
  cap$data$lineage_id <- paste0("ln-", 1:4)
  base <- .tt_synth_master(add_capacity = FALSE)
  mst <- applications_transform(
    structure(list(reconciled = base$data, reconciliation_log = tibble::tibble(),
                    summary = tibble::tibble(), meta = base$meta),
              class = "alprek_applications_reconciled"),
    capacity_clean = cap)
  expect_true(mst$capacity_data$is_oversubscribed[1])
})

test_that("derived_log records every derivation", {
  mst <- .tt_synth_master()
  expect_s3_class(mst$derived_log, "tbl_df")
  expect_gte(nrow(mst$derived_log), 7L)
  expect_true(all(c("variable", "formula", "n_non_na", "n_na", "note") %in%
                    names(mst$derived_log)))
  expect_true("is_renewal" %in% mst$derived_log$variable)
  expect_true("capacity_utilization" %in% mst$derived_log$variable)
})

test_that("invalid input class raises clear error", {
  expect_error(applications_transform(data.frame(x = 1)),
                "alprek_applications_reconciled")
  expect_error(applications_transform(NULL),
                "alprek_applications_reconciled")
})

test_that("capacity_clean with wrong kind is rejected", {
  base <- .tt_synth_master(add_capacity = FALSE)
  s <- alprek_synthetic_applications(n_renewals = 3, seed = 8L)
  s$renewals$data_source <- "ADECE-renewals-sheet"
  wrong_kind <- structure(list(data = tibble::as_tibble(s$renewals),
                                  cleaning_log = tibble::tibble(),
                                  meta = list(kind = "renewals",
                                                cycle_year = "2026-2027",
                                                file_sha256 = "tt", git_sha = "tt",
                                                cleaned_at = format(Sys.time()))),
                             class = "alprek_applications_clean")
  reco <- structure(list(reconciled = base$data,
                            reconciliation_log = tibble::tibble(),
                            summary = tibble::tibble(),
                            meta = base$meta),
                       class = "alprek_applications_reconciled")
  expect_error(applications_transform(reco, capacity_clean = wrong_kind),
                "kind='capacity'")
})

test_that("tier_bands argument validated", {
  base <- .tt_synth_master(add_capacity = FALSE)
  reco <- structure(list(reconciled = base$data,
                            reconciliation_log = tibble::tibble(),
                            summary = tibble::tibble(),
                            meta = base$meta),
                       class = "alprek_applications_reconciled")
  expect_error(applications_transform(reco, tier_bands = c(100, 50)),
                "non-decreasing")
  expect_error(applications_transform(reco, tier_bands = 100),
                "non-decreasing")
})

test_that("print method runs without error", {
  mst <- .tt_synth_master()
  expect_output(print(mst), "alprek_applications_master")
  expect_output(print(mst), "Cycle:")
})
