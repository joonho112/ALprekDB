# tests/testthat/test-applications-export.R

.te_master <- function(seed = 5L, cycle_year = "2026-2027") {
  s <- alprek_synthetic_applications(n_renewals = 6, n_new = 2,
                                       n_capacity_sites = 5,
                                       cycle_year = cycle_year, seed = seed)
  s$renewals$data_source <- "ADECE-renewals-sheet"
  s$new_apps$data_source <- "ADECE-new-sheet"
  s$capacity$data_source <- "ADECE-capacity-sheet"
  mk <- function(df, kind) {
    df <- tibble::as_tibble(df)
    df$raw_row_index <- seq_len(nrow(df))
    df$lineage_id <- sprintf("te-%s-%s-%04d", cycle_year, kind, df$raw_row_index)
    structure(list(data = df, cleaning_log = tibble::tibble(),
                    meta = list(kind = kind, cycle_year = cycle_year,
                                  cycle = "cycle1", n_rows_in = nrow(df),
                                  n_rows_out = nrow(df), n_rows_dropped = 0L,
                                  file_sha256 = "te", git_sha = "te",
                                  cleaned_at = format(Sys.time()))),
              class = "alprek_applications_clean")
  }
  ren <- mk(s$renewals, "renewals")
  new <- mk(s$new_apps, "new_apps")
  cap <- mk(s$capacity, "capacity")
  rec <- applications_reconcile(ren, new, allow_degraded = TRUE)
  applications_transform(rec, capacity_clean = cap)
}


test_that("CSV export round-trip matches row count and columns", {
  mst <- .te_master()
  td <- withr::local_tempdir()
  p <- file.path(td, "out.csv")
  applications_export_csv(mst, p)
  back <- read.csv(p, stringsAsFactors = FALSE)
  expect_equal(nrow(back), nrow(mst$data))
  expect_equal(ncol(back), ncol(mst$data))
})

test_that("CSV export with grain = 'capacity' writes capacity rows", {
  mst <- .te_master()
  td <- withr::local_tempdir()
  p <- file.path(td, "cap.csv")
  applications_export_csv(mst, p, grain = "capacity")
  back <- read.csv(p)
  expect_equal(nrow(back), nrow(mst$capacity_data))
})

test_that("CSV export auto-path generates output/ filename", {
  mst <- .te_master()
  withr::with_dir(withr::local_tempdir(), {
    p <- applications_export_csv(mst)
    expect_true(file.exists(p))
    expect_match(p, "applications_2026-2027_apps\\.csv$")
  })
})

test_that("RDS export round-trip preserves entire S3 object", {
  mst <- .te_master()
  td <- withr::local_tempdir()
  p <- file.path(td, "out.rds")
  applications_export_rds(mst, p)
  back <- readRDS(p)
  expect_s3_class(back, "alprek_applications_master")
  expect_equal(nrow(back$data), nrow(mst$data))
  expect_equal(nrow(back$capacity_data), nrow(mst$capacity_data))
	expect_identical(back$meta$cycle_year, mst$meta$cycle_year)
})

test_that("non-CSV exports can auto-generate output paths", {
  mst <- .te_master()
  withr::with_dir(withr::local_tempdir(), {
    p <- applications_export_rds(mst)
    expect_true(file.exists(p))
    expect_match(p, "applications_2026-2027_object\\.rds$")
  })
})

test_that("Parquet export round-trip (if arrow available)", {
  skip_if_not_installed("arrow")
  mst <- .te_master()
  td <- withr::local_tempdir()
  p <- file.path(td, "out.parquet")
  applications_export_parquet(mst, p)
  back <- as.data.frame(arrow::read_parquet(p))
  expect_equal(nrow(back), nrow(mst$data))
})

test_that("Excel export round-trip writes 2-3 sheets (if openxlsx available)", {
  skip_if_not_installed("openxlsx")
  mst <- .te_master()
  td <- withr::local_tempdir()
  p <- file.path(td, "out.xlsx")
  applications_export_excel(mst, p)
  sheets <- openxlsx::getSheetNames(p)
  expect_true("Applications" %in% sheets)
  expect_true("Capacity" %in% sheets)
  expect_true("Summary" %in% sheets)
  back_apps <- openxlsx::read.xlsx(p, "Applications")
  expect_equal(nrow(back_apps), nrow(mst$data))
})

test_that("Stata export round-trip (if haven available)", {
  skip_if_not_installed("haven")
  mst <- .te_master()
  td <- withr::local_tempdir()
  p <- file.path(td, "out.dta")
  applications_export_stata(mst, p)
  back <- haven::read_dta(p)
  expect_equal(nrow(back), nrow(mst$data))
})

test_that("invalid input class rejected", {
  td <- withr::local_tempdir()
  expect_error(applications_export_csv(list(), file.path(td, "x.csv")),
                "alprek_applications_master")
})

test_that("requesting capacity grain when no capacity data errors clearly", {
  s <- alprek_synthetic_applications(n_renewals = 3, n_new = 1, seed = 4L)
  s$renewals$data_source <- "ADECE-renewals-sheet"
  s$new_apps$data_source <- "ADECE-new-sheet"
  mk <- function(df, kind) {
    df <- tibble::as_tibble(df)
    df$raw_row_index <- seq_len(nrow(df))
    df$lineage_id <- sprintf("xe-%s-%04d", kind, df$raw_row_index)
    structure(list(data = df, cleaning_log = tibble::tibble(),
                    meta = list(kind = kind, cycle_year = "2026-2027",
                                  cycle = "cycle1", n_rows_in = nrow(df),
                                  n_rows_out = nrow(df), n_rows_dropped = 0L,
                                  file_sha256 = "te", git_sha = "te",
                                  cleaned_at = format(Sys.time()))),
              class = "alprek_applications_clean")
  }
  rec <- applications_reconcile(mk(s$renewals, "renewals"),
                                  mk(s$new_apps, "new_apps"),
                                  allow_degraded = TRUE)
  mst_nocap <- applications_transform(rec)  # no capacity_clean
  td <- withr::local_tempdir()
  expect_error(applications_export_csv(mst_nocap,
                                         file.path(td, "x.csv"),
                                         grain = "capacity"),
                "No capacity_data slot")
})
