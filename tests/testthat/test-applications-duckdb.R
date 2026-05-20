# tests/testthat/test-applications-duckdb.R

skip_if_not_installed("duckdb")
skip_if_not_installed("DBI")


.td_master <- function(cycle_year = "2026-2027", seed = 31L,
                        with_capacity = TRUE) {
  s <- alprek_synthetic_applications(n_renewals = 5, n_new = 2,
                                       n_capacity_sites = 4,
                                       cycle_year = cycle_year, seed = seed)
  s$renewals$data_source <- "ADECE-renewals-sheet"
  s$new_apps$data_source <- "ADECE-new-sheet"
  s$capacity$data_source <- "ADECE-capacity-sheet"
  mk <- function(df, kind) {
    df <- tibble::as_tibble(df)
    df$raw_row_index <- seq_len(nrow(df))
    df$lineage_id <- sprintf("td-%s-%s-%04d", cycle_year, kind, df$raw_row_index)
    structure(list(data = df, cleaning_log = tibble::tibble(),
                    meta = list(kind = kind, cycle_year = cycle_year,
                                  cycle = "cycle1", n_rows_in = nrow(df),
                                  n_rows_out = nrow(df), n_rows_dropped = 0L,
                                  file_sha256 = paste0("hash-", cycle_year),
                                  git_sha = "td", cleaned_at = format(Sys.time()))),
              class = "alprek_applications_clean")
  }
  ren <- mk(s$renewals, "renewals")
  new <- mk(s$new_apps, "new_apps")
  cap <- if (with_capacity) mk(s$capacity, "capacity") else NULL
  rec <- applications_reconcile(ren, new, allow_degraded = TRUE)
  applications_transform(rec, capacity_clean = cap)
}


test_that("db_write_applications_master + db_read_applications_master round-trip", {
  mst <- .td_master()
  db_path <- tempfile(fileext = ".duckdb")
  conn <- db_init(db_path)
  on.exit({ DBI::dbDisconnect(conn, shutdown = TRUE); unlink(db_path) })

	  written <- db_write_applications_master(conn, mst)
	  expect_true(all(c("applications_clean", "applications_capacity",
	                      "applications_lineage",
	                      "applications_derived_log") %in% written))

	  back <- db_read_applications_master(conn, cycle_year = "2026-2027")
	  expect_s3_class(back, "alprek_applications_master")
	  expect_equal(nrow(back$data), nrow(mst$data))
	  expect_equal(nrow(back$capacity_data), nrow(mst$capacity_data))
	  expect_equal(nrow(back$derived_log), nrow(mst$derived_log))
	  expect_setequal(back$derived_log$variable, mst$derived_log$variable)
	})

test_that("master write rejects non-master input", {
  db_path <- tempfile(fileext = ".duckdb")
  conn <- db_init(db_path)
  on.exit({ DBI::dbDisconnect(conn, shutdown = TRUE); unlink(db_path) })
  expect_error(db_write_applications_master(conn, list()),
                "alprek_applications_master")
})

test_that("master write skips capacity when capacity_data NULL", {
  mst <- .td_master(with_capacity = FALSE)
  db_path <- tempfile(fileext = ".duckdb")
  conn <- db_init(db_path)
  on.exit({ DBI::dbDisconnect(conn, shutdown = TRUE); unlink(db_path) })

  written <- db_write_applications_master(conn, mst)
  expect_false("applications_capacity" %in% written)
  expect_true("applications_clean" %in% written)
})

test_that("master write errors on duplicate cycle_year unless overwrite", {
  mst <- .td_master(cycle_year = "2026-2027")
  db_path <- tempfile(fileext = ".duckdb")
  conn <- db_init(db_path)
  on.exit({ DBI::dbDisconnect(conn, shutdown = TRUE); unlink(db_path) })

  db_write_applications_master(conn, mst)
  expect_error(db_write_applications_master(conn, mst),
                "already contains rows for cycle_year")
  expect_silent(db_write_applications_master(conn, mst, overwrite = TRUE))
})

test_that("two cycles written + read returns latest by default", {
  m26 <- .td_master(cycle_year = "2026-2027", seed = 1L)
  m25 <- .td_master(cycle_year = "2025-2026", seed = 2L)
  db_path <- tempfile(fileext = ".duckdb")
  conn <- db_init(db_path)
  on.exit({ DBI::dbDisconnect(conn, shutdown = TRUE); unlink(db_path) })

  db_write_applications_master(conn, m26)
  db_write_applications_master(conn, m25)
  back <- db_read_applications_master(conn)  # cycle_year = NULL → latest
  expect_equal(back$meta$cycle_year, "2026-2027")
})

test_that("db_write_applications_panel + db_read_applications_panel round-trip", {
  m25 <- .td_master(cycle_year = "2025-2026", seed = 5L)
  m26 <- .td_master(cycle_year = "2026-2027", seed = 6L)
  panel <- applications_bind_years(m25, m26)

  db_path <- tempfile(fileext = ".duckdb")
  conn <- db_init(db_path)
  on.exit({ DBI::dbDisconnect(conn, shutdown = TRUE); unlink(db_path) })

  written <- db_write_applications_panel(conn, panel)
  expect_true("applications_panel" %in% written)

  back <- db_read_applications_panel(conn)
  expect_s3_class(back, "alprek_applications_panel")
  expect_setequal(back$cycle_years, c("2025-2026", "2026-2027"))
  expect_equal(nrow(back$data), nrow(panel$data))
})

test_that("panel write rejects non-panel input", {
  db_path <- tempfile(fileext = ".duckdb")
  conn <- db_init(db_path)
  on.exit({ DBI::dbDisconnect(conn, shutdown = TRUE); unlink(db_path) })
  expect_error(db_write_applications_panel(conn, list()),
                "alprek_applications_panel")
})

test_that("db_read_applications_master errors when table missing", {
  db_path <- tempfile(fileext = ".duckdb")
  conn <- db_init(db_path)
  on.exit({ DBI::dbDisconnect(conn, shutdown = TRUE); unlink(db_path) })
  expect_error(db_read_applications_master(conn),
                "No applications_clean table")
})

test_that("panel filter by cycle_years works", {
  m25 <- .td_master(cycle_year = "2025-2026", seed = 11L)
  m26 <- .td_master(cycle_year = "2026-2027", seed = 12L)
  panel <- applications_bind_years(m25, m26)
  db_path <- tempfile(fileext = ".duckdb")
  conn <- db_init(db_path)
  on.exit({ DBI::dbDisconnect(conn, shutdown = TRUE); unlink(db_path) })

  db_write_applications_panel(conn, panel)
  back <- db_read_applications_panel(conn, cycle_years = "2025-2026")
  expect_equal(back$cycle_years, "2025-2026")
})

test_that("lineage table captures metadata", {
  mst <- .td_master()
  db_path <- tempfile(fileext = ".duckdb")
  conn <- db_init(db_path)
  on.exit({ DBI::dbDisconnect(conn, shutdown = TRUE); unlink(db_path) })

  db_write_applications_master(conn, mst)
  lin <- DBI::dbGetQuery(conn, "SELECT * FROM applications_lineage")
  expect_equal(nrow(lin), 1L)
	  expect_true("cycle_year" %in% names(lin))
	  expect_true("n_apps" %in% names(lin))
	  expect_true("written_at" %in% names(lin))
	  expect_false(is.na(lin$file_sha256[1]))
	  expect_false(is.na(lin$git_sha[1]))
	  expect_false(is.na(lin$reconciled_at[1]))
	})

test_that("panel lineage rows retain per-cycle source metadata", {
  m25 <- .td_master(cycle_year = "2025-2026", seed = 15L)
  m26 <- .td_master(cycle_year = "2026-2027", seed = 16L)
  panel <- applications_bind_years(m25, m26)

  db_path <- tempfile(fileext = ".duckdb")
  conn <- db_init(db_path)
  on.exit({ DBI::dbDisconnect(conn, shutdown = TRUE); unlink(db_path) })

  db_write_applications_panel(conn, panel)
  lin <- DBI::dbGetQuery(conn, "SELECT * FROM applications_lineage")
  expect_equal(nrow(lin), 2L)
  expect_false(any(is.na(lin$file_sha256)))
  expect_false(any(is.na(lin$git_sha)))
})
