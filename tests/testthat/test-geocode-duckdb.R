# tests/testthat/test-geocode-duckdb.R
#
# Tests for R/db-geocode.R (Step 6.2 of the v0.8.0 geocode rollout).
# Covers the four new DuckDB tables (geocode_clean, geocode_reconciled,
# geocode_panel, geocode_lineage), ordered-factor round-trip, lineage
# preservation, and additive-schema coexistence with applications tables.
#
# Per Decision §11.5, schema_version stays at "1" — these tests assert
# that — and a forward-compat flag `geocode_module_present = TRUE` is
# present on db_init().

skip_if_not_installed("duckdb")
skip_if_not_installed("DBI")


# ---------------------------------------------------------------------------
# Helpers — build clean/reconciled/panel objects without touching disk
# ---------------------------------------------------------------------------

.td_geo_clean <- function(n_sites = 6L, n_years = 2L,
                           seed = 20260520L,
                           cycle_year = "2024-2025",
                           receipt_date = as.Date("2024-03-04"),
                           source = "melissa") {
  df <- alprek_synthetic_geocode(n_sites = n_sites, n_years = n_years,
                                  seed = seed,
                                  share_missing_adece = 0.05,
                                  share_missing_site_code = 0.05)
  # Mimic what geocode_clean() produces dtypes-wise.
  df$LAT <- suppressWarnings(as.numeric(as.character(df$LAT)))
  df$LNG <- suppressWarnings(as.numeric(as.character(df$LNG)))
  df$ERRORCODE <- as.character(df$ERRORCODE)
  df$site_zip <- as.character(df$site_zip)
  df$raw_row_index <- seq_len(nrow(df))
  df$lineage_id <- sprintf("lin_%s_%05d", cycle_year, df$raw_row_index)

  meta <- list(
    path             = "/tmp/fake-melissa.xlsx",
    sheet            = "Sheet1",
    source           = source,
    cycle_year       = cycle_year,
    receipt_date     = receipt_date,
    file_basename    = "fake-melissa.xlsx",
    file_sha256      = paste(rep("a", 64L), collapse = ""),
    git_sha          = "abc1234",
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


.td_geo_reconciled <- function(...) {
  clean <- .td_geo_clean(...)
  geocode_reconcile(clean)
}


.td_geo_panel_two_runs <- function() {
  # Two independent runs (different receipt_dates → different run_ids).
  c1 <- .td_geo_clean(seed = 101L,
                       receipt_date = as.Date("2024-03-04"))
  c2 <- .td_geo_clean(seed = 202L,
                       receipt_date = as.Date("2025-03-04"))
  m1 <- geocode_transform(geocode_reconcile(c1))
  m2 <- geocode_transform(geocode_reconcile(c2))
  geocode_bind_years(list(m1, m2))
}


# ---------------------------------------------------------------------------
# 1. db_init: schema_version is "1" and geocode_module_present is "TRUE"
# ---------------------------------------------------------------------------

test_that("db_init() carries geocode_module_present flag and schema 1", {
  conn <- db_init(":memory:")
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  meta <- DBI::dbGetQuery(conn, "SELECT key, value FROM _alprek_meta")
  expect_true("schema_version" %in% meta$key)
  expect_equal(meta$value[meta$key == "schema_version"], "1")
  expect_true("geocode_module_present" %in% meta$key)
  expect_equal(meta$value[meta$key == "geocode_module_present"], "TRUE")
})


# ---------------------------------------------------------------------------
# 2. geocode_reconciled: write -> read round-trip preserves data + lineage_id
# ---------------------------------------------------------------------------

test_that("db_write_geocode_reconciled() + db_read_geocode_reconciled() round-trip", {
  rec <- .td_geo_reconciled()
  conn <- db_init(":memory:")
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  written <- db_write_geocode_reconciled(conn, rec)
  expect_true("geocode_reconciled" %in% written)
  expect_true("geocode_lineage" %in% written)

  back <- db_read_geocode_reconciled(conn)
  expect_s3_class(back, "alprek_geocode_reconciled")
  expect_equal(nrow(back$data), nrow(rec$data))

  # lineage_id is preserved 1:1 across the round-trip.
  expect_true("lineage_id" %in% names(back$data))
  expect_setequal(as.character(back$data$lineage_id),
                  as.character(rec$data$lineage_id))
})


# ---------------------------------------------------------------------------
# 3. Ordered factor preservation: lat_precision + coord_model_status
# ---------------------------------------------------------------------------

test_that("ordered factors round-trip with exact level order", {
  rec <- .td_geo_reconciled()
  conn <- db_init(":memory:")
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  db_write_geocode_reconciled(conn, rec)
  back <- db_read_geocode_reconciled(conn)

  # coord_model_status: ordered {missing < not_model_ready <
  #                              provisional_followup < model_ready}
  expect_true(is.ordered(back$data$coord_model_status))
  expect_equal(levels(back$data$coord_model_status),
               c("missing", "not_model_ready",
                 "provisional_followup", "model_ready"))

  # lat_precision: ordered (ascending from "none")
  expect_true(is.ordered(back$data$lat_precision))
  expect_equal(levels(back$data$lat_precision),
               c("none", "unknown", "centroid", "zip5", "zip4",
                 "area", "parcel", "rooftop"))
})


test_that("non-ordered factor lat_source round-trips with stable levels", {
  rec <- .td_geo_reconciled()
  conn <- db_init(":memory:")
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  db_write_geocode_reconciled(conn, rec)
  back <- db_read_geocode_reconciled(conn)

  expect_true(is.factor(back$data$lat_source))
  expect_false(is.ordered(back$data$lat_source))
  expect_equal(levels(back$data$lat_source),
               c("melissa", "adece", "disputed_melissa", "none"))
})


# ---------------------------------------------------------------------------
# 4. geocode_clean: round-trip
# ---------------------------------------------------------------------------

test_that("db_write_geocode_clean() + db_read_geocode_clean() round-trip", {
  clean <- .td_geo_clean()
  conn <- db_init(":memory:")
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  written <- db_write_geocode_clean(conn, clean)
  expect_true("geocode_clean" %in% written)

  back <- db_read_geocode_clean(conn)
  expect_s3_class(back, "alprek_geocode_clean")
  expect_equal(nrow(back$data), nrow(clean$data))
  # lineage_id present
  expect_true("lineage_id" %in% names(back$data))
  expect_setequal(as.character(back$data$lineage_id),
                  as.character(clean$data$lineage_id))
  # geocode_run_id stamped on read
  expect_true("geocode_run_id" %in% names(back$data))
  expect_equal(unique(back$data$geocode_run_id),
               "melissa_v1_2024-03")
})


# ---------------------------------------------------------------------------
# 5. Panel write supports 2+ run_ids
# ---------------------------------------------------------------------------

test_that("db_write_geocode_panel() supports multiple run_ids", {
  panel <- .td_geo_panel_two_runs()
  expect_gte(length(panel$meta$run_ids), 2L)

  conn <- db_init(":memory:")
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  written <- db_write_geocode_panel(conn, panel)
  expect_true("geocode_panel" %in% written)
  expect_true("geocode_lineage" %in% written)

  back <- db_read_geocode_panel(conn)
  expect_s3_class(back, "alprek_geocode_panel")
  expect_setequal(as.character(back$meta$run_ids),
                  as.character(panel$meta$run_ids))
  expect_equal(nrow(back$data), nrow(panel$data))
  # Ordered factor survives panel round-trip
  expect_true(is.ordered(back$data$coord_model_status))
})


# ---------------------------------------------------------------------------
# 6. Lineage table: one row per write
# ---------------------------------------------------------------------------

test_that("geocode_lineage records one row per write", {
  rec <- .td_geo_reconciled()
  conn <- db_init(":memory:")
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  db_write_geocode_reconciled(conn, rec)
  lin <- db_read_geocode_lineage(conn)
  expect_gte(nrow(lin), 1L)
  expect_true(all(c("geocode_run_id", "source", "cycle_year",
                     "file_sha256", "git_sha", "n_rows", "n_followup",
                     "distance_threshold_rules", "flat_threshold_m",
                     "written_at") %in% names(lin)))
  expect_equal(as.character(lin$source[1L]), "melissa")
  expect_equal(as.character(lin$cycle_year[1L]), "2024-2025")
  expect_false(is.na(lin$file_sha256[1L]))
  expect_false(is.na(lin$git_sha[1L]))
})


# ---------------------------------------------------------------------------
# 7. Argument validation
# ---------------------------------------------------------------------------

test_that("non-class inputs are rejected", {
  conn <- db_init(":memory:")
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  expect_error(db_write_geocode_clean(conn, list()),
                "alprek_geocode_clean")
  expect_error(db_write_geocode_reconciled(conn, list()),
                "alprek_geocode_reconciled")
  expect_error(db_write_geocode_panel(conn, list()),
                "alprek_geocode_panel")
  expect_error(db_write_geocode_lineage(conn, list()),
                "unsupported input class")
})


# ---------------------------------------------------------------------------
# 8. Schema version stays at "1" after the geocode tables are added
# ---------------------------------------------------------------------------

test_that("writing geocode tables does not bump schema_version", {
  rec <- .td_geo_reconciled()
  conn <- db_init(":memory:")
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  db_write_geocode_reconciled(conn, rec)
  v <- DBI::dbGetQuery(conn,
    "SELECT value FROM _alprek_meta WHERE key = 'schema_version'")$value
  expect_equal(v, "1")
})


# ---------------------------------------------------------------------------
# 9. Coexistence with applications tables (no name collision)
# ---------------------------------------------------------------------------

test_that("geocode_clean coexists with applications tables", {
  # Build an applications master + a geocode reconciled side by side.
  s <- alprek_synthetic_applications(n_renewals = 4, n_new = 1,
                                       n_capacity_sites = 2,
                                       cycle_year = "2026-2027",
                                       seed = 7L)
  s$renewals$data_source <- "ADECE-renewals-sheet"
  s$new_apps$data_source <- "ADECE-new-sheet"
  s$capacity$data_source <- "ADECE-capacity-sheet"
  mk <- function(df, kind) {
    df <- tibble::as_tibble(df)
    df$raw_row_index <- seq_len(nrow(df))
    df$lineage_id <- sprintf("td-%s-%04d", kind, df$raw_row_index)
    structure(list(data = df, cleaning_log = tibble::tibble(),
                    meta = list(kind = kind, cycle_year = "2026-2027",
                                  cycle = "cycle1", n_rows_in = nrow(df),
                                  n_rows_out = nrow(df), n_rows_dropped = 0L,
                                  file_sha256 = "hash-2026-2027",
                                  git_sha = "td",
                                  cleaned_at = format(Sys.time()))),
              class = "alprek_applications_clean")
  }
  apps_rec <- applications_reconcile(mk(s$renewals, "renewals"),
                                       mk(s$new_apps, "new_apps"),
                                       allow_degraded = TRUE)
  app_mst <- applications_transform(apps_rec,
                                      capacity_clean = mk(s$capacity, "capacity"))

  conn <- db_init(":memory:")
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  db_write_applications_master(conn, app_mst)
  rec <- .td_geo_reconciled()
  db_write_geocode_clean(conn, geocode_clean_from_reconciled <- {
    # Cheap conversion: take the reconciled and back-fill a synthetic
    # alprek_geocode_clean structure pointing at the same input rows.
    clean <- .td_geo_clean()
    clean
  })
  db_write_geocode_reconciled(conn, rec)

  tbls <- db_list_tables(conn)
  expect_true(all(c("applications_clean", "geocode_clean",
                     "geocode_reconciled") %in% tbls))
  # No name collision: applications_* tables are intact.
  expect_true("applications_clean" %in% tbls)
  expect_true("applications_lineage" %in% tbls)
  expect_true("geocode_lineage" %in% tbls)
})


# ---------------------------------------------------------------------------
# 10. Overwrite vs duplicate-run rejection
# ---------------------------------------------------------------------------

test_that("duplicate run_id rejected unless overwrite = TRUE", {
  rec <- .td_geo_reconciled()
  conn <- db_init(":memory:")
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  db_write_geocode_reconciled(conn, rec)
  expect_error(db_write_geocode_reconciled(conn, rec),
                "already contains rows for geocode_run_id")
  expect_silent(db_write_geocode_reconciled(conn, rec,
                                             overwrite = TRUE))
})


# ---------------------------------------------------------------------------
# 11. Filtering on read by run_id
# ---------------------------------------------------------------------------

test_that("db_read_geocode_panel(run_ids = ...) filters", {
  panel <- .td_geo_panel_two_runs()
  conn <- db_init(":memory:")
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  db_write_geocode_panel(conn, panel)
  one_rid <- panel$meta$run_ids[1L]
  back <- db_read_geocode_panel(conn, run_ids = one_rid)
  expect_setequal(as.character(back$meta$run_ids), one_rid)
  expect_equal(nrow(back$data),
                sum(as.character(panel$data$geocode_run_id) == one_rid))
})
