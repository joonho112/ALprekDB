# ===========================================================================
# Tests for Database Module (DuckDB)
# ===========================================================================

# Helper: skip if DuckDB not available
skip_if_no_duckdb <- function() {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    skip("duckdb not installed")
  }
  if (!requireNamespace("DBI", quietly = TRUE)) {
    skip("DBI not installed")
  }
}


# ---- Connection & Schema ----

test_that("db_init creates new database with schema tables", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  all_tables <- DBI::dbListTables(conn)
  expect_true("_alprek_meta" %in% all_tables)
  expect_true("_alprek_column_types" %in% all_tables)

  # Check metadata
  meta <- DBI::dbGetQuery(conn, "SELECT * FROM _alprek_meta ORDER BY key")
  expect_true("schema_version" %in% meta$key)
  expect_equal(meta$value[meta$key == "schema_version"], "1")
  expect_true("created_at" %in% meta$key)
})


test_that("db_init opens existing database without error", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  # Create

  conn1 <- db_init(tmp)
  db_close(conn1)

  # Reopen
  conn2 <- db_init(tmp)
  on.exit(db_close(conn2), add = TRUE)

  all_tables <- DBI::dbListTables(conn2)
  expect_true("_alprek_meta" %in% all_tables)
})


test_that("db_init read_only mode works", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  # Create first
  conn1 <- db_init(tmp)
  db_close(conn1)

  # Open read-only
  conn2 <- db_init(tmp, read_only = TRUE)
  on.exit(db_close(conn2), add = TRUE)

  expect_true(DBI::dbIsValid(conn2))
})


test_that("db_list_tables excludes internal _alprek_* tables", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  # Fresh DB: no user tables
  user_tables <- db_list_tables(conn)
  expect_length(user_tables, 0)

  # Internal tables exist
  all_tables <- DBI::dbListTables(conn)
  internal <- all_tables[grepl("^_alprek_", all_tables)]
  expect_true(length(internal) >= 2)
})


# ---- Write Panel ----

test_that("db_write_panel writes budget panel correctly", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  db_write_panel(conn, fixtures$budget_panel)

  expect_true("budget_panel" %in% db_list_tables(conn))
  result <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM budget_panel")
  expect_equal(result$n, nrow(fixtures$budget_panel$data))
})


test_that("db_write_panel writes classroom panel correctly", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  db_write_panel(conn, fixtures$classroom_panel)

  expect_true("classroom_panel" %in% db_list_tables(conn))
  result <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM classroom_panel")
  expect_equal(result$n, nrow(fixtures$classroom_panel$data))
})


test_that("db_write_panel writes student panel correctly", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  db_write_panel(conn, fixtures$student_panel)

  expect_true("student_panel" %in% db_list_tables(conn))
  result <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM student_panel")
  expect_equal(result$n, nrow(fixtures$student_panel$data))
})


test_that("db_write_panel rejects non-panel objects", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  expect_error(db_write_panel(conn, data.frame(x = 1)), "Unsupported panel")
})


test_that("db_write_panel overwrite replaces data", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  db_write_panel(conn, fixtures$budget_panel)

  # Without overwrite: error
  expect_error(db_write_panel(conn, fixtures$budget_panel), "already exists")

  # With overwrite: success
  db_write_panel(conn, fixtures$budget_panel, overwrite = TRUE)
  result <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM budget_panel")
  expect_equal(result$n, nrow(fixtures$budget_panel$data))
})


# ---- Write Master ----

test_that("db_write_master writes both levels", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  master <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel
  )

  db_write_master(conn, master)

  tables <- db_list_tables(conn)
  expect_true("master_classroom" %in% tables)
  expect_true("master_student" %in% tables)
})


test_that("db_write_master row counts match original", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  master <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel
  )

  db_write_master(conn, master)

  cl_n <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM master_classroom")$n
  st_n <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM master_student")$n

  expect_equal(cl_n, nrow(master$classroom_level))
  expect_equal(st_n, nrow(master$student_level))
})


# ---- Write Year (Incremental) ----

test_that("db_write_year appends new year", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  # Write initial data
  db_write_panel(conn, fixtures$budget_panel)
  n_initial <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM budget_panel")$n

  # Create new-year data (different school year)
  new_budget <- fixtures$budget_panel
  new_budget$data$school_year <- "2024-2025"
  new_budget$data$year <- 2024L
  new_budget$years <- "2024-2025"
  new_budget$by_year <- list("2024-2025" = list(
    school_year = "2024-2025", format = "new", n_classrooms = nrow(new_budget$data)
  ))

  db_write_year(conn, new_budget)

  n_after <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM budget_panel")$n
  expect_equal(n_after, n_initial + nrow(new_budget$data))
})


test_that("db_write_year rejects duplicate year", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  db_write_panel(conn, fixtures$budget_panel)
  expect_error(db_write_year(conn, fixtures$budget_panel), "already exist")
})


test_that("db_write_year validates column compatibility", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  db_write_panel(conn, fixtures$budget_panel)

  # New year with extra column — should warn but succeed
  new_budget <- fixtures$budget_panel
  new_budget$data$school_year <- "2024-2025"
  new_budget$data$year <- 2024L
  new_budget$data$extra_col <- "test"
  new_budget$years <- "2024-2025"
  new_budget$by_year <- list("2024-2025" = list(
    school_year = "2024-2025", format = "new", n_classrooms = nrow(new_budget$data)
  ))

  expect_message(db_write_year(conn, new_budget), "not in existing")
})


# ---- Read Panel ----

test_that("db_read_panel reconstructs budget panel S3 class", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  db_write_panel(conn, fixtures$budget_panel)
  result <- db_read_panel(conn, "budget")

  expect_s3_class(result, "alprek_budget_panel")
  expect_equal(nrow(result$data), nrow(fixtures$budget_panel$data))
  expect_true(length(result$years) > 0)
})


test_that("db_read_panel reconstructs classroom panel S3 class", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  db_write_panel(conn, fixtures$classroom_panel)
  result <- db_read_panel(conn, "classroom")

  expect_s3_class(result, "alprek_classroom_panel")
  expect_equal(nrow(result$data), nrow(fixtures$classroom_panel$data))
})


test_that("db_read_panel reconstructs student panel S3 class", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  db_write_panel(conn, fixtures$student_panel)
  result <- db_read_panel(conn, "student")

  expect_s3_class(result, "alprek_student_panel")
  expect_equal(nrow(result$data), nrow(fixtures$student_panel$data))
})


test_that("db_read_panel year filter works", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  db_write_panel(conn, fixtures$budget_panel)

  # Add second year
  new_budget <- fixtures$budget_panel
  new_budget$data$school_year <- "2024-2025"
  new_budget$data$year <- 2024L
  new_budget$years <- "2024-2025"
  new_budget$by_year <- list("2024-2025" = list(
    school_year = "2024-2025", format = "new", n_classrooms = nrow(new_budget$data)
  ))
  db_write_year(conn, new_budget)

  # Filter to one year
  result <- db_read_panel(conn, "budget", years = "2024-2025")
  expect_equal(unique(result$data$school_year), "2024-2025")
  expect_equal(nrow(result$data), nrow(new_budget$data))
})


test_that("db_read_panel returns empty for nonexistent year", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  db_write_panel(conn, fixtures$budget_panel)
  result <- db_read_panel(conn, "budget", years = "1999-2000")

  expect_equal(nrow(result$data), 0)
})


# ---- Read Master ----

test_that("db_read_master reconstructs linkage master S3 class", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  master <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel
  )

  db_write_master(conn, master)
  result <- db_read_master(conn)

  expect_s3_class(result, "alprek_linkage_master")
  expect_equal(nrow(result$classroom_level), nrow(master$classroom_level))
  expect_equal(nrow(result$student_level), nrow(master$student_level))
})


test_that("db_read_master year filter works", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  master <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel
  )

  db_write_master(conn, master)

  # Filter on the year in fixtures (should be "2023-2024")
  sy <- unique(master$classroom_level$school_year)
  result <- db_read_master(conn, years = sy[1])

  expect_true(all(result$classroom_level$school_year == sy[1]))
})


# ---- Type Round-Trip ----

test_that("Factor columns survive write-read round trip", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  original <- fixtures$classroom_panel
  db_write_panel(conn, original)
  result <- db_read_panel(conn, "classroom")

  # Check delivery_type factor
  orig_dt <- original$data$delivery_type
  result_dt <- result$data$delivery_type

  expect_true(is.factor(result_dt))
  expect_equal(levels(result_dt), levels(orig_dt))
  # Values should match
  expect_equal(as.character(result_dt), as.character(orig_dt))
})


test_that("Date columns survive write-read round trip", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  # Student panel has dob as Date
  original <- fixtures$student_panel
  db_write_panel(conn, original)
  result <- db_read_panel(conn, "student")

  if ("dob" %in% names(original$data) && "dob" %in% names(result$data)) {
    expect_true(inherits(result$data$dob, "Date"))
    expect_equal(result$data$dob, original$data$dob)
  }
})


test_that("Integer columns survive write-read round trip", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  original <- fixtures$budget_panel
  db_write_panel(conn, original)
  result <- db_read_panel(conn, "budget")

  if ("year" %in% names(original$data)) {
    expect_true(is.integer(result$data$year))
  }
})


test_that("Column type registry populated correctly", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  db_write_panel(conn, fixtures$classroom_panel)

  types <- .db_get_column_types(conn, "classroom_panel")
  expect_true(nrow(types) > 0)
  expect_true("delivery_type" %in% types$column_name)

  dt_row <- types[types$column_name == "delivery_type", ]
  expect_equal(dt_row$r_type, "factor")
  expect_false(is.na(dt_row$factor_levels))
})


# ---- Query & Inspection ----

test_that("db_query returns correct aggregation results", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  db_write_panel(conn, fixtures$budget_panel)

  result <- db_query(conn, "
    SELECT school_year, COUNT(*) as n, AVG(grand_total) as mean_budget
    FROM budget_panel
    GROUP BY school_year
  ")

  expect_true(is.data.frame(result))
  expect_true("n" %in% names(result))
  expect_true("mean_budget" %in% names(result))

  # Verify count matches
  expected_n <- nrow(fixtures$budget_panel$data)
  expect_equal(sum(result$n), expected_n)
})


test_that("db_table_info returns column metadata with R types", {
  skip_if_no_duckdb()
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  conn <- db_init(tmp)
  on.exit(db_close(conn), add = TRUE)

  db_write_panel(conn, fixtures$classroom_panel)

  info <- db_table_info(conn, "classroom_panel")

  expect_true(is.data.frame(info))
  expect_true("column_name" %in% names(info))
  expect_true("duckdb_type" %in% names(info))
  expect_true("r_type" %in% names(info))
  expect_true("is_factor" %in% names(info))

  # delivery_type should be marked as factor
  dt_info <- info[info$column_name == "delivery_type", ]
  expect_equal(dt_info$r_type, "factor")
  expect_true(dt_info$is_factor)
})
