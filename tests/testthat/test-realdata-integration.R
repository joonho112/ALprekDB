# These tests are intentionally skipped unless explicitly enabled with:
# ALPREKDB_RUN_REALDATA=1
# ALPREKDB_DATA_DIR=/path/to/local/source-data-folder
#
# They use aggregate assertions only. Do not add row-level values, classroom
# codes, child identifiers, names, dates of birth, or raw records to failures.

alprek_realdata_truthy <- function(name) {
  value <- tolower(trimws(Sys.getenv(name, unset = "")))
  value %in% c("1", "true", "t", "yes", "y")
}

alprek_realdata_data_dir <- function() {
  data_dir <- Sys.getenv("ALPREKDB_DATA_DIR", unset = "")
  if (!nzchar(data_dir)) {
    return("")
  }

  candidates <- c(
    data_dir,
    file.path(test_path("../../"), data_dir)
  )
  candidates <- candidates[dir.exists(candidates)]

  if (length(candidates) == 0) {
    return("")
  }

  normalizePath(candidates[[1]], mustWork = TRUE)
}

skip_if_not_alprek_realdata <- function() {
  skip_if_not(
    alprek_realdata_truthy("ALPREKDB_RUN_REALDATA"),
    "Set ALPREKDB_RUN_REALDATA=1 to run local real-data integration checks"
  )

  data_dir <- alprek_realdata_data_dir()
  skip_if(
    !nzchar(data_dir),
    "Set ALPREKDB_DATA_DIR to the local ADECE source-data folder"
  )
}

skip_if_no_realdata_duckdb <- function() {
  skip_if_not(
    alprek_realdata_truthy("ALPREKDB_WRITE_OUTPUTS"),
    "Set ALPREKDB_WRITE_OUTPUTS=1 to allow temporary real-data DuckDB writes"
  )
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    skip("duckdb not installed")
  }
  if (!requireNamespace("DBI", quietly = TRUE)) {
    skip("DBI not installed")
  }
}

alprek_realdata_template_functions <- function() {
  path <- system.file(
    "templates",
    "targets",
    "R",
    "functions.R",
    package = "ALprekDB"
  )

  if (!nzchar(path)) {
    path <- test_path("../../inst/templates/targets/R/functions.R")
  }

  path
}

alprek_realdata_applications_file <- function() {
  explicit <- Sys.getenv("ALPREKDB_APPLICATIONS_FILE", unset = "")
  data_dir <- alprek_realdata_data_dir()
  candidates <- character()
  if (nzchar(explicit)) {
    candidates <- c(candidates, explicit)
    if (nzchar(data_dir)) {
      candidates <- c(candidates, file.path(data_dir, explicit))
    }
  }
  if (nzchar(data_dir)) {
    candidates <- c(
      candidates,
      list.files(
        data_dir,
        pattern = "Classroom Applications.*[.]xlsx$",
        recursive = TRUE,
        full.names = TRUE
      )
    )
  }
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) == 0L) return("")
  normalizePath(candidates[[1]], mustWork = TRUE)
}

.alprek_realdata_cache <- new.env(parent = emptyenv())

alprek_realdata_build <- function() {
  if (exists("result", envir = .alprek_realdata_cache, inherits = FALSE)) {
    return(get("result", envir = .alprek_realdata_cache, inherits = FALSE))
  }

  skip_if_not_alprek_realdata()

  helpers <- new.env(parent = globalenv())
  sys.source(alprek_realdata_template_functions(), helpers)

  data_dir <- alprek_realdata_data_dir()
  output_dir <- tempfile("alprekdb-realdata-smoke-")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  known_warnings <- character()
  manifest <- NULL
  budget <- NULL
  classroom <- NULL
  student <- NULL
  student_panel <- NULL
  master <- NULL
  validation <- NULL

  withCallingHandlers(
    {
      manifest <- helpers$alprek_targets_realdata_manifest(data_dir, validate = TRUE)

      budget <- helpers$alprek_targets_process_budget(
        helpers$alprek_targets_budget_configs(manifest, output_dir)
      )
      classroom <- helpers$alprek_targets_process_classroom(
        helpers$alprek_targets_classroom_configs(manifest, output_dir)
      )
      student <- helpers$alprek_targets_process_student(
        helpers$alprek_targets_student_configs(manifest, output_dir)
      )

      student_panel <- student_transform(student$panel)
      master <- linkage_create_master(budget$panel, classroom$panel, student_panel)
      validation <- linkage_validate(master)
    },
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("unsupported date prior to 1900", msg, fixed = TRUE)) {
        known_warnings <<- c(known_warnings, msg)
        invokeRestart("muffleWarning")
      }
    }
  )

  result <- list(
    manifest = manifest,
    budget = budget,
    classroom = classroom,
    student = student,
    student_panel = student_panel,
    master = master,
    validation = validation,
    known_warnings = known_warnings
  )

  assign("result", result, envir = .alprek_realdata_cache)
  result
}

test_that("real-data integration is opt-in by default", {
  withr::local_envvar(c(
    ALPREKDB_RUN_REALDATA = NA,
    ALPREKDB_DATA_DIR = NA
  ))

  expect_false(alprek_realdata_truthy("ALPREKDB_RUN_REALDATA"))
})

test_that("env-gated real-data manifest matches 0.6.0 source scope", {
  skip_if_not_alprek_realdata()

  helpers <- new.env(parent = globalenv())
  sys.source(alprek_realdata_template_functions(), helpers)
  manifest <- helpers$alprek_targets_realdata_manifest(
    alprek_realdata_data_dir(),
    validate = TRUE
  )
  canonical <- manifest[manifest$status == "canonical", ]

  expect_true(all(canonical$exists))
  expect_equal(sum(canonical$module == "budget"), 4L)
  expect_equal(sum(canonical$module == "classroom"), 5L)
  expect_equal(sum(canonical$module == "student"), 5L)
  expect_false(any(canonical$module == "budget" &
                     canonical$school_year == "2025-2026"))
  expect_true(any(canonical$module == "classroom" &
                    canonical$school_year == "2025-2026"))
  expect_true(any(canonical$module == "student" &
                    canonical$school_year == "2025-2026"))
  expect_true(any(manifest$status == "excluded" &
                    manifest$filename == "FCPK Student Details 24-25.xlsx"))
})

test_that("env-gated real-data configs preserve privacy defaults", {
  skip_if_not_alprek_realdata()

  helpers <- new.env(parent = globalenv())
  sys.source(alprek_realdata_template_functions(), helpers)
  data_dir <- alprek_realdata_data_dir()
  output_dir <- tempfile("alprekdb-realdata-privacy-")
  manifest <- helpers$alprek_targets_realdata_manifest(data_dir, validate = TRUE)

  student_configs <- helpers$alprek_targets_student_configs(manifest, output_dir)
  classroom_configs <- helpers$alprek_targets_classroom_configs(manifest, output_dir)

  expect_true(all(vapply(student_configs, `[[`, logical(1), "include_pii") == FALSE))
  expect_true(all(vapply(classroom_configs, `[[`, logical(1), "include_dob") == FALSE))

  cfg <- withr::with_envvar(
    c(
      ALPREKDB_RUN_REALDATA = "1",
      ALPREKDB_DATA_DIR = data_dir,
      ALPREKDB_WRITE_OUTPUTS = NA
    ),
    helpers$alprek_targets_config(project_dir = test_path("../../"))
  )
  expect_false(cfg$write_outputs)
})

test_that("env-gated real-data panels and linkage pass aggregate smoke checks", {
  smoke <- alprek_realdata_build()

  expect_s3_class(smoke$budget$panel, "alprek_budget_panel")
  expect_s3_class(smoke$classroom$panel, "alprek_classroom_panel")
  expect_s3_class(smoke$student_panel, "alprek_student_panel")
  expect_s3_class(smoke$master, "alprek_linkage_master")

  expect_equal(smoke$budget$panel$years, c(
    "2021-2022", "2022-2023", "2023-2024", "2024-2025"
  ))
  expect_equal(smoke$classroom$panel$years, c(
    "2021-2022", "2022-2023", "2023-2024", "2024-2025", "2025-2026"
  ))
  expect_equal(smoke$student_panel$years, c(
    "2021-2022", "2022-2023", "2023-2024", "2024-2025", "2025-2026"
  ))

  expect_equal(nrow(smoke$budget$panel$data), 5867L)
  expect_equal(nrow(smoke$classroom$panel$data), 7409L)
  expect_equal(nrow(smoke$student_panel$data), 116689L)
  expect_equal(nrow(smoke$master$classroom_level), 7409L)
  expect_equal(nrow(smoke$master$student_level), 116689L)

  expect_true(all(smoke$budget$validation_summary$n_errors == 0L))
  expect_true(all(smoke$classroom$validation_summary$n_errors == 0L))
  expect_true(all(smoke$student$validation_summary$n_errors == 0L))

  expect_equal(sum(smoke$budget$validation_summary$n_warnings), 1L)
  expect_equal(sum(smoke$classroom$validation_summary$n_warnings), 5L)
  expect_equal(sum(smoke$student$validation_summary$n_warnings), 4L)

  expect_true(smoke$validation$passed)
  expect_equal(smoke$validation$n_errors, 0L)
  expect_equal(smoke$validation$n_warnings, 1L)
  expect_equal(smoke$validation$n_info, 4L)
  expect_true("2025-2026" %in% smoke$master$meta$coverage$missing_budget_years)
  expect_equal(smoke$master$diagnostics$classroom_budget$n_left_orphan_overlap_years, 21L)
  expect_equal(smoke$master$diagnostics$classroom_budget$n_left_orphan_missing_budget_years, 1521L)
  expect_equal(smoke$master$diagnostics$student_classroom$n_student_orphan_overlap_years, 0L)
  expect_equal(smoke$master$diagnostics$student_classroom$n_classroom_orphan, 31L)
  expect_true(all(grepl("unsupported date prior to 1900", smoke$known_warnings, fixed = TRUE)))

  checks <- smoke$validation$checks
  expect_equal(
    checks$status[checks$check_name == "budget_overlap_orphans"],
    "WARN"
  )
  expect_equal(
    checks$n_issues[checks$check_name == "budget_overlap_orphans"],
    21L
  )
  expect_equal(
    checks$status[checks$check_name == "budget_missing_coverage"],
    "INFO"
  )
  expect_equal(
    checks$status[checks$check_name == "student_classroom_overlap_orphans"],
    "PASS"
  )
  expect_equal(
    checks$status[checks$check_name == "empty_classrooms"],
    "INFO"
  )
  expect_equal(
    checks$n_issues[checks$check_name == "empty_classrooms"],
    31L
  )

  student_pii_cols <- c(
    .get_student_pii_columns("legacy"),
    .get_student_pii_columns("new")
  )
  expect_length(intersect(student_pii_cols, names(smoke$student_panel$data)), 0L)
  expect_false(any(grepl("_dob$|_username$", names(smoke$classroom$panel$data))))
})

test_that("env-gated real-data DuckDB round-trip preserves aggregate counts", {
  skip_if_not_alprek_realdata()
  skip_if_no_realdata_duckdb()

  smoke <- alprek_realdata_build()
  db_path <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db_path), add = TRUE)

  conn <- db_init(db_path)
  on.exit(db_close(conn), add = TRUE)

  db_write_panel(conn, smoke$budget$panel)
  db_write_panel(conn, smoke$classroom$panel)
  db_write_panel(conn, smoke$student_panel)
  db_write_master(conn, smoke$master)

  expect_equal(
    DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM budget_panel")$n,
    nrow(smoke$budget$panel$data)
  )
  expect_equal(
    DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM classroom_panel")$n,
    nrow(smoke$classroom$panel$data)
  )
  expect_equal(
    DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM student_panel")$n,
    nrow(smoke$student_panel$data)
  )
  expect_equal(
    DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM master_classroom")$n,
    nrow(smoke$master$classroom_level)
  )
  expect_equal(
    DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM master_student")$n,
    nrow(smoke$master$student_level)
  )

  roundtrip <- db_read_master(conn)
  roundtrip_validation <- linkage_validate(roundtrip)
  budget_read <- db_read_panel(conn, "budget")
  classroom_read <- db_read_panel(conn, "classroom")
  student_read <- db_read_panel(conn, "student")

  expect_equal(nrow(roundtrip$classroom_level), nrow(smoke$master$classroom_level))
  expect_equal(nrow(roundtrip$student_level), nrow(smoke$master$student_level))
  expect_equal(nrow(budget_read$data), nrow(smoke$budget$panel$data))
  expect_equal(nrow(classroom_read$data), nrow(smoke$classroom$panel$data))
  expect_equal(nrow(student_read$data), nrow(smoke$student_panel$data))
  expect_type(budget_read$data$year, "integer")
  expect_s3_class(classroom_read$data$delivery_type, "factor")
  expect_type(student_read$data$year, "integer")
  expect_s3_class(student_read$data$delivery_type, "factor")
  expect_true(roundtrip_validation$passed)
  expect_equal(roundtrip_validation$n_errors, 0L)
  expect_equal(roundtrip_validation$n_warnings, 1L)
  expect_equal(roundtrip_validation$n_info, 4L)
  expect_true("2025-2026" %in% roundtrip$meta$coverage$missing_budget_years)
})

test_that("env-gated applications cycle-1 smoke passes aggregate contract", {
  skip_if_not_alprek_realdata()
  app_path <- alprek_realdata_applications_file()
  skip_if(!nzchar(app_path),
          "Set ALPREKDB_APPLICATIONS_FILE or place the applications workbook under ALPREKDB_DATA_DIR")

  smoke <- alprek_realdata_build()

  ren_raw <- applications_read_renewals(
    app_path,
    cycle_year = "2026-2027",
    receipt_date = "2026-04-20"
  )
  new_raw <- applications_read_new(
    app_path,
    cycle_year = "2026-2027",
    receipt_date = "2026-04-20"
  )
  cap_raw <- applications_read_capacity(
    app_path,
    cycle_year = "2026-2027",
    receipt_date = "2026-04-20"
  )

  ren <- applications_clean(ren_raw)
  new <- applications_clean(new_raw)
  cap <- applications_clean(cap_raw)
  rec <- applications_reconcile(
    ren,
    new,
    prior_classroom_panel = smoke$classroom$panel,
    fuzzy_threshold = 0.85,
    seed = 20260519L
  )
  mst <- applications_transform(rec, capacity_clean = cap)
  lk <- linkage_applications_classroom(
    mst,
    smoke$classroom$panel,
    target_school_year = rec$meta$prior_school_year
  )

  expect_s3_class(mst, "alprek_applications_master")
  expect_s3_class(lk, "alprek_applications_linkage")
  expect_equal(nrow(mst$data), nrow(ren$data) + nrow(new$data))
  expect_true(all(c("application_id", "lineage_id", "bucket") %in% names(mst$data)))
  expect_equal(applications_validate(rec)$n_errors, 0L)
  expect_equal(applications_validate(lk)$n_errors, 0L)
})
