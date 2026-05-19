alprek_test_targets_template_dir <- function() {
  template_dir <- system.file(
    "templates",
    "targets",
    package = "ALprekDB"
  )

  if (!nzchar(template_dir)) {
    template_dir <- test_path("../../inst/templates/targets")
  }

  template_dir
}

alprek_test_targets_functions <- function() {
  file.path(alprek_test_targets_template_dir(), "R", "functions.R")
}

test_that("targets workflow template files are present and parseable", {
  template_dir <- alprek_test_targets_template_dir()
  expect_true(dir.exists(template_dir))
  expect_true(file.exists(file.path(template_dir, "README.md")))
  expect_true(file.exists(file.path(template_dir, "_targets.R")))
  expect_true(file.exists(file.path(template_dir, "R", "functions.R")))
  expect_true(file.exists(file.path(template_dir, "local.env.example")))

  expect_silent(parse(file.path(template_dir, "_targets.R")))
  expect_silent(parse(file.path(template_dir, "R", "functions.R")))
})

test_that("targets workflow template avoids local absolute paths", {
  template_dir <- alprek_test_targets_template_dir()
  files <- list.files(
    template_dir,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  text <- unlist(lapply(files, readLines, warn = FALSE), use.names = FALSE)

  expect_false(any(grepl(paste0("/", "Users/"), text, fixed = TRUE)))
  expect_false(any(grepl(paste0("/", "home/"), text, fixed = TRUE)))
  expect_false(any(grepl("^[A-Za-z]:/", text)))
})

test_that("targets real-data manifest encodes 0.6.0 coverage policy", {
  env <- new.env(parent = globalenv())
  sys.source(alprek_test_targets_functions(), env)

  manifest <- env$alprek_targets_realdata_manifest(tempdir(), validate = FALSE)
  canonical <- manifest[manifest$status == "canonical", ]

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
                    grepl("partial duplicate", manifest$reason)))
})

test_that("targets real-data mode is explicit and env-gated", {
  env <- new.env(parent = globalenv())
  sys.source(alprek_test_targets_functions(), env)

  withr::local_envvar(c(
    ALPREKDB_RUN_REALDATA = NA,
    ALPREKDB_DATA_DIR = NA,
    ALPREKDB_OUTPUT_DIR = NA
  ))
  cfg <- env$alprek_targets_config(project_dir = tempdir())
  expect_false(cfg$run_realdata)
  expect_true(cfg$write_outputs)
  expect_match(cfg$data_dir, "ORIGINAL-DATA", fixed = TRUE)
  expect_equal(cfg$synthetic$n_years, 2L)

  withr::local_envvar(c(
    ALPREKDB_RUN_REALDATA = "1",
    ALPREKDB_DATA_DIR = tempdir(),
    ALPREKDB_OUTPUT_DIR = file.path(tempdir(), "out"),
    ALPREKDB_WRITE_OUTPUTS = NA
  ))
  cfg <- env$alprek_targets_config(project_dir = tempdir())
  expect_true(cfg$run_realdata)
  expect_false(cfg$write_outputs)
  expect_identical(cfg$data_dir, tempdir())
})

test_that("targets row-level outputs are disabled by default in real-data mode", {
  env <- new.env(parent = globalenv())
  sys.source(alprek_test_targets_functions(), env)

  output_dir <- file.path(tempdir(), "alprekdb-targets-output")
  path <- env$alprek_targets_write_rds_outputs(
    budget_panel = NULL,
    classroom_panel = NULL,
    student_panel = NULL,
    linkage_master = NULL,
    output_dir = output_dir,
    write_outputs = FALSE
  )

  expect_true(file.exists(path))
  expect_match(basename(path), "rds_outputs_disabled.txt", fixed = TRUE)
})
