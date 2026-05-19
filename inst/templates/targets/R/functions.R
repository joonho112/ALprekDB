alprek_targets_truthy <- function(x) {
  value <- tolower(trimws(Sys.getenv(x, unset = "")))
  value %in% c("1", "true", "t", "yes", "y")
}

alprek_targets_run_realdata <- function() {
  alprek_targets_truthy("ALPREKDB_RUN_REALDATA")
}

alprek_targets_int_env <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) {
    return(default)
  }
  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed) || parsed <= 0) {
    stop(
      name,
      " must be a positive integer when set.",
      call. = FALSE
    )
  }
  parsed
}

alprek_targets_config <- function(project_dir = getwd()) {
  project_dir <- normalizePath(project_dir, mustWork = TRUE)
  run_realdata <- alprek_targets_run_realdata()
  write_outputs <- !run_realdata || alprek_targets_truthy("ALPREKDB_WRITE_OUTPUTS")

  default_data_dir <- file.path(
    project_dir,
    "ORIGINAL-DATA",
    "ADECE-source-files"
  )
  data_dir <- Sys.getenv("ALPREKDB_DATA_DIR", unset = default_data_dir)
  output_dir <- Sys.getenv(
    "ALPREKDB_OUTPUT_DIR",
    unset = file.path(project_dir, "output", "alprekdb")
  )

  if (run_realdata && !dir.exists(data_dir)) {
    stop(
      "ALPREKDB_RUN_REALDATA=1 but ALPREKDB_DATA_DIR does not exist: ",
      data_dir,
      call. = FALSE
    )
  }

  list(
    run_realdata = run_realdata,
    write_outputs = write_outputs,
    data_dir = data_dir,
    output_dir = output_dir,
    include_pii = FALSE,
    synthetic = list(
      n_classrooms = alprek_targets_int_env("ALPREKDB_SYNTHETIC_N_CLASSROOMS", 20L),
      n_students = alprek_targets_int_env("ALPREKDB_SYNTHETIC_N_STUDENTS", 100L),
      n_years = alprek_targets_int_env("ALPREKDB_SYNTHETIC_N_YEARS", 2L),
      seed = alprek_targets_int_env("ALPREKDB_SYNTHETIC_SEED", 42L)
    ),
    schema = "0.6.0-workflow-template"
  )
}

alprek_targets_realdata_manifest <- function(data_dir, validate = TRUE) {
  manifest <- tibble::tribble(
    ~module, ~school_year, ~filename, ~sheet, ~status, ~reason,
    "budget", "2021-2022", "rptClassBudgets 2021-2022.xlsx", NA_character_, "canonical", "production budget source",
    "budget", "2022-2023", "rptClassBudgets 2022-2023.xlsx", NA_character_, "canonical", "production budget source",
    "budget", "2023-2024", "rptClassBudgets 2023-2024.xlsx", NA_character_, "canonical", "production budget source",
    "budget", "2024-2025", "24-25 FCPK Budgets.xlsx", NA_character_, "canonical", "production budget source",
    "classroom", "2021-2022", "FCPK Classroom Details 21-22.xlsx", "rptRIF", "canonical", "production classroom source",
    "classroom", "2022-2023", "FCPK Classroom Details 22-23.xlsx", "rptRIF", "canonical", "production classroom source",
    "classroom", "2023-2024", "FCPK Classroom Details 23-24.xlsx", "rptRIF", "canonical", "production classroom source",
    "classroom", "2024-2025", "24-25 Classroom Details.xlsx", "rptRIF", "canonical", "production classroom source",
    "classroom", "2025-2026", "25-26 Classroom Details.xlsx", "rptRIF", "canonical", "production classroom source",
    "student", "2021-2022", "FCPK Student Details 21-22.xlsx", "rptChildren_Excel", "canonical", "production student source",
    "student", "2022-2023", "FCPK Student Details 22-23.xlsx", "rptChildren_Excel", "canonical", "production student source",
    "student", "2023-2024", "FCPK Student Details 23-24.xlsx", "rptChildren_Excel", "canonical", "production student source",
    "student", "2024-2025", "24-25 FCPK Child Details.xlsx", "rptChildren_Excel", "canonical", "production student source",
    "student", "2025-2026", "25-26 FCPK Child Details.xlsx", "rptChildren_Excel", "canonical", "production student source",
    "budget", "2024-2025", "FCPK Classroom Budgets as of 12-5-24.xlsx", NA_character_, "excluded", "dated snapshot, not canonical production panel input",
    "budget_request", "2024-2025", "Budget Requests Grant Applications Foundant 092724.xlsx", NA_character_, "excluded", "request/application export, not production budget panel input",
    "student", "2024-2025", "FCPK Student Details 24-25.xlsx", "rptChildren_Excel", "excluded", "partial duplicate student extract; canonical source is 24-25 FCPK Child Details.xlsx",
    "waitlist_access", "2025-2026", "Waitlist_by Site and County_11_7_2025.xlsx", NA_character_, "excluded", "outside 0.6.0 core panel scope",
    "access", "2024-2025", "24-25 FCPK Children and Counties.xlsx", NA_character_, "excluded", "outside 0.6.0 core panel scope",
    "class", "2024-2025", "24-25 CLASS Results Export.xlsx", NA_character_, "excluded", "outside 0.6.0 core panel scope"
  )

  manifest$path <- file.path(data_dir, manifest$filename)
  manifest$exists <- file.exists(manifest$path)

  if (validate) {
    missing <- manifest[manifest$status == "canonical" & !manifest$exists, ]
    if (nrow(missing) > 0) {
      stop(
        "Missing canonical ALprekDB source file(s): ",
        paste(missing$filename, collapse = ", "),
        call. = FALSE
      )
    }
  }

  manifest
}

alprek_targets_module_manifest <- function(manifest, module) {
  out <- manifest[manifest$module == module & manifest$status == "canonical", ]
  out[order(out$school_year), ]
}

alprek_targets_module_paths <- function(manifest, module) {
  paths <- alprek_targets_module_manifest(manifest, module)$path
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) {
    stop(
      "Missing canonical source file(s) for module '",
      module,
      "': ",
      paste(basename(missing), collapse = ", "),
      call. = FALSE
    )
  }
  paths
}

alprek_targets_sheet <- function(x) {
  if (length(x) == 0 || is.na(x)) {
    NULL
  } else {
    x
  }
}

alprek_targets_budget_configs <- function(manifest, output_dir) {
  budget <- alprek_targets_module_manifest(manifest, "budget")
  lapply(seq_len(nrow(budget)), function(i) {
    ALprekDB::budget_config(
      school_year = budget$school_year[[i]],
      budget_path = budget$path[[i]],
      sheet = alprek_targets_sheet(budget$sheet[[i]]),
      output_dir = file.path(output_dir, "budget", budget$school_year[[i]]),
      verbose = FALSE
    )
  })
}

alprek_targets_classroom_configs <- function(manifest, output_dir) {
  classroom <- alprek_targets_module_manifest(manifest, "classroom")
  lapply(seq_len(nrow(classroom)), function(i) {
    ALprekDB::classroom_config(
      school_year = classroom$school_year[[i]],
      classroom_path = classroom$path[[i]],
      sheet = alprek_targets_sheet(classroom$sheet[[i]]),
      include_dob = FALSE,
      output_dir = file.path(output_dir, "classroom", classroom$school_year[[i]])
    )
  })
}

alprek_targets_student_configs <- function(manifest, output_dir) {
  student <- alprek_targets_module_manifest(manifest, "student")
  lapply(seq_len(nrow(student)), function(i) {
    ALprekDB::student_config(
      school_year = student$school_year[[i]],
      path = student$path[[i]],
      sheet = alprek_targets_sheet(student$sheet[[i]]),
      include_pii = FALSE,
      output_dir = file.path(output_dir, "student", student$school_year[[i]])
    )
  })
}

alprek_targets_synthetic_panels <- function(
  n_classrooms = 20,
  n_students = 100,
  n_years = 2,
  seed = 42
) {
  list(
    budget = ALprekDB::alprek_synthetic_budget(
      n_classrooms = n_classrooms,
      n_years = n_years,
      seed = seed
    ),
    classroom = ALprekDB::alprek_synthetic_classroom(
      n_classrooms = n_classrooms,
      n_years = n_years,
      seed = seed
    ),
    student = ALprekDB::alprek_synthetic_student(
      n_students = n_students,
      n_classrooms = n_classrooms,
      n_years = n_years,
      seed = seed
    )
  )
}

alprek_targets_process_budget <- function(configs) {
  result <- ALprekDB::budget_process_years(configs, export = FALSE)
  list(
    panel = result$panel,
    validation_summary = result$validation_summary
  )
}

alprek_targets_process_classroom <- function(configs) {
  result <- ALprekDB::classroom_process_years(configs, export = FALSE)
  list(
    panel = result$panel,
    validation_summary = result$validation_summary,
    degree_audit = result$degree_audit
  )
}

alprek_targets_process_student <- function(configs) {
  result <- ALprekDB::student_process_years(configs, export = FALSE)
  list(
    panel = result$panel,
    validation_summary = result$validation_summary
  )
}

alprek_targets_validation_summary <- function(
  budget_processed,
  classroom_processed,
  student_processed,
  linkage_master
) {
  linkage_validation <- ALprekDB::linkage_validate(linkage_master)

  dplyr::bind_rows(
    dplyr::mutate(budget_processed$validation_summary, module = "budget", .before = 1),
    dplyr::mutate(classroom_processed$validation_summary, module = "classroom", .before = 1),
    dplyr::mutate(student_processed$validation_summary, module = "student", .before = 1),
    tibble::tibble(
      module = "linkage",
      school_year = NA_character_,
      passed = linkage_validation$passed,
      n_errors = linkage_validation$n_errors,
      n_warnings = linkage_validation$n_warnings,
      n_info = linkage_validation$n_info
    )
  )
}

alprek_targets_synthetic_validation_summary <- function(linkage_master) {
  validation <- ALprekDB::linkage_validate(linkage_master)
  tibble::tibble(
    module = "synthetic_linkage",
    school_year = NA_character_,
    passed = validation$passed,
    n_errors = validation$n_errors,
    n_warnings = validation$n_warnings,
    n_info = validation$n_info
  )
}

alprek_targets_write_summaries <- function(
  validation_summary,
  linkage_summary,
  output_dir
) {
  summary_dir <- file.path(output_dir, "summary")
  dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)

  validation_path <- file.path(summary_dir, "validation_summary.csv")
  linkage_path <- file.path(summary_dir, "linkage_summary.csv")

  utils::write.csv(validation_summary, validation_path, row.names = FALSE)
  utils::write.csv(linkage_summary, linkage_path, row.names = FALSE)

  normalizePath(c(validation_path, linkage_path), mustWork = TRUE)
}

alprek_targets_disabled_marker <- function(output_dir, name, reason) {
  control_dir <- file.path(output_dir, "control")
  dir.create(control_dir, recursive = TRUE, showWarnings = FALSE)

  path <- file.path(control_dir, paste0(name, "_disabled.txt"))
  writeLines(reason, path)
  normalizePath(path, mustWork = TRUE)
}

alprek_targets_write_rds_outputs <- function(
  budget_panel,
  classroom_panel,
  student_panel,
  linkage_master,
  output_dir,
  write_outputs = TRUE
) {
  if (!isTRUE(write_outputs)) {
    return(alprek_targets_disabled_marker(
      output_dir,
      "rds_outputs",
      "Row-level RDS outputs disabled. Set ALPREKDB_WRITE_OUTPUTS=1 to enable in real-data mode."
    ))
  }

  rds_dir <- file.path(output_dir, "rds")
  dir.create(rds_dir, recursive = TRUE, showWarnings = FALSE)

  paths <- c(
    budget = file.path(rds_dir, "budget_panel.rds"),
    classroom = file.path(rds_dir, "classroom_panel.rds"),
    student = file.path(rds_dir, "student_panel.rds"),
    linkage = file.path(rds_dir, "linkage_master.rds")
  )

  saveRDS(budget_panel, paths[["budget"]])
  saveRDS(classroom_panel, paths[["classroom"]])
  saveRDS(student_panel, paths[["student"]])
  saveRDS(linkage_master, paths[["linkage"]])

  normalizePath(paths, mustWork = TRUE)
}

alprek_targets_write_database <- function(
  db_path,
  budget_panel,
  classroom_panel,
  student_panel,
  linkage_master,
  write_outputs = TRUE
) {
  if (!isTRUE(write_outputs)) {
    return(alprek_targets_disabled_marker(
      dirname(db_path),
      "duckdb_output",
      "DuckDB output disabled. Set ALPREKDB_WRITE_OUTPUTS=1 to enable in real-data mode."
    ))
  }

  if (!requireNamespace("duckdb", quietly = TRUE) ||
      !requireNamespace("DBI", quietly = TRUE)) {
    return(alprek_targets_disabled_marker(
      dirname(db_path),
      "duckdb_output",
      "DuckDB output skipped because optional packages duckdb and DBI are not installed."
    ))
  }

  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(db_path)) {
    unlink(db_path)
  }

  conn <- ALprekDB::db_init(db_path)
  on.exit(ALprekDB::db_close(conn), add = TRUE)

  ALprekDB::db_write_panel(conn, budget_panel)
  ALprekDB::db_write_panel(conn, classroom_panel)
  ALprekDB::db_write_panel(conn, student_panel)
  ALprekDB::db_write_master(conn, linkage_master)

  normalizePath(db_path, mustWork = TRUE)
}
