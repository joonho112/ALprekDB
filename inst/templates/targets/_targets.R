library(targets)

if (file.exists("local.env")) {
  readRenviron("local.env")
}

source("R/functions.R")

tar_option_set(
  packages = c("ALprekDB", "dplyr", "tibble"),
  error = "stop"
)

if (alprek_targets_run_realdata()) {
  list(
    tar_target(workflow_config, alprek_targets_config()),
    tar_target(
      realdata_manifest,
      alprek_targets_realdata_manifest(workflow_config$data_dir)
    ),
    tar_target(
      budget_source_files,
      alprek_targets_module_paths(realdata_manifest, "budget"),
      format = "file"
    ),
    tar_target(
      classroom_source_files,
      alprek_targets_module_paths(realdata_manifest, "classroom"),
      format = "file"
    ),
    tar_target(
      student_source_files,
      alprek_targets_module_paths(realdata_manifest, "student"),
      format = "file"
    ),
    tar_target(
      budget_configs,
      alprek_targets_budget_configs(realdata_manifest, workflow_config$output_dir)
    ),
    tar_target(
      classroom_configs,
      alprek_targets_classroom_configs(realdata_manifest, workflow_config$output_dir)
    ),
    tar_target(
      student_configs,
      alprek_targets_student_configs(realdata_manifest, workflow_config$output_dir)
    ),
    tar_target(
      budget_processed,
      alprek_targets_process_budget(budget_configs)
    ),
    tar_target(
      classroom_processed,
      alprek_targets_process_classroom(classroom_configs)
    ),
    tar_target(
      student_processed,
      alprek_targets_process_student(student_configs)
    ),
    tar_target(budget_panel, budget_processed$panel),
    tar_target(classroom_panel, classroom_processed$panel),
    tar_target(student_panel, ALprekDB::student_transform(student_processed$panel)),
    tar_target(
      linkage_master,
      ALprekDB::linkage_create_master(budget_panel, classroom_panel, student_panel)
    ),
    tar_target(
      validation_summary,
      alprek_targets_validation_summary(
        budget_processed,
        classroom_processed,
        student_processed,
        linkage_master
      )
    ),
    tar_target(
      linkage_summary,
      ALprekDB::linkage_summary_stats(linkage_master)
    ),
    tar_target(
      summary_files,
      alprek_targets_write_summaries(
        validation_summary,
        linkage_summary,
        workflow_config$output_dir
      ),
      format = "file"
    ),
    tar_target(
      rds_files,
      alprek_targets_write_rds_outputs(
        budget_panel,
        classroom_panel,
        student_panel,
        linkage_master,
        workflow_config$output_dir,
        workflow_config$write_outputs
      ),
      format = "file"
    ),
    tar_target(
      duckdb_file,
      alprek_targets_write_database(
        file.path(workflow_config$output_dir, "db", "alprekdb.duckdb"),
        budget_panel,
        classroom_panel,
        student_panel,
        linkage_master,
        workflow_config$write_outputs
      ),
      format = "file"
    )
  )
} else {
  list(
    tar_target(workflow_config, alprek_targets_config()),
    tar_target(
      synthetic_panels,
      alprek_targets_synthetic_panels(
        n_classrooms = workflow_config$synthetic$n_classrooms,
        n_students = workflow_config$synthetic$n_students,
        n_years = workflow_config$synthetic$n_years,
        seed = workflow_config$synthetic$seed
      )
    ),
    tar_target(budget_panel, synthetic_panels$budget),
    tar_target(classroom_panel, synthetic_panels$classroom),
    tar_target(student_panel, ALprekDB::student_transform(synthetic_panels$student)),
    tar_target(
      linkage_master,
      ALprekDB::linkage_create_master(budget_panel, classroom_panel, student_panel)
    ),
    tar_target(
      validation_summary,
      alprek_targets_synthetic_validation_summary(linkage_master)
    ),
    tar_target(
      linkage_summary,
      ALprekDB::linkage_summary_stats(linkage_master)
    ),
    tar_target(
      summary_files,
      alprek_targets_write_summaries(
        validation_summary,
        linkage_summary,
        workflow_config$output_dir
      ),
      format = "file"
    ),
    tar_target(
      rds_files,
      alprek_targets_write_rds_outputs(
        budget_panel,
        classroom_panel,
        student_panel,
        linkage_master,
        workflow_config$output_dir,
        workflow_config$write_outputs
      ),
      format = "file"
    ),
    tar_target(
      duckdb_file,
      alprek_targets_write_database(
        file.path(workflow_config$output_dir, "db", "alprekdb.duckdb"),
        budget_panel,
        classroom_panel,
        student_panel,
        linkage_master,
        workflow_config$write_outputs
      ),
      format = "file"
    )
  )
}
