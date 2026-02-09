#' Create a Student Processing Configuration
#'
#' @description Creates a typed configuration object that controls the student
#'   processing pipeline.
#'
#' @param school_year Character. School year in `"YYYY-YYYY"` format (required).
#' @param path Character. Path to the Student/Child Details Excel file (required).
#' @param sheet Character or numeric. Excel sheet to read.
#'   Default `"rptChildren_Excel"`.
#' @param include_pii Logical. Include PII columns? Default `FALSE`.
#' @param remove_footer Logical. Drop footer/summary rows? Default `TRUE`.
#' @param output_dir Character. Output directory. Default `NULL` (auto-generates).
#'
#' @return An `alprek_student_config` S3 object.
#'
#' @examples
#' \dontrun{
#' cfg <- student_config(
#'   school_year = "2024-2025",
#'   path = "data/24-25 FCPK Child Details.xlsx"
#' )
#' result <- student_process(cfg)
#' }
#'
#' @export
student_config <- function(school_year,
                           path,
                           sheet = "rptChildren_Excel",
                           include_pii = FALSE,
                           remove_footer = TRUE,
                           output_dir = NULL) {

  if (missing(school_year) || is.null(school_year)) {
    stop("school_year is required (e.g., '2024-2025').", call. = FALSE)
  }
  if (missing(path) || is.null(path)) {
    stop("path is required.", call. = FALSE)
  }

  if (is.null(output_dir)) {
    output_dir <- file.path("output", "student", school_year)
  }

  structure(
    list(
      school_year = school_year,
      path = path,
      sheet = sheet,
      include_pii = include_pii,
      remove_footer = remove_footer,
      output_dir = output_dir
    ),
    class = "alprek_student_config"
  )
}


#' Print method for alprek_student_config
#' @param x An alprek_student_config object.
#' @param ... Ignored.
#' @export
print.alprek_student_config <- function(x, ...) {
  cat("<alprek_student_config>\n")
  cat("  School year:", x$school_year, "\n")
  cat("  Student file:", x$path, "\n")
  cat("  Sheet:", x$sheet, "\n")
  cat("  Include PII:", x$include_pii, "\n")
  cat("  Output dir:", x$output_dir, "\n")
  invisible(x)
}


#' Run Complete Student Processing Pipeline
#'
#' @description Convenience function that runs the full pipeline for a single
#'   year: read -> clean -> validate.
#'
#' @param config An `alprek_student_config` object.
#' @param export Logical. Export results? Default `FALSE`.
#' @param export_formats Character vector of export formats.
#'   Options: `"csv"`, `"excel"`, `"rds"`, `"stata"`, `"parquet"`. Default `"csv"`.
#'
#' @return A list with elements: `raw`, `clean`, `validation`.
#'
#' @examples
#' \dontrun{
#' cfg <- student_config("2024-2025", "data/24-25 FCPK Child Details.xlsx")
#' result <- student_process(cfg)
#' result$clean$data
#' }
#'
#' @export
student_process <- function(config,
                            export = FALSE,
                            export_formats = "csv") {

  if (!inherits(config, "alprek_student_config")) {
    stop("Expected an 'alprek_student_config' object. Use student_config() first.",
         call. = FALSE)
  }

  # Step 1: Read
  raw <- student_read(
    path = config$path,
    school_year = config$school_year,
    sheet = config$sheet,
    remove_footer = config$remove_footer
  )

  # Step 2: Clean
  clean <- student_clean(raw, include_pii = config$include_pii)

  # Step 3: Validate
  validation <- student_validate(clean)

  # Step 4: Export (optional)
  if (export) {
    if (!dir.exists(config$output_dir)) {
      dir.create(config$output_dir, recursive = TRUE)
    }

    sy <- gsub("-", "_", config$school_year)

    if ("csv" %in% export_formats) {
      student_export_csv(clean,
                         file.path(config$output_dir, paste0("student_", sy, ".csv")))
    }
    if ("rds" %in% export_formats) {
      student_export_rds(clean,
                         file.path(config$output_dir, paste0("student_", sy, ".rds")))
    }
    if ("excel" %in% export_formats) {
      student_export_excel(clean,
                           file.path(config$output_dir, paste0("student_", sy, ".xlsx")))
    }
    if ("stata" %in% export_formats) {
      student_export_stata(clean,
                           file.path(config$output_dir, paste0("student_", sy, ".dta")))
    }
    if ("parquet" %in% export_formats) {
      student_export_parquet(clean,
                             file.path(config$output_dir, paste0("student_", sy, ".parquet")))
    }
  }

  list(
    raw = raw,
    clean = clean,
    validation = validation
  )
}


#' Run Complete Pipeline for Multiple Years
#'
#' @description Processes multiple years and combines them into a panel dataset.
#'
#' @param configs A list of `alprek_student_config` objects.
#' @param export Logical. Export panel results? Default `FALSE`.
#' @param export_formats Character vector of export formats.
#'
#' @return A list with elements: `by_year`, `panel`, `validation_summary`.
#'
#' @examples
#' \dontrun{
#' configs <- list(
#'   student_config("2021-2022", "data/FCPK Student Details 21-22.xlsx"),
#'   student_config("2022-2023", "data/FCPK Student Details 22-23.xlsx"),
#'   student_config("2023-2024", "data/FCPK Student Details 23-24.xlsx"),
#'   student_config("2024-2025", "data/24-25 FCPK Child Details.xlsx")
#' )
#' result <- student_process_years(configs)
#' result$panel$data
#' }
#'
#' @export
student_process_years <- function(configs,
                                  export = FALSE,
                                  export_formats = "csv") {

  by_year <- lapply(configs, function(cfg) {
    student_process(cfg, export = FALSE)
  })
  names(by_year) <- sapply(configs, function(cfg) cfg$school_year)

  # Combine into panel
  clean_list <- lapply(by_year, function(res) res$clean)
  panel <- student_bind_years(clean_list = clean_list)

  # Validation summary
  validation_summary <- dplyr::bind_rows(lapply(names(by_year), function(yr) {
    v <- by_year[[yr]]$validation
    tibble::tibble(
      school_year = yr,
      passed = v$passed,
      n_errors = v$n_errors,
      n_warnings = v$n_warnings
    )
  }))

  # Export panel if requested
  if (export) {
    out_dir <- file.path("output", "student")
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

    if ("csv" %in% export_formats) {
      student_export_csv(panel, file.path(out_dir, "student_panel.csv"))
    }
    if ("rds" %in% export_formats) {
      student_export_rds(panel, file.path(out_dir, "student_panel.rds"))
    }
    if ("excel" %in% export_formats) {
      student_export_excel(panel, file.path(out_dir, "student_panel.xlsx"))
    }
    if ("stata" %in% export_formats) {
      student_export_stata(panel, file.path(out_dir, "student_panel.dta"))
    }
    if ("parquet" %in% export_formats) {
      student_export_parquet(panel, file.path(out_dir, "student_panel.parquet"))
    }
  }

  list(
    by_year = by_year,
    panel = panel,
    validation_summary = validation_summary
  )
}
