#' Create a Classroom Processing Configuration
#'
#' @description Creates a typed configuration object that controls the classroom
#'   processing pipeline.
#'
#' @param school_year Character. School year in `"YYYY-YYYY"` format (required).
#' @param classroom_path Character. Path to the classroom Excel file (required).
#' @param sheet Character or numeric. Excel sheet to read. Default `"rptRIF"`.
#' @param include_dob Logical. Include Date of Birth columns? Default `FALSE`.
#' @param remove_footer Logical. Drop footer/summary rows? Default `TRUE`.
#' @param output_dir Character. Output directory. Default `NULL` (auto-generates).
#'
#' @return An `alprek_classroom_config` S3 object.
#'
#' @examples
#' \dontrun{
#' cfg <- classroom_config(
#'   school_year = "2024-2025",
#'   classroom_path = "data/24-25 Classroom Details.xlsx"
#' )
#' result <- classroom_process(cfg)
#' }
#'
#' @export
classroom_config <- function(school_year,
                             classroom_path,
                             sheet = "rptRIF",
                             include_dob = FALSE,
                             remove_footer = TRUE,
                             output_dir = NULL) {

  if (missing(school_year) || is.null(school_year)) {
    stop("school_year is required (e.g., '2024-2025').", call. = FALSE)
  }
  if (missing(classroom_path) || is.null(classroom_path)) {
    stop("classroom_path is required.", call. = FALSE)
  }

  if (is.null(output_dir)) {
    output_dir <- file.path("output", "classroom", school_year)
  }

  structure(
    list(
      school_year = school_year,
      classroom_path = classroom_path,
      sheet = sheet,
      include_dob = include_dob,
      remove_footer = remove_footer,
      output_dir = output_dir
    ),
    class = "alprek_classroom_config"
  )
}


#' Print method for alprek_classroom_config
#' @param x An alprek_classroom_config object.
#' @param ... Ignored.
#' @export
print.alprek_classroom_config <- function(x, ...) {
  cat("<alprek_classroom_config>\n")
  cat("  School year:", x$school_year, "\n")
  cat("  Classroom file:", x$classroom_path, "\n")
  cat("  Sheet:", x$sheet, "\n")
  cat("  Include DOB:", x$include_dob, "\n")
  cat("  Output dir:", x$output_dir, "\n")
  invisible(x)
}


#' Run Complete Classroom Processing Pipeline
#'
#' @description Convenience function that runs the full pipeline for a single
#'   year: read -> clean -> validate.
#'
#' @param config An `alprek_classroom_config` object.
#' @param export Logical. Export results? Default `FALSE`.
#' @param export_formats Character vector of export formats.
#'   Options: `"csv"`, `"excel"`, `"rds"`, `"stata"`, `"parquet"`. Default `"csv"`.
#'
#' @return A list with elements: `raw`, `clean`, `validation`.
#'
#' @examples
#' \dontrun{
#' cfg <- classroom_config("2024-2025", "data/24-25 Classroom Details.xlsx")
#' result <- classroom_process(cfg)
#' result$clean$data
#' }
#'
#' @export
classroom_process <- function(config,
                              export = FALSE,
                              export_formats = "csv") {

  if (!inherits(config, "alprek_classroom_config")) {
    stop("Expected an 'alprek_classroom_config' object. Use classroom_config() first.",
         call. = FALSE)
  }

  # Step 1: Read
  raw <- classroom_read(
    path = config$classroom_path,
    school_year = config$school_year,
    sheet = config$sheet,
    remove_footer = config$remove_footer
  )

  # Step 2: Clean
  clean <- classroom_clean(raw, include_dob = config$include_dob)

  # Step 3: Validate
  validation <- classroom_validate(clean)

  # Step 4: Export (optional)
  if (export) {
    if (!dir.exists(config$output_dir)) {
      dir.create(config$output_dir, recursive = TRUE)
    }

    sy <- gsub("-", "_", config$school_year)

    if ("csv" %in% export_formats) {
      classroom_export_csv(clean,
                           file.path(config$output_dir, paste0("classroom_", sy, ".csv")))
    }
    if ("rds" %in% export_formats) {
      classroom_export_rds(clean,
                           file.path(config$output_dir, paste0("classroom_", sy, ".rds")))
    }
    if ("excel" %in% export_formats) {
      classroom_export_excel(clean,
                             file.path(config$output_dir, paste0("classroom_", sy, ".xlsx")))
    }
    if ("stata" %in% export_formats) {
      classroom_export_stata(clean,
                             file.path(config$output_dir, paste0("classroom_", sy, ".dta")))
    }
    if ("parquet" %in% export_formats) {
      classroom_export_parquet(clean,
                               file.path(config$output_dir, paste0("classroom_", sy, ".parquet")))
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
#' @param configs A list of `alprek_classroom_config` objects.
#' @param export Logical. Export panel results? Default `FALSE`.
#' @param export_formats Character vector of export formats.
#'
#' @return A list with elements: `by_year`, `panel`, `validation_summary`.
#'
#' @examples
#' \dontrun{
#' configs <- list(
#'   classroom_config("2021-2022", "data/FCPK Classroom Details 21-22.xlsx"),
#'   classroom_config("2022-2023", "data/FCPK Classroom Details 22-23.xlsx"),
#'   classroom_config("2023-2024", "data/FCPK Classroom Details 23-24.xlsx"),
#'   classroom_config("2024-2025", "data/24-25 Classroom Details.xlsx")
#' )
#' result <- classroom_process_years(configs)
#' result$panel$data
#' }
#'
#' @export
classroom_process_years <- function(configs,
                                    export = FALSE,
                                    export_formats = "csv") {

  by_year <- lapply(configs, function(cfg) {
    classroom_process(cfg, export = FALSE)
  })
  names(by_year) <- sapply(configs, function(cfg) cfg$school_year)

  # Combine into panel
  clean_list <- lapply(by_year, function(res) res$clean)
  panel <- classroom_bind_years(clean_list = clean_list)

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

  # Degree audit summary
  degree_audit <- dplyr::bind_rows(lapply(names(by_year), function(yr) {
    audit <- by_year[[yr]]$clean$degree_audit
    if (nrow(audit) > 0) {
      audit$school_year <- yr
      audit
    } else {
      NULL
    }
  }))

  # Export panel if requested
  if (export) {
    out_dir <- file.path("output", "classroom")
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

    if ("csv" %in% export_formats) {
      classroom_export_csv(panel, file.path(out_dir, "classroom_panel.csv"))
    }
    if ("rds" %in% export_formats) {
      classroom_export_rds(panel, file.path(out_dir, "classroom_panel.rds"))
    }
    if ("excel" %in% export_formats) {
      classroom_export_excel(panel, file.path(out_dir, "classroom_panel.xlsx"))
    }
    if ("stata" %in% export_formats) {
      classroom_export_stata(panel, file.path(out_dir, "classroom_panel.dta"))
    }
    if ("parquet" %in% export_formats) {
      classroom_export_parquet(panel, file.path(out_dir, "classroom_panel.parquet"))
    }
  }

  list(
    by_year = by_year,
    panel = panel,
    validation_summary = validation_summary,
    degree_audit = degree_audit
  )
}
