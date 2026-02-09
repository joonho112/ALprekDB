#' Create a Budget Processing Configuration
#'
#' @description Creates a typed configuration object that controls the budget
#'   processing pipeline.
#'
#' @param school_year Character. School year in `"YYYY-YYYY"` format (required).
#' @param budget_path Character. Path to the budget Excel file (required).
#' @param sheet Character or numeric. Excel sheet to read. Default `NULL` (first sheet).
#' @param output_dir Character. Output directory. Default `NULL` (auto-generates).
#' @param tolerance Numeric. Dollar tolerance for reconciliation. Default `1.00`.
#' @param fill_na_zero Logical. Fill NA budget cells with 0? Default `TRUE`.
#' @param remove_footer Logical. Drop footer/summary rows? Default `TRUE`.
#' @param verbose Logical. Print progress messages? Default `TRUE`.
#'
#' @return An `alprek_budget_config` S3 object.
#'
#' @examples
#' \dontrun{
#' cfg <- budget_config(
#'   school_year = "2024-2025",
#'   budget_path = "data/24-25 FCPK Budgets.xlsx"
#' )
#' result <- budget_process(cfg)
#' }
#'
#' @export
budget_config <- function(school_year,
                          budget_path,
                          sheet = NULL,
                          output_dir = NULL,
                          tolerance = 1.00,
                          fill_na_zero = TRUE,
                          remove_footer = TRUE,
                          verbose = TRUE) {

  if (missing(school_year) || is.null(school_year)) {
    stop("school_year is required (e.g., '2024-2025').", call. = FALSE)
  }
  if (missing(budget_path) || is.null(budget_path)) {
    stop("budget_path is required.", call. = FALSE)
  }

  if (is.null(output_dir)) {
    output_dir <- file.path("output", school_year)
  }

  structure(
    list(
      school_year = school_year,
      budget_path = budget_path,
      sheet = sheet,
      output_dir = output_dir,
      tolerance = tolerance,
      fill_na_zero = fill_na_zero,
      remove_footer = remove_footer,
      verbose = verbose
    ),
    class = "alprek_budget_config"
  )
}


#' Print method for alprek_budget_config
#' @param x An alprek_budget_config object.
#' @param ... Ignored.
#' @export
print.alprek_budget_config <- function(x, ...) {
  cat("<alprek_budget_config>\n")
  cat("  School year:", x$school_year, "\n")
  cat("  Budget file:", x$budget_path, "\n")
  cat("  Output dir:", x$output_dir, "\n")
  cat("  Tolerance: $", x$tolerance, "\n")
  invisible(x)
}


#' Run Complete Budget Processing Pipeline
#'
#' @description Convenience function that runs the full pipeline for a single
#'   year: read -> clean -> validate -> transform.
#'
#' @param config An `alprek_budget_config` object.
#' @param export Logical. Export results? Default `FALSE`.
#' @param export_formats Character vector of export formats.
#'   Options: `"csv"`, `"parquet"`, `"excel"`, `"rds"`. Default `"csv"`.
#'
#' @return A list with elements: `raw`, `long`, `validation`, `master`.
#'
#' @examples
#' \dontrun{
#' cfg <- budget_config("2024-2025", "data/24-25 FCPK Budgets.xlsx")
#' result <- budget_process(cfg)
#' result$master$data  # analysis-ready data
#' }
#'
#' @export
budget_process <- function(config,
                           export = FALSE,
                           export_formats = "csv") {

  if (!inherits(config, "alprek_budget_config")) {
    stop("Expected an 'alprek_budget_config' object. Use budget_config() first.",
         call. = FALSE)
  }

  # Step 1: Read
  raw <- budget_read(
    path = config$budget_path,
    school_year = config$school_year,
    sheet = config$sheet,
    remove_footer = config$remove_footer
  )

  # Step 2: Clean
  long <- budget_clean(raw, tolerance = config$tolerance)

  # Step 3: Validate
  validation <- budget_validate(long, tolerance = config$tolerance)

  # Step 4: Transform
  master <- budget_transform(long)

  # Step 5: Export (optional)
  if (export) {
    if (!dir.exists(config$output_dir)) {
      dir.create(config$output_dir, recursive = TRUE)
    }

    sy <- gsub("-", "_", config$school_year)

    if ("csv" %in% export_formats) {
      budget_export_csv(master,
                        file.path(config$output_dir, paste0("budget_", sy, ".csv")))
    }
    if ("rds" %in% export_formats) {
      budget_export_rds(master,
                        file.path(config$output_dir, paste0("budget_", sy, ".rds")))
    }
    if ("parquet" %in% export_formats) {
      budget_export_parquet(master,
                            file.path(config$output_dir, paste0("budget_", sy, ".parquet")))
    }
    if ("excel" %in% export_formats) {
      budget_export_excel(master,
                          file.path(config$output_dir, paste0("budget_", sy, ".xlsx")))
    }
  }

  list(
    raw = raw,
    long = long,
    validation = validation,
    master = master
  )
}


#' Run Complete Pipeline for Multiple Years
#'
#' @description Processes multiple years and combines them into a panel dataset.
#'
#' @param configs A list of `alprek_budget_config` objects.
#' @param export Logical. Export panel results? Default `FALSE`.
#' @param export_formats Character vector of export formats.
#'
#' @return A list with elements: `by_year`, `panel`, `validation_summary`.
#'
#' @examples
#' \dontrun{
#' configs <- list(
#'   budget_config("2021-2022", "data/rptClassBudgets 2021-2022.xlsx"),
#'   budget_config("2022-2023", "data/rptClassBudgets 2022-2023.xlsx"),
#'   budget_config("2023-2024", "data/rptClassBudgets 2023-2024.xlsx"),
#'   budget_config("2024-2025", "data/24-25 FCPK Budgets.xlsx")
#' )
#' result <- budget_process_years(configs)
#' result$panel$data  # multi-year panel
#' }
#'
#' @export
budget_process_years <- function(configs,
                                 export = FALSE,
                                 export_formats = "csv") {

  by_year <- lapply(configs, function(cfg) {
    budget_process(cfg, export = FALSE)
  })
  names(by_year) <- sapply(configs, function(cfg) cfg$school_year)

  # Combine into panel
  master_list <- lapply(by_year, function(res) res$master)
  panel <- budget_bind_years(master_list = master_list)

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
    out_dir <- "output"
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

    if ("csv" %in% export_formats) {
      budget_export_csv(panel, file.path(out_dir, "budget_panel.csv"))
    }
    if ("rds" %in% export_formats) {
      budget_export_rds(panel, file.path(out_dir, "budget_panel.rds"))
    }
  }

  list(
    by_year = by_year,
    panel = panel,
    validation_summary = validation_summary
  )
}
