#' Create an Applications Processing Configuration
#'
#' @description Creates a typed configuration object that controls the
#'   applications module pipeline (read -> clean -> reconcile -> validate ->
#'   transform -> panel -> export).
#'
#' @param cycle_year Character. Cycle year in `"YYYY-YYYY"` format (e.g.,
#'   `"2026-2027"`). Required.
#' @param master_path Character. Path to the cycle-1 master xlsx file
#'   containing all renewal/new/non-renewal/capacity sheets. Required for
#'   cycle-1 (combined-file layout).
#' @param renewals_path Character. Path to cycle-0 renewals xlsx file
#'   (separate-file layout). Used when cycle-0 is being re-processed.
#'   Default `NULL`.
#' @param new_apps_path Character. Path to cycle-0 new applications xlsx file.
#'   Default `NULL`.
#' @param consolidated_path Character. Path to "Classroom requests" or
#'   equivalent supplementary xlsx file. Optional. Default `NULL`.
#' @param prior_deliverable_path Character. Path to prior cycle final
#'   deliverable xlsx (e.g., 2025-2026 full application list_Added_Economic
#'   Needs.xlsx) — used as reference / YoY comparison. Default `NULL`.
#' @param output_dir Character. Output directory. Default `NULL` (auto:
#'   `output/applications/<cycle_year>`).
#' @param fuzzy_threshold Numeric in (0, 1). Jaro-Winkler similarity threshold
#'   for fuzzy classroom-name matching in `applications_reconcile()`. Default
#'   `0.85`.
#' @param cycle Character. Cycle schema label ("cycle1" / "cycle0"). Used by
#'   `applications_detect_format()` and codebook loaders. Default `"cycle1"`.
#' @param seed Integer. Random seed for reproducibility (used by fuzzy match
#'   deterministic tiebreaks etc.). Default `20260519L`.
#' @param remove_noise_rows Logical. Drop noise rows (e.g., "Show the Debugger
#'   Trace Report") at clean step? Default `TRUE`.
#' @param verbose Logical. Print progress messages? Default `TRUE`.
#'
#' @return An `alprek_applications_config` S3 object.
#'
#' @examples
#' \dontrun{
#' cfg <- applications_config(
#'   cycle_year = "2026-2027",
#'   master_path = file.path("ORIGINAL-DATA", "applications_2026_2027.xlsx")
#' )
#' result <- applications_process(cfg)
#' }
#'
#' @export
applications_config <- function(cycle_year,
                                  master_path = NULL,
                                  renewals_path = NULL,
                                  new_apps_path = NULL,
                                  consolidated_path = NULL,
                                  prior_deliverable_path = NULL,
                                  output_dir = NULL,
                                  fuzzy_threshold = 0.85,
                                  cycle = c("cycle1", "cycle0"),
                                  seed = 20260519L,
                                  remove_noise_rows = TRUE,
                                  verbose = TRUE) {

  # ---- validation ----
  if (missing(cycle_year) || is.null(cycle_year) || !nzchar(cycle_year)) {
    stop("cycle_year is required (e.g., '2026-2027').", call. = FALSE)
  }
  if (!grepl("^\\d{4}-\\d{4}$", cycle_year)) {
    stop("cycle_year must be in 'YYYY-YYYY' format. Got: ", cycle_year,
         call. = FALSE)
  }

  cycle <- match.arg(cycle)

  # For cycle1, master_path is required; for cycle0, renewals_path is required
  if (cycle == "cycle1" && (is.null(master_path) || !nzchar(master_path))) {
    stop("master_path is required when cycle = 'cycle1'.", call. = FALSE)
  }
  if (cycle == "cycle0" && (is.null(renewals_path) || !nzchar(renewals_path))) {
    stop("renewals_path is required when cycle = 'cycle0'.", call. = FALSE)
  }

  if (!is.numeric(fuzzy_threshold) || length(fuzzy_threshold) != 1L ||
      fuzzy_threshold <= 0 || fuzzy_threshold >= 1) {
    stop("fuzzy_threshold must be a single numeric in (0, 1). Got: ",
         fuzzy_threshold, call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1L) {
    stop("seed must be a single integer.", call. = FALSE)
  }
  seed <- as.integer(seed)

  if (is.null(output_dir)) {
    output_dir <- file.path("output", "applications", cycle_year)
  }

  structure(
    list(
      cycle_year = cycle_year,
      cycle = cycle,
      master_path = master_path,
      renewals_path = renewals_path,
      new_apps_path = new_apps_path,
      consolidated_path = consolidated_path,
      prior_deliverable_path = prior_deliverable_path,
      output_dir = output_dir,
      fuzzy_threshold = fuzzy_threshold,
      seed = seed,
      remove_noise_rows = remove_noise_rows,
      verbose = verbose
    ),
    class = "alprek_applications_config"
  )
}


#' Print method for alprek_applications_config
#'
#' @param x An `alprek_applications_config` object.
#' @param ... Ignored.
#' @export
print.alprek_applications_config <- function(x, ...) {
  cat("<alprek_applications_config>\n")
  cat("  Cycle year:        ", x$cycle_year, "\n")
  cat("  Cycle schema:      ", x$cycle, "\n")
  if (!is.null(x$master_path)) {
    cat("  Master xlsx:       ", x$master_path, "\n")
  }
  if (!is.null(x$renewals_path)) {
    cat("  Renewals (cycle0): ", x$renewals_path, "\n")
  }
  if (!is.null(x$new_apps_path)) {
    cat("  New apps (cycle0): ", x$new_apps_path, "\n")
  }
  if (!is.null(x$consolidated_path)) {
    cat("  Consolidated:      ", x$consolidated_path, "\n")
  }
  if (!is.null(x$prior_deliverable_path)) {
    cat("  Prior deliverable: ", x$prior_deliverable_path, "\n")
  }
  cat("  Output dir:        ", x$output_dir, "\n")
  cat("  Fuzzy threshold:   ", x$fuzzy_threshold, "\n")
  cat("  Seed:              ", x$seed, "\n")
  cat("  Remove noise rows: ", x$remove_noise_rows, "\n")
  cat("  Verbose:           ", x$verbose, "\n")
  invisible(x)
}
