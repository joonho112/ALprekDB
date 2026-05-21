#' Create a Geocoding Processing Configuration
#'
#' @description Creates a typed configuration object that controls the
#'   geocoding module pipeline (read -> clean -> validate -> reconcile ->
#'   followup -> export). The object is consumed by `geocode_process()`
#'   and the lower-level step functions (`geocode_read()`,
#'   `geocode_clean()`, `geocode_validate()`, `geocode_reconcile()`, etc.).
#'
#' @param path Character. Path to the Melissa-returned geocoded xlsx file
#'   (e.g., `"ORIGINAL-DATA/2026-03-04_Pre-K Geocoding Melissa/2026-03-04_geocoding_master_Final.xlsx"`).
#'   Required at call site. Existence is **not** checked at constructor
#'   time — `geocode_read()` is responsible for the file-system check so
#'   the config object can be assembled in dry-run / test contexts.
#' @param sheet Character. Worksheet name within the xlsx file. Default
#'   `"Sheet1"` (the v1 Melissa contract).
#' @param vendor Character. Geocoding vendor identifier. Default
#'   `"melissa"`. Reserved for future multi-vendor support.
#' @param cycle_year Character. Cycle year in `"YYYY-YYYY"` format
#'   (e.g., `"2026-2027"`). Required.
#' @param delivery_date Date or character. Date the geocoded file was
#'   delivered by the vendor (e.g., `"2026-03-04"` or
#'   `as.Date("2026-03-04")`). Required. Character input is coerced to
#'   `Date` via `as.Date()`; an unparsable string throws.
#' @param seed Integer. Random seed for reproducibility (deterministic
#'   tiebreaks, sampling for diagnostics). Default `20260520L`.
#' @param verbose Logical. Print progress messages? Default `TRUE`.
#' @param authoritative_priority Character. Which source wins when both
#'   ADECE and Melissa have valid coordinates but they disagree. One of
#'   `c("melissa_first", "adece_first")`. Default `"melissa_first"`.
#' @param distance_threshold_rules Character. How `geocode_reconcile()`
#'   decides which (ADECE, Melissa) lat/long pairs require manual
#'   followup. One of `c("by_resultcode", "flat_100m", "flat_250m")`.
#'   Default `"by_resultcode"` (per-RESULTCODE tiered thresholds — see
#'   `tiered_thresholds`).
#' @param flat_threshold_m Integer. Flat distance threshold in meters,
#'   used only when `distance_threshold_rules` starts with `"flat_"`.
#'   Default `250L`.
#' @param tiered_thresholds Named list. Per-RESULTCODE thresholds in
#'   meters used when `distance_threshold_rules == "by_resultcode"`.
#'   Names must be RESULTCODE strings (e.g., `"GS01"`); values may be
#'   `Inf` (always flag for review). Default
#'   `list(GS01 = 50, GS05 = 250, GS06 = 500, GS03 = Inf)`.
#' @param acceptable_resultcodes Character vector. Melissa RESULTCODE
#'   values expected to be master-acceptable under the current codebook.
#'   This is retained as configuration metadata and a print surface; the
#'   reconciler enforces `acceptable_for_master` from
#'   [alprek_geocode_resultcode_meaning()] so the CSV remains the source of
#'   truth. Default
#'   `c("GS01", "GS05", "GS06")`, matching
#'   `melissa_resultcode_codes.csv$acceptable_for_master == TRUE`.
#' @param al_lat_bounds Numeric length-2. Alabama latitude bounding box
#'   `c(min, max)` for the in-state sanity check. Default `c(30, 36)`.
#' @param al_lng_bounds Numeric length-2. Alabama longitude bounding box
#'   `c(min, max)`. Default `c(-89, -84)`.
#'
#' @return An `alprek_geocode_config` S3 object — a named list with all
#'   parameters above, ready for consumption by the geocode pipeline.
#'
#' @examples
#' \dontrun{
#' cfg <- geocode_config(
#'   path = file.path("ORIGINAL-DATA",
#'                    "2026-03-04_Pre-K Geocoding Melissa",
#'                    "2026-03-04_geocoding_master_Final.xlsx"),
#'   cycle_year = "2026-2027",
#'   delivery_date = "2026-03-04"
#' )
#' print(cfg)
#' }
#'
#' @export
geocode_config <- function(path,
                           sheet = "Sheet1",
                           vendor = "melissa",
                           cycle_year,
                           delivery_date,
                           seed = 20260520L,
                           verbose = TRUE,
                           authoritative_priority = c("melissa_first",
                                                      "adece_first"),
                           distance_threshold_rules = c("by_resultcode",
                                                        "flat_100m",
                                                        "flat_250m"),
                           flat_threshold_m = 250L,
                           tiered_thresholds = list(GS01 = 50,
                                                    GS05 = 250,
                                                    GS06 = 500,
                                                    GS03 = Inf),
                           acceptable_resultcodes = c("GS01", "GS05", "GS06"),
                           al_lat_bounds = c(30, 36),
                           al_lng_bounds = c(-89, -84)) {

  if (missing(path) || is.null(path) || !is.character(path) ||
      length(path) != 1L || !nzchar(path)) {
    stop("path is required (single non-empty character; Melissa xlsx file).",
         call. = FALSE)
  }

  if (!is.character(sheet) || length(sheet) != 1L || !nzchar(sheet)) {
    stop("sheet must be a single non-empty character.", call. = FALSE)
  }

  if (!is.character(vendor) || length(vendor) != 1L || !nzchar(vendor)) {
    stop("vendor must be a single non-empty character.", call. = FALSE)
  }

  if (missing(cycle_year) || is.null(cycle_year) || !nzchar(cycle_year)) {
    stop("cycle_year is required (e.g., '2026-2027').", call. = FALSE)
  }
  if (!grepl("^\\d{4}-\\d{4}$", cycle_year)) {
    stop("cycle_year must be in 'YYYY-YYYY' format. Got: ", cycle_year,
         call. = FALSE)
  }

  if (missing(delivery_date) || is.null(delivery_date)) {
    stop("delivery_date is required (Date or 'YYYY-MM-DD' character).",
         call. = FALSE)
  }
  if (inherits(delivery_date, "Date")) {
    # already a Date
  } else if (is.character(delivery_date) && length(delivery_date) == 1L &&
              nzchar(delivery_date)) {
    parsed <- suppressWarnings(as.Date(delivery_date))
    if (is.na(parsed)) {
      stop("delivery_date could not be parsed as a Date. Got: ",
           delivery_date, " (expected 'YYYY-MM-DD').", call. = FALSE)
    }
    delivery_date <- parsed
  } else {
    stop("delivery_date must be a Date or single 'YYYY-MM-DD' character.",
         call. = FALSE)
  }

  if (!is.numeric(seed) || length(seed) != 1L) {
    stop("seed must be a single integer.", call. = FALSE)
  }
  seed <- as.integer(seed)

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("verbose must be a single logical (TRUE/FALSE).", call. = FALSE)
  }

  authoritative_priority   <- match.arg(authoritative_priority)
  distance_threshold_rules <- match.arg(distance_threshold_rules)

  if (!is.numeric(flat_threshold_m) || length(flat_threshold_m) != 1L ||
      is.na(flat_threshold_m) || flat_threshold_m <= 0) {
    stop("flat_threshold_m must be a single positive number (meters).",
         call. = FALSE)
  }
  flat_threshold_m <- as.integer(flat_threshold_m)

  if (!is.list(tiered_thresholds) || is.null(names(tiered_thresholds)) ||
      any(!nzchar(names(tiered_thresholds))) ||
      anyDuplicated(names(tiered_thresholds)) != 0L) {
    stop("tiered_thresholds must be a named list with unique, non-empty names.",
         call. = FALSE)
  }
  bad_vals <- !vapply(tiered_thresholds, function(v) {
    is.numeric(v) && length(v) == 1L && !is.na(v) && v >= 0
  }, logical(1))
  if (any(bad_vals)) {
    stop("tiered_thresholds values must each be a single non-negative number. ",
         "Offending name(s): ",
         paste(names(tiered_thresholds)[bad_vals], collapse = ", "),
         call. = FALSE)
  }

  if (!is.character(acceptable_resultcodes) ||
      length(acceptable_resultcodes) == 0L ||
      any(!nzchar(acceptable_resultcodes)) ||
      anyDuplicated(acceptable_resultcodes) != 0L) {
    stop("acceptable_resultcodes must be a non-empty character vector with ",
         "unique, non-empty values.", call. = FALSE)
  }

  if (!is.numeric(al_lat_bounds) || length(al_lat_bounds) != 2L ||
      anyNA(al_lat_bounds) || al_lat_bounds[1] >= al_lat_bounds[2]) {
    stop("al_lat_bounds must be a numeric length-2 vector c(min, max) ",
         "with min < max.", call. = FALSE)
  }
  if (!is.numeric(al_lng_bounds) || length(al_lng_bounds) != 2L ||
      anyNA(al_lng_bounds) || al_lng_bounds[1] >= al_lng_bounds[2]) {
    stop("al_lng_bounds must be a numeric length-2 vector c(min, max) ",
         "with min < max.", call. = FALSE)
  }

  structure(
    list(
      path = path,
      sheet = sheet,
      vendor = vendor,
      cycle_year = cycle_year,
      delivery_date = delivery_date,
      seed = seed,
      verbose = verbose,
      authoritative_priority = authoritative_priority,
      distance_threshold_rules = distance_threshold_rules,
      flat_threshold_m = flat_threshold_m,
      tiered_thresholds = tiered_thresholds,
      acceptable_resultcodes = acceptable_resultcodes,
      al_lat_bounds = al_lat_bounds,
      al_lng_bounds = al_lng_bounds
    ),
    class = "alprek_geocode_config"
  )
}


#' Print method for alprek_geocode_config
#'
#' @param x An `alprek_geocode_config` object.
#' @param ... Ignored.
#' @export
print.alprek_geocode_config <- function(x, ...) {
  cat("<alprek_geocode_config>\n")
  cat("  Vendor:                   ", x$vendor, "\n")
  cat("  Cycle year:               ", x$cycle_year, "\n")
  cat("  Delivery date:            ", format(x$delivery_date), "\n")
  cat("  Path:                     ", x$path, "\n")
  cat("  Sheet:                    ", x$sheet, "\n")
  cat("  Seed:                     ", x$seed, "\n")
  cat("  Verbose:                  ", x$verbose, "\n")
  cat("  Authoritative priority:   ", x$authoritative_priority, "\n")
  cat("  Distance threshold rules: ", x$distance_threshold_rules, "\n")
  if (startsWith(x$distance_threshold_rules, "flat_")) {
    cat("  Flat threshold (m):       ", x$flat_threshold_m, "\n")
  } else {
    cat("  Tiered thresholds (m):\n")
    for (nm in names(x$tiered_thresholds)) {
      val <- x$tiered_thresholds[[nm]]
      cat("    ", format(nm, width = 6), "= ",
          if (is.infinite(val)) "Inf (always flag)" else val, "\n",
          sep = "")
    }
  }
  cat("  Acceptable RESULTCODEs:   ",
      paste(x$acceptable_resultcodes, collapse = ", "), "\n")
  cat("  AL lat bounds:            [",
      x$al_lat_bounds[1], ", ", x$al_lat_bounds[2], "]\n", sep = "")
  cat("  AL lng bounds:            [",
      x$al_lng_bounds[1], ", ", x$al_lng_bounds[2], "]\n", sep = "")
  invisible(x)
}
