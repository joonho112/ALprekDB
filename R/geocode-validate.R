#' Validate Cleaned Geocode Data
#'
#' @description Comprehensive data-quality checks on an
#'   `alprek_geocode_clean` object (output of [geocode_clean()]). Mirrors
#'   the API of [applications_validate()], [budget_validate()], and
#'   [classroom_validate()]: each check is logged with a structured row
#'   (`check_id`, `description`, `status` one of `PASS`/`ERROR`/`WARN`/`INFO`,
#'   `n_issues`, `details`), and offending rows accumulate in `$issues`.
#'
#'   Validation is scoped to the **data contract layer**: column existence,
#'   value ranges, AL geographic bounds, codebook membership, key
#'   consistency, provenance. Per-row coordinate reconciliation (ADECE vs
#'   Melissa, distance-tier-driven decisions, follow-up routing) is the
#'   responsibility of [geocode_reconcile()] in Phase 4.
#'
#'   ## 15 Checks
#'
#'   1. `required_columns` (ERROR): all 29 expected columns present.
#'   2. `row_id_unique` (ERROR): `row_id` has no duplicates.
#'   3. `row_id_format` (WARN): `row_id` matches `{YYYY-YYYY}_{site_code}`
#'      or `{YYYY-YYYY}_new_NNNN`.
#'   4. `school_year_canonical` (ERROR): values in the canonical 5-level
#'      set.
#'   5. `site_code_missingness_in_new_only` (ERROR): `site_code` NA rows
#'      must all carry `school_year == "*_new"`.
#'   6. `melissa_lat_lng_present` (ERROR): Melissa `LAT`/`LNG` are 100%
#'      non-NA.
#'   7. `has_latlon_consistency` (ERROR): `has_latlon == !is.na(latitude)`.
#'   8. `melissa_coord_in_al_bounds` (ERROR): non-NA Melissa coords inside
#'      AL bounding box.
#'   9. `adece_coord_in_al_bounds` (WARN): non-NA ADECE coords inside AL
#'      bounding box.
#'   10. `resultcode_canonical` (WARN): RESULTCODE in `{GS01..GS08}`.
#'   11. `statuscode_canonical` (WARN): STATUSCODE in observed codebook.
#'   12. `resultcode_statuscode_consistency` (WARN): 1:1 pairings observed
#'       in the STATUSCODE codebook.
#'   13. `errorcode_all_na_in_v080` (INFO): all `ERRORCODE` values are NA
#'       (v0.8.0 contract; future deliveries may populate).
#'   14. `provenance_complete` (ERROR): `meta` carries `file_sha256`,
#'       `cycle_year`, `receipt_date`, `git_sha`.
#'   15. `lineage_id_complete` (ERROR): row-level `lineage_id` exists,
#'       is non-blank, and is unique.
#'
#'   Plus a final `summary_coverage` (INFO) check reporting RESULTCODE
#'   coverage %, follow-up queue size estimate, and PLACENAME missingness.
#'
#' @param clean An `alprek_geocode_clean` object from [geocode_clean()].
#' @param strict Logical. If `TRUE`, treats warnings as overall failure.
#'   Default `FALSE`.
#' @param config Optional `alprek_geocode_config` (from [geocode_config()]).
#'   When provided, supplies `al_lat_bounds` / `al_lng_bounds`. RESULTCODE
#'   canonicality remains fixed to the documented Melissa set `{GS01..GS08}`;
#'   master acceptability is enforced later from the RESULTCODE codebook.
#'
#' @return An `alprek_geocode_validation` S3 list with elements:
#'   * `passed`: logical (overall result).
#'   * `n_errors`, `n_warnings`, `n_info`: integer counts.
#'   * `checks`: tibble with columns `check_id`, `description`, `status`,
#'     `n_issues`, `details`.
#'   * `issues`: tibble with columns `row_id`, `check_id`, `severity`,
#'     `value`, `expected`, `note`.
#'
#' @examples
#' \dontrun{
#' raw <- geocode_read(path, cycle_year = "2026-2027",
#'                     receipt_date = "2026-03-04")
#' clean <- geocode_clean(raw)
#' v <- geocode_validate(clean)
#' print(v)
#' v$checks
#' v$issues
#' }
#'
#' @seealso [geocode_clean()], [geocode_config()],
#'   [alprek_geocode_al_fips_counties()],
#'   [alprek_geocode_resultcode_meaning()].
#'
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble as_tibble
#' @export
geocode_validate <- function(clean, strict = FALSE, config = NULL) {

  # ---- argument validation ----
  if (!inherits(clean, "alprek_geocode_clean")) {
    stop("clean must be an alprek_geocode_clean object ",
         "(from geocode_clean()).", call. = FALSE)
  }
  if (!is.logical(strict) || length(strict) != 1L || is.na(strict)) {
    stop("strict must be a single TRUE/FALSE.", call. = FALSE)
  }
  if (!is.null(config) && !inherits(config, "alprek_geocode_config")) {
    stop("config must be NULL or an alprek_geocode_config object ",
         "(from geocode_config()).", call. = FALSE)
  }

  data <- clean$data
  meta <- clean$meta

  # ---- defaults (override from config if provided) ----
  al_lat_bounds <- if (!is.null(config)) config$al_lat_bounds else c(30, 36)
  al_lng_bounds <- if (!is.null(config)) config$al_lng_bounds else c(-89, -84)
  # Canonicality is vendor-codebook scope, not a user-configurable master
  # acceptability policy. Unknown future values should still warn even if a
  # caller passes them through config$acceptable_resultcodes.
  canonical_resultcodes <- c("GS01", "GS02", "GS03", "GS04",
                             "GS05", "GS06", "GS07", "GS08")

  canonical_school_years <- c("2021-2022", "2022-2023", "2023-2024",
                              "2024-2025", "2025-2026_new")

  # RESULTCODE <-> STATUSCODE pairing observed in v0.8.0, derived from
  # melissa_statuscode_codes.csv rather than hardcoded in the validator.
  canonical_rc_sc <- .gv_statuscode_pair_map()

  # accumulator
  acc <- new.env(parent = emptyenv())
  acc$checks <- list()
  acc$issues <- list()

  # ---- 1. required_columns (ERROR) ----
  .gv_check_required_columns(data, acc)

  # ---- 2. row_id_unique (ERROR) ----
  .gv_check_row_id_unique(data, acc)

  # ---- 3. row_id_format (WARN) ----
  .gv_check_row_id_format(data, acc)

  # ---- 4. school_year_canonical (ERROR) ----
  .gv_check_school_year_canonical(data, canonical_school_years, acc)

  # ---- 5. site_code_missingness_in_new_only (ERROR) ----
  .gv_check_site_code_missingness_in_new_only(data, acc)

  # ---- 6. melissa_lat_lng_present (ERROR) ----
  .gv_check_melissa_lat_lng_present(data, acc)

  # ---- 7. has_latlon_consistency (ERROR) ----
  .gv_check_has_latlon_consistency(data, acc)

  # ---- 8. melissa_coord_in_al_bounds (ERROR) ----
  .gv_check_melissa_coord_in_al_bounds(data, al_lat_bounds,
                                        al_lng_bounds, acc)

  # ---- 9. adece_coord_in_al_bounds (WARN) ----
  .gv_check_adece_coord_in_al_bounds(data, al_lat_bounds,
                                       al_lng_bounds, acc)

  # ---- 10. resultcode_canonical (WARN) ----
  .gv_check_resultcode_canonical(data, canonical_resultcodes, acc)

  # ---- 11. statuscode_canonical (WARN) ----
  .gv_check_statuscode_canonical(data, acc)

  # ---- 12. resultcode_statuscode_consistency (WARN) ----
  .gv_check_resultcode_statuscode_consistency(data, canonical_rc_sc, acc)

  # ---- 13. errorcode_all_na_in_v080 (INFO) ----
  .gv_check_errorcode_all_na_in_v080(data, acc)

  # ---- 14. provenance_complete (ERROR) ----
  .gv_check_provenance_complete(meta, acc)

  # ---- 15. lineage_id_complete (ERROR) ----
  .gv_check_lineage_id_complete(data, acc)

  # ---- final summary (INFO) ----
  .gv_check_summary_coverage(data, acc)

  # ---- assemble result ----
  .gv_assemble_result(acc$checks, acc$issues, strict = strict)
}


#' Print method for `alprek_geocode_validation`
#'
#' @param x An `alprek_geocode_validation` object.
#' @param ... Ignored.
#' @export
print.alprek_geocode_validation <- function(x, ...) {
  cat("<alprek_geocode_validation>\n")
  cat("  Overall: ", if (x$passed) "PASSED" else "FAILED", "\n", sep = "")
  cat("  Errors: ", x$n_errors, " | Warnings: ", x$n_warnings,
      " | Info: ", x$n_info, "\n", sep = "")
  if (nrow(x$checks) > 0L) {
    cat("\n  Checks:\n")
    for (i in seq_len(nrow(x$checks))) {
      row <- x$checks[i, ]
      icon <- switch(row$status,
                     PASS  = "+",
                     ERROR = "x",
                     WARN  = "!",
                     INFO  = "i",
                     "?")
      cat("    [", icon, "] ", row$check_id, " -- ", row$description,
          sep = "")
      if (!is.na(row$details) && nzchar(row$details)) {
        cat(" -- ", row$details, sep = "")
      }
      cat("\n")
    }
  }
  if (!is.null(x$issues) && nrow(x$issues) > 0L) {
    cat("\n  Issues: ", nrow(x$issues), " row(s) flagged across ",
        length(unique(x$issues$check_id)), " check(s)\n", sep = "")
  }
  invisible(x)
}


# ============================================================================
# Atomic check helpers (internal)
# ============================================================================

#' @keywords internal
#' @noRd
.gv_add_check <- function(acc, check_id, description, status, n_issues,
                          details) {
  acc$checks[[length(acc$checks) + 1L]] <- tibble::tibble(
    check_id    = as.character(check_id),
    description = as.character(description),
    status      = as.character(status),
    n_issues    = as.integer(n_issues),
    details     = if (is.null(details) ||
                        (length(details) == 1L && is.na(details)) ||
                        identical(details, ""))
                    NA_character_ else as.character(details)
  )
  invisible(NULL)
}


#' @keywords internal
#' @noRd
.gv_add_issues <- function(acc, row_ids, check_id, severity,
                           value = NA_character_,
                           expected = NA_character_,
                           note = NA_character_) {
  if (length(row_ids) == 0L) return(invisible(NULL))
  n <- length(row_ids)
  .vec_or_na <- function(x, n) {
    if (length(x) == 1L) rep(x, n) else x
  }
  acc$issues[[length(acc$issues) + 1L]] <- tibble::tibble(
    row_id   = as.character(row_ids),
    check_id = rep(as.character(check_id), n),
    severity = rep(as.character(severity), n),
    value    = as.character(.vec_or_na(value, n)),
    expected = as.character(.vec_or_na(expected, n)),
    note     = as.character(.vec_or_na(note, n))
  )
  invisible(NULL)
}


# ----------------------------------------------------------------------------
# 1. required_columns (ERROR)
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.gv_required_columns <- function() {
  c("row_id", "school_year", "site_name", "site_code", "geocode_address",
    "site_street", "site_city", "site_state", "site_zip",
    "latitude", "longitude", "has_latlon",
    "md_street", "md_city", "md_state", "GEOZIP", "PLUS4", "DPB",
    "LAT", "LNG", "CT", "CENSUSBLOC", "FIPS", "COUNTYNAME",
    "PLACENAME", "PLACECODE", "RESULTCODE", "STATUSCODE", "ERRORCODE")
}

#' @keywords internal
#' @noRd
.gv_check_required_columns <- function(data, acc) {
  required <- .gv_required_columns()
  missing_cols <- setdiff(required, names(data))
  status <- if (length(missing_cols) == 0L) "PASS" else "ERROR"
  details <- if (length(missing_cols) > 0L) {
    paste("Missing:", paste(missing_cols, collapse = ", "))
  } else {
    sprintf("All %d expected columns present", length(required))
  }
  .gv_add_check(acc, "required_columns",
                "All 29 expected columns present",
                status, length(missing_cols), details)
}


# ----------------------------------------------------------------------------
# 2. row_id_unique (ERROR)
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.gv_check_row_id_unique <- function(data, acc) {
  if (!"row_id" %in% names(data)) {
    .gv_add_check(acc, "row_id_unique",
                  "row_id is unique across all rows",
                  "ERROR", nrow(data), "row_id column missing")
    return(invisible(NULL))
  }
  rid <- as.character(data$row_id)
  # any duplicate (both copies) is an issue
  dup_mask <- duplicated(rid) | duplicated(rid, fromLast = TRUE)
  n_dups <- sum(dup_mask, na.rm = TRUE)
  status <- if (n_dups == 0L) "PASS" else "ERROR"
  details <- if (n_dups > 0L) {
    dup_vals <- unique(rid[dup_mask])
    sprintf("%d row(s) involved in duplicate row_id (%d distinct value(s))",
            n_dups, length(dup_vals))
  } else {
    sprintf("%d unique row_id values", length(unique(rid)))
  }
  .gv_add_check(acc, "row_id_unique",
                "row_id is unique across all rows",
                status, n_dups, details)
  if (n_dups > 0L) {
    .gv_add_issues(acc, rid[dup_mask], "row_id_unique",
                   "ERROR",
                   value = rid[dup_mask],
                   expected = "unique",
                   note = "duplicate row_id")
  }
}


# ----------------------------------------------------------------------------
# 3. row_id_format (WARN)
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.gv_check_row_id_format <- function(data, acc) {
  if (!"row_id" %in% names(data)) {
    .gv_add_check(acc, "row_id_format",
                  "row_id matches {YYYY-YYYY}_{site_code} or *_new_NNNN",
                  "WARN", nrow(data), "row_id column missing")
    return(invisible(NULL))
  }
  rid <- as.character(data$row_id)
  # Accepted patterns:
  #  - "{YYYY-YYYY}_{site_code}" where site_code is [A-Z0-9]+
  #  - "{YYYY-YYYY}_new_{NNNN}"  where NNNN is exactly 4 digits
  pat_renewal <- "^\\d{4}-\\d{4}_[A-Z0-9]+$"
  pat_new     <- "^\\d{4}-\\d{4}_new_\\d{4}$"
  nonblank <- !is.na(rid) & nzchar(rid)
  ok <- nonblank & (grepl(pat_renewal, rid) | grepl(pat_new, rid))
  bad_mask <- nonblank & !ok
  n_bad <- sum(bad_mask, na.rm = TRUE)
  status <- if (n_bad == 0L) "PASS" else "WARN"
  details <- if (n_bad > 0L) {
    bad_vals <- unique(rid[bad_mask])
    sprintf("%d row(s) with non-canonical row_id; example(s): %s",
            n_bad,
            paste(sprintf("'%s'",
                          bad_vals[seq_len(min(3L, length(bad_vals)))]),
                  collapse = ", "))
  } else {
    "all row_id values match canonical pattern"
  }
  .gv_add_check(acc, "row_id_format",
                "row_id matches {YYYY-YYYY}_{site_code} or *_new_NNNN",
                status, n_bad, details)
  if (n_bad > 0L) {
    .gv_add_issues(acc, rid[bad_mask], "row_id_format",
                   "WARN",
                   value = rid[bad_mask],
                   expected = "^\\d{4}-\\d{4}_[A-Z0-9]+$ or *_new_NNNN",
                   note = "non-canonical row_id format")
  }
}


# ----------------------------------------------------------------------------
# 4. school_year_canonical (ERROR)
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.gv_check_school_year_canonical <- function(data, canonical, acc) {
  if (!"school_year" %in% names(data)) {
    .gv_add_check(acc, "school_year_canonical",
                  "school_year is in the canonical 5-level set",
                  "ERROR", nrow(data), "school_year column missing")
    return(invisible(NULL))
  }
  sy <- as.character(data$school_year)
  nonblank <- !is.na(sy) & nzchar(sy)
  bad_mask <- nonblank & !(sy %in% canonical)
  n_bad <- sum(bad_mask, na.rm = TRUE)
  status <- if (n_bad == 0L) "PASS" else "ERROR"
  details <- if (n_bad > 0L) {
    bad_vals <- unique(sy[bad_mask])
    sprintf("%d row(s) with non-canonical school_year; bad value(s): %s",
            n_bad,
            paste(sprintf("'%s'",
                          bad_vals[seq_len(min(3L, length(bad_vals)))]),
                  collapse = ", "))
  } else {
    sprintf("all values in {%s}", paste(canonical, collapse = ", "))
  }
  .gv_add_check(acc, "school_year_canonical",
                "school_year is in the canonical 5-level set",
                status, n_bad, details)
  if (n_bad > 0L) {
    rids <- if ("row_id" %in% names(data)) as.character(data$row_id) else
              as.character(seq_len(nrow(data)))
    .gv_add_issues(acc, rids[bad_mask], "school_year_canonical",
                   "ERROR",
                   value = sy[bad_mask],
                   expected = paste(canonical, collapse = "|"),
                   note = "school_year outside canonical set")
  }
}


# ----------------------------------------------------------------------------
# 5. site_code_missingness_in_new_only (ERROR)
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.gv_check_site_code_missingness_in_new_only <- function(data, acc) {
  if (!all(c("site_code", "school_year") %in% names(data))) {
    .gv_add_check(acc, "site_code_missingness_in_new_only",
                  "site_code NA only when school_year ends in '_new'",
                  "ERROR", nrow(data),
                  "site_code or school_year column missing")
    return(invisible(NULL))
  }
  sc <- as.character(data$site_code)
  sy <- as.character(data$school_year)
  sc_na <- is.na(sc) | !nzchar(trimws(sc))
  sy_is_new <- !is.na(sy) & grepl("_new$", sy)
  # bad: site_code NA but school_year is not *_new
  bad_mask <- sc_na & !sy_is_new
  n_bad <- sum(bad_mask, na.rm = TRUE)
  status <- if (n_bad == 0L) "PASS" else "ERROR"
  details <- if (n_bad > 0L) {
    sprintf("%d row(s) with NA site_code but non-_new school_year", n_bad)
  } else {
    n_new <- sum(sc_na & sy_is_new, na.rm = TRUE)
    if (n_new > 0L) {
      sprintf("%d _new row(s) with NA site_code (allowed by contract)",
              n_new)
    } else {
      "no site_code NA"
    }
  }
  .gv_add_check(acc, "site_code_missingness_in_new_only",
                "site_code NA only when school_year ends in '_new'",
                status, n_bad, details)
  if (n_bad > 0L) {
    rids <- if ("row_id" %in% names(data)) as.character(data$row_id) else
              as.character(seq_len(nrow(data)))
    .gv_add_issues(acc, rids[bad_mask], "site_code_missingness_in_new_only",
                   "ERROR",
                   value = sy[bad_mask],
                   expected = "school_year ending in _new",
                   note = "NA site_code outside _new cohort")
  }
}


# ----------------------------------------------------------------------------
# 6. melissa_lat_lng_present (ERROR)
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.gv_check_melissa_lat_lng_present <- function(data, acc) {
  if (!all(c("LAT", "LNG") %in% names(data))) {
    .gv_add_check(acc, "melissa_lat_lng_present",
                  "Melissa LAT and LNG are 100% non-NA",
                  "ERROR", nrow(data), "LAT or LNG column missing")
    return(invisible(NULL))
  }
  lat <- data$LAT
  lng <- data$LNG
  bad_mask <- is.na(lat) | is.na(lng)
  n_bad <- sum(bad_mask, na.rm = TRUE)
  status <- if (n_bad == 0L) "PASS" else "ERROR"
  details <- if (n_bad > 0L) {
    sprintf("%d row(s) with NA Melissa LAT or LNG", n_bad)
  } else {
    "all rows have non-NA Melissa LAT/LNG"
  }
  .gv_add_check(acc, "melissa_lat_lng_present",
                "Melissa LAT and LNG are 100% non-NA",
                status, n_bad, details)
  if (n_bad > 0L) {
    rids <- if ("row_id" %in% names(data)) as.character(data$row_id) else
              as.character(seq_len(nrow(data)))
    .gv_add_issues(acc, rids[bad_mask], "melissa_lat_lng_present",
                   "ERROR",
                   value = ifelse(is.na(lat[bad_mask]),
                                  "LAT=NA",
                                  ifelse(is.na(lng[bad_mask]),
                                         "LNG=NA",
                                         "NA")),
                   expected = "non-NA",
                   note = "Melissa LAT/LNG missing")
  }
}


# ----------------------------------------------------------------------------
# 7. has_latlon_consistency (ERROR)
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.gv_check_has_latlon_consistency <- function(data, acc) {
  if (!all(c("has_latlon", "latitude") %in% names(data))) {
    .gv_add_check(acc, "has_latlon_consistency",
                  "has_latlon == !is.na(latitude)",
                  "ERROR", nrow(data),
                  "has_latlon or latitude column missing")
    return(invisible(NULL))
  }
  hll <- data$has_latlon
  expected <- !is.na(data$latitude)
  # both must be defined; NA in has_latlon counts as inconsistent.
  bad_mask <- is.na(hll) | (hll != expected)
  n_bad <- sum(bad_mask, na.rm = TRUE)
  status <- if (n_bad == 0L) "PASS" else "ERROR"
  details <- if (n_bad > 0L) {
    sprintf("%d row(s) where has_latlon != !is.na(latitude)", n_bad)
  } else {
    "has_latlon agrees with !is.na(latitude) on all rows"
  }
  .gv_add_check(acc, "has_latlon_consistency",
                "has_latlon == !is.na(latitude)",
                status, n_bad, details)
  if (n_bad > 0L) {
    rids <- if ("row_id" %in% names(data)) as.character(data$row_id) else
              as.character(seq_len(nrow(data)))
    .gv_add_issues(acc, rids[bad_mask], "has_latlon_consistency",
                   "ERROR",
                   value = sprintf("has_latlon=%s, lat_is_na=%s",
                                   as.character(hll[bad_mask]),
                                   as.character(!expected[bad_mask])),
                   expected = "has_latlon == !is.na(latitude)",
                   note = "logical-consistency violation")
  }
}


# ----------------------------------------------------------------------------
# 8. melissa_coord_in_al_bounds (ERROR)
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.gv_check_melissa_coord_in_al_bounds <- function(data, lat_bounds,
                                                  lng_bounds, acc) {
  if (!all(c("LAT", "LNG") %in% names(data))) {
    .gv_add_check(acc, "melissa_coord_in_al_bounds",
                  sprintf("Melissa coords in AL bounds [%g,%g] x [%g,%g]",
                          lat_bounds[1], lat_bounds[2],
                          lng_bounds[1], lng_bounds[2]),
                  "ERROR", nrow(data), "LAT or LNG column missing")
    return(invisible(NULL))
  }
  lat <- suppressWarnings(as.numeric(data$LAT))
  lng <- suppressWarnings(as.numeric(data$LNG))
  nonblank <- !is.na(lat) & !is.na(lng)
  bad_mask <- nonblank &
    (lat < lat_bounds[1] | lat > lat_bounds[2] |
       lng < lng_bounds[1] | lng > lng_bounds[2])
  n_bad <- sum(bad_mask, na.rm = TRUE)
  status <- if (n_bad == 0L) "PASS" else "ERROR"
  details <- if (n_bad > 0L) {
    sprintf("%d row(s) with Melissa coord outside AL bounds", n_bad)
  } else {
    sprintf("all %d non-NA Melissa coord(s) within AL bounds",
            sum(nonblank, na.rm = TRUE))
  }
  .gv_add_check(acc, "melissa_coord_in_al_bounds",
                sprintf("Melissa coords in AL bounds [%g,%g] x [%g,%g]",
                        lat_bounds[1], lat_bounds[2],
                        lng_bounds[1], lng_bounds[2]),
                status, n_bad, details)
  if (n_bad > 0L) {
    rids <- if ("row_id" %in% names(data)) as.character(data$row_id) else
              as.character(seq_len(nrow(data)))
    .gv_add_issues(acc, rids[bad_mask], "melissa_coord_in_al_bounds",
                   "ERROR",
                   value = sprintf("(%g, %g)", lat[bad_mask],
                                   lng[bad_mask]),
                   expected = sprintf("[%g,%g] x [%g,%g]",
                                      lat_bounds[1], lat_bounds[2],
                                      lng_bounds[1], lng_bounds[2]),
                   note = "Melissa coord outside AL")
  }
}


# ----------------------------------------------------------------------------
# 9. adece_coord_in_al_bounds (WARN)
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.gv_check_adece_coord_in_al_bounds <- function(data, lat_bounds,
                                                 lng_bounds, acc) {
  if (!all(c("latitude", "longitude") %in% names(data))) {
    .gv_add_check(acc, "adece_coord_in_al_bounds",
                  sprintf("ADECE coords in AL bounds [%g,%g] x [%g,%g]",
                          lat_bounds[1], lat_bounds[2],
                          lng_bounds[1], lng_bounds[2]),
                  "WARN", nrow(data),
                  "latitude or longitude column missing")
    return(invisible(NULL))
  }
  lat <- data$latitude
  lng <- data$longitude
  nonblank <- !is.na(lat) & !is.na(lng)
  bad_mask <- nonblank &
    (lat < lat_bounds[1] | lat > lat_bounds[2] |
       lng < lng_bounds[1] | lng > lng_bounds[2])
  n_bad <- sum(bad_mask, na.rm = TRUE)
  status <- if (n_bad == 0L) "PASS" else "WARN"
  details <- if (n_bad > 0L) {
    sprintf("%d row(s) with ADECE coord outside AL bounds", n_bad)
  } else {
    sprintf("all %d non-NA ADECE coord(s) within AL bounds",
            sum(nonblank, na.rm = TRUE))
  }
  .gv_add_check(acc, "adece_coord_in_al_bounds",
                sprintf("ADECE coords in AL bounds [%g,%g] x [%g,%g]",
                        lat_bounds[1], lat_bounds[2],
                        lng_bounds[1], lng_bounds[2]),
                status, n_bad, details)
  if (n_bad > 0L) {
    rids <- if ("row_id" %in% names(data)) as.character(data$row_id) else
              as.character(seq_len(nrow(data)))
    .gv_add_issues(acc, rids[bad_mask], "adece_coord_in_al_bounds",
                   "WARN",
                   value = sprintf("(%g, %g)", lat[bad_mask],
                                   lng[bad_mask]),
                   expected = sprintf("[%g,%g] x [%g,%g]",
                                      lat_bounds[1], lat_bounds[2],
                                      lng_bounds[1], lng_bounds[2]),
                   note = "ADECE coord outside AL")
  }
}


# ----------------------------------------------------------------------------
# 10. resultcode_canonical (WARN)
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.gv_check_resultcode_canonical <- function(data, canonical, acc) {
  if (!"RESULTCODE" %in% names(data)) {
    .gv_add_check(acc, "resultcode_canonical",
                  "RESULTCODE is in the documented Melissa set {GS01..GS08}",
                  "WARN", nrow(data), "RESULTCODE column missing")
    return(invisible(NULL))
  }
  rc <- as.character(data$RESULTCODE)
  nonblank <- !is.na(rc) & nzchar(rc)
  bad_mask <- nonblank & !(rc %in% canonical)
  n_bad <- sum(bad_mask, na.rm = TRUE)
  status <- if (n_bad == 0L) "PASS" else "WARN"
  details <- if (n_bad > 0L) {
    bad_vals <- unique(rc[bad_mask])
    sprintf("%d row(s) with non-canonical RESULTCODE; value(s): %s",
            n_bad,
            paste(sprintf("'%s'",
                          bad_vals[seq_len(min(3L, length(bad_vals)))]),
                  collapse = ", "))
  } else {
    sprintf("all RESULTCODE values in {%s}",
            paste(canonical, collapse = ", "))
  }
  .gv_add_check(acc, "resultcode_canonical",
                "RESULTCODE is in the documented Melissa set {GS01..GS08}",
                status, n_bad, details)
  if (n_bad > 0L) {
    rids <- if ("row_id" %in% names(data)) as.character(data$row_id) else
              as.character(seq_len(nrow(data)))
    .gv_add_issues(acc, rids[bad_mask], "resultcode_canonical",
                   "WARN",
                   value = rc[bad_mask],
                   expected = paste(canonical, collapse = "|"),
                   note = "RESULTCODE outside Melissa wiki set")
  }
}


# ----------------------------------------------------------------------------
# 11. statuscode_canonical (WARN)
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.gv_check_statuscode_canonical <- function(data, acc) {
  if (!"STATUSCODE" %in% names(data)) {
    .gv_add_check(acc, "statuscode_canonical",
                  "STATUSCODE in observed codebook set",
                  "WARN", nrow(data), "STATUSCODE column missing")
    return(invisible(NULL))
  }
  # canonical set derived from STATUSCODE codebook (v0.8.0 observed)
  canonical_sc <- tryCatch({
    sc <- alprek_geocode_statuscode_meaning()
    as.character(sc$code)
  }, error = function(e) c("9", "5", "A", "B"))

  sc <- as.character(data$STATUSCODE)
  nonblank <- !is.na(sc) & nzchar(sc)
  bad_mask <- nonblank & !(sc %in% canonical_sc)
  n_bad <- sum(bad_mask, na.rm = TRUE)
  status <- if (n_bad == 0L) "PASS" else "WARN"
  details <- if (n_bad > 0L) {
    bad_vals <- unique(sc[bad_mask])
    sprintf("%d row(s) with non-canonical STATUSCODE; value(s): %s",
            n_bad,
            paste(sprintf("'%s'",
                          bad_vals[seq_len(min(3L, length(bad_vals)))]),
                  collapse = ", "))
  } else {
    sprintf("all STATUSCODE values in {%s}",
            paste(canonical_sc, collapse = ", "))
  }
  .gv_add_check(acc, "statuscode_canonical",
                "STATUSCODE in observed codebook set",
                status, n_bad, details)
  if (n_bad > 0L) {
    rids <- if ("row_id" %in% names(data)) as.character(data$row_id) else
              as.character(seq_len(nrow(data)))
    .gv_add_issues(acc, rids[bad_mask], "statuscode_canonical",
                   "WARN",
                   value = sc[bad_mask],
                   expected = paste(canonical_sc, collapse = "|"),
                   note = "STATUSCODE outside observed codebook")
  }
}


# ----------------------------------------------------------------------------
# 12. resultcode_statuscode_consistency (WARN)
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.gv_statuscode_pair_map <- function() {
  sc <- tryCatch(alprek_geocode_statuscode_meaning(),
                 error = function(e) NULL)
  if (is.null(sc) || !all(c("code", "paired_resultcode_in_v080") %in% names(sc))) {
    return(stats::setNames(character(0), character(0)))
  }

  rc <- as.character(sc$paired_resultcode_in_v080)
  code <- as.character(sc$code)
  keep <- !is.na(rc) & nzchar(rc) & !is.na(code) & nzchar(code)
  stats::setNames(code[keep], rc[keep])
}


#' @keywords internal
#' @noRd
.gv_check_resultcode_statuscode_consistency <- function(data, rc_sc_map, acc) {
  if (!all(c("RESULTCODE", "STATUSCODE") %in% names(data))) {
    .gv_add_check(acc, "resultcode_statuscode_consistency",
                  "RESULTCODE <-> STATUSCODE follows codebook 1:1 pairing",
                  "WARN", nrow(data),
                  "RESULTCODE or STATUSCODE column missing")
    return(invisible(NULL))
  }
  rc <- as.character(data$RESULTCODE)
  sc <- as.character(data$STATUSCODE)
  # We only enforce the consistency on rows where RESULTCODE is a known
  # value in the codebook-derived 1:1 mapping. Unknown RESULTCODEs are flagged
  # by check 10 separately.
  known_rc <- names(rc_sc_map)
  if (length(known_rc) == 0L) {
    .gv_add_check(acc, "resultcode_statuscode_consistency",
                  "RESULTCODE <-> STATUSCODE follows codebook 1:1 pairing",
                  "WARN", nrow(data),
                  "STATUSCODE codebook did not provide pairings")
    return(invisible(NULL))
  }
  enforce <- !is.na(rc) & rc %in% known_rc
  expected_sc <- unname(unlist(rc_sc_map)[rc[enforce]])
  observed_sc <- sc[enforce]
  mismatch <- !is.na(observed_sc) & observed_sc != expected_sc
  n_bad <- sum(mismatch, na.rm = TRUE)
  status <- if (n_bad == 0L) "PASS" else "WARN"
  details <- if (n_bad > 0L) {
    sprintf("%d row(s) violate canonical RESULTCODE<->STATUSCODE pairing",
            n_bad)
  } else {
    pairs_str <- paste(sprintf("%s<->%s",
                                names(rc_sc_map), unlist(rc_sc_map)),
                       collapse = ", ")
    sprintf("all known-RESULTCODE rows follow %s", pairs_str)
  }
  .gv_add_check(acc, "resultcode_statuscode_consistency",
                "RESULTCODE <-> STATUSCODE follows codebook 1:1 pairing",
                status, n_bad, details)
  if (n_bad > 0L) {
    enforce_idx <- which(enforce)
    bad_idx <- enforce_idx[mismatch]
    rids <- if ("row_id" %in% names(data)) as.character(data$row_id) else
              as.character(seq_len(nrow(data)))
    .gv_add_issues(acc, rids[bad_idx],
                   "resultcode_statuscode_consistency",
                   "WARN",
                   value = sprintf("RC=%s, SC=%s",
                                   rc[bad_idx], sc[bad_idx]),
                   expected = sprintf("RC=%s -> SC=%s",
                                      rc[bad_idx],
                                      unname(unlist(rc_sc_map))[
                                        match(rc[bad_idx],
                                              names(rc_sc_map))]),
                   note = "RESULTCODE / STATUSCODE pairing violation")
  }
}


# ----------------------------------------------------------------------------
# 13. errorcode_all_na_in_v080 (INFO)
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.gv_check_errorcode_all_na_in_v080 <- function(data, acc) {
  if (!"ERRORCODE" %in% names(data)) {
    .gv_add_check(acc, "errorcode_all_na_in_v080",
                  "ERRORCODE is 100% NA (v0.8.0 contract)",
                  "INFO", nrow(data), "ERRORCODE column missing")
    return(invisible(NULL))
  }
  ec <- data$ERRORCODE
  # Non-NA AND nonblank when character
  is_set <- if (is.character(ec)) {
    !is.na(ec) & nzchar(trimws(ec))
  } else {
    !is.na(ec)
  }
  n_set <- sum(is_set, na.rm = TRUE)
  # In v0.8.0, contract says all NA. If any are populated, that is INFO
  # (a future delivery introducing non-NA values is informational, not an
  # error per the contract layer).
  status <- if (n_set == 0L) "PASS" else "INFO"
  details <- if (n_set > 0L) {
    sprintf("%d row(s) have non-NA ERRORCODE (v0.8.0 expected 100%% NA)",
            n_set)
  } else {
    "ERRORCODE is 100% NA (matches v0.8.0 contract)"
  }
  .gv_add_check(acc, "errorcode_all_na_in_v080",
                "ERRORCODE is 100% NA (v0.8.0 contract)",
                status, n_set, details)
}


# ----------------------------------------------------------------------------
# 14. provenance_complete (ERROR)
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.gv_check_provenance_complete <- function(meta, acc) {
  required <- c("file_sha256", "cycle_year", "receipt_date", "git_sha")
  missing_keys <- character(0)
  for (k in required) {
    v <- meta[[k]]
    if (is.null(v) || length(v) == 0L ||
        (length(v) == 1L && is.na(v)) ||
        (is.character(v) && !nzchar(as.character(v)))) {
      missing_keys <- c(missing_keys, k)
    }
  }
  n_missing <- length(missing_keys)
  status <- if (n_missing == 0L) "PASS" else "ERROR"
  details <- if (n_missing > 0L) {
    paste("Missing meta keys:", paste(missing_keys, collapse = ", "))
  } else {
    "meta has all provenance keys (file_sha256, cycle_year, receipt_date, git_sha)"
  }
  .gv_add_check(acc, "provenance_complete",
                "meta carries file_sha256, cycle_year, receipt_date, git_sha",
                status, n_missing, details)
}


# ----------------------------------------------------------------------------
# 15. lineage_id_complete (ERROR)
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.gv_check_lineage_id_complete <- function(data, acc) {
  if (!"lineage_id" %in% names(data)) {
    .gv_add_check(acc, "lineage_id_complete",
                  "lineage_id exists, is non-blank, and is unique",
                  "ERROR", nrow(data), "lineage_id column missing")
    rids <- if ("row_id" %in% names(data)) as.character(data$row_id) else
              as.character(seq_len(nrow(data)))
    .gv_add_issues(acc, rids, "lineage_id_complete", "ERROR",
                   value = NA_character_, expected = "nonblank unique lineage_id",
                   note = "lineage_id column missing")
    return(invisible(NULL))
  }

  lineage <- as.character(data$lineage_id)
  missing_mask <- is.na(lineage) | !nzchar(trimws(lineage))
  dup_mask <- !missing_mask & (duplicated(lineage) |
                                duplicated(lineage, fromLast = TRUE))
  bad_mask <- missing_mask | dup_mask
  n_bad <- sum(bad_mask, na.rm = TRUE)

  status <- if (n_bad == 0L) "PASS" else "ERROR"
  details <- if (n_bad > 0L) {
    sprintf("%d row(s) with missing/blank or duplicated lineage_id", n_bad)
  } else {
    "lineage_id present, non-blank, and unique for every row"
  }

  .gv_add_check(acc, "lineage_id_complete",
                "lineage_id exists, is non-blank, and is unique",
                status, n_bad, details)

  if (n_bad > 0L) {
    rids <- if ("row_id" %in% names(data)) as.character(data$row_id) else
              as.character(seq_len(nrow(data)))
    note <- ifelse(missing_mask[bad_mask], "missing lineage_id",
                   "duplicated lineage_id")
    .gv_add_issues(acc, rids[bad_mask], "lineage_id_complete", "ERROR",
                   value = lineage[bad_mask],
                   expected = "nonblank unique lineage_id",
                   note = note)
  }
}


# ----------------------------------------------------------------------------
# Summary INFO check: coverage %, followup queue estimate, PLACENAME NA
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.gv_check_summary_coverage <- function(data, acc) {
  parts <- character(0)
  n_total <- nrow(data)

  if ("RESULTCODE" %in% names(data) && n_total > 0L) {
    rc_tab <- table(data$RESULTCODE, useNA = "ifany")
    rc_share <- prop.table(rc_tab) * 100
    rc_str <- paste(sprintf("%s=%.1f%%",
                            names(rc_share), as.numeric(rc_share)),
                    collapse = ", ")
    parts <- c(parts, sprintf("RESULTCODE coverage: %s", rc_str))
  }

  # Follow-up queue estimate: rows that look risky for downstream
  # reconciliation: ADECE coord missing, or non-canonical RESULTCODE,
  # or RESULTCODE GS03/GS06 (low-precision), or LAT/LNG missing.
  fu_count <- 0L
  if (n_total > 0L) {
    adece_missing <- if ("latitude" %in% names(data))
                       is.na(data$latitude) else rep(FALSE, n_total)
    rc_low <- if ("RESULTCODE" %in% names(data))
                as.character(data$RESULTCODE) %in% c("GS03", "GS06") else
                  rep(FALSE, n_total)
    melissa_missing <- if (all(c("LAT", "LNG") %in% names(data)))
                          is.na(data$LAT) | is.na(data$LNG) else
                            rep(FALSE, n_total)
    fu_count <- sum(adece_missing | rc_low | melissa_missing, na.rm = TRUE)
    parts <- c(parts,
               sprintf("follow-up queue estimate: %d row(s) (~%.1f%%)",
                       fu_count,
                       100 * fu_count / max(1L, n_total)))
  }

  if ("PLACENAME" %in% names(data)) {
    place <- as.character(data$PLACENAME)
    n_place_na <- sum(is.na(place) | !nzchar(trimws(place)), na.rm = TRUE)
    parts <- c(parts,
               sprintf("PLACENAME missingness: %d row(s) (~%.1f%%)",
                       n_place_na,
                       100 * n_place_na / max(1L, n_total)))
  }

  details <- if (length(parts) > 0L) {
    paste(parts, collapse = "; ")
  } else {
    "no summary statistics available"
  }
  .gv_add_check(acc, "summary_coverage",
                "Summary coverage and follow-up queue estimate",
                "INFO", as.integer(fu_count), details)
}


# ----------------------------------------------------------------------------
# Assemble result
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.gv_assemble_result <- function(checks, issues, strict) {
  checks_df <- if (length(checks) > 0L) {
    dplyr::bind_rows(checks)
  } else {
    tibble::tibble(check_id = character(0),
                   description = character(0),
                   status = character(0),
                   n_issues = integer(0),
                   details = character(0))
  }

  issues_df <- if (length(issues) > 0L) {
    dplyr::bind_rows(issues)
  } else {
    tibble::tibble(row_id = character(0),
                   check_id = character(0),
                   severity = character(0),
                   value = character(0),
                   expected = character(0),
                   note = character(0))
  }

  n_errors   <- sum(checks_df$status == "ERROR")
  n_warnings <- sum(checks_df$status == "WARN")
  n_info     <- sum(checks_df$status == "INFO")

  passed <- if (isTRUE(strict)) {
    n_errors == 0L && n_warnings == 0L
  } else {
    n_errors == 0L
  }

  structure(list(
    passed     = passed,
    n_errors   = as.integer(n_errors),
    n_warnings = as.integer(n_warnings),
    n_info     = as.integer(n_info),
    checks     = checks_df,
    issues     = issues_df
  ), class = "alprek_geocode_validation")
}
