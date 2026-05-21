#' Reconcile ADECE and Melissa Geocoded Coordinates (Step 4.3)
#'
#' @description Applies the LOCKED 15-cell decision matrix from Step 4.2 to a
#'   cleaned Melissa-returned geocoded dataset. For each site row, picks an
#'   authoritative (lat, lng) pair, records the precision tier, computes the
#'   ADECE<->Melissa distance (when both sources are present), assigns a
#'   coordinate-agreement band, and flags rows that need analyst followup.
#'   Every decision is logged in `reconciliation_log` with the matrix cell ID
#'   so downstream consumers (Step 4.4 followup queue, Step 4.5 sanity tests)
#'   can audit any single-row decision without re-running the reconciler.
#'
#'   Decision-matrix scope (Step 4.2 LOCKED):
#'   * **D1-D9** -- both sources present; outcome driven by RESULTCODE
#'     (`GS01`/`GS05`/`GS06`), per-tier threshold, and gross-outlier check
#'     (>=10 km).
#'   * **D10** -- both sources present, RESULTCODE == `GS03`, any distance:
#'     `disputed_melissa` (centroid is unreliable; flag for followup).
#'   * **D11** -- ADECE only (Melissa unexpectedly missing): use ADECE, flag.
#'   * **D12** -- Melissa only (`GS01`/`GS05`): use Melissa, OK (Melissa is
#'     the authoritative geocode source for these rows).
#'   * **D13** -- Melissa only (`GS06`, interpolated rooftop): use Melissa,
#'     flag for analyst review.
#'   * **D14** -- Melissa only (`GS03`, ZIP centroid): use Melissa, flag.
#'   * **D15** -- both missing: no coordinate; flag with `both_missing`.
#'
#'   The matrix can be inverted at the priority level via
#'   `config$authoritative_priority == "adece_first"`, in which case D1-D9
#'   resolve to ADECE-anchored decisions (still flagged when distances exceed
#'   the per-tier threshold).
#'
#'   Tier thresholds come from `config$tiered_thresholds` (default
#'   `list(GS01 = 50, GS05 = 250, GS06 = 500, GS03 = Inf)`). The reconciler
#'   never hardcodes RESULTCODE -> precision_tier; it looks them up from
#'   [alprek_geocode_resultcode_meaning()] so future codebook updates flow
#'   through without code changes. `acceptable_for_master` in that same
#'   codebook is also enforced: unacceptable or unknown RESULTCODE values are
#'   retained for analyst review but flagged and marked `not_model_ready`.
#'
#' @param clean An `alprek_geocode_clean` object from [geocode_clean()].
#'   Must contain `latitude`, `longitude` (ADECE), `LAT`, `LNG` (Melissa),
#'   `RESULTCODE`, and an identifier column (`row_id` preferred,
#'   `raw_row_index` as fallback).
#' @param config Optional `alprek_geocode_config` (from [geocode_config()]).
#'   When `NULL`, the reconciler uses the default LOCKED matrix:
#'   `authoritative_priority = "melissa_first"`,
#'   `distance_threshold_rules = "by_resultcode"`,
#'   `tiered_thresholds = list(GS01 = 50, GS05 = 250, GS06 = 500, GS03 = Inf)`.
#'
#' @return An `alprek_geocode_reconciled` S3 list with elements:
#'   * `data`: tibble (1 row per input row) with all original columns plus 10
#'     new authoritative columns:
#'       - `lat_final`, `lng_final` (numeric or NA)
#'       - `lat_source` (factor:
#'         `{melissa, adece, disputed_melissa, none}`)
#'       - `lat_precision` (ordered factor:
#'         `{none, unknown, centroid, zip5, zip4, area, parcel, rooftop}`,
#'         increasing precision)
#'       - `distance_adece_melissa_m` (numeric or NA)
#'       - `coord_agreement_band` (factor:
#'         `{exact, tight, loose, drift, gross, one_source_only, none}`)
#'       - `needs_followup_geocoding` (logical)
#'       - `followup_reason` (factor with controlled vocabulary)
#'       - `coord_model_status` (ordered factor:
#'         `{missing, not_model_ready, provisional_followup, model_ready}`)
#'       - `geocode_provenance` (compact character string)
#'   * `reconciliation_log`: tibble (1 row per input row) of per-row audit
#'     records (`row_id`, `matrix_cell`, `decision_source`, etc.).
#'   * `summary`: tibble of decision-cell counts.
#'   * `meta`: list (`reconciled_at`, `authoritative_priority`,
#'     `distance_threshold_rules`, `tiered_thresholds`, `n_rows`,
#'     `n_needs_followup`, `n_disputed`, `n_lat_source_*`, `git_sha`,
#'     inherited from `clean$meta`).
#'
#' @section Decision matrix (15 cells):
#' \tabular{llllll}{
#'   Cell \tab adece \tab melissa \tab RESULTCODE \tab dist vs tier \tab outcome \cr
#'   D1   \tab Y     \tab Y       \tab GS01       \tab <=50m         \tab melissa, zip4, no followup \cr
#'   D2   \tab Y     \tab Y       \tab GS01       \tab 50m..10km     \tab melissa, zip4, flag (disagreement_above_threshold) \cr
#'   D3   \tab Y     \tab Y       \tab GS01       \tab >=10km        \tab disputed_melissa, zip4, flag (disagreement_gross) \cr
#'   D4   \tab Y     \tab Y       \tab GS05       \tab <=250m        \tab melissa, rooftop, no followup \cr
#'   D5   \tab Y     \tab Y       \tab GS05       \tab 250m..10km    \tab melissa, rooftop, flag \cr
#'   D6   \tab Y     \tab Y       \tab GS05       \tab >=10km        \tab disputed_melissa, rooftop, flag (gross) \cr
#'   D7   \tab Y     \tab Y       \tab GS06       \tab <=500m        \tab melissa, parcel, no followup \cr
#'   D8   \tab Y     \tab Y       \tab GS06       \tab 500m..10km    \tab melissa, parcel, flag \cr
#'   D9   \tab Y     \tab Y       \tab GS06       \tab >=10km        \tab disputed_melissa, parcel, flag (gross) \cr
#'   D10  \tab Y     \tab Y       \tab GS03       \tab any           \tab disputed_melissa, zip5, flag (gs03_always) \cr
#'   D11  \tab Y     \tab N       \tab --         \tab --            \tab adece, unknown, flag (melissa_unexpectedly_missing) \cr
#'   D12  \tab N     \tab Y       \tab GS01/GS05  \tab --            \tab melissa, zip4/rooftop, no followup \cr
#'   D13  \tab N     \tab Y       \tab GS06       \tab --            \tab melissa, parcel, flag (melissa_only_interpolated) \cr
#'   D14  \tab N     \tab Y       \tab GS03       \tab --            \tab melissa, zip5, flag (melissa_only_gs03) \cr
#'   D15  \tab N     \tab N       \tab --         \tab --            \tab none, none, flag (both_missing) \cr
#' }
#'
#' @section Band boundaries:
#'   `exact` (<10 m), `tight` (10-100 m), `loose` (100 m-1 km),
#'   `drift` (1-10 km), `gross` (>=10 km), `one_source_only` (only ADECE or
#'   only Melissa present), `none` (neither present).
#'
#' @examples
#' \dontrun{
#' raw   <- geocode_read(path = "...", cycle_year = "2026-2027",
#'                       receipt_date = "2026-03-04")
#' clean <- geocode_clean(raw)
#' rec   <- geocode_reconcile(clean)
#' print(rec)
#' rec$summary
#' head(rec$data[, c("row_id", "lat_final", "lng_final",
#'                    "lat_source", "lat_precision",
#'                    "coord_agreement_band",
#'                    "needs_followup_geocoding")])
#' }
#'
#' @seealso [geocode_clean()], [geocode_config()],
#'   [alprek_geocode_resultcode_meaning()], [alprek_haversine_m()].
#'
#' @importFrom tibble tibble as_tibble
#' @export
geocode_reconcile <- function(clean, config = NULL) {

  # ---- 0. Validate inputs --------------------------------------------------
  if (!inherits(clean, "alprek_geocode_clean")) {
    stop("clean must be an alprek_geocode_clean object ",
         "(from geocode_clean()).", call. = FALSE)
  }
  if (!is.null(config) && !inherits(config, "alprek_geocode_config")) {
    stop("config must be NULL or an alprek_geocode_config object ",
         "(from geocode_config()).", call. = FALSE)
  }

  data_in <- clean$data
  n <- nrow(data_in)

  # ---- 1. Resolve config knobs (locked defaults if NULL) -------------------
  auth_priority <- if (!is.null(config)) {
    config$authoritative_priority
  } else {
    "melissa_first"
  }
  if (!auth_priority %in% c("melissa_first", "adece_first")) {
    stop("config$authoritative_priority must be ",
         "'melissa_first' or 'adece_first'. Got: ", auth_priority,
         call. = FALSE)
  }

  rule <- if (!is.null(config)) {
    config$distance_threshold_rules
  } else {
    "by_resultcode"
  }

  tiered <- if (!is.null(config)) {
    config$tiered_thresholds
  } else {
    list(GS01 = 50, GS05 = 250, GS06 = 500, GS03 = Inf)
  }

  flat_m <- if (!is.null(config)) {
    config$flat_threshold_m
  } else {
    250L
  }

  # ---- 2. Load codebook for RESULTCODE -> precision_tier --------------------
  cb <- tryCatch(alprek_geocode_resultcode_meaning(),
                  error = function(e) NULL)
  if (is.null(cb)) {
    stop("alprek_geocode_resultcode_meaning() codebook not available. ",
         "geocode_reconcile() depends on it for precision tier lookup.",
         call. = FALSE)
  }

  # Map RESULTCODE -> precision_tier / acceptability from codebook
  # (no hardcoding of vendor semantics).
  rc_to_tier <- setNames(cb$precision_tier, cb$code)
  rc_to_acceptable <- setNames(cb$acceptable_for_master, cb$code)

  # ---- 3. Per-row inputs ---------------------------------------------------
  has_row_id <- "row_id" %in% names(data_in)
  has_raw_idx <- "raw_row_index" %in% names(data_in)
  row_ids <- if (has_row_id) {
    as.character(data_in$row_id)
  } else if (has_raw_idx) {
    sprintf("rec_%06d", as.integer(data_in$raw_row_index))
  } else {
    sprintf("rec_%06d", seq_len(n))
  }
  lineage_ids <- if ("lineage_id" %in% names(data_in)) {
    as.character(data_in$lineage_id)
  } else {
    rep(NA_character_, n)
  }

  adece_lat <- if ("latitude"  %in% names(data_in)) as.numeric(data_in$latitude)  else rep(NA_real_, n)
  adece_lng <- if ("longitude" %in% names(data_in)) as.numeric(data_in$longitude) else rep(NA_real_, n)
  melissa_lat <- if ("LAT" %in% names(data_in)) as.numeric(data_in$LAT) else rep(NA_real_, n)
  melissa_lng <- if ("LNG" %in% names(data_in)) as.numeric(data_in$LNG) else rep(NA_real_, n)
  result_code <- if ("RESULTCODE" %in% names(data_in)) {
    as.character(data_in$RESULTCODE)
  } else {
    rep(NA_character_, n)
  }

  adece_present   <- !is.na(adece_lat)   & !is.na(adece_lng)
  melissa_present <- !is.na(melissa_lat) & !is.na(melissa_lng)
  both_present    <- adece_present & melissa_present

  # ---- 4. Distance (haversine) where both present --------------------------
  dist_m <- rep(NA_real_, n)
  if (any(both_present)) {
    dist_m[both_present] <- .geocode_haversine_m(
      lat1 = adece_lat[both_present],
      lon1 = adece_lng[both_present],
      lat2 = melissa_lat[both_present],
      lon2 = melissa_lng[both_present]
    )
  }

  # ---- 5. Band (depends only on distance + presence) ----------------------
  band_chr <- vapply(
    seq_len(n),
    function(i) .geocode_band(dist_m[i], adece_present[i], melissa_present[i]),
    character(1)
  )

  # ---- 6. Per-row decision-matrix dispatch --------------------------------
  out_source     <- rep(NA_character_, n)
  out_tier       <- rep(NA_character_, n)
  out_followup   <- rep(FALSE, n)
  out_reason     <- rep(NA_character_, n)
  out_matrix_id  <- rep(NA_character_, n)
  out_threshold  <- rep(NA_real_, n)
  out_threshold_name <- rep(NA_character_, n)
  out_note       <- rep(NA_character_, n)

  GROSS_THRESHOLD_M <- 10000  # 10 km hard cap for "gross" outlier band

  for (i in seq_len(n)) {
    cell <- .geocode_decide(
      adece_present   = adece_present[i],
      melissa_present = melissa_present[i],
      result_code     = result_code[i],
      distance_m      = dist_m[i],
      band            = band_chr[i],
      auth_priority   = auth_priority,
      rule            = rule,
      tiered          = tiered,
      flat_m          = flat_m,
      rc_to_tier      = rc_to_tier,
      rc_to_acceptable = rc_to_acceptable,
      gross_m         = GROSS_THRESHOLD_M
    )
    out_source[i]    <- cell$lat_source
    out_tier[i]      <- cell$precision_tier
    out_followup[i]  <- cell$needs_followup
    out_reason[i]    <- cell$followup_reason
    out_matrix_id[i] <- cell$matrix_cell
    out_threshold[i] <- cell$threshold_used
    out_threshold_name[i] <- cell$threshold_name
    out_note[i]      <- cell$note
  }

  # ---- 7. Compute lat_final / lng_final from lat_source --------------------
  lat_final <- rep(NA_real_, n)
  lng_final <- rep(NA_real_, n)
  # melissa / disputed_melissa -> Melissa coords
  mfi <- out_source %in% c("melissa", "disputed_melissa")
  lat_final[mfi] <- melissa_lat[mfi]
  lng_final[mfi] <- melissa_lng[mfi]
  # adece -> ADECE coords
  afi <- out_source == "adece"
  lat_final[afi] <- adece_lat[afi]
  lng_final[afi] <- adece_lng[afi]
  # none -> NA (already)

  # ---- 8. Compact provenance string per row --------------------------------
  provenance <- vapply(
    seq_len(n),
    function(i) {
      .geocode_provenance_string(
        melissa_present = melissa_present[i],
        adece_present   = adece_present[i],
        result_code     = result_code[i],
        distance_m      = dist_m[i],
        band            = band_chr[i],
        lat_source      = out_source[i],
        precision_tier  = out_tier[i]
      )
    },
    character(1)
  )

  # ---- 9. Factorize categorical outputs (stable level order) --------------
  src_levels <- c("melissa", "adece", "disputed_melissa", "none")
  tier_levels_ord <- c("none", "unknown", "centroid", "zip5", "zip4",
                       "area", "parcel", "rooftop")
  band_levels <- c("exact", "tight", "loose", "drift", "gross",
                   "one_source_only", "none")
  reason_levels <- c("both_missing",
                     "melissa_unexpectedly_missing",
                     "melissa_only_interpolated",
                     "melissa_only_gs03",
                     "disagreement_above_threshold",
                     "disagreement_gross",
                     "resultcode_not_acceptable_for_master",
                     "resultcode_gs03_always_flag")
  model_status_levels <- c("missing", "not_model_ready",
                           "provisional_followup", "model_ready")

  lat_source_f    <- factor(out_source, levels = src_levels)
  lat_precision_f <- factor(out_tier, levels = tier_levels_ord,
                            ordered = TRUE)
  band_f          <- factor(band_chr, levels = band_levels)
  followup_reason_f <- factor(out_reason, levels = reason_levels)
  rc_acceptable <- vapply(
    result_code,
    .geocode_lookup_acceptable,
    logical(1),
    rc_to_acceptable = rc_to_acceptable
  )
  model_status_chr <- ifelse(
    out_source == "none", "missing",
    ifelse(!rc_acceptable, "not_model_ready",
           ifelse(out_followup, "provisional_followup", "model_ready"))
  )
  coord_model_status_f <- factor(model_status_chr,
                                 levels = model_status_levels,
                                 ordered = TRUE)

  # ---- 10. Assemble output data tibble ------------------------------------
  out_data <- data_in
  out_data$lat_final               <- lat_final
  out_data$lng_final               <- lng_final
  out_data$lat_source              <- lat_source_f
  out_data$lat_precision           <- lat_precision_f
  out_data$distance_adece_melissa_m <- dist_m
  out_data$coord_agreement_band    <- band_f
  out_data$needs_followup_geocoding <- as.logical(out_followup)
  out_data$followup_reason         <- followup_reason_f
  out_data$coord_model_status      <- coord_model_status_f
  out_data$geocode_provenance      <- provenance

  out_tbl <- tibble::as_tibble(out_data)
  attr(out_tbl, "data_source_map") <- attr(data_in, "data_source_map")

  # ---- 11. Per-row reconciliation log -------------------------------------
  reconciled_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  git_sha <- tryCatch(alprek_git_sha(), error = function(e) NA_character_)

  log_df <- tibble::tibble(
    row_id                = row_ids,
    lineage_id            = lineage_ids,
    raw_row_index         = if (has_raw_idx) as.integer(data_in$raw_row_index)
                            else seq_len(n),
    matrix_cell           = as.character(out_matrix_id),
    adece_present         = as.logical(adece_present),
    melissa_present       = as.logical(melissa_present),
    result_code           = as.character(result_code),
    distance_adece_melissa_m = as.numeric(dist_m),
    coord_agreement_band  = as.character(band_chr),
    lat_source            = as.character(out_source),
    lat_precision         = as.character(out_tier),
    threshold_used_m      = as.numeric(out_threshold),
    threshold_name        = as.character(out_threshold_name),
    needs_followup_geocoding = as.logical(out_followup),
    followup_reason       = as.character(out_reason),
    decision_source       = "code",
    decision_timestamp    = reconciled_at,
    note                  = as.character(out_note)
  )

  # ---- 12. Summary tibble -------------------------------------------------
  summary_df <- .geocode_summary(out_matrix_id, out_source, out_followup)

  # ---- 13. Meta -----------------------------------------------------------
  src_tab <- table(factor(out_source, levels = src_levels))
  meta_out <- list(
    reconciled_at            = reconciled_at,
    authoritative_priority   = auth_priority,
    distance_threshold_rules = rule,
    flat_threshold_m         = if (!is.null(config)) flat_m else NA_integer_,
    tiered_thresholds        = tiered,
    gross_threshold_m        = GROSS_THRESHOLD_M,
    n_rows                   = as.integer(n),
    n_needs_followup         = as.integer(sum(out_followup, na.rm = TRUE)),
    n_disputed               = as.integer(sum(out_source == "disputed_melissa",
                                              na.rm = TRUE)),
    n_lat_source_melissa     = as.integer(src_tab["melissa"]),
    n_lat_source_adece       = as.integer(src_tab["adece"]),
    n_lat_source_disputed    = as.integer(src_tab["disputed_melissa"]),
    n_lat_source_none        = as.integer(src_tab["none"]),
    file_sha256              = clean$meta$file_sha256,
    cycle_year               = clean$meta$cycle_year,
    receipt_date             = clean$meta$receipt_date,
    file_basename            = clean$meta$file_basename,
    sheet                    = clean$meta$sheet,
    source                   = clean$meta$source,
    geocoding_source         = clean$meta$geocoding_source,
    git_sha                  = if (!is.na(git_sha)) git_sha
                                else clean$meta$git_sha
  )
  # NA -> 0L for counts so summaries are clean
  for (nm in c("n_lat_source_melissa", "n_lat_source_adece",
                "n_lat_source_disputed", "n_lat_source_none")) {
    if (is.na(meta_out[[nm]])) meta_out[[nm]] <- 0L
  }

  structure(list(
    data                = out_tbl,
    reconciliation_log  = log_df,
    summary             = summary_df,
    meta                = meta_out
  ), class = "alprek_geocode_reconciled")
}


#' Print method for `alprek_geocode_reconciled`
#'
#' @param x An `alprek_geocode_reconciled` object.
#' @param ... Ignored.
#' @export
print.alprek_geocode_reconciled <- function(x, ...) {
  m <- x$meta
  cat("<alprek_geocode_reconciled>\n")
  cat("  Authoritative priority:  ", m$authoritative_priority, "\n", sep = "")
  cat("  Distance rule:           ", m$distance_threshold_rules, "\n", sep = "")
  if (identical(m$distance_threshold_rules, "by_resultcode")) {
    cat("  Tiered thresholds (m):\n")
    for (nm in names(m$tiered_thresholds)) {
      val <- m$tiered_thresholds[[nm]]
      cat("    ", format(nm, width = 6), "= ",
          if (is.infinite(val)) "Inf (always flag)" else val, "\n",
          sep = "")
    }
  } else if (startsWith(m$distance_threshold_rules, "flat_")) {
    cat("  Flat threshold (m):      ", m$flat_threshold_m, "\n", sep = "")
  }
  cat("  Rows:                    ", m$n_rows, "\n", sep = "")
  cat("    lat_source = melissa:          ", m$n_lat_source_melissa, "\n",
      sep = "")
  cat("    lat_source = adece:            ", m$n_lat_source_adece, "\n",
      sep = "")
  cat("    lat_source = disputed_melissa: ", m$n_lat_source_disputed, "\n",
      sep = "")
  cat("    lat_source = none:             ", m$n_lat_source_none, "\n",
      sep = "")
  cat("  needs_followup_geocoding: ", m$n_needs_followup, " (",
      sprintf("%.1f%%", 100 * m$n_needs_followup / max(1L, m$n_rows)),
      ")\n", sep = "")
  cat("  Reconciled at:           ", m$reconciled_at, "\n", sep = "")
  if (!is.null(x$summary) && nrow(x$summary) > 0L) {
    cat("\n  Decision-cell counts:\n")
    for (i in seq_len(nrow(x$summary))) {
      cat("    ", format(x$summary$matrix_cell[i], width = 4),
          " n=", format(x$summary$n[i], width = 5), " ",
          x$summary$description[i], "\n", sep = "")
    }
  }
  invisible(x)
}


# ============================================================================
# Internal helpers
# ============================================================================

#' Compute coordinate-agreement band from distance and presence flags
#'
#' @param d Distance in meters (numeric scalar; NA when not both present).
#' @param adece_present TRUE/FALSE.
#' @param melissa_present TRUE/FALSE.
#'
#' @return Character scalar (one of the seven band levels).
#'
#' @keywords internal
#' @noRd
.geocode_band <- function(d, adece_present, melissa_present) {
  if (!adece_present && !melissa_present) return("none")
  if (xor(adece_present, melissa_present)) return("one_source_only")
  # both present -> graded by distance
  if (is.na(d)) return("none")  # defensive (haversine returned NA)
  if (d < 10)        return("exact")
  if (d < 100)       return("tight")
  if (d < 1000)      return("loose")
  if (d < 10000)     return("drift")
  "gross"
}


#' Look up precision tier for a RESULTCODE
#'
#' @param result_code Character scalar (`"GS01"`, `"GS05"`, etc.).
#' @param rc_to_tier Named character vector from codebook.
#'
#' @return Tier label string. Returns `"unknown"` if the code is missing or
#'   not in the codebook.
#'
#' @keywords internal
#' @noRd
.geocode_lookup_precision <- function(result_code, rc_to_tier) {
  if (is.na(result_code) || !nzchar(result_code)) return("unknown")
  tier <- unname(rc_to_tier[as.character(result_code)])
  if (is.null(tier) || is.na(tier) || !nzchar(tier)) "unknown" else as.character(tier)
}


#' Look up whether a RESULTCODE is acceptable for master coordinates
#'
#' @return Logical scalar. Returns `FALSE` for missing, blank, unknown, or
#'   codebook-unacceptable RESULTCODEs.
#'
#' @keywords internal
#' @noRd
.geocode_lookup_acceptable <- function(result_code, rc_to_acceptable) {
  if (is.na(result_code) || !nzchar(result_code)) return(FALSE)
  ok <- unname(rc_to_acceptable[as.character(result_code)])
  isTRUE(ok)
}


#' Resolve the per-RESULTCODE threshold value used to flag disagreement
#'
#' @return List with `threshold_m` (numeric, possibly `Inf`) and
#'   `threshold_name` (e.g., `"tiered:GS05"`, `"flat_100m"`).
#'
#' @keywords internal
#' @noRd
.geocode_threshold_for <- function(result_code, rule, tiered, flat_m) {
  if (identical(rule, "by_resultcode")) {
    if (!is.na(result_code) && result_code %in% names(tiered)) {
      val <- tiered[[result_code]]
      return(list(threshold_m = as.numeric(val),
                  threshold_name = sprintf("tiered:%s", result_code)))
    }
    return(list(threshold_m = Inf,
                threshold_name = "tiered:unmapped"))
  }
  if (startsWith(rule %||% "", "flat_")) {
    return(list(threshold_m = as.numeric(flat_m),
                threshold_name = sprintf("flat_%dm",
                                          as.integer(flat_m))))
  }
  list(threshold_m = as.numeric(flat_m),
       threshold_name = "flat_default")
}


#' Decide matrix cell for a single row
#'
#' Returns a list with: `lat_source`, `precision_tier`, `needs_followup`,
#' `followup_reason`, `matrix_cell`, `threshold_used`, `threshold_name`,
#' `note`. Implements all 15 cells of the LOCKED Step 4.2 matrix.
#'
#' @keywords internal
#' @noRd
.geocode_decide <- function(adece_present, melissa_present,
                            result_code, distance_m, band,
                            auth_priority, rule, tiered, flat_m,
                            rc_to_tier, rc_to_acceptable,
                            gross_m = 10000) {

  # ----- D15: both missing ------------------------------------------------
  if (!adece_present && !melissa_present) {
    return(list(
      lat_source     = "none",
      precision_tier = "none",
      needs_followup = TRUE,
      followup_reason = "both_missing",
      matrix_cell    = "D15",
      threshold_used = NA_real_,
      threshold_name = NA_character_,
      note           = "both ADECE and Melissa coordinates missing"
    ))
  }

  # ----- D11: ADECE only --------------------------------------------------
  if (adece_present && !melissa_present) {
    return(list(
      lat_source     = "adece",
      precision_tier = "unknown",
      needs_followup = TRUE,
      followup_reason = "melissa_unexpectedly_missing",
      matrix_cell    = "D11",
      threshold_used = NA_real_,
      threshold_name = NA_character_,
      note           = "Melissa unexpectedly missing (n_expected=0)"
    ))
  }

  # ----- D12-D14: Melissa only -------------------------------------------
  if (!adece_present && melissa_present) {
    tier <- .geocode_lookup_precision(result_code, rc_to_tier)
    acceptable <- .geocode_lookup_acceptable(result_code, rc_to_acceptable)
    if (!is.na(result_code) && result_code == "GS03") {
      return(list(
        lat_source     = "melissa",
        precision_tier = tier,
        needs_followup = TRUE,
        followup_reason = "melissa_only_gs03",
        matrix_cell    = "D14",
        threshold_used = NA_real_,
        threshold_name = NA_character_,
        note           = "Melissa-only ZIP centroid; ADECE missing"
      ))
    }
    if (!is.na(result_code) && result_code == "GS06") {
      return(list(
        lat_source     = "melissa",
        precision_tier = tier,
        needs_followup = TRUE,
        followup_reason = "melissa_only_interpolated",
        matrix_cell    = "D13",
        threshold_used = NA_real_,
        threshold_name = NA_character_,
        note           = "Melissa-only interpolated rooftop; ADECE missing"
      ))
    }
    if (!acceptable) {
      return(list(
        lat_source     = "melissa",
        precision_tier = tier,
        needs_followup = TRUE,
        followup_reason = "resultcode_not_acceptable_for_master",
        matrix_cell    = "D12",
        threshold_used = NA_real_,
        threshold_name = "codebook:acceptable_for_master_false",
        note           = sprintf(
          "Melissa-only RESULTCODE %s is not acceptable_for_master; retained for follow-up",
          if (is.na(result_code) || !nzchar(result_code)) "no_rc"
          else result_code)
      ))
    }
    # D12: codebook-acceptable Melissa-only code -> use Melissa, no flag
    return(list(
      lat_source     = "melissa",
      precision_tier = tier,
      needs_followup = FALSE,
      followup_reason = NA_character_,
      matrix_cell    = "D12",
      threshold_used = NA_real_,
      threshold_name = NA_character_,
      note           = sprintf("Melissa-only (%s); ADECE missing",
                                if (is.na(result_code)) "no_rc"
                                else result_code)
    ))
  }

  # ----- D1-D10: both present --------------------------------------------
  # Common bits
  tier <- .geocode_lookup_precision(result_code, rc_to_tier)
  acceptable <- .geocode_lookup_acceptable(result_code, rc_to_acceptable)

  # D10: GS03 with both present -> always disputed_melissa, always flag.
  # Tier is zip5 (low precision); centroid is unreliable even when ADECE
  # happens to agree.
  if (!is.na(result_code) && result_code == "GS03") {
    return(list(
      lat_source     = if (auth_priority == "adece_first") "adece"
                       else "disputed_melissa",
      precision_tier = tier,
      needs_followup = TRUE,
      followup_reason = "resultcode_gs03_always_flag",
      matrix_cell    = "D10",
      threshold_used = Inf,
      threshold_name = "tiered:GS03",
      note           = sprintf(
        "GS03 ZIP-centroid always flagged; dist=%s m",
        if (is.na(distance_m)) "NA" else sprintf("%.0f", distance_m))
    ))
  }

  # Gross outlier (>=10 km) wins regardless of tier threshold.
  is_gross <- !is.na(distance_m) && distance_m >= gross_m
  if (is_gross) {
    # D3 / D6 / D9: gross outlier
    cell_id <- switch(as.character(result_code),
                       GS01 = "D3", GS05 = "D6", GS06 = "D9",
                       "D6")  # fallback to D6 row for unknown RC
    return(list(
      lat_source     = if (auth_priority == "adece_first") "adece"
                       else "disputed_melissa",
      precision_tier = tier,
      needs_followup = TRUE,
      followup_reason = "disagreement_gross",
      matrix_cell    = cell_id,
      threshold_used = gross_m,
      threshold_name = "gross_10km",
      note           = sprintf("Gross outlier %.0f m >= %d m",
                                distance_m, as.integer(gross_m))
    ))
  }

  # Tiered/flat threshold check for sub-gross disagreement.
  thr <- .geocode_threshold_for(result_code, rule, tiered, flat_m)
  thr_m <- thr$threshold_m

  if (!acceptable) {
    return(list(
      lat_source     = if (auth_priority == "adece_first") "adece"
                       else "melissa",
      precision_tier = tier,
      needs_followup = TRUE,
      followup_reason = "resultcode_not_acceptable_for_master",
      matrix_cell    = switch(as.character(result_code),
                              GS01 = "D2", GS05 = "D5", GS06 = "D8",
                              "D4"),
      threshold_used = thr_m,
      threshold_name = thr$threshold_name,
      note           = sprintf(
        "RESULTCODE %s is not acceptable_for_master; retained coordinate for follow-up",
        if (is.na(result_code) || !nzchar(result_code)) "no_rc"
        else result_code)
    ))
  }

  # If distance is NA but both are present (shouldn't normally happen),
  # treat as exact (band logic returned "none" already) - flag defensively.
  exceeds_threshold <- if (is.na(distance_m)) FALSE else (distance_m > thr_m)

  if (!exceeds_threshold) {
    # D1 / D4 / D7: within threshold -> melissa, no flag
    cell_id <- switch(as.character(result_code),
                       GS01 = "D1", GS05 = "D4", GS06 = "D7",
                       "D4")  # fallback to D4 for unknown RC
    return(list(
      lat_source     = if (auth_priority == "adece_first") "adece"
                       else "melissa",
      precision_tier = tier,
      needs_followup = FALSE,
      followup_reason = NA_character_,
      matrix_cell    = cell_id,
      threshold_used = thr_m,
      threshold_name = thr$threshold_name,
      note           = sprintf("Within %s threshold (%.0f m vs %s m)",
                                thr$threshold_name,
                                ifelse(is.na(distance_m), 0, distance_m),
                                if (is.infinite(thr_m)) "Inf"
                                else as.character(as.integer(thr_m)))
    ))
  }

  # D2 / D5 / D8: above tier threshold, below gross -> melissa, flag
  cell_id <- switch(as.character(result_code),
                     GS01 = "D2", GS05 = "D5", GS06 = "D8",
                     "D5")
  list(
    lat_source     = if (auth_priority == "adece_first") "adece"
                     else "melissa",
    precision_tier = tier,
    needs_followup = TRUE,
    followup_reason = "disagreement_above_threshold",
    matrix_cell    = cell_id,
    threshold_used = thr_m,
    threshold_name = thr$threshold_name,
    note           = sprintf("Disagreement %.0f m > %s m (%s)",
                              distance_m,
                              if (is.infinite(thr_m)) "Inf"
                              else as.character(as.integer(thr_m)),
                              thr$threshold_name)
  )
}


#' Build a compact provenance string for one row
#'
#' Format: `"melissa:GS05;adece:present;dist=87m;band=tight"` etc.
#'
#' @keywords internal
#' @noRd
.geocode_provenance_string <- function(melissa_present, adece_present,
                                       result_code, distance_m, band,
                                       lat_source, precision_tier) {
  parts <- character(0)
  parts <- c(parts, sprintf("melissa:%s",
                            if (!melissa_present) "absent"
                            else if (is.na(result_code) || !nzchar(result_code))
                              "no_rc"
                            else result_code))
  parts <- c(parts, sprintf("adece:%s",
                            if (adece_present) "present" else "absent"))
  parts <- c(parts, sprintf("dist=%s",
                            if (is.na(distance_m)) "NA"
                            else sprintf("%.0fm", distance_m)))
  parts <- c(parts, sprintf("band=%s",
                            if (is.na(band)) "NA" else band))
  parts <- c(parts, sprintf("source=%s",
                            if (is.na(lat_source)) "NA" else lat_source))
  parts <- c(parts, sprintf("tier=%s",
                            if (is.na(precision_tier)) "NA"
                            else precision_tier))
  paste(parts, collapse = ";")
}


#' Build the per-cell summary tibble
#'
#' @keywords internal
#' @noRd
.geocode_summary <- function(matrix_cell, lat_source, needs_followup) {
  descriptions <- c(
    D1  = "Both present, GS01, within 50m: melissa, zip4",
    D2  = "Both present, GS01, 50m-10km: melissa, zip4, flagged",
    D3  = "Both present, GS01, >=10km: disputed_melissa, zip4, gross",
    D4  = "Both present, GS05 within 250m: melissa, rooftop; future unacceptable codes route here flagged",
    D5  = "Both present, GS05, 250m-10km: melissa, rooftop, flagged",
    D6  = "Both present, GS05, >=10km: disputed_melissa, rooftop, gross",
    D7  = "Both present, GS06, within 500m: melissa, parcel",
    D8  = "Both present, GS06, 500m-10km: melissa, parcel, flagged",
    D9  = "Both present, GS06, >=10km: disputed_melissa, parcel, gross",
    D10 = "Both present, GS03 (ZIP centroid): disputed_melissa, zip5",
    D11 = "ADECE only (Melissa unexpectedly missing)",
    D12 = "Melissa only (GS01/GS05): melissa, no follow-up; future unacceptable codes route here flagged",
    D13 = "Melissa only (GS06 interpolated rooftop): flagged",
    D14 = "Melissa only (GS03 ZIP centroid): flagged",
    D15 = "Both missing: no coordinate, flagged"
  )
  cells <- names(descriptions)
  counts <- vapply(cells,
                    function(c) sum(matrix_cell == c, na.rm = TRUE),
                    integer(1))
  tibble::tibble(
    matrix_cell = cells,
    n           = as.integer(counts),
    description = unname(descriptions[cells])
  )
}


# ---------------------------------------------------------------------------
# %||% fallback (rlang re-exports this; provide local for a single operator)
# ---------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a


# ============================================================================
# Step 4.4: Follow-up queue surface
# ============================================================================

#' Build a Follow-Up Queue From a Reconciled Geocode Object (Step 4.4)
#'
#' @description Surfaces the subset of reconciled site-rows that need analyst
#'   follow-up geocoding, sorted for triage and annotated with a suggested
#'   action. This is the consumer-facing helper that turns the per-row
#'   `needs_followup_geocoding` flag (from [geocode_reconcile()]) into a
#'   short, actionable queue.
#'
#'   Each row in the output represents one site/year that the reconciler
#'   flagged for follow-up. The `suggested_action` column is a controlled
#'   vocabulary derived deterministically from `followup_reason`, so the
#'   downstream caller can group/filter the queue without re-reading the
#'   decision matrix.
#'
#'   Sort order (descending priority):
#'   \enumerate{
#'     \item `school_year` descending (latest year first).
#'     \item `distance_adece_melissa_m` descending (largest disagreement
#'       first); rows with `NA` distance (one-source-only or both-missing)
#'       sort to the end.
#'   }
#'
#'   `suggested_action` mapping (controlled vocabulary):
#'   \tabular{ll}{
#'     `followup_reason`              \tab `suggested_action` \cr
#'     `disagreement_above_threshold` \tab `manual_source_adjudication` \cr
#'     `disagreement_gross`           \tab `verify_adece_address_and_request_recheck` \cr
#'     `resultcode_not_acceptable_for_master` \tab `request_higher_precision_geocode` \cr
#'     `both_missing`                 \tab `obtain_coord` \cr
#'     `melissa_unexpectedly_missing` \tab `request_melissa_geocode` \cr
#'     `melissa_only_interpolated`    \tab `request_higher_precision_geocode` \cr
#'     `melissa_only_gs03`            \tab `request_higher_precision_geocode` \cr
#'     `resultcode_gs03_always_flag`  \tab `manual_review_gs03` \cr
#'     other / `NA`                   \tab `manual_review` \cr
#'   }
#'
#' @param reconciled An `alprek_geocode_reconciled` object from
#'   [geocode_reconcile()].
#' @param include_disputed Logical. When `TRUE` (default), rows whose
#'   `lat_source == "disputed_melissa"` (matrix cells D3 / D6 / D9 / D10)
#'   are included in the queue. Set to `FALSE` to exclude them (e.g., when
#'   the analyst handles disputed rows in a separate workflow).
#'
#' @return A tibble (not S3) with one row per site needing follow-up, ordered
#'   per the sort rules above. Columns, in order:
#'   * `lineage_id` (character; stable row-level lineage key)
#'   * `row_id` (character)
#'   * `school_year` (character)
#'   * `site_code` (character)
#'   * `site_name` (character)
#'   * `site_street`, `site_city`, `site_state`, `site_zip` (character)
#'   * `lat_source` (character; coerced from the reconciled factor)
#'   * `coord_agreement_band` (character)
#'   * `distance_adece_melissa_m` (numeric; NA where not computable)
#'   * `melissa_result_code` (character; renamed from `RESULTCODE`)
#'   * `lat_precision` (character)
#'   * `followup_reason` (character)
#'   * `suggested_action` (character; one of the controlled vocab values)
#'
#'   Returns a 0-row tibble with the same schema when no rows need follow-up
#'   (or when all flagged rows are disputed and `include_disputed = FALSE`).
#'   All returned queues, including 0-row outputs, carry attributes
#'   `privacy_level = "internal_address_followup"` and
#'   `contains_address_fields = TRUE`.
#'
#' @examples
#' \dontrun{
#' raw   <- geocode_read(path = "...", cycle_year = "2026-2027",
#'                       receipt_date = "2026-03-04")
#' clean <- geocode_clean(raw)
#' rec   <- geocode_reconcile(clean)
#' fq    <- geocode_followup_queue(rec)
#' table(fq$suggested_action)
#' # Exclude disputed Melissa rows (handled separately):
#' fq_no_disputed <- geocode_followup_queue(rec, include_disputed = FALSE)
#' }
#'
#' @seealso [geocode_reconcile()] for the upstream decision matrix that
#'   produces `needs_followup_geocoding` and `followup_reason`.
#'
#' @importFrom tibble tibble as_tibble
#' @export
geocode_followup_queue <- function(reconciled, include_disputed = TRUE) {

  # ---- 0. Validate inputs --------------------------------------------------
  if (!inherits(reconciled, "alprek_geocode_reconciled")) {
    stop("reconciled must be an alprek_geocode_reconciled object ",
         "(from geocode_reconcile()).", call. = FALSE)
  }
  if (!is.logical(include_disputed) || length(include_disputed) != 1L ||
      is.na(include_disputed)) {
    stop("include_disputed must be a single TRUE/FALSE.", call. = FALSE)
  }

  out_cols <- c(
    "lineage_id", "row_id", "school_year", "site_code", "site_name",
    "site_street", "site_city", "site_state", "site_zip",
    "lat_source", "coord_agreement_band", "distance_adece_melissa_m",
    "melissa_result_code", "lat_precision", "followup_reason",
    "suggested_action"
  )

  # Build a 0-row tibble with the exact target schema. Reused for the
  # empty-input and empty-after-filter paths so callers always get the
  # same column set/dtypes regardless of input.
  empty_out <- tibble::tibble(
    lineage_id                = character(0),
    row_id                    = character(0),
    school_year               = character(0),
    site_code                 = character(0),
    site_name                 = character(0),
    site_street               = character(0),
    site_city                 = character(0),
    site_state                = character(0),
    site_zip                  = character(0),
    lat_source                = character(0),
    coord_agreement_band      = character(0),
    distance_adece_melissa_m  = numeric(0),
    melissa_result_code       = character(0),
    lat_precision             = character(0),
    followup_reason           = character(0),
    suggested_action          = character(0)
  )
  empty_out <- .geocode_mark_followup_privacy(empty_out)

  data_in <- reconciled$data
  n <- nrow(data_in)
  if (n == 0L) return(empty_out)

  # ---- 1. Filter to followup rows -----------------------------------------
  if (!"needs_followup_geocoding" %in% names(data_in)) {
    stop("reconciled$data is missing `needs_followup_geocoding`; ",
         "did geocode_reconcile() run cleanly?", call. = FALSE)
  }
  needs_fu <- as.logical(data_in$needs_followup_geocoding)
  needs_fu[is.na(needs_fu)] <- FALSE
  keep <- needs_fu

  # Optional: exclude lat_source == "disputed_melissa"
  if (!include_disputed && "lat_source" %in% names(data_in)) {
    src_chr <- as.character(data_in$lat_source)
    keep <- keep & (is.na(src_chr) | src_chr != "disputed_melissa")
  }

  if (!any(keep)) return(empty_out)

  df <- data_in[keep, , drop = FALSE]

  # ---- 2. Coerce inputs and pluck columns ---------------------------------
  # Always coerce to character / numeric explicitly so that output schema is
  # stable even when upstream columns are missing (defensive: synthesizes
  # NA-filled vectors of the right length).
  k <- nrow(df)
  pull_chr <- function(col) {
    if (col %in% names(df)) as.character(df[[col]]) else rep(NA_character_, k)
  }
  pull_num <- function(col) {
    if (col %in% names(df)) as.numeric(df[[col]]) else rep(NA_real_, k)
  }

  row_id              <- pull_chr("row_id")
  lineage_id          <- pull_chr("lineage_id")
  school_year         <- pull_chr("school_year")
  site_code           <- pull_chr("site_code")
  site_name           <- pull_chr("site_name")
  site_street         <- pull_chr("site_street")
  site_city           <- pull_chr("site_city")
  site_state          <- pull_chr("site_state")
  site_zip            <- pull_chr("site_zip")
  lat_source_chr      <- pull_chr("lat_source")
  band_chr            <- pull_chr("coord_agreement_band")
  dist_m              <- pull_num("distance_adece_melissa_m")
  result_code         <- pull_chr("RESULTCODE")
  lat_precision_chr   <- pull_chr("lat_precision")
  followup_reason_chr <- pull_chr("followup_reason")

  # ---- 3. Suggested action (controlled vocabulary) ------------------------
  suggested_action <- .geocode_followup_suggested_action(followup_reason_chr)

  # ---- 4. Assemble output tibble in spec order ----------------------------
  out <- tibble::tibble(
    lineage_id                = lineage_id,
    row_id                    = row_id,
    school_year               = school_year,
    site_code                 = site_code,
    site_name                 = site_name,
    site_street               = site_street,
    site_city                 = site_city,
    site_state                = site_state,
    site_zip                  = site_zip,
    lat_source                = lat_source_chr,
    coord_agreement_band      = band_chr,
    distance_adece_melissa_m  = dist_m,
    melissa_result_code       = result_code,
    lat_precision             = lat_precision_chr,
    followup_reason           = followup_reason_chr,
    suggested_action          = suggested_action
  )

  # ---- 5. Sort: school_year DESC, then distance DESC (NAs at end) ---------
  # base::order() puts NAs last by default (na.last = TRUE), and supports
  # multiple keys; we negate the school_year ranks and distances to get
  # descending order for both. school_year is character, so we rank it
  # explicitly via factor(levels = sort(unique, decreasing = FALSE)) +
  # as.integer() and then negate.
  sy_levels <- sort(unique(out$school_year), na.last = TRUE)
  sy_rank   <- match(out$school_year, sy_levels)
  ord <- order(-sy_rank, -out$distance_adece_melissa_m, na.last = TRUE)
  out <- out[ord, , drop = FALSE]

  # Make sure col order is exactly the spec
  out <- out[, out_cols, drop = FALSE]
  .geocode_mark_followup_privacy(out)
}


#' Mark a follow-up queue as internal address-bearing material
#'
#' @keywords internal
#' @noRd
.geocode_mark_followup_privacy <- function(x) {
  attr(x, "privacy_level") <- "internal_address_followup"
  attr(x, "contains_address_fields") <- TRUE
  x
}


#' Map a followup_reason value to a suggested_action label
#'
#' Internal helper for [geocode_followup_queue()]. Vectorized over
#' `followup_reason`. Returns a character vector of identical length.
#' Any input outside the documented vocabulary -- including `NA` -- maps
#' to `"manual_review"`.
#'
#' @param followup_reason Character vector of `followup_reason` values
#'   (possibly with `NA`s).
#'
#' @return Character vector of `suggested_action` labels, length-matched
#'   to `followup_reason`.
#'
#' @keywords internal
#' @noRd
.geocode_followup_suggested_action <- function(followup_reason) {
  fr <- as.character(followup_reason)
  out <- rep("manual_review", length(fr))
  out[!is.na(fr) & fr == "disagreement_above_threshold"] <-
    "manual_source_adjudication"
  out[!is.na(fr) & fr == "disagreement_gross"] <-
    "verify_adece_address_and_request_recheck"
  out[!is.na(fr) & fr == "resultcode_not_acceptable_for_master"] <-
    "request_higher_precision_geocode"
  out[!is.na(fr) & fr == "both_missing"] <-
    "obtain_coord"
  out[!is.na(fr) & fr == "melissa_unexpectedly_missing"] <-
    "request_melissa_geocode"
  out[!is.na(fr) & fr == "melissa_only_interpolated"] <-
    "request_higher_precision_geocode"
  out[!is.na(fr) & fr == "melissa_only_gs03"] <-
    "request_higher_precision_geocode"
  out[!is.na(fr) & fr == "resultcode_gs03_always_flag"] <-
    "manual_review_gs03"
  out
}
