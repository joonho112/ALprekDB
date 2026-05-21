#' Transform Reconciled Geocode Data Into Master Object (Step 5.1)
#'
#' @description Adds 5 derived analytical variables on top of the reconciled
#'   geocode output. Mirrors the `applications_transform()` / `budget_transform()`
#'   pattern in this package: a thin, deterministic data-layer transform that
#'   produces an `alprek_geocode_master` S3 object consumable by downstream
#'   linkage, export, and modeling code.
#'
#'   **Derived variables (in order):**
#'
#'   1. `precision_tier` (ordered factor; rooftop > parcel > zip4 > zip5 >
#'      centroid > area > unknown > none) - aliased from `lat_precision`
#'      with descending-order levels for sorting. The underlying tier value
#'      is unchanged; only the factor-level order is inverted so that
#'      `sort(precision_tier)` puts highest-precision rows first.
#'   2. `in_alabama` (logical; NA-able) - `lat_final` in `c(30, 36)` and
#'      `lng_final` in `c(-89, -84)`. Both bounds inclusive. `NA` when
#'      either `lat_final` or `lng_final` is `NA`.
#'   3. `county_check_match` (logical; NA-able) - compares Melissa
#'      `COUNTYNAME` against an `adece_county` sidecar (if present in the
#'      reconciled `$data`; e.g., G16 fixture). Returns `NA` for every row
#'      in Step 5.1 standalone runs because the ADECE county column is not
#'      part of the 29-column Melissa contract. Linkage diagnostics can use
#'      this column when a county sidecar has been materialized; the default
#'      v0.8.0 master join does not require it. Comparison is
#'      case-insensitive (`tolower()` both sides).
#'   4. `coord_age_years` (integer; NA-able) -
#'      `cycle_year_first - school_year_first`. `cycle_year` and
#'      `school_year` are both `"YYYY-YYYY"` (with optional `"_new"`
#'      suffix on `school_year`). The first 4 characters of each are
#'      coerced to integer; the suffix is ignored because `substr()` only
#'      takes the leading year. Negative or implausible values are
#'      preserved (analytical responsibility, not data-layer correction).
#'   5. `geocode_run_id` (character) - panel-stable identifier built from
#'      `config$vendor`, `"v1"`, and `format(config$delivery_date, "%Y-%m")`.
#'      Default for the v0.8.0 single-snapshot path:
#'      `"melissa_v1_2026-03"`. Every row in a single transform shares the
#'      same value (the snapshot is one run); panels built across release
#'      cycles (Step 5.2) carry distinct `geocode_run_id` values.
#'
#'   **Phase 5 contract:** Exports must carry both
#'   `coord_model_status` (from Step 4.3) and `lineage_id` (from Step 3.1).
#'   Rows with `coord_model_status != "model_ready"` remain visible in
#'   `$data` and must NOT be silently promoted into downstream SAE-ready
#'   master tables. This transform preserves both columns untouched.
#'
#' @param reconciled An `alprek_geocode_reconciled` object from
#'   [geocode_reconcile()].
#' @param config Optional `alprek_geocode_config` (from [geocode_config()]).
#'   When `NULL`, a minimal default config is constructed from
#'   `reconciled$meta` (`vendor = meta$source %||% "melissa"`,
#'   `delivery_date = meta$receipt_date`). Used to build `geocode_run_id`.
#'
#' @return An `alprek_geocode_master` S3 list with elements:
#'   * `data`: tibble of reconciled data + 5 new derived columns.
#'     Preserves `lineage_id` and `coord_model_status` columns intact.
#'   * `transform_log`: tibble (one row per derivation rule) with columns
#'     `rule`, `n_affected`, `details`, `severity` (one of
#'     `"INFO"`/`"WARN"`/`"ERROR"`).
#'   * `meta`: list inheriting from `reconciled$meta` plus
#'     `transformed_at` and `geocode_run_id`.
#'
#' @section Implementation notes:
#'   * `precision_tier` is an ordered factor with levels listed
#'     **descending** (highest precision first). This makes
#'     `sort(precision_tier)` put rooftop rows first. The underlying
#'     `lat_precision` column (ascending order, set by `geocode_reconcile()`)
#'     is left in place.
#'   * `county_check_match` reads an optional `adece_county` column that
#'     test fixtures (e.g., G16) attach during the clean->reconcile pass.
#'     Production callers should leave this column absent and rely on
#'     Phase 6.1 linkage to materialize the comparison.
#'   * `geocode_run_id` matches the `geocode_run_id` token mentioned in
#'     the Phase 5 plan book chapter (`06-phase5-transform-panel-export.qmd`)
#'     and in the future `geocode_bind_years()` (Step 5.2) panel scaffolding.
#'
#' @examples
#' \dontrun{
#' raw   <- geocode_read(path = "...", cycle_year = "2026-2027",
#'                       receipt_date = "2026-03-04")
#' clean <- geocode_clean(raw)
#' rec   <- geocode_reconcile(clean)
#' mst   <- geocode_transform(rec)
#' mst
#' }
#'
#' @seealso [geocode_reconcile()], [geocode_config()].
#'
#' @importFrom tibble tibble as_tibble
#' @export
geocode_transform <- function(reconciled, config = NULL) {

  # ---- 0. Argument validation ----------------------------------------------
  if (!inherits(reconciled, "alprek_geocode_reconciled")) {
    stop("reconciled must be an alprek_geocode_reconciled object ",
         "(from geocode_reconcile()).", call. = FALSE)
  }
  if (!is.null(config) && !inherits(config, "alprek_geocode_config")) {
    stop("config must be NULL or an alprek_geocode_config object ",
         "(from geocode_config()).", call. = FALSE)
  }

  data_in <- reconciled$data
  n_rows  <- nrow(data_in)

  # ---- 1. Resolve config (defaults from reconciled$meta if NULL) -----------
  meta_in <- reconciled$meta
  vendor <- if (!is.null(config)) {
    config$vendor
  } else {
    as.character(meta_in$source %||% "melissa")
  }
  delivery_date <- if (!is.null(config)) {
    config$delivery_date
  } else {
    meta_in$receipt_date
  }
  # Date coercion: receipt_date can be Date or "YYYY-MM-DD" character.
  if (inherits(delivery_date, "Date")) {
    # already a Date
  } else if (is.character(delivery_date) && length(delivery_date) == 1L &&
              nzchar(delivery_date)) {
    parsed <- suppressWarnings(as.Date(delivery_date))
    if (is.na(parsed)) {
      stop("Could not parse delivery_date / receipt_date as Date. ",
           "Got: ", delivery_date, call. = FALSE)
    }
    delivery_date <- parsed
  } else if (is.null(delivery_date) ||
             (is.atomic(delivery_date) && all(is.na(delivery_date)))) {
    delivery_date <- NA
  } else {
    stop("delivery_date / receipt_date must be a Date or 'YYYY-MM-DD' ",
         "character.", call. = FALSE)
  }

  # ---- 2. Per-row derivations ----------------------------------------------
  log_rows <- list()
  add_log <- function(rule, n_affected, details, severity = "INFO") {
    log_rows[[length(log_rows) + 1L]] <<- tibble::tibble(
      rule       = as.character(rule),
      n_affected = as.integer(n_affected),
      details    = as.character(details),
      severity   = as.character(severity)
    )
  }

  data_out <- data_in

  # ---- 2.1 precision_tier (ordered factor; descending) --------------------
  # Levels in the spec (descending, rooftop highest):
  #   rooftop > parcel > zip4 > zip5 > centroid > area > unknown > none
  tier_levels_desc <- c("rooftop", "parcel", "zip4", "zip5",
                        "centroid", "area", "unknown", "none")
  if ("lat_precision" %in% names(data_in)) {
    src_chr <- as.character(data_in$lat_precision)
    # Any value not in the canonical 8 (defensive) is coerced to "unknown".
    unknown_idx <- !is.na(src_chr) & !src_chr %in% tier_levels_desc
    n_unknown <- sum(unknown_idx)
    if (n_unknown > 0L) {
      src_chr[unknown_idx] <- "unknown"
    }
    precision_tier_f <- factor(src_chr, levels = tier_levels_desc,
                               ordered = TRUE)
  } else {
    # Defensive: lat_precision should be present (Step 4.3 guarantees it).
    src_chr <- rep(NA_character_, n_rows)
    precision_tier_f <- factor(src_chr, levels = tier_levels_desc,
                               ordered = TRUE)
    n_unknown <- 0L
  }
  data_out$precision_tier <- precision_tier_f
  add_log(
    rule = "precision_tier",
    n_affected = sum(!is.na(precision_tier_f)),
    details = sprintf(
      "Aliased from lat_precision; descending-order ordered factor (rooftop > ... > none); %d unmapped value(s) coerced to 'unknown'.",
      as.integer(n_unknown)),
    severity = "INFO"
  )

  # ---- 2.2 in_alabama (logical; NA-able) ----------------------------------
  has_latlng_final <- all(c("lat_final", "lng_final") %in% names(data_in))
  if (has_latlng_final) {
    lat_v <- as.numeric(data_in$lat_final)
    lng_v <- as.numeric(data_in$lng_final)
    in_al <- rep(NA, n_rows)
    valid <- !is.na(lat_v) & !is.na(lng_v)
    in_al[valid] <- (lat_v[valid] >= 30 & lat_v[valid] <= 36 &
                      lng_v[valid] >= -89 & lng_v[valid] <= -84)
    in_al <- as.logical(in_al)
  } else {
    in_al <- rep(NA, n_rows)
  }
  data_out$in_alabama <- in_al
  add_log(
    rule = "in_alabama",
    n_affected = sum(!is.na(in_al)),
    details = sprintf(
      "lat_final in [30, 36] AND lng_final in [-89, -84]; %d TRUE / %d FALSE / %d NA",
      sum(in_al, na.rm = TRUE),
      sum(!in_al & !is.na(in_al)),
      sum(is.na(in_al))),
    severity = "INFO"
  )

  # ---- 2.3 county_check_match (logical; NA-able) --------------------------
  # adece_county is NOT part of the 29-column Melissa contract. In Step 5.1
  # standalone runs the column is absent and county_check_match is NA for
  # every row. Fixtures (e.g., G16) may attach `adece_county` to exercise
  # the comparison path; linkage diagnostics can consume the materialized
  # comparison when one is supplied.
  ccm <- rep(NA, n_rows)
  has_adece_county <- "adece_county" %in% names(data_in)
  has_melissa_county <- "COUNTYNAME" %in% names(data_in)
  if (has_adece_county && has_melissa_county) {
    ad_chr <- tolower(trimws(as.character(data_in$adece_county)))
    me_chr <- tolower(trimws(as.character(data_in$COUNTYNAME)))
    valid <- !is.na(ad_chr) & nzchar(ad_chr) &
             !is.na(me_chr) & nzchar(me_chr)
    ccm[valid] <- ad_chr[valid] == me_chr[valid]
    ccm <- as.logical(ccm)
    add_log(
      rule = "county_check_match",
      n_affected = sum(!is.na(ccm)),
      details = sprintf(
        "Compared Melissa COUNTYNAME vs. adece_county sidecar (case-insensitive); %d match / %d mismatch / %d NA",
        sum(ccm, na.rm = TRUE),
        sum(!ccm & !is.na(ccm)),
        sum(is.na(ccm))),
      severity = "INFO"
    )
  } else {
    data_out$county_check_match <- ccm
    add_log(
      rule = "county_check_match",
      n_affected = 0L,
      details = paste0(
        "adece_county sidecar absent; column is NA in Step 5.1 standalone. ",
        "Linkage diagnostics consume it when a sidecar is supplied."),
      severity = "INFO"
    )
  }
  data_out$county_check_match <- ccm

  # ---- 2.4 coord_age_years (integer; NA-able) -----------------------------
  cycle_year_chr <- as.character(meta_in$cycle_year %||% NA_character_)
  cy_first <- if (length(cycle_year_chr) == 1L && !is.na(cycle_year_chr) &&
                   nchar(cycle_year_chr) >= 4L) {
    suppressWarnings(as.integer(substr(cycle_year_chr, 1L, 4L)))
  } else {
    NA_integer_
  }
  if ("school_year" %in% names(data_in)) {
    sy_chr <- as.character(data_in$school_year)
    sy_first <- suppressWarnings(as.integer(substr(sy_chr, 1L, 4L)))
  } else {
    sy_first <- rep(NA_integer_, n_rows)
  }
  if (!is.na(cy_first) && length(cy_first) == 1L) {
    coord_age <- cy_first - sy_first
  } else {
    coord_age <- rep(NA_integer_, n_rows)
  }
  coord_age <- suppressWarnings(as.integer(coord_age))
  data_out$coord_age_years <- coord_age
  add_log(
    rule = "coord_age_years",
    n_affected = sum(!is.na(coord_age)),
    details = sprintf(
      "cycle_year_first (%s) - school_year_first; range [%s, %s] over %d non-NA value(s).",
      if (is.na(cy_first)) "NA" else as.character(cy_first),
      if (all(is.na(coord_age))) "NA" else as.character(min(coord_age, na.rm = TRUE)),
      if (all(is.na(coord_age))) "NA" else as.character(max(coord_age, na.rm = TRUE)),
      sum(!is.na(coord_age))),
    severity = "INFO"
  )

  # ---- 2.5 geocode_run_id (character; panel-stable) ------------------------
  if (inherits(delivery_date, "Date") && !is.na(delivery_date)) {
    date_token <- format(delivery_date, "%Y-%m")
  } else {
    date_token <- "unknown-date"
  }
  run_id <- sprintf("%s_v1_%s",
                    if (nzchar(vendor)) vendor else "unknown",
                    date_token)
  data_out$geocode_run_id <- rep(run_id, n_rows)
  add_log(
    rule = "geocode_run_id",
    n_affected = n_rows,
    details = sprintf("Snapshot identifier '%s' (same value for all rows).",
                      run_id),
    severity = "INFO"
  )

  # ---- 3. Assemble outputs --------------------------------------------------
  transform_log <- if (length(log_rows) > 0L) {
    do.call(rbind, log_rows)
  } else {
    tibble::tibble(rule = character(0), n_affected = integer(0),
                   details = character(0), severity = character(0))
  }

  out_data <- tibble::as_tibble(data_out)
  # Preserve any data_source_map attribute from upstream.
  attr(out_data, "data_source_map") <- attr(data_in, "data_source_map")

  meta_out <- c(meta_in, list(
    transformed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    geocode_run_id = run_id
  ))

  structure(
    list(
      data          = out_data,
      transform_log = transform_log,
      meta          = meta_out
    ),
    class = c("alprek_geocode_master", "list")
  )
}


#' Print method for `alprek_geocode_master`
#'
#' @param x An `alprek_geocode_master` object.
#' @param ... Ignored.
#' @export
print.alprek_geocode_master <- function(x, ...) {
  m <- x$meta
  cat("<alprek_geocode_master>\n")
  cat("  geocode_run_id:   ", m$geocode_run_id %||% "?", "\n", sep = "")
  cat("  Vendor / cycle:   ",
      m$source %||% "?", " / ", m$cycle_year %||% "?", "\n", sep = "")
  cat("  Delivery date:    ",
      if (is.null(m$receipt_date)) "?" else format(m$receipt_date), "\n",
      sep = "")
  cat("  Rows:             ", nrow(x$data),
      " (", ncol(x$data), " cols)\n", sep = "")

  # Quick summary of in_alabama if present
  if ("in_alabama" %in% names(x$data)) {
    ia <- x$data$in_alabama
    cat("    in_alabama:     ",
        sum(ia, na.rm = TRUE), " TRUE / ",
        sum(!ia & !is.na(ia)), " FALSE / ",
        sum(is.na(ia)), " NA\n", sep = "")
  }

  # Highest precision count
  if ("precision_tier" %in% names(x$data)) {
    pt <- x$data$precision_tier
    tab <- table(factor(pt, levels = c("rooftop", "parcel", "zip4", "zip5",
                                        "centroid", "area", "unknown",
                                        "none")))
    cat("    precision_tier: ",
        paste(sprintf("%s=%d", names(tab), as.integer(tab)),
              collapse = ", "),
        "\n", sep = "")
  }

  # coord_model_status pass-through summary (Phase 5 contract).
  if ("coord_model_status" %in% names(x$data)) {
    cm <- as.character(x$data$coord_model_status)
    tab <- table(factor(cm, levels = c("missing", "not_model_ready",
                                        "provisional_followup",
                                        "model_ready")))
    cat("    coord_model_status: ",
        paste(sprintf("%s=%d", names(tab), as.integer(tab)),
              collapse = ", "),
        "\n", sep = "")
  }

  cat("  Transform log:    ", nrow(x$transform_log), " rule(s)\n",
      sep = "")
  cat("  Transformed at:   ", m$transformed_at %||% "?", "\n", sep = "")
  invisible(x)
}


# ---------------------------------------------------------------------------
# %||% fallback (local; rlang re-exports this, but keep self-contained for
# parity with R/geocode-reconcile.R).
# ---------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a
