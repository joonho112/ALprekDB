#' Generate Synthetic Geocoded Master Data
#'
#' @description Creates a synthetic geocoded master tibble matching the
#'   29-column Melissa v1 delivery contract. Mirrors the empirical
#'   distributions observed in the v0.8.0 audit (school_year, RESULTCODE,
#'   ADECE-coord missing patterns, AL geography) but uses fake site_codes
#'   (`999P`-prefix) and synthetic anchors so that examples cannot be
#'   confused with confidential ADECE source records.
#'
#'   Designed for vignette, tests, and demonstrations. Returns a flat
#'   tibble (not an S3 panel object) so callers can hand it directly into
#'   `geocode_clean()`, `geocode_validate()`, and `geocode_reconcile()`.
#'
#'   When `edge_case` is non-NULL (one of `"G01"`-`"G18"`), returns a
#'   package-internal 5-row fixture with one row deliberately mutated to
#'   trigger that edge case. Useful for golden tests of `geocode_validate()`
#'   / `geocode_reconcile()` behavior.
#'
#' @param n_sites Integer. Number of distinct sites (renewal pattern).
#'   Default `50L`. Sites are stable across years (same `row_id`,
#'   `site_code`, `site_name` repeated).
#' @param n_years Integer. Number of school years to span. Default `3L`.
#'   Most-recent year is `cycle_year_anchor` and the panel extends
#'   backward. Total rows = `n_sites * n_years` (plus a small "_new"
#'   cohort if `n_years >= 4`).
#' @param share_missing_adece Numeric between 0 and 1, inclusive. Share
#'   of rows where the ADECE `latitude`/`longitude` is `NA` (and
#'   `has_latlon` is `FALSE`). Default `0.10` (deliberately higher than
#'   the v0.8.0 empirical ~5.4% so the synthetic signal is unambiguous in
#'   tests).
#' @param share_missing_site_code Numeric between 0 and 1, inclusive.
#'   Share of rows where `site_code` is `NA` (the `_new` cohort pattern).
#'   Default `0.03`.
#' @param share_high_resultcode_agreement Numeric between 0 and 1,
#'   inclusive. Share of rows assigned `RESULTCODE == "GS05"` (rooftop,
#'   high agreement quality). The remainder split across `c("GS06",
#'   "GS03", "GS01")` at the v0.8.0 empirical ratios. Default `0.7`.
#' @param edge_case Character or NULL. If non-NULL, must be one of
#'   `"G01"`..`"G18"`; the function returns the package-internal
#'   fixture's `$data` tibble. Default `NULL` (generate full synthetic
#'   panel).
#' @param cycle_year_anchor Integer. Anchor year (Y for the
#'   `"YYYY-YYYY+1"` school_year of the most recent observation). The
#'   synthetic panel extends backward by `n_years - 1` years from this
#'   anchor. Default `2024L` (so anchor school_year is `"2024-2025"`).
#' @param seed Integer. Random seed for reproducibility. Default
#'   `20260520L`.
#'
#' @return A tibble with 29 columns matching the Melissa v1 column map
#'   (see `alprek_geocode_column_map()`):
#'   * 5 id cols: row_id, school_year, site_name, site_code,
#'     geocode_address
#'   * 7 adece cols: site_street, site_city, site_state, site_zip,
#'     latitude, longitude, has_latlon
#'   * 6 melissa_norm cols: md_street, md_city, md_state, GEOZIP, PLUS4,
#'     DPB
#'   * 11 melissa_out cols: LAT, LNG, CT, CENSUSBLOC, FIPS, COUNTYNAME,
#'     PLACENAME, PLACECODE, RESULTCODE, STATUSCODE, ERRORCODE
#'
#'   Notable dtype contracts (v0.8.0):
#'   * `LAT` and `LNG` are CHARACTER (Melissa source contract; coerced
#'     to numeric only in `geocode_clean()`).
#'   * `ERRORCODE` is LOGICAL and 100% `NA` (v0.8.0 contract).
#'   * `latitude` / `longitude` are NUMERIC (ADECE source).
#'
#' @examples
#' # Default invocation: 50 sites x 3 years ~ 150 rows
#' g <- alprek_synthetic_geocode()
#' nrow(g)
#' ncol(g)
#'
#' # Smaller panel for tests
#' g_small <- alprek_synthetic_geocode(n_sites = 10, n_years = 2,
#'                                       seed = 42)
#' nrow(g_small)
#'
#' # Edge-case mini-fixture (G05: drift; ADECE-Melissa distance 1-10km)
#' g_g05 <- alprek_synthetic_geocode(edge_case = "G05")
#' nrow(g_g05)  # 5 rows; row 1 has the drift mutation
#'
#' @importFrom tibble tibble
#' @export
alprek_synthetic_geocode <- function(n_sites = 50L,
                                       n_years = 3L,
                                       share_missing_adece = 0.10,
                                       share_missing_site_code = 0.03,
                                       share_high_resultcode_agreement = 0.7,
                                       edge_case = NULL,
                                       cycle_year_anchor = 2024L,
                                       seed = 20260520L) {

  # ---- edge-case shortcut: delegate to fixture builder ----
  if (!is.null(edge_case)) {
    if (!is.character(edge_case) || length(edge_case) != 1L ||
        !nzchar(edge_case)) {
      stop("edge_case must be a single non-empty character (e.g., 'G05').",
           call. = FALSE)
    }
    edge_case <- toupper(edge_case)
    if (!grepl("^G(0[1-9]|1[0-8])$", edge_case)) {
      stop("edge_case must be one of 'G01'..'G18'. Got: ", edge_case,
           call. = FALSE)
    }
    fx <- make_geocode_edge_case_fixture(case_id = edge_case,
                                          n_rows = 5L,
                                          seed = as.integer(seed))
    return(fx$data)
  }

  # ---- validation ----
  stopifnot(
    is.numeric(n_sites), length(n_sites) == 1L, n_sites >= 1,
    is.numeric(n_years), length(n_years) == 1L, n_years >= 1,
    is.numeric(share_missing_adece), length(share_missing_adece) == 1L,
    share_missing_adece >= 0, share_missing_adece <= 1,
    is.numeric(share_missing_site_code), length(share_missing_site_code) == 1L,
    share_missing_site_code >= 0, share_missing_site_code <= 1,
    is.numeric(share_high_resultcode_agreement),
    length(share_high_resultcode_agreement) == 1L,
    share_high_resultcode_agreement >= 0,
    share_high_resultcode_agreement <= 1,
    is.numeric(cycle_year_anchor), length(cycle_year_anchor) == 1L,
    cycle_year_anchor >= 2000, cycle_year_anchor <= 2100,
    is.numeric(seed), length(seed) == 1L
  )

  n_sites <- as.integer(n_sites)
  n_years <- as.integer(n_years)
  cycle_year_anchor <- as.integer(cycle_year_anchor)
  seed <- as.integer(seed)

  withr::with_seed(seed, {

    # ---- AL anchors: top-10 cities by population for diversity ----
    al_anchors <- tibble::tibble(
      site_city   = c("Birmingham", "Montgomery", "Huntsville",
                       "Mobile", "Tuscaloosa", "Decatur",
                       "Auburn", "Florence", "Selma", "Anniston"),
      county      = c("Jefferson", "Montgomery", "Madison",
                       "Mobile", "Tuscaloosa", "Morgan",
                       "Lee", "Lauderdale", "Dallas", "Calhoun"),
      county_fips = c("01073", "01101", "01089", "01097", "01125",
                       "01103", "01081", "01077", "01047", "01015"),
      lat         = c(33.5207, 32.3668, 34.7304, 30.6954, 33.2098,
                       34.6059, 32.6099, 34.7998, 32.4074, 33.6598),
      lng         = c(-86.8025, -86.2999, -86.5861, -88.0399, -87.5692,
                       -86.9833, -85.4808, -87.6773, -87.0211, -85.8316),
      zip5        = c("35203", "36104", "35801", "36602", "35401",
                       "35601", "36830", "35630", "36701", "36201")
    )

    # ---- build school_year vector (panel goes backward from anchor) ----
    yr_starts <- seq.int(cycle_year_anchor - n_years + 1L,
                          cycle_year_anchor)
    school_years <- sprintf("%d-%d", yr_starts, yr_starts + 1L)

    # ---- site metadata (stable across years: renewal pattern) ----
    site_idx_to_anchor <- ((seq_len(n_sites) - 1L) %%
                              nrow(al_anchors)) + 1L
    site_anchors <- al_anchors[site_idx_to_anchor, , drop = FALSE]

    site_codes_master  <- sprintf("999P%06d", seq_len(n_sites))
    site_names_master  <- sprintf("Synthetic Site %04d", seq_len(n_sites))
    site_streets_master <- sprintf("%d MAIN ST",
                                    100L * seq_len(n_sites))

    # ---- expand to year x site panel ----
    n_total <- n_sites * n_years
    site_pos <- rep(seq_len(n_sites), times = n_years)
    yr_pos   <- rep(seq_len(n_years), each = n_sites)

    row_school_year <- school_years[yr_pos]
    row_site_code   <- site_codes_master[site_pos]
    row_site_name   <- site_names_master[site_pos]
    row_site_street <- site_streets_master[site_pos]
    row_site_city   <- site_anchors$site_city[site_pos]
    row_county      <- site_anchors$county[site_pos]
    row_county_fips <- site_anchors$county_fips[site_pos]
    row_lat_anchor  <- site_anchors$lat[site_pos]
    row_lng_anchor  <- site_anchors$lng[site_pos]
    row_zip5        <- site_anchors$zip5[site_pos]

    # ---- per-row jitter so years aren't identical ----
    jitter_lat       <- stats::runif(n_total, -0.0008,  0.0008)
    jitter_lng       <- stats::runif(n_total, -0.0008,  0.0008)
    melissa_jit_lat  <- stats::runif(n_total, -0.0004,  0.0004)
    melissa_jit_lng  <- stats::runif(n_total, -0.0004,  0.0004)

    adece_lat   <- row_lat_anchor + jitter_lat
    adece_lng   <- row_lng_anchor + jitter_lng
    melissa_lat <- row_lat_anchor + melissa_jit_lat
    melissa_lng <- row_lng_anchor + melissa_jit_lng

    # ---- ADECE coord missingness (Bernoulli) ----
    n_missing_adece <- round(n_total * share_missing_adece)
    if (n_missing_adece > 0L) {
      idx_missing_adece <- sample.int(n_total, size = n_missing_adece)
      adece_lat[idx_missing_adece] <- NA_real_
      adece_lng[idx_missing_adece] <- NA_real_
    }
    has_latlon <- !is.na(adece_lat)

    # ---- row_id and site_code (renewal vs new) ----
    row_id   <- sprintf("%s_%s", row_school_year, row_site_code)
    site_code <- row_site_code

    # share_missing_site_code: NA out site_code and re-pattern row_id
    n_missing_sc <- round(n_total * share_missing_site_code)
    if (n_missing_sc > 0L) {
      idx_missing_sc <- sample.int(n_total, size = n_missing_sc)
      site_code[idx_missing_sc] <- NA_character_
      row_school_year[idx_missing_sc] <- "2025-2026_new"
      row_id[idx_missing_sc] <- sprintf(
        "2025-2026_new_%04d", seq_along(idx_missing_sc)
      )
    }

    # ---- RESULTCODE / STATUSCODE distribution ----
    # share_high_resultcode_agreement => GS05; rest split across
    # GS06 / GS03 / GS01 at v0.8.0 empirical ratios:
    # GS06 0.092, GS03 0.074, GS01 0.008 -> normalize residual to 1
    residual <- 1 - share_high_resultcode_agreement
    if (residual <= 0) {
      rest_probs <- c(0, 0, 0)
    } else {
      raw <- c(0.092, 0.074, 0.008)
      raw <- raw / sum(raw)
      rest_probs <- raw * residual
    }
    rc_levels <- c("GS05", "GS06", "GS03", "GS01")
    rc_probs  <- c(share_high_resultcode_agreement, rest_probs)
    rc_probs  <- rc_probs / sum(rc_probs)  # safety renormalization
    RESULTCODE <- sample(rc_levels, size = n_total, replace = TRUE,
                          prob = rc_probs)
    # STATUSCODE: 1:1 with RESULTCODE per v0.8.0
    sc_map <- c(GS05 = "B", GS06 = "A", GS03 = "5", GS01 = "9")
    STATUSCODE <- unname(sc_map[RESULTCODE])

    # PLUS4 / DPB: NA when RESULTCODE == 'GS03' (v0.8.0 pattern)
    PLUS4 <- sprintf("%04d",
                     seq.int(1234L, length.out = n_total, by = 11L) %%
                       10000L)
    DPB   <- sprintf("%02d",
                     seq.int(10L, length.out = n_total, by = 7L) %% 100L)
    gs03_mask <- RESULTCODE == "GS03"
    if (any(gs03_mask)) {
      PLUS4[gs03_mask] <- NA_character_
      DPB[gs03_mask]   <- NA_character_
    }

    # ---- assemble tibble (29 cols, order matches column map) ----
    tibble::tibble(
      # id (5)
      row_id          = row_id,
      school_year     = row_school_year,
      site_name       = row_site_name,
      site_code       = site_code,
      geocode_address = sprintf("%s, %s, AL, %s",
                                  row_site_street, row_site_city,
                                  row_zip5),
      # adece (7)
      site_street     = row_site_street,
      site_city       = row_site_city,
      site_state      = rep("AL", n_total),
      site_zip        = as.numeric(row_zip5),
      latitude        = adece_lat,
      longitude       = adece_lng,
      has_latlon      = has_latlon,
      # melissa_norm (6)
      md_street       = sprintf("%d Main St",
                                  100L * site_pos),
      md_city         = row_site_city,
      md_state        = rep("AL", n_total),
      GEOZIP          = row_zip5,
      PLUS4           = PLUS4,
      DPB             = DPB,
      # melissa_out (11)
      LAT             = sprintf("%.6f", melissa_lat),
      LNG             = sprintf("%.6f", melissa_lng),
      CT              = sprintf("0%05d",
                                  (seq.int(100100L,
                                           length.out = n_total,
                                           by = 7L)) %% 1000000L),
      CENSUSBLOC      = sprintf("%04d",
                                  (seq.int(1001L,
                                           length.out = n_total,
                                           by = 3L)) %% 10000L),
      FIPS            = row_county_fips,
      COUNTYNAME      = row_county,
      PLACENAME       = row_site_city,
      PLACECODE       = sprintf("01%05d",
                                  (seq.int(50000L,
                                           length.out = n_total,
                                           by = 5L)) %% 100000L),
      RESULTCODE      = RESULTCODE,
      STATUSCODE      = STATUSCODE,
      ERRORCODE       = rep(NA, n_total)
    )
  })
}


# Internal edge-case fixtures used by alprek_synthetic_geocode(edge_case = ).
# These are intentionally package-runtime helpers, not test-only helpers, so
# examples and installed-package tests exercise the same path that users get.
.fixture_base_geocode_rows <- function(n_rows = 5L,
                                       school_year = "2024-2025",
                                       seed = 42L) {
  stopifnot(is.numeric(n_rows), n_rows >= 1L)
  withr::with_seed(seed, {
    al_anchors <- tibble::tibble(
      site_city   = c("Birmingham", "Montgomery", "Huntsville",
                       "Mobile", "Tuscaloosa"),
      county      = c("Jefferson", "Montgomery", "Madison",
                       "Mobile", "Tuscaloosa"),
      county_fips = c("01073", "01101", "01089", "01097", "01125"),
      lat         = c(33.5207, 32.3668, 34.7304, 30.6954, 33.2098),
      lng         = c(-86.8025, -86.2999, -86.5861, -88.0399, -87.5692),
      zip5        = c("35203", "36104", "35801", "36602", "35401")
    )
    idx <- ((seq_len(n_rows) - 1L) %% nrow(al_anchors)) + 1L
    anchors <- al_anchors[idx, , drop = FALSE]

    jitter_lat <- stats::runif(n_rows, min = -0.0008, max = 0.0008)
    jitter_lng <- stats::runif(n_rows, min = -0.0008, max = 0.0008)
    melissa_jit_lat <- stats::runif(n_rows, min = -0.0004, max = 0.0004)
    melissa_jit_lng <- stats::runif(n_rows, min = -0.0004, max = 0.0004)

    adece_lat <- anchors$lat + jitter_lat
    adece_lng <- anchors$lng + jitter_lng
    melissa_lat <- anchors$lat + melissa_jit_lat
    melissa_lng <- anchors$lng + melissa_jit_lng

    site_code <- sprintf("999P%06d", seq_len(n_rows))
    site_name <- sprintf("Test Site %d", seq_len(n_rows))
    site_street <- sprintf("%d MAIN ST", 100L * seq_len(n_rows))
    md_street <- sprintf("%d Main St", 100L * seq_len(n_rows))

    tibble::tibble(
      row_id          = sprintf("%s_%s", school_year, site_code),
      school_year     = rep(school_year, n_rows),
      site_name       = site_name,
      site_code       = site_code,
      geocode_address = sprintf("%s, %s, AL, %s",
                                 site_street, anchors$site_city,
                                 anchors$zip5),
      site_street     = site_street,
      site_city       = anchors$site_city,
      site_state      = rep("AL", n_rows),
      site_zip        = as.numeric(anchors$zip5),
      latitude        = adece_lat,
      longitude       = adece_lng,
      has_latlon      = rep(TRUE, n_rows),
      md_street       = md_street,
      md_city         = anchors$site_city,
      md_state        = rep("AL", n_rows),
      GEOZIP          = anchors$zip5,
      PLUS4           = sprintf("%04d", seq.int(1234L, by = 11L,
                                                  length.out = n_rows)),
      DPB             = sprintf("%02d", seq.int(10L, by = 7L,
                                                   length.out = n_rows)),
      LAT             = sprintf("%.6f", melissa_lat),
      LNG             = sprintf("%.6f", melissa_lng),
      CT              = sprintf("0%05d", seq.int(100100L, by = 7L,
                                                    length.out = n_rows)),
      CENSUSBLOC      = sprintf("%04d",
                                  seq.int(1001L, by = 3L,
                                          length.out = n_rows)),
      FIPS            = anchors$county_fips,
      COUNTYNAME      = anchors$county,
      PLACENAME       = anchors$site_city,
      PLACECODE       = sprintf("01%05d", seq.int(50000L, by = 5L,
                                                     length.out = n_rows)),
      RESULTCODE      = rep("GS05", n_rows),
      STATUSCODE      = rep("B", n_rows),
      ERRORCODE       = rep(NA, n_rows)
    )
  })
}


make_geocode_edge_case_fixture <- function(case_id, n_rows = 5L,
                                           seed = 42L) {
  case_id <- toupper(case_id)
  stopifnot(is.character(case_id), length(case_id) == 1L)

  .ret <- function(data, bad_idx, severity, lat_source, needs_fu, fname) {
    list(
      case_id                          = case_id,
      data                             = tibble::as_tibble(data),
      bad_row_index                    = as.integer(bad_idx),
      expected_severity                = severity,
      expected_reconciler_lat_source   = lat_source,
      expected_needs_followup          = needs_fu,
      fixture_name                     = fname
    )
  }

  df <- .fixture_base_geocode_rows(n_rows = n_rows, seed = seed)

  switch(case_id,
    "G01" = {
      df$latitude[1] <- NA_real_
      df$longitude[1] <- NA_real_
      df$has_latlon[1] <- FALSE
      df$LAT[1] <- NA_character_
      df$LNG[1] <- NA_character_
      .ret(df, 1L, "ERROR", "none", TRUE, "g01_both_missing")
    },
    "G02" = {
      df$LAT[1] <- sprintf("%.6f", df$latitude[1])
      df$LNG[1] <- sprintf("%.6f", df$longitude[1])
      .ret(df, 1L, "PASS", "melissa", FALSE, "g02_exact_agreement")
    },
    "G03" = {
      df$LAT[1] <- sprintf("%.6f", df$latitude[1] + 0.00045)
      df$LNG[1] <- sprintf("%.6f", df$longitude[1])
      .ret(df, 1L, "PASS", "melissa", FALSE, "g03_tight_agreement")
    },
    "G04" = {
      df$LAT[1] <- sprintf("%.6f", df$latitude[1] + 0.0045)
      df$LNG[1] <- sprintf("%.6f", df$longitude[1])
      df$RESULTCODE[1] <- "GS06"
      df$STATUSCODE[1] <- "A"
      .ret(df, 1L, "INFO", "melissa", FALSE, "g04_loose_agreement")
    },
    "G05" = {
      df$LAT[1] <- sprintf("%.6f", df$latitude[1] + 0.027)
      df$LNG[1] <- sprintf("%.6f", df$longitude[1])
      df$RESULTCODE[1] <- "GS03"
      df$STATUSCODE[1] <- "5"
      df$PLUS4[1] <- NA_character_
      df$DPB[1] <- NA_character_
      .ret(df, 1L, "WARN", "melissa", TRUE, "g05_drift")
    },
    "G06" = {
      df$LAT[1] <- sprintf("%.6f", df$latitude[1] - 0.45)
      df$LNG[1] <- sprintf("%.6f", df$longitude[1] - 0.20)
      df$RESULTCODE[1] <- "GS06"
      df$STATUSCODE[1] <- "A"
      .ret(df, 1L, "WARN", "disputed_melissa", TRUE,
            "g06_gross_outlier")
    },
    "G07" = {
      df$latitude[1] <- NA_real_
      df$longitude[1] <- NA_real_
      df$has_latlon[1] <- FALSE
      df$RESULTCODE[1] <- "GS05"
      df$STATUSCODE[1] <- "B"
      .ret(df, 1L, "INFO", "melissa", FALSE,
            "g07_adece_missing_rescue")
    },
    "G08" = {
      df$latitude[1] <- NA_real_
      df$longitude[1] <- NA_real_
      df$has_latlon[1] <- FALSE
      df$RESULTCODE[1] <- "GS03"
      df$STATUSCODE[1] <- "5"
      df$PLUS4[1] <- NA_character_
      df$DPB[1] <- NA_character_
      .ret(df, 1L, "WARN", "melissa", TRUE,
            "g08_adece_missing_lowprec")
    },
    "G09" = {
      df$LAT[1] <- NA_character_
      df$LNG[1] <- NA_character_
      .ret(df, 1L, "ERROR", "adece", FALSE, "g09_melissa_missing")
    },
    "G10" = {
      df$site_code[1] <- NA_character_
      df$row_id[1] <- sprintf("%s_new_%04d", "2025-2026", 1L)
      df$school_year[1] <- "2025-2026_new"
      .ret(df, 1L, "INFO", "melissa", TRUE, "g10_site_code_missing")
    },
    "G11" = {
      df$latitude[1] <- 40.7128
      df$longitude[1] <- -74.0060
      df$has_latlon[1] <- TRUE
      .ret(df, 1L, "WARN", "melissa", TRUE,
            "g11_adece_out_of_bounds")
    },
    "G12" = {
      df$LAT[1] <- sprintf("%.6f", 40.7128)
      df$LNG[1] <- sprintf("%.6f", -74.0060)
      .ret(df, 1L, "ERROR", "adece", TRUE,
            "g12_melissa_out_of_bounds")
    },
    "G13" = {
      df$RESULTCODE[1] <- "GS99"
      df$STATUSCODE[1] <- "Z"
      .ret(df, 1L, "WARN", "melissa", TRUE,
            "g13_unknown_resultcode")
    },
    "G14" = {
      df$latitude[1] <- NA_real_
      df$longitude[1] <- NA_real_
      df$has_latlon[1] <- TRUE
      .ret(df, 1L, "ERROR", "none", FALSE,
            "g14_has_latlon_inconsistent")
    },
    "G15" = {
      df$row_id[2] <- df$row_id[1]
      .ret(df, c(1L, 2L), "ERROR", "none", FALSE,
            "g15_row_id_duplicate")
    },
    "G16" = {
      df$site_city[1] <- "Loachapoka"
      df$md_city[1] <- "Loachapoka"
      df$LAT[1] <- sprintf("%.6f", 32.6000)
      df$LNG[1] <- sprintf("%.6f", -85.5930)
      df$latitude[1] <- 32.9076
      df$longitude[1] <- -85.4337
      df$COUNTYNAME[1] <- "Lee"
      df$FIPS[1] <- "01081"
      df$RESULTCODE[1] <- "GS03"
      df$STATUSCODE[1] <- "5"
      df$PLUS4[1] <- NA_character_
      df$DPB[1] <- NA_character_
      df$adece_county <- c("Chambers",
                            df$COUNTYNAME[seq.int(2L, n_rows)])
      .ret(df, 1L, "WARN", "disputed_melissa", TRUE,
            "g16_county_mismatch")
    },
    "G17" = {
      yrs <- c("2021-2022", "2022-2023", "2023-2024")
      df3 <- .fixture_base_geocode_rows(n_rows = 3L, seed = seed)
      df3$school_year <- yrs
      df3$site_code <- rep("841P004103", 3L)
      df3$site_name <- rep("Persistent Outlier Site", 3L)
      df3$row_id <- sprintf("%s_%s", yrs, df3$site_code)
      df3$latitude[] <- 32.60003
      df3$longitude[] <- -85.59304
      df3$LAT[] <- sprintf("%.6f", 32.9076)
      df3$LNG[] <- sprintf("%.6f", -85.4337)
      df3$RESULTCODE[] <- "GS03"
      df3$STATUSCODE[] <- "5"
      df3$PLUS4[] <- NA_character_
      df3$DPB[] <- NA_character_
      df3$COUNTYNAME[] <- "Lee"
      df3$FIPS[] <- "01081"
      .ret(df3, seq_len(3L), "INFO", "disputed_melissa", TRUE,
            "g17_persistent_multiyear")
    },
    "G18" = {
      df$RESULTCODE[1] <- "GS05"
      df$STATUSCODE[1] <- "B"
      df$LAT[1] <- sprintf("%.6f", df$latitude[1] + 0.06)
      df$LNG[1] <- sprintf("%.6f", df$longitude[1])
      .ret(df, 1L, "WARN", "disputed_melissa", TRUE,
            "g18_surprising_gs05")
    },
    stop(sprintf("Unknown case_id '%s'. Supported: G01-G18.", case_id),
         call. = FALSE)
  )
}
