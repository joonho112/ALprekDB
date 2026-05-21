# tests/testthat/helper-geocode-fixtures.R
#
# Synthetic fixture generators for geocoding edge-case testing.
# Each fixture deliberately injects ONE edge case (G01-G18) into a
# baseline 5-row tibble whose schema mirrors the 29-column Melissa v1
# delivery contract. Mirrors the pattern in helper-applications-fixtures.R.

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
    md_street   <- sprintf("%d Main St", 100L * seq_len(n_rows))

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
      df$latitude[1]   <- NA_real_
      df$longitude[1]  <- NA_real_
      df$has_latlon[1] <- FALSE
      df$LAT[1]        <- NA_character_
      df$LNG[1]        <- NA_character_
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
      df$DPB[1]   <- NA_character_
      .ret(df, 1L, "WARN", "melissa", TRUE, "g05_drift")
    },
    "G06" = {
      df$LAT[1] <- sprintf("%.6f", df$latitude[1] - 0.45)
      df$LNG[1] <- sprintf("%.6f", df$longitude[1] - 0.20)
      df$RESULTCODE[1] <- "GS06"
      df$STATUSCODE[1] <- "A"
      .ret(df, 1L, "WARN", "disputed_melissa", TRUE, "g06_gross_outlier")
    },
    "G07" = {
      df$latitude[1]   <- NA_real_
      df$longitude[1]  <- NA_real_
      df$has_latlon[1] <- FALSE
      df$RESULTCODE[1] <- "GS05"
      df$STATUSCODE[1] <- "B"
      .ret(df, 1L, "INFO", "melissa", FALSE, "g07_adece_missing_rescue")
    },
    "G08" = {
      df$latitude[1]   <- NA_real_
      df$longitude[1]  <- NA_real_
      df$has_latlon[1] <- FALSE
      df$RESULTCODE[1] <- "GS03"
      df$STATUSCODE[1] <- "5"
      df$PLUS4[1] <- NA_character_
      df$DPB[1]   <- NA_character_
      .ret(df, 1L, "WARN", "melissa", TRUE, "g08_adece_missing_lowprec")
    },
    "G09" = {
      df$LAT[1] <- NA_character_
      df$LNG[1] <- NA_character_
      .ret(df, 1L, "ERROR", "adece", FALSE, "g09_melissa_missing")
    },
    "G10" = {
      df$site_code[1] <- NA_character_
      df$row_id[1]    <- sprintf("%s_new_%04d", "2025-2026", 1L)
      df$school_year[1] <- "2025-2026_new"
      .ret(df, 1L, "INFO", "melissa", TRUE, "g10_site_code_missing")
    },
    "G11" = {
      df$latitude[1]  <- 40.7128
      df$longitude[1] <- -74.0060
      df$has_latlon[1] <- TRUE
      .ret(df, 1L, "WARN", "melissa", TRUE, "g11_adece_out_of_bounds")
    },
    "G12" = {
      df$LAT[1] <- sprintf("%.6f", 40.7128)
      df$LNG[1] <- sprintf("%.6f", -74.0060)
      .ret(df, 1L, "ERROR", "adece", TRUE, "g12_melissa_out_of_bounds")
    },
    "G13" = {
      df$RESULTCODE[1] <- "GS99"
      df$STATUSCODE[1] <- "Z"
      .ret(df, 1L, "WARN", "melissa", TRUE, "g13_unknown_resultcode")
    },
    "G14" = {
      df$latitude[1]   <- NA_real_
      df$longitude[1]  <- NA_real_
      df$has_latlon[1] <- TRUE
      .ret(df, 1L, "ERROR", "none", FALSE, "g14_has_latlon_inconsistent")
    },
    "G15" = {
      df$row_id[2] <- df$row_id[1]
      .ret(df, c(1L, 2L), "ERROR", "none", FALSE, "g15_row_id_duplicate")
    },
    "G16" = {
      df$site_city[1]   <- "Loachapoka"
      df$md_city[1]     <- "Loachapoka"
      df$LAT[1] <- sprintf("%.6f", 32.6000)
      df$LNG[1] <- sprintf("%.6f", -85.5930)
      df$latitude[1]  <- 32.9076
      df$longitude[1] <- -85.4337
      df$COUNTYNAME[1] <- "Lee"
      df$FIPS[1]       <- "01081"
      df$RESULTCODE[1] <- "GS03"
      df$STATUSCODE[1] <- "5"
      df$PLUS4[1] <- NA_character_
      df$DPB[1]   <- NA_character_
      df$adece_county <- c("Chambers",
                            df$COUNTYNAME[seq.int(2L, n_rows)])
      .ret(df, 1L, "WARN", "disputed_melissa", TRUE, "g16_county_mismatch")
    },
    "G17" = {
      yrs <- c("2021-2022", "2022-2023", "2023-2024")
      df3 <- .fixture_base_geocode_rows(n_rows = 3L, seed = seed)
      df3$school_year <- yrs
      df3$site_code   <- rep("841P004103", 3L)
      df3$site_name   <- rep("Persistent Outlier Site", 3L)
      df3$row_id      <- sprintf("%s_%s", yrs, df3$site_code)
      df3$latitude[]  <- 32.60003
      df3$longitude[] <- -85.59304
      df3$LAT[] <- sprintf("%.6f", 32.9076)
      df3$LNG[] <- sprintf("%.6f", -85.4337)
      df3$RESULTCODE[] <- "GS03"
      df3$STATUSCODE[] <- "5"
      df3$PLUS4[] <- NA_character_
      df3$DPB[]   <- NA_character_
      df3$COUNTYNAME[] <- "Lee"
      df3$FIPS[]       <- "01081"
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
    stop(sprintf(
      "Unknown case_id '%s'. Supported: G01-G18.", case_id),
      call. = FALSE)
  )
}


load_geocode_edge_cases_codebook <- function() {
  path <- system.file("extdata", "codebooks",
                       "geocode_edge_cases.csv",
                       package = "ALprekDB", mustWork = TRUE)
  suppressMessages(readr::read_csv(path, show_col_types = FALSE,
                                     progress = FALSE))
}
