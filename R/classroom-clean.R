#' Clean Classroom Detail Data
#'
#' @description Applies comprehensive cleaning to raw classroom data: renames
#'   columns using codebook mappings, standardizes administrative variables,
#'   classifies free-text teacher degree/credential fields, normalizes
#'   demographic variables, and derives new variables.
#'
#' @param raw_obj An `alprek_classroom_raw` object from [classroom_read()].
#' @param include_dob Logical. If `TRUE`, includes Date of Birth columns in
#'   the output (new format only). Default `FALSE` for PII protection.
#'
#' @return An `alprek_classroom_clean` S3 object (list) with elements:
#'   - `data`: tibble of cleaned classroom data.
#'   - `meta`: list of metadata.
#'   - `degree_audit`: tibble of unmatched degree text values.
#'
#' @examples
#' \dontrun{
#' raw <- classroom_read("FCPK Classroom Details 21-22.xlsx")
#' clean <- classroom_clean(raw)
#' clean$data
#' clean$degree_audit  # check unmatched values
#' }
#'
#' @importFrom dplyr mutate select any_of all_of rename_with case_when
#' @importFrom tibble tibble as_tibble
#' @importFrom stringr str_squish str_trim str_detect str_to_title
#' @export
classroom_clean <- function(raw_obj, include_dob = FALSE) {

  if (!inherits(raw_obj, "alprek_classroom_raw")) {
    stop("Expected an 'alprek_classroom_raw' object. Use classroom_read() first.",
         call. = FALSE)
  }

  format_type <- raw_obj$meta$format
  school_year <- raw_obj$meta$school_year
  year <- raw_obj$meta$year

  msg_info("Cleaning classroom data for {school_year} ({format_type} format)")

  # Load codebooks
  column_map <- .load_classroom_column_map(format_type)
  degree_patterns <- alprek_degree_patterns()
  race_mapping <- alprek_race_mapping()
  language_mapping <- alprek_language_mapping()

  # Step 1: Rename columns
  df <- .rename_classroom_columns(raw_obj$data, column_map)
  msg_info("Renamed {ncol(df)} columns")

  # Ensure school_year and year columns
  df$school_year <- school_year
  df$year <- year

  # Step 2: Clean administrative variables
  df <- .clean_administrative_vars(df)

  # Step 3: Clean grant amounts
  df <- .clean_grant_amounts(df)

  # Step 4: Clean geographic variables
  df <- .clean_geographic_vars(df)


  # Step 5: Combine staff names
  df <- .clean_staff_contacts(df)

  # Step 6: Clean teacher roles (lead, aux, 2nd aux)
  teacher_roles <- list(
    list(prefix = "lead_tch", label = "Lead Teacher"),
    list(prefix = "aux_tch", label = "Aux Teacher"),
    list(prefix = "second_aux", label = "2nd Aux Teacher")
  )

  all_unmatched <- tibble::tibble()
  for (role in teacher_roles) {
    result <- .clean_teacher_role(
      df, role$prefix, degree_patterns, race_mapping, language_mapping
    )
    df <- result$data
    if (nrow(result$unmatched) > 0) {
      result$unmatched$teacher_role <- role$label
      all_unmatched <- dplyr::bind_rows(all_unmatched, result$unmatched)
    }
    msg_info("Cleaned {role$label} demographics")
  }

  # Step 7: Remove PII columns if not requested
  if (!include_dob) {
    dob_cols <- grep("_dob$|_username$", names(df), value = TRUE)
    if (length(dob_cols) > 0) {
      df <- df[, !names(df) %in% dob_cols, drop = FALSE]
      msg_info("Removed {length(dob_cols)} PII columns (DOB/UserName)")
    }
  }

  # Step 8: Order columns logically
  df <- .order_classroom_columns(df)

  # Build result
  result <- structure(
    list(
      data = df,
      meta = list(
        school_year = school_year,
        year = year,
        format = format_type,
        n_classrooms = nrow(df),
        n_columns = ncol(df),
        include_dob = include_dob,
        cleaned_at = Sys.time()
      ),
      degree_audit = all_unmatched
    ),
    class = "alprek_classroom_clean"
  )

  n_unmatched <- nrow(all_unmatched)
  if (n_unmatched > 0) {
    msg_warn("Degree classification: {n_unmatched} unmatched unique value(s). See $degree_audit")
  }

  msg_success("Cleaned {nrow(df)} classrooms ({ncol(df)} columns) for {school_year}")
  result
}


#' Print method for alprek_classroom_clean
#' @param x An alprek_classroom_clean object.
#' @param ... Ignored.
#' @export
print.alprek_classroom_clean <- function(x, ...) {
  cat("<alprek_classroom_clean>\n")
  cat("  School year:", x$meta$school_year, "\n")
  cat("  Format:", x$meta$format, "\n")
  cat("  Classrooms:", x$meta$n_classrooms, "\n")
  cat("  Columns:", x$meta$n_columns, "\n")
  cat("  DOB included:", x$meta$include_dob, "\n")
  if (nrow(x$degree_audit) > 0) {
    cat("  Unmatched degree values:", nrow(x$degree_audit), "\n")
  }
  invisible(x)
}


# ==========================================================================
# Internal helpers
# ==========================================================================

#' Rename columns using the column map CSV
#' @keywords internal
.rename_classroom_columns <- function(df, column_map) {
  # Build a named vector: old name -> new name
  raw_names <- column_map$raw_column
  std_names <- column_map$standard_name

  # Match current column names to the map
  current_names <- names(df)
  rename_map <- stats::setNames(std_names, raw_names)

  # Apply renaming for columns that exist
  new_names <- vapply(current_names, function(cn) {
    if (cn %in% names(rename_map)) rename_map[[cn]] else cn
  }, character(1), USE.NAMES = FALSE)

  names(df) <- new_names
  df
}


#' Clean administrative variables
#' @keywords internal
.clean_administrative_vars <- function(df) {
  # Region: ensure integer
  if ("region" %in% names(df)) {
    df$region <- suppressWarnings(as.integer(df$region))
  }

  # County code: ensure character, zero-padded
  if ("county_num" %in% names(df)) {
    df$county_code <- sprintf("%03d", suppressWarnings(as.integer(df$county_num)))
    # Keep county_num for reference, add cleaned county_code
  }

  # Delivery type: map codes to full names, then create factor
  if ("delivery_type" %in% names(df)) {
    dt_levels <- c("Public School", "Private Child Care", "Head Start",
                   "Community Organization", "Faith-Based Organization",
                   "University Operated", "Private School")
    # Load delivery type code mapping
    dt_codes <- alprek_delivery_types()
    dt_map <- stats::setNames(dt_codes$name, dt_codes$code)

    raw_dt <- stringr::str_trim(as.character(df$delivery_type))
    # Map single-letter codes to full names; keep full names as-is
    mapped_dt <- vapply(raw_dt, function(v) {
      if (is.na(v) || nchar(v) == 0) return(NA_character_)
      if (v %in% names(dt_map)) return(unname(dt_map[v]))
      if (v %in% dt_levels) return(v)
      NA_character_
    }, character(1), USE.NAMES = FALSE)

    df$delivery_type <- factor(mapped_dt, levels = dt_levels)
  }

  # Title I: clean string to Y/N/NA
  if ("title_i" %in% names(df)) {
    raw_ti <- stringr::str_trim(as.character(df$title_i))
    raw_ti[raw_ti %in% c("NA", "na", "N/A", "")] <- NA_character_
    df$title_i <- raw_ti
    # Numeric version
    df$title_i_numeric <- dplyr::case_when(
      raw_ti == "Y" ~ 1,
      raw_ti == "N" ~ 0,
      TRUE ~ NA_real_
    )
  }

  # Turnaround school: clean to Y/N
  if ("turnaround_school" %in% names(df)) {
    df$turnaround_school <- stringr::str_trim(as.character(df$turnaround_school))
    df$turnaround_school[df$turnaround_school %in% c("NA", "na", "N/A", "")] <- NA_character_
  }

  # Year first funded: ensure integer
  if ("year_first_funded" %in% names(df)) {
    df$year_first_funded <- suppressWarnings(as.integer(df$year_first_funded))
  }

  # Seat count (new format): ensure integer
  if ("seat_count" %in% names(df)) {
    df$seat_count <- suppressWarnings(as.integer(df$seat_count))
  }

  df
}


#' Clean grant amount columns
#' @keywords internal
.clean_grant_amounts <- function(df) {
  grant_cols <- c("classroom_grant", "enhancement_grant",
                  "salary_enhancement_grant", "equity_grant", "total_grant")
  for (col in intersect(grant_cols, names(df))) {
    df[[col]] <- currency_to_numeric(df[[col]])
  }
  df
}


#' Clean geographic variables
#' @keywords internal
.clean_geographic_vars <- function(df) {
  # Latitude/Longitude: ensure numeric
  if ("latitude" %in% names(df)) {
    df$latitude <- suppressWarnings(as.numeric(df$latitude))
  }
  if ("longitude" %in% names(df)) {
    df$longitude <- suppressWarnings(as.numeric(df$longitude))
  }

  # Senate/House district: ensure integer
  if ("senate_dist" %in% names(df)) {
    df$senate_dist <- suppressWarnings(as.integer(df$senate_dist))
  }
  if ("house_dist" %in% names(df)) {
    df$house_dist <- suppressWarnings(as.integer(df$house_dist))
  }

  df
}


#' Combine first name + last name into single name columns for staff
#' @keywords internal
.clean_staff_contacts <- function(df) {
  # Staff roles that have fname/lname columns
  staff_roles <- c("rd", "director", "admin", "principal", "asst_principal",
                   "cfo", "registrar", "monitor", "coach")

  for (role in staff_roles) {
    fname_col <- paste0(role, "_fname")
    lname_col <- paste0(role, "_lname")
    name_col <- paste0(role, "_name")

    if (fname_col %in% names(df) && lname_col %in% names(df)) {
      df[[name_col]] <- .concat_name(df[[fname_col]], df[[lname_col]])
      # Remove individual fname/lname columns
      df[[fname_col]] <- NULL
      df[[lname_col]] <- NULL
    }
  }

  df
}


#' Clean a single teacher role's data
#' @return List with `data` (updated df) and `unmatched` (tibble of unmatched degree values).
#' @keywords internal
.clean_teacher_role <- function(df, prefix, degree_patterns, race_mapping,
                                language_mapping) {
  degree_col <- paste0(prefix, "_degree_raw")
  race_col <- paste0(prefix, "_race_raw")
  eth_col <- paste0(prefix, "_ethnicity_raw")
  gender_col <- paste0(prefix, "_gender_raw")
  lang_col <- paste0(prefix, "_fluent_lang_raw")
  cert_col <- paste0(prefix, "_cert_num")
  osr_exp_col <- paste0(prefix, "_osr_exp")
  total_exp_col <- paste0(prefix, "_total_exp")
  fname_col <- paste0(prefix, "_fname")
  lname_col <- paste0(prefix, "_lname")

  unmatched <- tibble::tibble()

  # --- Degree classification ---
  if (degree_col %in% names(df)) {
    degree_result <- .classify_degree_vectorized(
      df[[degree_col]], degree_patterns
    )
    df[[paste0(prefix, "_degree_level")]] <- degree_result$degree_level
    df[[paste0(prefix, "_degree_area")]] <- degree_result$degree_area
    df[[paste0(prefix, "_degree_area_simple")]] <- degree_result$degree_area_simple
    df[[paste0(prefix, "_waiver")]] <- degree_result$waiver
    unmatched <- degree_result$unmatched
  }

  # --- Name concat ---
  if (fname_col %in% names(df) && lname_col %in% names(df)) {
    df[[paste0(prefix, "_name")]] <- .concat_name(df[[fname_col]], df[[lname_col]])
    df[[fname_col]] <- NULL
    df[[lname_col]] <- NULL
  }

  # --- Race ---
  if (race_col %in% names(df)) {
    df[[paste0(prefix, "_race")]] <- .clean_race(df[[race_col]], race_mapping)
    # Keep raw column for auditing
  }

  # --- Ethnicity ---
  if (eth_col %in% names(df)) {
    df[[paste0(prefix, "_ethnicity")]] <- .clean_ethnicity(df[[eth_col]])
    # Keep raw column
  }

  # --- Gender ---
  if (gender_col %in% names(df)) {
    df[[paste0(prefix, "_gender")]] <- .clean_gender(df[[gender_col]])
    # Keep raw column
  }

  # --- Language ---
  if (lang_col %in% names(df)) {
    df[[paste0(prefix, "_fluent_language")]] <- .clean_fluent_language(
      df[[lang_col]], language_mapping
    )
  }

  # --- Emergency cert ---
  if (cert_col %in% names(df)) {
    df[[paste0(prefix, "_emergency")]] <- .derive_emergency(df[[cert_col]])
  }

  # --- Experience: ensure numeric ---
  if (osr_exp_col %in% names(df)) {
    df[[osr_exp_col]] <- suppressWarnings(as.numeric(df[[osr_exp_col]]))
  }
  if (total_exp_col %in% names(df)) {
    df[[total_exp_col]] <- suppressWarnings(as.numeric(df[[total_exp_col]]))
  }

  list(data = df, unmatched = unmatched)
}


#' Classify degree text into level and area (vectorized)
#' @param raw_texts Character vector of raw degree/credential text.
#' @param degree_patterns Degree patterns codebook tibble.
#' @return List with vectors: degree_level, degree_area, degree_area_simple,
#'   waiver, and a tibble of unmatched values.
#' @keywords internal
.classify_degree_vectorized <- function(raw_texts, degree_patterns) {
  n <- length(raw_texts)

  degree_level <- rep(NA_character_, n)
  degree_area <- rep(NA_character_, n)
  degree_area_simple <- rep(NA_character_, n)
  waiver <- rep(NA_integer_, n)

  # Separate pattern types
  level_pats <- degree_patterns[degree_patterns$pattern_type == "degree_level", ]
  area_pats <- degree_patterns[degree_patterns$pattern_type == "degree_area", ]
  consolidation <- degree_patterns[degree_patterns$pattern_type == "degree_area_consolidation", ]

  # Sort by priority (descending) — highest priority first

  level_pats <- level_pats[order(-level_pats$priority), ]
  area_pats <- area_pats[order(-area_pats$priority), ]

  # Build consolidation lookup
  consol_map <- stats::setNames(consolidation$result, consolidation$regex)

  for (i in seq_len(n)) {
    txt <- raw_texts[i]
    if (is.na(txt) || nchar(stringr::str_trim(txt)) == 0) {
      waiver[i] <- NA_integer_
      next
    }

    txt <- stringr::str_squish(txt)

    # --- Degree level ---
    for (j in seq_len(nrow(level_pats))) {
      if (grepl(level_pats$regex[j], txt, perl = TRUE)) {
        degree_level[i] <- level_pats$result[j]
        break
      }
    }

    # --- Check for waiver override ---
    # Waiver has highest priority (100). If detected, override everything.
    is_waiver <- !is.na(degree_level[i]) && degree_level[i] == "Waiver"
    waiver[i] <- as.integer(is_waiver)

    if (is_waiver) {
      degree_area[i] <- "Waiver"
      degree_area_simple[i] <- "Waiver"
      next
    }

    # --- Degree area ---
    for (j in seq_len(nrow(area_pats))) {
      if (grepl(area_pats$regex[j], txt, perl = TRUE)) {
        degree_area[i] <- area_pats$result[j]
        break
      }
    }

    # If degree level matched but area didn't, set area to "Other"
    if (!is.na(degree_level[i]) && is.na(degree_area[i])) {
      degree_area[i] <- "Other"
    }

    # --- Consolidate area ---
    if (!is.na(degree_area[i])) {
      if (degree_area[i] %in% names(consol_map)) {
        degree_area_simple[i] <- unname(consol_map[degree_area[i]])
      } else {
        degree_area_simple[i] <- "Other"
      }
    }

    waiver[i] <- 0L
  }

  # Create ordered factors for degree_level
  level_order <- c("Waiver", "Doctoral degree", "Ed.S.", "Master's degree",
                   "Bachelor's degree", "Associate degree", "College Coursework")
  degree_level <- factor(degree_level, levels = level_order, ordered = FALSE)

  # Track unmatched values
  unmatched_idx <- which(
    !is.na(raw_texts) &
    nchar(stringr::str_trim(raw_texts)) > 0 &
    is.na(as.character(degree_level))
  )
  unmatched_vals <- unique(raw_texts[unmatched_idx])
  unmatched_tbl <- tibble::tibble(
    raw_value = unmatched_vals,
    n_occurrences = vapply(unmatched_vals, function(v) {
      sum(raw_texts == v, na.rm = TRUE)
    }, integer(1))
  )

  list(
    degree_level = degree_level,
    degree_area = degree_area,
    degree_area_simple = degree_area_simple,
    waiver = waiver,
    unmatched = unmatched_tbl
  )
}


#' Clean race values using mapping
#' @keywords internal
.clean_race <- function(raw_race, race_mapping) {
  # Build lookup
  lookup <- stats::setNames(race_mapping$standardized, race_mapping$raw_value)
  race_levels <- c("White", "Black", "Latino/Hispanic", "Asian",
                   "Mixed", "Other", "Unknown")

  cleaned <- vapply(as.character(raw_race), function(v) {
    if (is.na(v) || nchar(stringr::str_trim(v)) == 0) return(NA_character_)
    v <- stringr::str_trim(v)
    if (v %in% names(lookup)) return(unname(lookup[v]))
    NA_character_
  }, character(1), USE.NAMES = FALSE)

  factor(cleaned, levels = race_levels)
}


#' Clean ethnicity values
#' @keywords internal
.clean_ethnicity <- function(raw_ethnicity) {
  cleaned <- vapply(as.character(raw_ethnicity), function(v) {
    if (is.na(v) || nchar(stringr::str_trim(v)) == 0) return(NA_character_)
    v <- stringr::str_trim(v)
    if (grepl("(?i)^hispanic$", v, perl = TRUE)) return("Hispanic")
    if (grepl("(?i)non.?hispanic", v, perl = TRUE)) return("Non-Hispanic")
    if (grepl("(?i)^not\\s", v, perl = TRUE)) return("Non-Hispanic")
    NA_character_
  }, character(1), USE.NAMES = FALSE)

  factor(cleaned, levels = c("Hispanic", "Non-Hispanic"))
}


#' Clean gender values
#' @keywords internal
.clean_gender <- function(raw_gender) {
  cleaned <- vapply(as.character(raw_gender), function(v) {
    if (is.na(v) || nchar(stringr::str_trim(v)) == 0) return(NA_character_)
    v <- stringr::str_trim(v)
    if (grepl("(?i)^f", v, perl = TRUE)) return("Female")
    if (grepl("(?i)^m", v, perl = TRUE)) return("Male")
    NA_character_
  }, character(1), USE.NAMES = FALSE)

  factor(cleaned, levels = c("Female", "Male"))
}


#' Clean fluent language field
#' @keywords internal
.clean_fluent_language <- function(raw_lang, language_mapping) {
  # Build lookups
  null_vals <- language_mapping$raw_value[language_mapping$is_null == TRUE]
  lang_lookup <- language_mapping[language_mapping$is_null == FALSE, ]
  lang_map <- stats::setNames(lang_lookup$standardized, lang_lookup$raw_value)

  cleaned <- vapply(as.character(raw_lang), function(v) {
    if (is.na(v) || nchar(stringr::str_trim(v)) == 0) return(NA_character_)
    v <- stringr::str_trim(v)
    # Check null values first
    if (v %in% null_vals) return(NA_character_)
    # Check exact match
    if (v %in% names(lang_map)) return(unname(lang_map[v]))
    # Case-insensitive match
    for (k in seq_along(lang_map)) {
      if (tolower(v) == tolower(names(lang_map)[k])) {
        return(unname(lang_map[k]))
      }
    }
    # Return trimmed value as-is if no match (it's a real language)
    stringr::str_to_title(v)
  }, character(1), USE.NAMES = FALSE)

  cleaned
}


#' Derive emergency certification flag from cert number
#' @keywords internal
.derive_emergency <- function(cert_num) {
  vapply(as.character(cert_num), function(v) {
    if (is.na(v) || nchar(stringr::str_trim(v)) == 0) return(NA_integer_)
    v <- stringr::str_trim(v)
    if (grepl("(?i)emerg", v, perl = TRUE)) return(1L)
    0L
  }, integer(1), USE.NAMES = FALSE)
}


#' Concatenate first and last name, handling NA
#' @keywords internal
.concat_name <- function(fname, lname) {
  fname <- as.character(fname)
  lname <- as.character(lname)

  vapply(seq_along(fname), function(i) {
    f <- stringr::str_trim(fname[i])
    l <- stringr::str_trim(lname[i])

    # Both NA or empty
    f_empty <- is.na(f) || nchar(f) == 0 || f == "NA"
    l_empty <- is.na(l) || nchar(l) == 0 || l == "NA"

    if (f_empty && l_empty) return(NA_character_)
    if (f_empty) return(l)
    if (l_empty) return(f)

    result <- paste(f, l)
    if (result == "NA NA") return(NA_character_)
    result
  }, character(1))
}


#' Order columns in logical groups
#' @keywords internal
.order_classroom_columns <- function(df) {
  # Define preferred column order groups
  id_cols <- c("classroom_code", "classroom_name", "school_year", "year")
  admin_cols <- c("region", "county_code", "county_num", "county_name",
                  "delivery_type", "program_code", "site_num", "class_num")
  char_cols <- c("title_i", "title_i_numeric", "turnaround_school",
                 "class_type", "year_first_funded", "description",
                 "seat_count", "gold_curriculum_type", "gold_class_level",
                 "fund_source")
  grant_cols <- c("classroom_grant", "enhancement_grant",
                  "salary_enhancement_grant", "equity_grant", "total_grant")
  location_cols <- c("program_name", "program_street", "program_city",
                     "program_state", "program_zip", "site_name", "site_code",
                     "site_code_formula", "site_street", "site_city",
                     "site_state", "site_zip", "latitude", "longitude",
                     "senate_dist", "house_dist", "site_affiliation_id")
  contact_cols <- c("program_phone", "site_phone", "gold_country_id",
                    "vendor_code", "address_code",
                    "rd_name", "rd_email",
                    "director_name", "director_email",
                    "admin_name", "admin_email",
                    "principal_name", "principal_email",
                    "asst_principal_name", "asst_principal_email",
                    "cfo_name", "cfo_email",
                    "registrar_name", "registrar_email",
                    "monitor_name", "monitor_email",
                    "coach_name", "coach_email")
  lead_cols <- c("lead_tch_name", "lead_tch_email", "lead_tch_cert_num",
                 "lead_tch_emergency",
                 "lead_tch_degree_raw", "lead_tch_degree_level",
                 "lead_tch_degree_area", "lead_tch_degree_area_simple",
                 "lead_tch_waiver",
                 "lead_tch_osr_exp", "lead_tch_total_exp",
                 "lead_tch_race_raw", "lead_tch_race",
                 "lead_tch_ethnicity_raw", "lead_tch_ethnicity",
                 "lead_tch_gender_raw", "lead_tch_gender",
                 "lead_tch_fluent_lang_raw", "lead_tch_fluent_language")
  aux_cols <- gsub("^lead_tch_", "aux_tch_", lead_cols)
  sec_aux_cols <- gsub("^lead_tch_", "second_aux_", lead_cols)

  preferred <- c(id_cols, admin_cols, char_cols, grant_cols, location_cols,
                 contact_cols, lead_cols, aux_cols, sec_aux_cols)

  # Intersect with actual columns, then append any remaining
  existing_preferred <- intersect(preferred, names(df))
  remaining <- setdiff(names(df), existing_preferred)

  df[, c(existing_preferred, remaining), drop = FALSE]
}
