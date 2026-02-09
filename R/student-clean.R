#' Clean Student/Child Detail Data
#'
#' @description Transforms raw student data into analysis-ready format by
#'   renaming columns, standardizing demographics, parsing income, cleaning
#'   attendance data, processing assessment scores (GOLD, PPVT, eDECA, ASQ),
#'   deriving indicators (age, IEP2, binary dummies), and optionally removing
#'   PII fields.
#'
#' @param raw_obj An `alprek_student_raw` object from [student_read()].
#' @param include_pii Logical. If `FALSE` (default), removes personally
#'   identifiable information (child names, guardian info, state/student
#'   IDs). DOB and derived variables (age, birth_year) are always retained.
#'
#' @return An `alprek_student_clean` S3 object (list) with elements:
#'   - `data`: Cleaned tibble of student data.
#'   - `meta`: list of metadata.
#'   - `cleaning_log`: list of cleaning statistics.
#'
#' @examples
#' \dontrun{
#' raw <- student_read("FCPK Student Details 21-22.xlsx", "2021-2022")
#' clean <- student_clean(raw)
#' clean <- student_clean(raw, include_pii = TRUE)
#' }
#'
#' @importFrom dplyr mutate across if_else case_when select any_of all_of
#' @importFrom stringr str_detect str_extract str_remove_all str_trim str_squish
#' @importFrom lubridate year month day
#' @export
student_clean <- function(raw_obj, include_pii = FALSE) {

  if (!inherits(raw_obj, "alprek_student_raw")) {
    stop("Expected an 'alprek_student_raw' object. Use student_read() first.",
         call. = FALSE)
  }

  school_year <- raw_obj$meta$school_year
  year_val <- raw_obj$meta$year
  format_type <- raw_obj$meta$format
  df <- raw_obj$data

  msg_info("Cleaning student data for {school_year} ({format_type} format)")

  # --- Step 1: Load codebooks and rename columns ---
  column_map <- .load_student_column_map(format_type)
  race_mapping <- alprek_student_race_mapping()
  delivery_mapping <- alprek_student_delivery_mapping()

  df <- .rename_student_columns(df, column_map)
  msg_info("Renamed {ncol(df)} columns")

  # Add school_year and year
  df$school_year <- school_year
  df$year <- year_val

  # --- Step 2: Clean demographics ---
  df <- .clean_student_demographics(df, race_mapping, school_year)
  msg_info("Cleaned student demographics")

  # --- Step 3: Clean delivery type ---
  dt_result <- .clean_student_delivery_type(df, delivery_mapping)
  df <- dt_result$data
  n_dt_standardized <- dt_result$n_standardized

  # --- Step 4: Clean family/income variables ---
  income_result <- .clean_family_income(df)
  df <- income_result$data
  n_income_parsed <- income_result$n_parsed
  n_num_house_text <- income_result$n_text_converted

  # --- Step 5: Clean service indicators ---
  df <- .clean_service_indicators(df)
  msg_info("Cleaned service indicators")

  # --- Step 6: Clean attendance ---
  att_result <- .clean_attendance(df)
  df <- att_result$data
  n_neg_absent <- att_result$n_negative_absent
  n_extreme_absent <- att_result$n_extreme_absent
  n_neg_tardy <- att_result$n_negative_tardy

  # --- Step 7: Clean IEP referral + derive iep2 ---
  df <- .clean_iep_referral(df)
  msg_info("Cleaned IEP referral and derived iep2")

  # --- Step 8: Clean demographic flags ---
  df <- .clean_demographic_flags(df)

  # --- Step 9: Clean GOLD assessments ---
  df <- .clean_gold_assessments(df)
  msg_info("Cleaned GOLD assessments")

  # --- Step 10: Clean PPVT ---
  df <- .clean_ppvt(df)

  # --- Step 11: Clean eDECA (Pre AND Post) ---
  df <- .clean_edeca(df)
  msg_info("Cleaned eDECA assessments (Pre and Post)")

  # --- Step 12: Clean ASQ ---
  df <- .clean_asq(df)

  # --- Step 13: Clean staff contacts (new format only) ---
  if (format_type == "new") {
    df <- .clean_student_staff_contacts(df)
  }

  # --- Step 14: Remove PII if not requested ---
  n_pii_removed <- 0L
  if (!include_pii) {
    pii_cols <- .get_student_pii_columns(format_type)
    pii_present <- pii_cols[pii_cols %in% names(df)]
    n_pii_removed <- length(pii_present)
    if (n_pii_removed > 0) {
      df <- df[, !(names(df) %in% pii_present), drop = FALSE]
      msg_info("Removed {n_pii_removed} PII columns")
    }
  }

  # --- Step 15: Order columns ---
  df <- .order_student_columns(df)

  # Build cleaning log
  cleaning_log <- list(
    n_negative_absent = n_neg_absent,
    n_extreme_absent = n_extreme_absent,
    n_negative_tardy = n_neg_tardy,
    n_income_parsed = n_income_parsed,
    n_num_house_text = n_num_house_text,
    n_delivery_type_standardized = n_dt_standardized,
    n_pii_removed = n_pii_removed
  )

  result <- structure(
    list(
      data = df,
      meta = list(
        school_year = school_year,
        year = year_val,
        format = format_type,
        n_students = nrow(df),
        n_cols = ncol(df),
        cleaned_at = Sys.time()
      ),
      cleaning_log = cleaning_log
    ),
    class = "alprek_student_clean"
  )

  msg_success("Cleaned {nrow(df)} students ({ncol(df)} columns) for {school_year}")
  result
}


#' Print method for alprek_student_clean
#' @param x An alprek_student_clean object.
#' @param ... Ignored.
#' @export
print.alprek_student_clean <- function(x, ...) {
  cat("<alprek_student_clean>\n")
  cat("  School year:", x$meta$school_year, "\n")
  cat("  Format:", x$meta$format, "\n")
  cat("  Students:", x$meta$n_students, "\n")
  cat("  Columns:", x$meta$n_cols, "\n")
  log <- x$cleaning_log
  if (log$n_negative_absent > 0 || log$n_extreme_absent > 0) {
    cat("  Attendance fixes: abs()=", log$n_negative_absent,
        ", extreme_cap=", log$n_extreme_absent, "\n")
  }
  invisible(x)
}


# ==============================================================================
# Internal helper functions
# ==============================================================================

#' Rename student columns using CSV column map
#' @keywords internal
.rename_student_columns <- function(df, column_map) {
  raw_names <- names(df)
  map_raw <- column_map$raw_column
  map_std <- column_map$standard_name

  # Build lookup: squished raw_column -> standard_name
  lookup <- stats::setNames(map_std, stringr::str_squish(map_raw))

  # Rename matched columns
  new_names <- raw_names
  for (i in seq_along(raw_names)) {
    squished <- stringr::str_squish(raw_names[i])
    if (squished %in% names(lookup)) {
      new_names[i] <- lookup[[squished]]
    }
  }
  names(df) <- new_names
  df
}


#' Clean student demographics
#' @keywords internal
.clean_student_demographics <- function(df, race_mapping, school_year) {
  # Gender: M/F -> factor(Male, Female)
  if ("gender" %in% names(df)) {
    df$gender <- factor(
      dplyr::case_when(
        df$gender == "M" ~ "Male",
        df$gender == "F" ~ "Female",
        TRUE ~ NA_character_
      ),
      levels = c("Male", "Female")
    )
  }

  # Race: standardize via CSV mapping
  if ("race" %in% names(df)) {
    lookup <- stats::setNames(race_mapping$standardized, race_mapping$raw_value)
    race_levels <- c("White", "Black", "Latino/Hispanic", "Asian", "Other", "Mixed", "Unknown")
    standardized <- lookup[as.character(df$race)]
    df$race <- factor(unname(standardized), levels = race_levels)
  }

  # Ethnicity: factor
  if ("ethnicity" %in% names(df)) {
    df$ethnicity <- factor(df$ethnicity, levels = c("Hispanic", "Non-Hispanic"))
  }

  # Derive birth_year and age from DOB
  if ("dob" %in% names(df)) {
    # Ensure DOB is Date
    if (!inherits(df$dob, "Date") && !inherits(df$dob, "POSIXct")) {
      df$dob <- as.Date(df$dob)
    }

    df$birth_year <- lubridate::year(df$dob)

    # Age as of August 1 of school year start
    start_year <- as.integer(substr(school_year, 1, 4))
    aug1 <- as.Date(paste0(start_year, "-08-01"))

    # Calculate age: years between DOB and Aug 1
    df$age <- as.integer(floor(as.numeric(difftime(aug1, df$dob, units = "days")) / 365.25))
  }

  df
}


#' Clean delivery type using CSV mapping
#' @keywords internal
.clean_student_delivery_type <- function(df, delivery_mapping) {
  n_standardized <- 0L

  if ("delivery_type" %in% names(df)) {
    lookup <- stats::setNames(delivery_mapping$standardized, delivery_mapping$raw_value)

    raw_dt <- as.character(df$delivery_type)
    standardized <- lookup[raw_dt]

    # Count how many were actually different (standardized)
    n_standardized <- sum(!is.na(standardized) & raw_dt != standardized, na.rm = TRUE)

    # For any unmatched values, try case-insensitive match
    unmatched <- is.na(standardized) & !is.na(raw_dt)
    if (any(unmatched)) {
      lower_lookup <- stats::setNames(delivery_mapping$standardized,
                                       tolower(delivery_mapping$raw_value))
      standardized[unmatched] <- lower_lookup[tolower(raw_dt[unmatched])]
    }

    dt_levels <- c("Public School", "Private Child Care", "Head Start",
                    "Faith-Based Organization", "Community Organization",
                    "University Operated", "Private School")
    df$delivery_type <- factor(unname(standardized), levels = dt_levels)
  }

  list(data = df, n_standardized = n_standardized)
}


#' Clean family/income variables
#' @keywords internal
.clean_family_income <- function(df) {
  n_parsed <- 0L
  n_text_converted <- 0L

  # --- Poverty indicator ---
  if ("poverty_ind" %in% names(df)) {
    df$poverty_dum <- dplyr::case_when(
      df$poverty_ind == "Yes" ~ 1L,
      df$poverty_ind == "No" ~ 0L,
      TRUE ~ NA_integer_
    )
    df$poverty_ind <- factor(
      dplyr::case_when(
        df$poverty_ind == "Yes" ~ "Poverty: Yes",
        df$poverty_ind == "No" ~ "Poverty: No",
        TRUE ~ NA_character_
      ),
      levels = c("Poverty: No", "Poverty: Yes")
    )
  }

  # --- Title One ---
  if ("title_one" %in% names(df)) {
    df$title_one_dum <- dplyr::case_when(
      df$title_one == "Y" ~ 1L,
      df$title_one == "N" ~ 0L,
      TRUE ~ NA_integer_
    )
  }

  # --- Language / English learner ---
  if ("language" %in% names(df)) {
    df$english_learner <- dplyr::if_else(
      !is.na(df$language) & df$language != "English", 1L, 0L
    )
  }

  # --- Lives with / single parent ---
  if ("lives_with" %in% names(df)) {
    df$single_parent <- dplyr::case_when(
      is.na(df$lives_with) ~ NA_integer_,
      df$lives_with == "Both" ~ 0L,
      TRUE ~ 1L
    )
    df$lives_with <- factor(df$lives_with,
                             levels = c("Both", "Parent/guardian 1", "Parent/guardian 2"))
  }

  # --- Number in house ---
  if ("num_in_house" %in% names(df)) {
    raw_nih <- as.character(df$num_in_house)

    # Track text conversions
    text_words <- c("four", "Four", "Three", "three", "Six", "six",
                     "Seven", "seven", "Five", "five")
    n_text_converted <- sum(raw_nih %in% text_words, na.rm = TRUE)

    df$num_in_house <- dplyr::case_when(
      tolower(raw_nih) == "four" ~ 4,
      tolower(raw_nih) == "three" ~ 3,
      tolower(raw_nih) == "six" ~ 6,
      tolower(raw_nih) == "seven" ~ 7,
      tolower(raw_nih) == "five" ~ 5,
      raw_nih == ";" ~ NA_real_,
      grepl("^\\d+$", raw_nih) ~ as.numeric(raw_nih),
      grepl("\\d", raw_nih) ~ as.numeric(gsub("[^0-9]", "", raw_nih)),
      TRUE ~ NA_real_
    )
  }

  # --- Gross income parsing ---
  if ("gross_income" %in% names(df)) {
    income_result <- .parse_gross_income(df$gross_income)
    df$lower_income <- income_result$lower
    df$upper_income <- income_result$upper
    df$gross_income_midpoint <- income_result$midpoint
    n_parsed <- sum(!is.na(income_result$midpoint))
  }

  # --- Poverty level percentage ---
  if ("pov_lvl_pct" %in% names(df)) {
    df$pov_lvl_pct <- factor(
      as.character(df$pov_lvl_pct),
      levels = c("0 to 200%", "201 to 300%", "301 to 400%", "Greater than 400%"),
      ordered = TRUE
    )
  }

  # --- Monthly fees ---
  if ("monthly_fees" %in% names(df)) {
    df$monthly_fees <- suppressWarnings(as.numeric(df$monthly_fees))
  }

  list(data = df, n_parsed = n_parsed, n_text_converted = n_text_converted)
}


#' Parse gross income ranges to numeric
#' @keywords internal
.parse_gross_income <- function(x) {
  n <- length(x)
  lower <- rep(NA_real_, n)
  upper <- rep(NA_real_, n)
  midpoint <- rep(NA_real_, n)

  raw <- as.character(x)

  for (i in seq_len(n)) {
    val <- raw[i]
    if (is.na(val) || nchar(stringr::str_trim(val)) == 0) next

    # Remove $, commas, em-dash encoding artifacts, and trim
    clean <- stringr::str_remove_all(val, "[$,]")
    # Handle em-dash variants
    clean <- gsub("\u2013", "-", clean)           # real em-dash
    clean <- gsub("\xe2\x80\x93", "-", clean)     # UTF-8 em-dash bytes
    clean <- gsub("\u00e2\u0080\u0093", "-", clean)
    clean <- stringr::str_trim(clean)

    # Extract all numbers
    nums <- as.numeric(regmatches(clean, gregexpr("[0-9]+\\.?[0-9]*", clean))[[1]])

    if (length(nums) == 0) next

    if (grepl("less than", clean, ignore.case = TRUE)) {
      # "less than $X" -> midpoint = X - 1
      lower[i] <- 0
      upper[i] <- nums[1]
      midpoint[i] <- nums[1] - 1
    } else if (grepl("more than|or more", clean, ignore.case = TRUE)) {
      # "$X or more" / "more than $X" -> midpoint = X
      lower[i] <- nums[1]
      upper[i] <- nums[1]
      midpoint[i] <- nums[1]
    } else if (length(nums) >= 2) {
      # "$X - $Y" or "$X to $Y"
      lower[i] <- nums[1]
      upper[i] <- nums[length(nums)]
      midpoint[i] <- (nums[1] + nums[length(nums)]) / 2
    } else if (length(nums) == 1) {
      lower[i] <- nums[1]
      upper[i] <- nums[1]
      midpoint[i] <- nums[1]
    }
  }

  list(lower = lower, upper = upper, midpoint = midpoint)
}


#' Clean service indicators
#' @keywords internal
.clean_service_indicators <- function(df) {

  # Simple Yes/No -> 0/1 binary columns
  binary_cols <- c("childcare_subsidy", "tanf", "wic", "free_reduced_lunch",
                    "snap", "iep", "foster_care", "homeless", "intervention_svc")
  for (col in binary_cols) {
    if (col %in% names(df)) {
      df[[col]] <- dplyr::case_when(
        df[[col]] == "Yes" ~ 1L,
        df[[col]] == "No" ~ 0L,
        TRUE ~ NA_integer_
      )
    }
  }

  # Duration-based services: head_start, center_childcare, home_childcare, home_visiting
  duration_cols <- c("head_start", "center_childcare", "home_childcare", "home_visiting")
  duration_levels <- c("No", "Yes, less than 1 year", "Yes, 1 year", "Yes, more than 1 year")
  dummy_names <- c("head_start_dum", "center_care_dum", "home_care_dum", "home_visiting_dum")

  for (j in seq_along(duration_cols)) {
    col <- duration_cols[j]
    dum <- dummy_names[j]
    if (col %in% names(df)) {
      # Standardize comma variants
      df[[col]] <- dplyr::case_when(
        df[[col]] == "No" ~ "No",
        grepl("less than 1", df[[col]], ignore.case = TRUE) ~ "Yes, less than 1 year",
        grepl("(?<!less than |more than )1 year$", df[[col]], perl = TRUE, ignore.case = TRUE) ~ "Yes, 1 year",
        grepl("more than 1", df[[col]], ignore.case = TRUE) ~ "Yes, more than 1 year",
        TRUE ~ NA_character_
      )
      df[[col]] <- factor(df[[col]], levels = duration_levels)

      # Binary dummy
      df[[dum]] <- dplyr::if_else(
        !is.na(df[[col]]) & df[[col]] != "No", 1L, 0L
      )
      df[[dum]] <- dplyr::if_else(is.na(df[[col]]), NA_integer_, df[[dum]])
    }
  }

  # Behavior support -> numeric
  if ("behavior_support" %in% names(df)) {
    df$behavior_support <- suppressWarnings(as.numeric(df$behavior_support))
  }

  df
}


#' Clean attendance variables
#' @keywords internal
.clean_attendance <- function(df) {
  n_negative_absent <- 0L
  n_extreme_absent <- 0L
  n_negative_tardy <- 0L

  # Days absent - abs() for negatives, cap extremes
  for (col in c("days_absent_sem1", "days_absent_sem2")) {
    if (col %in% names(df)) {
      vals <- suppressWarnings(as.numeric(df[[col]]))
      neg_count <- sum(vals < 0, na.rm = TRUE)
      n_negative_absent <- n_negative_absent + neg_count
      vals <- abs(vals)
      # Cap extreme values (>180 days in a semester is impossible)
      extreme_count <- sum(vals > 180, na.rm = TRUE)
      n_extreme_absent <- n_extreme_absent + extreme_count
      vals[vals > 180] <- NA_real_
      df[[col]] <- vals
    }
  }

  # Derive total absent
  if (all(c("days_absent_sem1", "days_absent_sem2") %in% names(df))) {
    df$days_absent_total <- dplyr::if_else(
      is.na(df$days_absent_sem1) | is.na(df$days_absent_sem2),
      NA_real_,
      df$days_absent_sem1 + df$days_absent_sem2
    )
  }

  # Days tardy - abs() for negatives
  for (col in c("days_tardy_sem1", "days_tardy_sem2")) {
    if (col %in% names(df)) {
      vals <- suppressWarnings(as.numeric(df[[col]]))
      neg_count <- sum(vals < 0, na.rm = TRUE)
      n_negative_tardy <- n_negative_tardy + neg_count
      vals <- abs(vals)
      df[[col]] <- vals
    }
  }

  # Derive total tardy
  if (all(c("days_tardy_sem1", "days_tardy_sem2") %in% names(df))) {
    df$days_tardy_total <- dplyr::if_else(
      is.na(df$days_tardy_sem1) | is.na(df$days_tardy_sem2),
      NA_real_,
      df$days_tardy_sem1 + df$days_tardy_sem2
    )
  }

  # Family hours - abs() for semester totals
  for (col in c("sem1_family_hrs", "sem2_family_hrs")) {
    if (col %in% names(df)) {
      vals <- suppressWarnings(as.numeric(df[[col]]))
      vals <- abs(vals)
      df[[col]] <- vals
    }
  }

  # Derive calculated family hours total
  if (all(c("sem1_family_hrs", "sem2_family_hrs") %in% names(df))) {
    df$family_hrs_calculated <- dplyr::if_else(
      is.na(df$sem1_family_hrs) | is.na(df$sem2_family_hrs),
      NA_real_,
      df$sem1_family_hrs + df$sem2_family_hrs
    )
  }

  # Ensure monthly hours are numeric
  monthly_cols <- c("aug_family_hrs", "sept_family_hrs", "oct_family_hrs",
                     "nov_family_hrs", "dec_family_hrs", "jan_family_hrs",
                     "feb_family_hrs", "mar_family_hrs", "apr_family_hrs",
                     "may_family_hrs")
  for (col in monthly_cols) {
    if (col %in% names(df)) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
  }

  # Ensure family_hrs_total is numeric
  if ("family_hrs_total" %in% names(df)) {
    df$family_hrs_total <- suppressWarnings(as.numeric(df$family_hrs_total))
  }

  list(
    data = df,
    n_negative_absent = n_negative_absent,
    n_extreme_absent = n_extreme_absent,
    n_negative_tardy = n_negative_tardy
  )
}


#' Clean IEP referral variables and derive iep2
#' @keywords internal
.clean_iep_referral <- function(df) {
  # Convert dates
  for (col in c("iep_ref_date", "part_b_date", "enrolled_date")) {
    if (col %in% names(df)) {
      if (!inherits(df[[col]], "Date") && !inherits(df[[col]], "POSIXct")) {
        df[[col]] <- as.Date(suppressWarnings(as.numeric(df[[col]])),
                              origin = "1899-12-30")
      } else if (inherits(df[[col]], "POSIXct")) {
        df[[col]] <- as.Date(df[[col]])
      }
    }
  }

  # Derive iep2 (enhanced IEP indicator)
  if (all(c("iep", "iep_ref_outcome") %in% names(df))) {
    outcome <- as.character(df$iep_ref_outcome)

    # Check for eligible/approved but not ineligible
    is_eligible <- stringr::str_detect(
      outcome,
      stringr::regex("\\b(eligible|approved)\\b", ignore_case = TRUE)
    ) & !stringr::str_detect(
      outcome,
      stringr::regex("\\bineligible\\b", ignore_case = TRUE)
    )
    is_eligible[is.na(is_eligible)] <- FALSE

    df$iep2 <- dplyr::if_else(is_eligible, 1L, df$iep)
    df$iep2 <- dplyr::if_else(is.na(df$iep2), df$iep, df$iep2)
  }

  df
}


#' Clean demographic flag variables
#' @keywords internal
.clean_demographic_flags <- function(df) {
  # Yes/No/YES/NO -> 0/1
  flag_cols <- c("migrant_family", "indian_lands", "military_family")
  for (col in flag_cols) {
    if (col %in% names(df)) {
      df[[col]] <- dplyr::case_when(
        toupper(df[[col]]) == "YES" ~ 1L,
        toupper(df[[col]]) == "NO" ~ 0L,
        TRUE ~ NA_integer_
      )
    }
  }

  # Modified schedule (new format only, TRUE/FALSE)
  if ("modified_schedule" %in% names(df)) {
    if (is.logical(df$modified_schedule)) {
      df$modified_schedule <- as.integer(df$modified_schedule)
    } else {
      df$modified_schedule <- dplyr::case_when(
        toupper(as.character(df$modified_schedule)) == "TRUE" ~ 1L,
        toupper(as.character(df$modified_schedule)) == "FALSE" ~ 0L,
        TRUE ~ NA_integer_
      )
    }
  }

  # Modified schedule date -> Date
  if ("modified_schedule_date" %in% names(df)) {
    if (!inherits(df$modified_schedule_date, "Date") &&
        !inherits(df$modified_schedule_date, "POSIXct")) {
      df$modified_schedule_date <- as.Date(
        suppressWarnings(as.numeric(df$modified_schedule_date)),
        origin = "1899-12-30"
      )
    } else if (inherits(df$modified_schedule_date, "POSIXct")) {
      df$modified_schedule_date <- as.Date(df$modified_schedule_date)
    }
  }

  df
}


#' Clean GOLD assessment data
#' @keywords internal
.clean_gold_assessments <- function(df) {
  # Identify GOLD columns
  gold_raw_cols <- grep("^gold_.*_raw$", names(df), value = TRUE)
  gold_scale_cols <- grep("^gold_.*_scale$", names(df), value = TRUE)
  gold_whe_cols <- grep("^gold_.*_whe$", names(df), value = TRUE)
  gold_nn_cols <- grep("^gold_.*_nn$", names(df), value = TRUE)
  gold_kready_cols <- grep("^gold_.*_kready$", names(df), value = TRUE)

  # Numeric conversions for raw and scale scores
  for (col in c(gold_raw_cols, gold_scale_cols)) {
    if (col %in% names(df)) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
  }

  # WHE Status -> ordered factor
  whe_levels <- c("Below", "Meet", "Exceed")
  for (col in gold_whe_cols) {
    if (col %in% names(df)) {
      df[[col]] <- factor(df[[col]], levels = whe_levels, ordered = TRUE)
    }
  }

  # NN Status -> ordered factor
  nn_levels <- c("Below", "Meet", "Exceed")
  for (col in gold_nn_cols) {
    if (col %in% names(df)) {
      df[[col]] <- factor(df[[col]], levels = nn_levels, ordered = TRUE)
    }
  }

  # K-Readiness -> factor
  kready_levels <- c("Emerging", "Accomplished")
  for (col in gold_kready_cols) {
    if (col %in% names(df)) {
      df[[col]] <- factor(df[[col]], levels = kready_levels)
    }
  }

  # Growth columns (new format only) - keep as-is (all currently empty)
  # They will be included automatically; no special conversion needed

  df
}


#' Clean PPVT data
#' @keywords internal
.clean_ppvt <- function(df) {
  ppvt_numeric_cols <- c("ppvt_fall_standard", "ppvt_spring_standard",
                          "ppvt_fall_percentile", "ppvt_spring_percentile",
                          "ppvt_fall_stanine", "ppvt_spring_stanine",
                          "ppvt_fall_gsv", "ppvt_spring_gsv")

  for (col in ppvt_numeric_cols) {
    if (col %in% names(df)) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
  }

  # ppvt_category stays as character
  df
}


#' Clean eDECA Pre and Post data
#' @keywords internal
.clean_edeca <- function(df) {
  # Process both Pre and Post
  for (phase in c("pre", "post")) {
    prefix <- paste0("edeca_", phase, "_")

    # Dates
    for (suffix in c("entry_date", "rating_date")) {
      col <- paste0(prefix, suffix)
      if (col %in% names(df)) {
        if (!inherits(df[[col]], "Date") && !inherits(df[[col]], "POSIXct")) {
          df[[col]] <- as.Date(suppressWarnings(as.numeric(df[[col]])),
                                origin = "1899-12-30")
        } else if (inherits(df[[col]], "POSIXct")) {
          df[[col]] <- as.Date(df[[col]])
        }
      }
    }

    # Rater type: T/P -> factor
    rater_col <- paste0(prefix, "rater_type")
    if (rater_col %in% names(df)) {
      df[[rater_col]] <- factor(
        dplyr::case_when(
          df[[rater_col]] == "T" ~ "Teacher",
          df[[rater_col]] == "P" ~ "Parent",
          TRUE ~ NA_character_
        ),
        levels = c("Teacher", "Parent")
      )
    }

    # Numeric scores
    score_suffixes <- c("initiative_raw", "initiative_tscore", "initiative_pctile",
                         "self_reg_raw", "self_reg_tscore", "self_reg_pctile",
                         "attachment_raw", "attachment_tscore", "attachment_pctile",
                         "tpf_raw", "tpf_tscore", "tpf_pctile",
                         "behavior_raw", "behavior_tscore", "behavior_pctile")

    for (suffix in score_suffixes) {
      col <- paste0(prefix, suffix)
      if (col %in% names(df)) {
        df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
      }
    }
  }

  df
}


#' Clean ASQ data
#' @keywords internal
.clean_asq <- function(df) {
  # Dates
  for (col in c("asq_screening_date", "asq_entry_date")) {
    if (col %in% names(df)) {
      if (!inherits(df[[col]], "Date") && !inherits(df[[col]], "POSIXct")) {
        df[[col]] <- as.Date(suppressWarnings(as.numeric(df[[col]])),
                              origin = "1899-12-30")
      } else if (inherits(df[[col]], "POSIXct")) {
        df[[col]] <- as.Date(df[[col]])
      }
    }
  }

  # Domain results -> ordered factor
  asq_levels <- c("Below", "Monitoring", "Above")
  result_cols <- c("asq_comm_results", "asq_gross_motor_results",
                    "asq_fine_motor_results", "asq_prob_solving_results",
                    "asq_personal_social_results")
  for (col in result_cols) {
    if (col %in% names(df)) {
      df[[col]] <- factor(df[[col]], levels = asq_levels, ordered = TRUE)
    }
  }

  # Concern counts -> numeric
  for (col in c("asq_concern_count", "asq_no_concern_count")) {
    if (col %in% names(df)) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
  }

  df
}


#' Clean staff contacts (new format only)
#' @keywords internal
.clean_student_staff_contacts <- function(df) {
  # Concatenate monitor name
  if (all(c("monitor_first_name", "monitor_last_name") %in% names(df))) {
    df$monitor_name <- .concat_student_name(df$monitor_first_name, df$monitor_last_name)
  }

  # Concatenate coach name
  if (all(c("coach_first_name", "coach_last_name") %in% names(df))) {
    df$coach_name <- .concat_student_name(df$coach_first_name, df$coach_last_name)
  }

  df
}


#' Concatenate first and last name, handling NAs
#' @keywords internal
.concat_student_name <- function(first, last) {
  first_chr <- as.character(first)
  last_chr <- as.character(last)

  result <- dplyr::case_when(
    is.na(first_chr) & is.na(last_chr) ~ NA_character_,
    is.na(first_chr) ~ last_chr,
    is.na(last_chr) ~ first_chr,
    TRUE ~ paste(first_chr, last_chr)
  )
  result
}


#' Get list of PII columns to remove
#' @keywords internal
.get_student_pii_columns <- function(format) {
  # Columns always considered PII (DOB is NOT PII — retained for analysis)
  pii <- c("state_student_id", "student_id",
            "child_first_name", "child_last_name", "middle_name", "preferred_name")

  if (format == "new") {
    # Guardian columns
    guardian_cols <- c(
      paste0("guardian_1_", c("first_name", "last_name", "phone1", "phone2",
                               "email", "address1", "address2", "city", "state", "zip")),
      paste0("guardian_2_", c("first_name", "last_name", "phone1", "phone2",
                               "email", "address1", "address2", "city", "state", "zip"))
    )
    # Staff first/last names (but keep concatenated and email)
    staff_pii <- c("monitor_first_name", "monitor_last_name",
                    "coach_first_name", "coach_last_name")
    pii <- c(pii, guardian_cols, staff_pii)
  }

  pii
}


#' Order student columns logically
#' @keywords internal
.order_student_columns <- function(df) {
  # Define preferred column order by groups
  id_cols <- c("school_year", "year", "classroom_code", "classroom_name",
               "region_num", "site_code", "site_name", "program_code", "program_name",
               "adece_id")

  demo_cols <- c("gender", "race", "ethnicity", "birth_year", "age",
                  "dob", "state_student_id", "student_id",
                  "child_first_name", "child_last_name", "middle_name", "preferred_name")

  family_cols <- c("delivery_type", "poverty_ind", "poverty_dum",
                    "title_one", "title_one_dum",
                    "language", "english_learner",
                    "lives_with", "single_parent",
                    "num_in_house",
                    "gross_income", "lower_income", "upper_income", "gross_income_midpoint",
                    "pov_lvl_pct", "monthly_fees")

  service_cols <- c("childcare_subsidy", "tanf", "wic", "free_reduced_lunch", "snap",
                     "iep", "iep2",
                     "school_svc",
                     "head_start", "head_start_dum",
                     "center_childcare", "center_care_dum",
                     "home_childcare", "home_care_dum",
                     "home_visiting", "home_visiting_dum",
                     "foster_care", "homeless",
                     "allergies_medical", "school_services",
                     "intervention_svc", "behavior_support", "class_lvl_id")

  attendance_cols <- c("days_absent_sem1", "days_absent_sem2", "days_absent_total",
                        "days_tardy_sem1", "days_tardy_sem2", "days_tardy_total",
                        "family_hrs_total", "sem1_family_hrs", "sem2_family_hrs",
                        "family_hrs_calculated",
                        "aug_family_hrs", "sept_family_hrs", "oct_family_hrs",
                        "nov_family_hrs", "dec_family_hrs",
                        "jan_family_hrs", "feb_family_hrs", "mar_family_hrs",
                        "apr_family_hrs", "may_family_hrs")

  iep_cols <- c("iep_ref_date", "iep_ref_outcome", "ref_reason",
                 "part_b_date", "part_b_tool", "enrolled_date")

  flag_cols <- c("migrant_family", "indian_lands", "military_family",
                  "modified_schedule", "modified_schedule_date")

  # GOLD, PPVT, eDECA, ASQ will be captured by the catch-all
  # but let's define domain ordering
  gold_cols <- sort(grep("^gold_", names(df), value = TRUE))
  ppvt_cols <- sort(grep("^ppvt_", names(df), value = TRUE))
  edeca_pre_cols <- sort(grep("^edeca_pre_", names(df), value = TRUE))
  edeca_post_cols <- sort(grep("^edeca_post_", names(df), value = TRUE))
  asq_cols <- sort(grep("^asq_", names(df), value = TRUE))

  contact_cols <- c("monitor_name", "monitor_email",
                     "coach_name", "coach_email")

  # Build ordered column list
  preferred <- c(id_cols, demo_cols, family_cols, service_cols,
                  attendance_cols, iep_cols, flag_cols,
                  gold_cols, ppvt_cols, edeca_pre_cols, edeca_post_cols,
                  asq_cols, contact_cols)

  # Only keep columns that exist in df
  ordered <- preferred[preferred %in% names(df)]
  # Add remaining columns not in preferred list
  remaining <- setdiff(names(df), ordered)
  final_order <- c(ordered, remaining)

  df[, final_order, drop = FALSE]
}
