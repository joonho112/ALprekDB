#' Aggregate Student Data to Classroom Level
#'
#' @description Computes classroom-level summary statistics from student panel
#'   data, including demographic proportions, mean scores, and assessment
#'   completion rates.
#'
#' @param student An `alprek_student_panel` or `alprek_linkage_student` object.
#'
#' @return A tibble with one row per `(school_year, classroom_code)` and
#'   aggregated student statistics.
#'
#' @examples
#' \dontrun{
#' agg <- linkage_aggregate_students(student_panel)
#' # Merge with classroom-level data
#' }
#'
#' @importFrom dplyr group_by summarise n across
#' @export
linkage_aggregate_students <- function(student) {
  if (inherits(student, "alprek_linkage_student")) {
    df <- student$data
  } else if (inherits(student, "alprek_student_panel")) {
    df <- student$data
  } else if (inherits(student, "alprek_student_clean")) {
    df <- student$data
  } else {
    stop("Expected alprek_student_panel, alprek_linkage_student, or alprek_student_clean.",
         call. = FALSE)
  }

  if (!all(c("school_year", "classroom_code") %in% names(df))) {
    stop("Data must contain 'school_year' and 'classroom_code' columns.", call. = FALSE)
  }

  msg_info("Aggregating student data to classroom level")

  # Helper: safe proportion calculation
  safe_pct <- function(x, val) {
    if (length(x) == 0 || all(is.na(x))) return(NA_real_)
    mean(as.character(x) == val, na.rm = TRUE) * 100
  }

  safe_mean <- function(x) {
    if (length(x) == 0 || all(is.na(x))) return(NA_real_)
    mean(x, na.rm = TRUE)
  }

  safe_mean_binary <- function(x) {
    if (length(x) == 0 || all(is.na(x))) return(NA_real_)
    mean(x == 1L, na.rm = TRUE) * 100
  }

  safe_nonNA_count <- function(x) {
    sum(!is.na(x))
  }

  agg <- df |>
    dplyr::group_by(.data$school_year, .data$classroom_code) |>
    dplyr::summarise(
      # Counts
      n_children = dplyr::n(),

      # Gender
      pct_male = if ("gender" %in% names(df)) safe_pct(.data$gender, "Male") else NA_real_,
      pct_female = if ("gender" %in% names(df)) safe_pct(.data$gender, "Female") else NA_real_,

      # Race
      pct_white = if ("race" %in% names(df)) safe_pct(.data$race, "White") else NA_real_,
      pct_black = if ("race" %in% names(df)) safe_pct(.data$race, "Black") else NA_real_,
      pct_latino = if ("race" %in% names(df)) safe_pct(.data$race, "Latino/Hispanic") else NA_real_,
      pct_asian = if ("race" %in% names(df)) safe_pct(.data$race, "Asian") else NA_real_,
      pct_mixed = if ("race" %in% names(df)) safe_pct(.data$race, "Mixed") else NA_real_,
      pct_other_race = if ("race" %in% names(df)) {
        mean(as.character(.data$race) %in% c("Other", "Unknown"), na.rm = TRUE) * 100
      } else NA_real_,

      # Ethnicity
      pct_hispanic_eth = if ("ethnicity" %in% names(df)) {
        safe_pct(.data$ethnicity, "Hispanic")
      } else NA_real_,

      # Economic
      pct_poverty = if ("poverty_dum" %in% names(df)) safe_mean_binary(.data$poverty_dum) else NA_real_,

      # IEP
      pct_iep = if ("iep" %in% names(df)) safe_mean_binary(.data$iep) else NA_real_,
      pct_iep2 = if ("iep2" %in% names(df)) safe_mean_binary(.data$iep2) else NA_real_,

      # Age
      mean_age = if ("age" %in% names(df)) safe_mean(.data$age) else NA_real_,

      # Attendance
      mean_days_absent = if ("days_absent_total" %in% names(df)) {
        safe_mean(.data$days_absent_total)
      } else NA_real_,

      # Income
      mean_income_midpoint = if ("gross_income_midpoint" %in% names(df)) {
        safe_mean(.data$gross_income_midpoint)
      } else NA_real_,

      # GOLD Assessment: counts and means
      n_gold_lit_fall = if ("gold_literacy_fall_raw" %in% names(df)) {
        safe_nonNA_count(.data$gold_literacy_fall_raw)
      } else NA_integer_,
      n_gold_lit_spring = if ("gold_literacy_spring_raw" %in% names(df)) {
        safe_nonNA_count(.data$gold_literacy_spring_raw)
      } else NA_integer_,
      mean_gold_lit_fall = if ("gold_literacy_fall_raw" %in% names(df)) {
        safe_mean(.data$gold_literacy_fall_raw)
      } else NA_real_,
      mean_gold_lit_spring = if ("gold_literacy_spring_raw" %in% names(df)) {
        safe_mean(.data$gold_literacy_spring_raw)
      } else NA_real_,
      mean_gold_math_fall = if ("gold_math_fall_raw" %in% names(df)) {
        safe_mean(.data$gold_math_fall_raw)
      } else NA_real_,
      mean_gold_math_spring = if ("gold_math_spring_raw" %in% names(df)) {
        safe_mean(.data$gold_math_spring_raw)
      } else NA_real_,

      # --- Extended aggregates (from student_transform derived variables) ---

      # GOLD gain scores (classroom mean)
      mean_gold_lit_gain_raw = if ("gold_literacy_gain_raw" %in% names(df)) {
        safe_mean(.data$gold_literacy_gain_raw)
      } else NA_real_,
      mean_gold_math_gain_raw = if ("gold_math_gain_raw" %in% names(df)) {
        safe_mean(.data$gold_math_gain_raw)
      } else NA_real_,
      mean_gold_se_gain_raw = if ("gold_se_gain_raw" %in% names(df)) {
        safe_mean(.data$gold_se_gain_raw)
      } else NA_real_,

      # K-readiness improvement rates
      pct_lit_kready_improved = if ("gold_literacy_kready_improved" %in% names(df)) {
        safe_mean_binary(.data$gold_literacy_kready_improved)
      } else NA_real_,
      pct_math_kready_improved = if ("gold_math_kready_improved" %in% names(df)) {
        safe_mean_binary(.data$gold_math_kready_improved)
      } else NA_real_,

      # Chronic absence
      pct_chronic_absence = if ("chronic_absence" %in% names(df)) {
        safe_mean_binary(.data$chronic_absence)
      } else NA_real_,

      # Service density
      mean_n_services = if ("n_services" %in% names(df)) {
        safe_mean(.data$n_services)
      } else NA_real_,
      mean_risk_index = if ("risk_index" %in% names(df)) {
        safe_mean(.data$risk_index)
      } else NA_real_,

      # Demographics (from student_clean)
      pct_english_learner = if ("english_learner" %in% names(df)) {
        safe_mean_binary(.data$english_learner)
      } else NA_real_,
      pct_foster_care = if ("foster_care" %in% names(df)) {
        safe_mean_binary(.data$foster_care)
      } else NA_real_,
      pct_homeless = if ("homeless" %in% names(df)) {
        safe_mean_binary(.data$homeless)
      } else NA_real_,
      pct_military = if ("military_family" %in% names(df)) {
        safe_mean_binary(.data$military_family)
      } else NA_real_,
      pct_migrant = if ("migrant_family" %in% names(df)) {
        safe_mean_binary(.data$migrant_family)
      } else NA_real_,

      # eDECA gains
      mean_edeca_tpf_gain = if ("edeca_tpf_gain" %in% names(df)) {
        safe_mean(.data$edeca_tpf_gain)
      } else NA_real_,

      .groups = "drop"
    )

  # Round percentages
  pct_cols <- grep("^pct_", names(agg), value = TRUE)
  for (col in pct_cols) {
    agg[[col]] <- round(agg[[col]], 1)
  }
  mean_cols <- grep("^mean_", names(agg), value = TRUE)
  for (col in mean_cols) {
    agg[[col]] <- round(agg[[col]], 2)
  }

  msg_success("Aggregated {nrow(df)} students into {nrow(agg)} classroom-year groups")
  agg
}
