#' Generate Synthetic ALprekDB Data for Demonstrations
#'
#' @description These functions create realistic synthetic datasets that mirror
#'   the structure and types of real ADECE Pre-K administrative data. They are
#'   useful for demonstrations, vignettes, and testing without requiring access
#'   to confidential data files.
#'
#'   All three generators share classroom codes when called with the same
#'   `seed` and compatible `n_classrooms`, enabling cross-module linkage.
#'
#' @name synthetic-data
NULL


# Internal: Generate shared classroom codes
#
# Creates deterministic classroom codes for all 3 generators to share.
# Format: "CCCDNNNNN.NN" where CCC=county, D=delivery code, NNNNN=program, NN=class.
.alprek_synthetic_codes <- function(n, seed = 42) {
  withr::with_seed(seed, {
    paste0(
      sprintf("%03d", sample(1:67, n, replace = TRUE)),
      sample(c("P", "C", "H", "O", "F", "U", "S"), n, replace = TRUE),
      sprintf("%05d", sample(10000:99999, n)),
      ".",
      sprintf("%02d", sample(1:3, n, replace = TRUE))
    )
  })
}


# Internal: school years for n_years
.alprek_synthetic_years <- function(n_years) {
  all_years <- c("2021-2022", "2022-2023", "2023-2024", "2024-2025")
  if (n_years > 4) n_years <- 4
  all_years[seq_len(n_years)]
}


#' Generate Synthetic Budget Panel Data
#'
#' @description Creates a synthetic `alprek_budget_panel` object with realistic
#'   budget amounts across OSR and Other funding sources.
#'
#' @param n_classrooms Integer. Number of classrooms per year.
#' @param n_years Integer. Number of school years (1-4).
#' @param seed Integer. Random seed for reproducibility.
#'
#' @return An `alprek_budget_panel` S3 object.
#'
#' @examples
#' budget <- alprek_synthetic_budget(n_classrooms = 10, n_years = 2)
#' budget
#' head(budget$data)
#'
#' @export
alprek_synthetic_budget <- function(n_classrooms = 20, n_years = 2, seed = 42) {
  codes <- .alprek_synthetic_codes(n_classrooms, seed = seed)
  years <- .alprek_synthetic_years(n_years)

  all_rows <- list()
  by_year <- list()

  for (i in seq_along(years)) {
    sy <- years[i]
    yr <- as.integer(substr(sy, 1, 4))

    withr::with_seed(seed + i, {
      n <- n_classrooms

      delivery_types <- c("Public School", "Private Child Care", "Head Start",
                           "Community Organization", "Faith-Based Organization",
                           "University Operated", "Private School")
      dt <- delivery_types[match(substr(codes, 4, 4),
                                  c("P", "C", "H", "O", "F", "U", "S"))]

      # OSR amounts
      lt_sal <- round(runif(n, 28000, 48000), 0)
      lt_ben <- round(runif(n, 6000, 16000), 0)
      at_sal <- round(runif(n, 16000, 32000), 0)
      at_ben <- round(runif(n, 4000, 11000), 0)
      is_osr <- round(runif(n, 2000, 8000), 0)
      om_osr <- round(runif(n, 1000, 5000), 0)
      eq_osr <- round(runif(n, 500, 3000), 0)
      ad_osr <- round(runif(n, 1000, 4000), 0)

      # Other amounts (sparse)
      lt_sal_o <- round(runif(n, 0, 3000), 0)
      lt_ben_o <- round(runif(n, 0, 1000), 0)
      at_sal_o <- rep(0, n)
      at_ben_o <- rep(0, n)
      is_oth   <- round(runif(n, 0, 2000), 0)
      om_oth   <- rep(0, n)
      eq_oth   <- rep(0, n)
      ad_oth   <- rep(0, n)

      osr_total <- lt_sal + lt_ben + at_sal + at_ben + is_osr + om_osr + eq_osr + ad_osr
      other_total <- lt_sal_o + lt_ben_o + at_sal_o + at_ben_o + is_oth + om_oth + eq_oth + ad_oth
      grand <- osr_total + other_total
      tc <- lt_sal + lt_ben + at_sal + at_ben + lt_sal_o + lt_ben_o + at_sal_o + at_ben_o

      df <- tibble::tibble(
        school_year = factor(rep(sy, n), levels = years),
        year = rep(yr, n),
        classroom_code = codes,
        classroom_name = paste0("Classroom ", seq_len(n)),
        county_code = substr(codes, 1, 3),
        delivery_type = factor(dt, levels = delivery_types),
        delivery_type_binary = factor(
          ifelse(dt == "Public School", "Public", "Non-Public"),
          levels = c("Public", "Non-Public")
        ),
        delivery_type_3way = factor(
          ifelse(dt == "Public School", "Public",
                 ifelse(dt == "Head Start", "Head Start", "Private")),
          levels = c("Public", "Private", "Head Start")
        ),
        program_code = substr(codes, 5, 9),
        class_num = substr(codes, 11, 12),
        delivery_type_code = substr(codes, 4, 4),
        osr_lead_teacher_salary = lt_sal,
        osr_lead_teacher_benefits = lt_ben,
        osr_aux_teacher_salary = at_sal,
        osr_aux_teacher_benefits = at_ben,
        osr_instructional_support = is_osr,
        osr_operations_maintenance = om_osr,
        osr_equipment = eq_osr,
        osr_administrative = ad_osr,
        other_lead_teacher_salary = lt_sal_o,
        other_lead_teacher_benefits = lt_ben_o,
        other_aux_teacher_salary = at_sal_o,
        other_aux_teacher_benefits = at_ben_o,
        other_instructional_support = is_oth,
        other_operations_maintenance = om_oth,
        other_equipment = eq_oth,
        other_administrative = ad_oth,
        osr_total = osr_total,
        other_total = other_total,
        grand_total = grand,
        total_teacher_compensation = tc,
        share_osr = round(osr_total / grand, 4),
        share_other = round(other_total / grand, 4),
        share_lt_salary = round(lt_sal / grand, 4),
        share_lt_benefits = round(lt_ben / grand, 4),
        share_at_salary = round(at_sal / grand, 4),
        share_at_benefits = round(at_ben / grand, 4),
        share_teacher_compensation = round(tc / grand, 4)
      )

      all_rows[[i]] <- df
      by_year[[sy]] <- list(
        school_year = sy,
        format = if (yr >= 2024) "new" else "legacy",
        n_classrooms = n
      )
    })
  }

  combined <- dplyr::bind_rows(all_rows)
  combined$school_year <- factor(combined$school_year, levels = years)
  combined <- combined |> dplyr::arrange(.data$school_year, .data$classroom_code)

  structure(
    list(
      data = combined,
      years = years,
      n_years = length(years),
      by_year = by_year
    ),
    class = "alprek_budget_panel"
  )
}


#' Generate Synthetic Classroom Panel Data
#'
#' @description Creates a synthetic `alprek_classroom_panel` object with
#'   classroom characteristics, teacher demographics, and geographic data.
#'
#' @inheritParams alprek_synthetic_budget
#'
#' @return An `alprek_classroom_panel` S3 object.
#'
#' @examples
#' classroom <- alprek_synthetic_classroom(n_classrooms = 10, n_years = 2)
#' classroom
#' head(classroom$data)
#'
#' @export
alprek_synthetic_classroom <- function(n_classrooms = 20, n_years = 2, seed = 42) {
  codes <- .alprek_synthetic_codes(n_classrooms, seed = seed)
  years <- .alprek_synthetic_years(n_years)

  delivery_types <- c("Public School", "Private Child Care", "Head Start",
                       "Community Organization", "Faith-Based Organization",
                       "University Operated", "Private School")
  dt_vals <- delivery_types[match(substr(codes, 4, 4),
                                   c("P", "C", "H", "O", "F", "U", "S"))]

  race_levels <- c("White", "Black", "Latino/Hispanic", "Asian", "Mixed", "Other", "Unknown")
  degree_levels <- c("Waiver", "Doctoral degree", "Ed.S.", "Master's degree",
                      "Bachelor's degree", "Associate degree", "College Coursework")

  all_rows <- list()
  by_year <- list()

  for (i in seq_along(years)) {
    sy <- years[i]
    yr <- as.integer(substr(sy, 1, 4))
    n <- n_classrooms

    withr::with_seed(seed + 100 + i, {
      df <- tibble::tibble(
        classroom_code = codes,
        classroom_name = paste0("Classroom ", seq_len(n)),
        school_year = factor(rep(sy, n), levels = years),
        year = rep(yr, n),
        region = sample(1:11, n, replace = TRUE),
        county_code = substr(codes, 1, 3),
        county_name = sample(c("Jefferson", "Montgomery", "Tuscaloosa", "Madison",
                                "Mobile", "Baldwin"), n, replace = TRUE),
        delivery_type = factor(dt_vals, levels = delivery_types),
        program_code = substr(codes, 5, 9),
        site_name = paste0("Site ", seq_len(n)),
        site_code = substr(codes, 1, nchar(codes) - 3),
        class_num = substr(codes, 11, 12),
        title_i = sample(c("Y", "N", NA), n, replace = TRUE, prob = c(0.4, 0.5, 0.1)),
        title_i_numeric = NA_real_,
        total_grant = round(runif(n, 85000, 155000), 0),
        enhancement_grant = round(runif(n, 0, 5000), 0),
        latitude = round(runif(n, 30.5, 34.8), 6),
        longitude = round(runif(n, -88.3, -85.2), 6),
        year_first_funded = sample(2005:2022, n, replace = TRUE),
        class_type = rep("Full Day", n),
        senate_dist = sample(1:35, n, replace = TRUE),
        house_dist = sample(1:105, n, replace = TRUE),
        seat_count = sample(c(18:22, NA), n, replace = TRUE),
        # Lead teacher
        lead_tch_name = paste0("Teacher_", sprintf("%03d", seq_len(n))),
        lead_tch_degree_raw = sample(c("Bachelor's ECE", "Master's Elem Ed",
                                        "CDA", "Associate CD"), n, replace = TRUE),
        lead_tch_degree_level = factor(
          sample(c("Bachelor's degree", "Master's degree", "Associate degree",
                   "Doctoral degree"), n, replace = TRUE,
                 prob = c(0.45, 0.35, 0.15, 0.05)),
          levels = degree_levels
        ),
        lead_tch_degree_area = sample(c("Early Childhood Education",
                                         "Elementary Education",
                                         "Child Development"), n, replace = TRUE),
        lead_tch_degree_area_simple = sample(c("Early Childhood Education",
                                                "Elementary Education",
                                                "Child Development"), n, replace = TRUE),
        lead_tch_waiver = as.integer(sample(c(FALSE, FALSE, FALSE, TRUE), n, replace = TRUE)),
        lead_tch_race = factor(
          sample(c("White", "Black", "Latino/Hispanic"), n, replace = TRUE,
                 prob = c(0.55, 0.35, 0.10)),
          levels = race_levels
        ),
        lead_tch_ethnicity = factor(
          sample(c("Hispanic", "Non-Hispanic"), n, replace = TRUE, prob = c(0.1, 0.9)),
          levels = c("Hispanic", "Non-Hispanic")
        ),
        lead_tch_gender = factor(
          sample(c("Female", "Male"), n, replace = TRUE, prob = c(0.92, 0.08)),
          levels = c("Female", "Male")
        ),
        lead_tch_osr_exp = sample(0:25, n, replace = TRUE),
        lead_tch_total_exp = sample(1:35, n, replace = TRUE),
        # Aux teacher
        aux_tch_name = paste0("AuxTeach_", sprintf("%03d", seq_len(n))),
        aux_tch_degree_raw = sample(c("CDA", "Associate CD", "HS Diploma"),
                                     n, replace = TRUE),
        aux_tch_race = factor(
          sample(c("White", "Black"), n, replace = TRUE, prob = c(0.5, 0.5)),
          levels = race_levels
        ),
        aux_tch_gender = factor(
          sample(c("Female", "Male"), n, replace = TRUE, prob = c(0.90, 0.10)),
          levels = c("Female", "Male")
        ),
        aux_tch_osr_exp = sample(0:15, n, replace = TRUE),
        aux_tch_total_exp = sample(0:20, n, replace = TRUE),
        # Monitor/Coach
        monitor_name = paste0("Monitor_", sprintf("%03d", seq_len(n))),
        monitor_email = paste0("monitor", seq_len(n), "@test.edu"),
        coach_name = paste0("Coach_", sprintf("%03d", seq_len(n))),
        coach_email = paste0("coach", seq_len(n), "@test.edu")
      )

      df$title_i_numeric <- ifelse(is.na(df$title_i), NA_real_,
                                    ifelse(df$title_i == "Y", 1, 0))

      all_rows[[i]] <- df
      by_year[[sy]] <- list(
        school_year = sy,
        format = if (yr >= 2024) "new" else "legacy",
        n_classrooms = n
      )
    })
  }

  combined <- dplyr::bind_rows(all_rows)
  combined$school_year <- factor(combined$school_year, levels = years)
  combined <- combined |> dplyr::arrange(.data$school_year, .data$classroom_code)

  structure(
    list(
      data = combined,
      years = years,
      n_total = nrow(combined),
      by_year = by_year,
      imputation_log = tibble::tibble(
        classroom_code = character(), school_year = character(),
        variable = character(), imputed_value = character(), method = character()
      )
    ),
    class = "alprek_classroom_panel"
  )
}


#' Generate Synthetic Student Panel Data
#'
#' @description Creates a synthetic `alprek_student_panel` object with student
#'   demographics, GOLD assessment scores, attendance, service indicators, and
#'   derived variables (gain scores, chronic absence, risk index).
#'
#' @param n_students Integer. Number of students per year.
#' @param n_classrooms Integer. Number of classrooms (must match other generators
#'   for linkage). Defaults to 20.
#' @param n_years Integer. Number of school years (1-4).
#' @param seed Integer. Random seed for reproducibility.
#'
#' @return An `alprek_student_panel` S3 object.
#'
#' @examples
#' student <- alprek_synthetic_student(n_students = 50, n_years = 2)
#' student
#' head(student$data[, 1:10])
#'
#' @export
alprek_synthetic_student <- function(n_students = 100, n_classrooms = 20,
                                     n_years = 2, seed = 42) {
  codes <- .alprek_synthetic_codes(n_classrooms, seed = seed)
  years <- .alprek_synthetic_years(n_years)

  delivery_types <- c("Public School", "Private Child Care", "Head Start",
                       "Community Organization", "Faith-Based Organization",
                       "University Operated", "Private School")
  dt_vals <- delivery_types[match(substr(codes, 4, 4),
                                   c("P", "C", "H", "O", "F", "U", "S"))]

  race_levels <- c("White", "Black", "Latino/Hispanic", "Asian",
                    "Mixed", "Other", "Unknown")
  whe_levels <- c("Below", "Meet", "Exceed")
  kready_levels <- c("Emerging", "Accomplished")
  asq_levels <- c("Below", "Monitoring", "Above")

  gold_domains <- c("literacy", "math", "se", "physical", "cognitive", "language")
  edeca_constructs <- c("initiative", "self_reg", "attachment", "tpf", "behavior")

  all_rows <- list()
  by_year <- list()

  for (i in seq_along(years)) {
    sy <- years[i]
    yr <- as.integer(substr(sy, 1, 4))
    n <- n_students

    withr::with_seed(seed + 200 + i, {
      # Assign students to classrooms
      classroom_idx <- sample(seq_along(codes), n, replace = TRUE)
      student_codes <- codes[classroom_idx]
      student_dt <- dt_vals[classroom_idx]

      # --- IDs ---
      adece_ids <- sprintf("S%04d%02d", seq_len(n), i)

      # --- Demographics ---
      gender <- factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.51, 0.49)),
                        levels = c("Male", "Female"))
      race <- factor(
        sample(race_levels, n, replace = TRUE,
               prob = c(0.40, 0.30, 0.15, 0.03, 0.05, 0.02, 0.05)),
        levels = race_levels
      )
      ethnicity <- factor(
        sample(c("Hispanic", "Non-Hispanic"), n, replace = TRUE, prob = c(0.15, 0.85)),
        levels = c("Hispanic", "Non-Hispanic")
      )
      dob <- as.Date(paste0(yr - 4, "-01-01")) + sample(0:730, n, replace = TRUE)
      age <- as.integer(floor(as.numeric(difftime(
        as.Date(paste0(yr, "-08-01")), dob, units = "days")) / 365.25))
      birth_year <- as.integer(format(dob, "%Y"))

      # --- Family/Income ---
      poverty_dum <- sample(c(0L, 1L), n, replace = TRUE, prob = c(0.35, 0.65))
      title_one_dum <- sample(c(0L, 1L), n, replace = TRUE, prob = c(0.4, 0.6))
      language <- sample(c("English", "Spanish", "Arabic", "Vietnamese"),
                          n, replace = TRUE, prob = c(0.72, 0.15, 0.08, 0.05))
      english_learner <- as.integer(language != "English")
      lives_with <- factor(
        sample(c("Both", "Parent/guardian 1", "Parent/guardian 2"), n, replace = TRUE,
               prob = c(0.55, 0.35, 0.10)),
        levels = c("Both", "Parent/guardian 1", "Parent/guardian 2")
      )
      single_parent <- as.integer(lives_with != "Both")
      num_in_house <- sample(2:8, n, replace = TRUE)
      gross_income_midpoint <- round(runif(n, 8000, 85000), 0)
      monthly_fees <- round(runif(n, 0, 300), 2)

      # --- Service indicators ---
      childcare_subsidy <- sample(c(0L, 1L), n, replace = TRUE, prob = c(0.7, 0.3))
      tanf <- sample(c(0L, 1L), n, replace = TRUE, prob = c(0.85, 0.15))
      wic <- sample(c(0L, 1L), n, replace = TRUE, prob = c(0.5, 0.5))
      free_reduced_lunch <- sample(c(0L, 1L), n, replace = TRUE, prob = c(0.35, 0.65))
      snap <- sample(c(0L, 1L), n, replace = TRUE, prob = c(0.6, 0.4))
      iep <- sample(c(0L, 1L), n, replace = TRUE, prob = c(0.88, 0.12))
      iep2 <- iep
      foster_care <- sample(c(0L, 1L), n, replace = TRUE, prob = c(0.97, 0.03))
      homeless <- sample(c(0L, 1L), n, replace = TRUE, prob = c(0.96, 0.04))
      head_start_dum <- sample(c(0L, 1L), n, replace = TRUE, prob = c(0.8, 0.2))
      center_care_dum <- sample(c(0L, 1L), n, replace = TRUE, prob = c(0.7, 0.3))
      home_care_dum <- sample(c(0L, 1L), n, replace = TRUE, prob = c(0.9, 0.1))
      home_visiting_dum <- sample(c(0L, 1L), n, replace = TRUE, prob = c(0.85, 0.15))
      intervention_svc <- sample(c(0L, 1L), n, replace = TRUE, prob = c(0.9, 0.1))
      migrant_family <- sample(c(0L, 1L), n, replace = TRUE, prob = c(0.97, 0.03))
      military_family <- sample(c(0L, 1L), n, replace = TRUE, prob = c(0.92, 0.08))

      # --- Attendance ---
      days_absent_sem1 <- pmax(0L, as.integer(round(rnorm(n, 5, 3))))
      days_absent_sem2 <- pmax(0L, as.integer(round(rnorm(n, 5, 3))))
      days_absent_total <- days_absent_sem1 + days_absent_sem2
      days_tardy_sem1 <- pmax(0L, as.integer(round(rnorm(n, 3, 2))))
      days_tardy_sem2 <- pmax(0L, as.integer(round(rnorm(n, 3, 2))))
      days_tardy_total <- days_tardy_sem1 + days_tardy_sem2

      # --- GOLD scores (6 domains x fall/spring) ---
      # ~90% present
      gold_data <- list()
      for (dom in gold_domains) {
        present <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.90, 0.10))
        fall_raw <- ifelse(present, round(runif(n, 15, 65), 0), NA_real_)
        fall_scale <- ifelse(present, round(runif(n, 250, 650), 0), NA_real_)
        # Spring slightly higher (growth)
        spring_raw <- ifelse(present, pmin(99, fall_raw + round(runif(n, 3, 20), 0)), NA_real_)
        spring_scale <- ifelse(present, pmin(800, fall_scale + round(runif(n, 10, 80), 0)), NA_real_)
        fall_whe <- ifelse(present,
                           sample(whe_levels, n, replace = TRUE, prob = c(0.2, 0.5, 0.3)),
                           NA_character_)
        spring_whe <- ifelse(present,
                             sample(whe_levels, n, replace = TRUE, prob = c(0.1, 0.4, 0.5)),
                             NA_character_)
        fall_nn <- fall_whe
        spring_nn <- spring_whe
        fall_kready <- ifelse(present,
                              sample(kready_levels, n, replace = TRUE, prob = c(0.45, 0.55)),
                              NA_character_)
        spring_kready <- ifelse(present,
                                sample(kready_levels, n, replace = TRUE, prob = c(0.20, 0.80)),
                                NA_character_)

        gold_data[[paste0("gold_", dom, "_fall_raw")]] <- fall_raw
        gold_data[[paste0("gold_", dom, "_fall_scale")]] <- fall_scale
        gold_data[[paste0("gold_", dom, "_fall_whe")]] <- factor(fall_whe, levels = whe_levels)
        gold_data[[paste0("gold_", dom, "_fall_nn")]] <- factor(fall_nn, levels = whe_levels)
        gold_data[[paste0("gold_", dom, "_fall_kready")]] <- factor(fall_kready, levels = kready_levels)
        gold_data[[paste0("gold_", dom, "_spring_raw")]] <- spring_raw
        gold_data[[paste0("gold_", dom, "_spring_scale")]] <- spring_scale
        gold_data[[paste0("gold_", dom, "_spring_whe")]] <- factor(spring_whe, levels = whe_levels)
        gold_data[[paste0("gold_", dom, "_spring_nn")]] <- factor(spring_nn, levels = whe_levels)
        gold_data[[paste0("gold_", dom, "_spring_kready")]] <- factor(spring_kready, levels = kready_levels)
      }

      # --- GOLD Gain Scores (derived) ---
      gain_data <- list()
      for (dom in gold_domains) {
        fr <- gold_data[[paste0("gold_", dom, "_fall_raw")]]
        sr <- gold_data[[paste0("gold_", dom, "_spring_raw")]]
        fs <- gold_data[[paste0("gold_", dom, "_fall_scale")]]
        ss <- gold_data[[paste0("gold_", dom, "_spring_scale")]]
        fk <- gold_data[[paste0("gold_", dom, "_fall_kready")]]
        sk <- gold_data[[paste0("gold_", dom, "_spring_kready")]]

        gain_data[[paste0("gold_", dom, "_gain_raw")]] <- sr - fr
        gain_data[[paste0("gold_", dom, "_gain_scale")]] <- ss - fs
        gain_data[[paste0("gold_", dom, "_kready_improved")]] <- ifelse(
          !is.na(fk) & !is.na(sk),
          as.integer(as.character(fk) == "Emerging" & as.character(sk) == "Accomplished"),
          NA_integer_
        )
      }

      # --- eDECA T-scores (Pre and Post) ---
      edeca_data <- list()
      for (ec in edeca_constructs) {
        pre_present <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.85, 0.15))
        # Post only ~25% present (except 2024-2025 which has 95%)
        post_prob <- if (yr >= 2024) 0.95 else 0.05
        post_present <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(post_prob, 1 - post_prob))

        edeca_data[[paste0("edeca_", ec, "_pre_tscore")]] <-
          ifelse(pre_present, round(rnorm(n, 50, 10), 1), NA_real_)
        edeca_data[[paste0("edeca_", ec, "_post_tscore")]] <-
          ifelse(post_present,
                 pmax(20, round(rnorm(n, 53, 10), 1)),  # slight improvement
                 NA_real_)
      }

      # --- eDECA Gains (derived) ---
      for (ec in edeca_constructs) {
        pre <- edeca_data[[paste0("edeca_", ec, "_pre_tscore")]]
        post <- edeca_data[[paste0("edeca_", ec, "_post_tscore")]]
        gain_data[[paste0("edeca_", ec, "_gain")]] <- post - pre
      }

      # --- Chronic Absence (derived) ---
      chronic_absence <- as.integer(days_absent_total >= 18)
      chronic_absence_pct <- round(days_absent_total / 180 * 100, 2)

      # --- Service Density (derived) ---
      svc_cols <- cbind(childcare_subsidy, tanf, wic, free_reduced_lunch, snap,
                         foster_care, homeless, head_start_dum, center_care_dum)
      n_services <- as.integer(rowSums(svc_cols, na.rm = TRUE))

      risk_cols <- cbind(poverty_dum, single_parent, english_learner,
                          homeless, foster_care)
      risk_index <- as.integer(rowSums(risk_cols, na.rm = TRUE))

      # --- Build tibble ---
      df <- tibble::tibble(
        school_year = factor(rep(sy, n), levels = years),
        year = rep(yr, n),
        classroom_code = student_codes,
        classroom_name = paste0("Classroom ", classroom_idx),
        adece_id = adece_ids,
        region_num = sample(1:11, n, replace = TRUE),
        site_code = substr(student_codes, 1, nchar(student_codes) - 3),
        site_name = paste0("Site ", classroom_idx),
        program_code = substr(student_codes, 5, 9),
        program_name = paste0("Program ", classroom_idx),
        delivery_type = factor(student_dt, levels = delivery_types),
        gender = gender,
        race = race,
        ethnicity = ethnicity,
        dob = dob,
        age = age,
        birth_year = birth_year,
        poverty_dum = poverty_dum,
        title_one_dum = title_one_dum,
        language = language,
        english_learner = english_learner,
        lives_with = lives_with,
        single_parent = single_parent,
        num_in_house = num_in_house,
        gross_income_midpoint = as.numeric(gross_income_midpoint),
        monthly_fees = monthly_fees,
        childcare_subsidy = childcare_subsidy,
        tanf = tanf,
        wic = wic,
        free_reduced_lunch = free_reduced_lunch,
        snap = snap,
        iep = iep,
        iep2 = iep2,
        foster_care = foster_care,
        homeless = homeless,
        head_start_dum = head_start_dum,
        center_care_dum = center_care_dum,
        home_care_dum = home_care_dum,
        home_visiting_dum = home_visiting_dum,
        intervention_svc = intervention_svc,
        migrant_family = migrant_family,
        military_family = military_family,
        days_absent_sem1 = days_absent_sem1,
        days_absent_sem2 = days_absent_sem2,
        days_absent_total = days_absent_total,
        days_tardy_sem1 = days_tardy_sem1,
        days_tardy_sem2 = days_tardy_sem2,
        days_tardy_total = days_tardy_total
      )

      # Add GOLD columns
      for (nm in names(gold_data)) {
        df[[nm]] <- gold_data[[nm]]
      }

      # Add gain/derived columns
      for (nm in names(gain_data)) {
        df[[nm]] <- gain_data[[nm]]
      }

      # Add eDECA columns
      for (nm in names(edeca_data)) {
        df[[nm]] <- edeca_data[[nm]]
      }

      # Add remaining derived columns
      df$chronic_absence <- chronic_absence
      df$chronic_absence_pct <- chronic_absence_pct
      df$n_services <- n_services
      df$risk_index <- risk_index

      all_rows[[i]] <- df
      by_year[[sy]] <- list(
        school_year = sy,
        format = if (yr >= 2024) "new" else "legacy",
        n_students = n,
        n_cols = ncol(df)
      )
    })
  }

  combined <- dplyr::bind_rows(all_rows)
  combined$school_year <- factor(combined$school_year, levels = years)
  combined <- combined |> dplyr::arrange(.data$school_year, .data$adece_id)

  n_unique <- length(unique(combined$adece_id[!is.na(combined$adece_id)]))

  structure(
    list(
      data = combined,
      years = years,
      n_total = nrow(combined),
      n_unique_students = n_unique,
      by_year = by_year
    ),
    class = "alprek_student_panel"
  )
}
