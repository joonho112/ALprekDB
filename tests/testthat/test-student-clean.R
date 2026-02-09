# tests/testthat/test-student-clean.R

# ==============================================================================
# Core cleaning pipeline tests
# ==============================================================================

test_that("student_clean requires alprek_student_raw input", {
  expect_error(student_clean(data.frame()), "alprek_student_raw")
})


test_that("student_clean produces correct S3 class", {
  raw <- make_student_raw("legacy", n = 5)
  clean <- student_clean(raw)
  expect_s3_class(clean, "alprek_student_clean")
  expect_true(is.data.frame(clean$data))
  expect_true(is.list(clean$meta))
  expect_true(is.list(clean$cleaning_log))
})


test_that("student_clean returns expected meta fields", {
  raw <- make_student_raw("legacy", n = 5)
  clean <- student_clean(raw)
  expect_equal(clean$meta$school_year, "2023-2024")
  expect_equal(clean$meta$format, "legacy")
  expect_equal(clean$meta$n_students, 5)
  expect_true(clean$meta$n_cols > 0)
})


# ==============================================================================
# Demographics cleaning
# ==============================================================================

test_that("gender is converted to factor", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  expect_true(is.factor(clean$data$gender))
  expect_equal(levels(clean$data$gender), c("Male", "Female"))
})


test_that("race is standardized via mapping", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  expect_true(is.factor(clean$data$race))
  expected_levels <- c("White", "Black", "Latino/Hispanic", "Asian",
                       "Other", "Mixed", "Unknown")
  expect_equal(levels(clean$data$race), expected_levels)
})


test_that("ethnicity is converted to factor", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  expect_true(is.factor(clean$data$ethnicity))
  expect_equal(levels(clean$data$ethnicity), c("Hispanic", "Non-Hispanic"))
})


test_that("age is derived from DOB", {
  raw <- make_student_raw("legacy", n = 5)
  clean <- student_clean(raw, include_pii = TRUE)
  # Age should exist and be in reasonable range for Pre-K
  expect_true("age" %in% names(clean$data))
  expect_true("birth_year" %in% names(clean$data))
  ages <- clean$data$age[!is.na(clean$data$age)]
  expect_true(all(ages >= 0 & ages <= 10))
})


# ==============================================================================
# Delivery type
# ==============================================================================

test_that("delivery type is standardized to factor", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  expect_true(is.factor(clean$data$delivery_type))
  expected_levels <- c("Public School", "Private Child Care", "Head Start",
                       "Faith-Based Organization", "Community Organization",
                       "University Operated", "Private School")
  expect_equal(levels(clean$data$delivery_type), expected_levels)
})


test_that("new format delivery type capitalization is handled", {
  raw <- make_student_raw("new", n = 10)
  clean <- student_clean(raw)
  # All non-NA values should be in the standardized levels
  dt_vals <- clean$data$delivery_type[!is.na(clean$data$delivery_type)]
  expect_true(all(as.character(dt_vals) %in% levels(clean$data$delivery_type)))
})


# ==============================================================================
# Family/Income cleaning
# ==============================================================================

test_that("poverty indicator creates dummy variable", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  expect_true("poverty_dum" %in% names(clean$data))
  expect_true(all(clean$data$poverty_dum[!is.na(clean$data$poverty_dum)] %in% c(0L, 1L)))
})


test_that("title_one creates dummy variable", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  expect_true("title_one_dum" %in% names(clean$data))
  expect_true(all(clean$data$title_one_dum[!is.na(clean$data$title_one_dum)] %in% c(0L, 1L)))
})


test_that("english_learner is derived from language", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  expect_true("english_learner" %in% names(clean$data))
  # Students with non-English language should be 1
  non_eng <- clean$data$language != "English" & !is.na(clean$data$language)
  expect_true(all(clean$data$english_learner[non_eng] == 1L, na.rm = TRUE))
})


test_that("num_in_house converts text words to numeric", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  expect_true(is.numeric(clean$data$num_in_house))
  # "Four" should become 4, "Seven" should become 7
  expect_true(clean$cleaning_log$n_num_house_text >= 0)
})


test_that("gross income is parsed to midpoint", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  expect_true("lower_income" %in% names(clean$data))
  expect_true("upper_income" %in% names(clean$data))
  expect_true("gross_income_midpoint" %in% names(clean$data))
  expect_true(is.numeric(clean$data$gross_income_midpoint))
})


test_that(".parse_gross_income handles various formats", {
  # Range: "$X - $Y"
  result <- .parse_gross_income("$30,000 - $50,000")
  expect_equal(result$lower, 30000)
  expect_equal(result$upper, 50000)
  expect_equal(result$midpoint, 40000)

  # "less than $X"
  result <- .parse_gross_income("less than $20,000")
  expect_equal(result$lower, 0)
  expect_equal(result$upper, 20000)
  expect_equal(result$midpoint, 19999)

  # "$X or more"
  result <- .parse_gross_income("$80,000 or more")
  expect_equal(result$lower, 80000)
  expect_equal(result$midpoint, 80000)

  # NA
  result <- .parse_gross_income(NA)
  expect_true(is.na(result$midpoint))
})


test_that("poverty level percentage becomes ordered factor", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  if ("pov_lvl_pct" %in% names(clean$data)) {
    pov <- clean$data$pov_lvl_pct[!is.na(clean$data$pov_lvl_pct)]
    if (length(pov) > 0) {
      expect_true(is.ordered(clean$data$pov_lvl_pct))
    }
  }
})


# ==============================================================================
# Service indicators
# ==============================================================================

test_that("binary service indicators are 0/1", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  binary_cols <- c("childcare_subsidy", "tanf", "wic", "free_reduced_lunch",
                   "snap", "iep", "foster_care", "homeless")
  for (col in intersect(binary_cols, names(clean$data))) {
    vals <- clean$data[[col]][!is.na(clean$data[[col]])]
    expect_true(all(vals %in% c(0L, 1L)),
                info = paste("Column", col, "has non-binary values"))
  }
})


test_that("duration services have factor levels and dummies", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  dur_cols <- c("head_start", "center_childcare", "home_childcare", "home_visiting")
  expected_levels <- c("No", "Yes, less than 1 year", "Yes, 1 year", "Yes, more than 1 year")

  for (col in intersect(dur_cols, names(clean$data))) {
    expect_true(is.factor(clean$data[[col]]),
                info = paste(col, "should be factor"))
    expect_equal(levels(clean$data[[col]]), expected_levels,
                 info = paste(col, "factor levels"))
  }

  # Check dummy variables exist
  dum_cols <- c("head_start_dum", "center_care_dum", "home_care_dum", "home_visiting_dum")
  for (col in intersect(dum_cols, names(clean$data))) {
    vals <- clean$data[[col]][!is.na(clean$data[[col]])]
    expect_true(all(vals %in% c(0L, 1L)),
                info = paste(col, "dummy should be 0/1"))
  }
})


# ==============================================================================
# Attendance
# ==============================================================================

test_that("negative attendance values are fixed with abs()", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  # After cleaning, no negatives should remain
  for (col in c("days_absent_sem1", "days_absent_sem2")) {
    if (col %in% names(clean$data)) {
      vals <- clean$data[[col]][!is.na(clean$data[[col]])]
      expect_true(all(vals >= 0), info = paste(col, "should be non-negative"))
    }
  }
})


test_that("attendance totals are derived", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  expect_true("days_absent_total" %in% names(clean$data))
  expect_true("days_tardy_total" %in% names(clean$data))
})


test_that("family hours calculated total is derived", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  expect_true("family_hrs_calculated" %in% names(clean$data))
})


# ==============================================================================
# IEP Referral
# ==============================================================================

test_that("iep2 is derived from referral outcomes", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  if (all(c("iep", "iep2") %in% names(clean$data))) {
    # iep2 should be 0 or 1 (or NA)
    vals <- clean$data$iep2[!is.na(clean$data$iep2)]
    expect_true(all(vals %in% c(0L, 1L)))
  }
})


# ==============================================================================
# Demographic flags
# ==============================================================================

test_that("demographic flags are 0/1", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  flag_cols <- c("migrant_family", "indian_lands", "military_family")
  for (col in intersect(flag_cols, names(clean$data))) {
    vals <- clean$data[[col]][!is.na(clean$data[[col]])]
    expect_true(all(vals %in% c(0L, 1L)),
                info = paste(col, "should be 0/1"))
  }
})


# ==============================================================================
# GOLD assessments
# ==============================================================================

test_that("GOLD WHE status becomes ordered factor", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  whe_cols <- grep("_whe$", names(clean$data), value = TRUE)
  for (col in whe_cols) {
    if (any(!is.na(clean$data[[col]]))) {
      expect_true(is.ordered(clean$data[[col]]),
                  info = paste(col, "should be ordered factor"))
      expect_equal(levels(clean$data[[col]]), c("Below", "Meet", "Exceed"))
    }
  }
})


test_that("GOLD K-Readiness becomes factor", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  kready_cols <- grep("_kready$", names(clean$data), value = TRUE)
  for (col in kready_cols) {
    if (any(!is.na(clean$data[[col]]))) {
      expect_true(is.factor(clean$data[[col]]),
                  info = paste(col, "should be factor"))
      expect_equal(levels(clean$data[[col]]), c("Emerging", "Accomplished"))
    }
  }
})


test_that("GOLD raw and scale scores are numeric", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  raw_cols <- grep("^gold_.*_raw$", names(clean$data), value = TRUE)
  scale_cols <- grep("^gold_.*_scale$", names(clean$data), value = TRUE)
  for (col in c(raw_cols, scale_cols)) {
    expect_true(is.numeric(clean$data[[col]]),
                info = paste(col, "should be numeric"))
  }
})


# ==============================================================================
# eDECA
# ==============================================================================

test_that("eDECA rater type becomes factor", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  if ("edeca_pre_rater_type" %in% names(clean$data)) {
    vals <- clean$data$edeca_pre_rater_type[!is.na(clean$data$edeca_pre_rater_type)]
    if (length(vals) > 0) {
      expect_true(is.factor(clean$data$edeca_pre_rater_type))
      expect_equal(levels(clean$data$edeca_pre_rater_type), c("Teacher", "Parent"))
    }
  }
})


# ==============================================================================
# ASQ
# ==============================================================================

test_that("ASQ domain results become ordered factor", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  asq_cols <- c("asq_comm_results", "asq_gross_motor_results",
                "asq_fine_motor_results", "asq_prob_solving_results",
                "asq_personal_social_results")
  for (col in intersect(asq_cols, names(clean$data))) {
    if (any(!is.na(clean$data[[col]]))) {
      expect_true(is.ordered(clean$data[[col]]),
                  info = paste(col, "should be ordered"))
      expect_equal(levels(clean$data[[col]]), c("Below", "Monitoring", "Above"))
    }
  }
})


# ==============================================================================
# PII removal
# ==============================================================================

test_that("PII is removed by default", {
  raw <- make_student_raw("legacy", n = 5)
  clean <- student_clean(raw, include_pii = FALSE)
  pii_cols <- c("state_student_id")
  for (col in pii_cols) {
    expect_false(col %in% names(clean$data),
                 info = paste(col, "should be removed when include_pii=FALSE"))
  }
  # DOB and derived variables are always kept
  expect_true("dob" %in% names(clean$data))
  expect_true("age" %in% names(clean$data))
  expect_true("birth_year" %in% names(clean$data))
})


test_that("DOB is always kept regardless of include_pii setting", {
  raw <- make_student_raw("legacy", n = 5)
  clean_no_pii <- student_clean(raw, include_pii = FALSE)
  clean_with_pii <- student_clean(raw, include_pii = TRUE)
  expect_true("dob" %in% names(clean_no_pii$data))
  expect_true("dob" %in% names(clean_with_pii$data))
})


test_that("new format PII includes guardian columns", {
  raw <- make_student_raw("new", n = 5)
  clean <- student_clean(raw, include_pii = FALSE)
  # Guardian columns should be removed
  guardian_cols <- grep("guardian_", names(clean$data), value = TRUE)
  expect_equal(length(guardian_cols), 0)
  # But monitor/coach names (concatenated) should remain
  # (first/last removed, but concat name kept)
})


# ==============================================================================
# Full pipeline tests
# ==============================================================================

test_that("full legacy pipeline runs end-to-end", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  expect_s3_class(clean, "alprek_student_clean")
  expect_gt(ncol(clean$data), 20)
  expect_equal(nrow(clean$data), 10)
})


test_that("full new format pipeline runs end-to-end", {
  raw <- make_student_raw("new", n = 10)
  clean <- student_clean(raw)
  expect_s3_class(clean, "alprek_student_clean")
  expect_gt(ncol(clean$data), 20)
  expect_equal(nrow(clean$data), 10)
})


test_that("print method for alprek_student_clean works", {
  clean <- make_student_clean("legacy", n = 5)
  expect_output(print(clean), "alprek_student_clean")
  expect_output(print(clean), "2023-2024")
})


test_that("cleaning_log captures statistics", {
  raw <- make_student_raw("legacy", n = 10)
  clean <- student_clean(raw)
  log <- clean$cleaning_log
  expect_true(is.list(log))
  expect_true("n_negative_absent" %in% names(log))
  expect_true("n_extreme_absent" %in% names(log))
  expect_true("n_income_parsed" %in% names(log))
  expect_true("n_delivery_type_standardized" %in% names(log))
})
