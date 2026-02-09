# test-student-transform.R
# Tests for student_transform() and internal helpers

# --- Input validation ---

test_that("student_transform rejects non-S3 input", {
  expect_error(student_transform("not_a_clean_obj"))
  expect_error(student_transform(42))
  expect_error(student_transform(list(data = data.frame())))
})

test_that("student_transform accepts alprek_student_clean", {
  clean <- make_student_clean("legacy", n = 10)
  result <- student_transform(clean)
  expect_s3_class(result, "alprek_student_clean")
})

test_that("student_transform accepts alprek_student_panel", {
  fixtures <- make_linkage_fixtures()
  result <- student_transform(fixtures$student_panel)
  expect_s3_class(result, "alprek_student_panel")
})


# --- GOLD Gain Scores ---

test_that("GOLD literacy gain is computed correctly (spring - fall)", {
  clean <- make_student_clean("legacy", n = 20)
  result <- student_transform(clean, add_kready_transitions = FALSE,
                               add_chronic_absence = FALSE,
                               add_service_density = FALSE,
                               add_edeca_gains = FALSE)
  df <- result$data

  expect_true("gold_literacy_gain_raw" %in% names(df))
  expect_true("gold_literacy_gain_scale" %in% names(df))

  # Verify computation on non-NA rows
  has_both <- !is.na(df$gold_literacy_fall_raw) & !is.na(df$gold_literacy_spring_raw)
  if (any(has_both)) {
    expected <- df$gold_literacy_spring_raw[has_both] - df$gold_literacy_fall_raw[has_both]
    expect_equal(df$gold_literacy_gain_raw[has_both], expected)
  }
})

test_that("GOLD gain is NA when either fall or spring is NA", {
  clean <- make_student_clean("legacy", n = 20)
  # Force some NAs
  clean$data$gold_literacy_fall_raw[1] <- NA
  clean$data$gold_literacy_spring_raw[2] <- NA
  result <- student_transform(clean, add_kready_transitions = FALSE,
                               add_chronic_absence = FALSE,
                               add_service_density = FALSE,
                               add_edeca_gains = FALSE)

  expect_true(is.na(result$data$gold_literacy_gain_raw[1]))
  expect_true(is.na(result$data$gold_literacy_gain_raw[2]))
})

test_that("GOLD gain scores created for all 6 domains (12 columns)", {
  clean <- make_student_clean("legacy", n = 10)
  result <- student_transform(clean, add_kready_transitions = FALSE,
                               add_chronic_absence = FALSE,
                               add_service_density = FALSE,
                               add_edeca_gains = FALSE)

  domains <- c("literacy", "math", "se", "physical", "cognitive", "language")
  for (d in domains) {
    for (stype in c("raw", "scale")) {
      gain_col <- paste0("gold_", d, "_gain_", stype)
      expect_true(gain_col %in% names(result$data),
                  info = paste("Missing:", gain_col))
    }
  }
})

test_that("GOLD gain can be negative (student regressed)", {
  clean <- make_student_clean("legacy", n = 5)
  # Force fall > spring for first row
  clean$data$gold_literacy_fall_raw[1] <- 80
  clean$data$gold_literacy_spring_raw[1] <- 30
  result <- student_transform(clean, add_kready_transitions = FALSE,
                               add_chronic_absence = FALSE,
                               add_service_density = FALSE,
                               add_edeca_gains = FALSE)

  expect_true(result$data$gold_literacy_gain_raw[1] < 0)
})

test_that("GOLD gains skip missing source columns with info in skipped log", {
  clean <- make_student_clean("legacy", n = 5)
  # Remove a domain's columns
  clean$data$gold_cognitive_fall_raw <- NULL
  clean$data$gold_cognitive_spring_raw <- NULL
  clean$data$gold_cognitive_fall_scale <- NULL
  clean$data$gold_cognitive_spring_scale <- NULL
  result <- student_transform(clean, add_kready_transitions = FALSE,
                               add_chronic_absence = FALSE,
                               add_service_density = FALSE,
                               add_edeca_gains = FALSE)

  expect_true("gold_cognitive_gain_raw" %in% result$transform_log$skipped)
  expect_true("gold_cognitive_gain_scale" %in% result$transform_log$skipped)
  # But other domains should still exist
  expect_true("gold_literacy_gain_raw" %in% names(result$data))
})


# --- K-Readiness Transitions ---

test_that("K-readiness improved = 1 when Emerging -> Accomplished", {
  clean <- make_student_clean("legacy", n = 5)
  clean$data$gold_literacy_fall_kready <- factor(rep("Emerging", 5),
                                                  levels = c("Emerging", "Accomplished"))
  clean$data$gold_literacy_spring_kready <- factor(rep("Accomplished", 5),
                                                    levels = c("Emerging", "Accomplished"))
  result <- student_transform(clean, add_gold_gains = FALSE,
                               add_chronic_absence = FALSE,
                               add_service_density = FALSE,
                               add_edeca_gains = FALSE)

  expect_equal(result$data$gold_literacy_kready_improved, rep(1L, 5))
})

test_that("K-readiness improved = 0 when Accomplished -> Accomplished", {
  clean <- make_student_clean("legacy", n = 5)
  clean$data$gold_literacy_fall_kready <- factor(rep("Accomplished", 5),
                                                  levels = c("Emerging", "Accomplished"))
  clean$data$gold_literacy_spring_kready <- factor(rep("Accomplished", 5),
                                                    levels = c("Emerging", "Accomplished"))
  result <- student_transform(clean, add_gold_gains = FALSE,
                               add_chronic_absence = FALSE,
                               add_service_density = FALSE,
                               add_edeca_gains = FALSE)

  expect_equal(result$data$gold_literacy_kready_improved, rep(0L, 5))
})

test_that("K-readiness improved = NA when either season is NA", {
  clean <- make_student_clean("legacy", n = 5)
  clean$data$gold_literacy_fall_kready[1] <- NA
  clean$data$gold_literacy_spring_kready[2] <- NA
  result <- student_transform(clean, add_gold_gains = FALSE,
                               add_chronic_absence = FALSE,
                               add_service_density = FALSE,
                               add_edeca_gains = FALSE)

  expect_true(is.na(result$data$gold_literacy_kready_improved[1]))
  expect_true(is.na(result$data$gold_literacy_kready_improved[2]))
})


# --- Chronic Absence ---

test_that("Chronic absence flag = 1 when days_absent_total >= 18", {
  clean <- make_student_clean("legacy", n = 5)
  clean$data$days_absent_total <- c(20, 25, 18, 5, 10)
  result <- student_transform(clean, add_gold_gains = FALSE,
                               add_kready_transitions = FALSE,
                               add_service_density = FALSE,
                               add_edeca_gains = FALSE)

  expect_equal(result$data$chronic_absence, c(1L, 1L, 1L, 0L, 0L))
})

test_that("Chronic absence flag = 0 when days < threshold", {
  clean <- make_student_clean("legacy", n = 3)
  clean$data$days_absent_total <- c(5, 10, 17)
  result <- student_transform(clean, add_gold_gains = FALSE,
                               add_kready_transitions = FALSE,
                               add_service_density = FALSE,
                               add_edeca_gains = FALSE)

  expect_equal(result$data$chronic_absence, c(0L, 0L, 0L))
})

test_that("Custom chronic absence threshold works", {
  clean <- make_student_clean("legacy", n = 3)
  clean$data$days_absent_total <- c(5, 10, 17)
  result <- student_transform(clean, add_gold_gains = FALSE,
                               add_kready_transitions = FALSE,
                               add_service_density = FALSE,
                               add_edeca_gains = FALSE,
                               chronic_absence_threshold = 10)

  expect_equal(result$data$chronic_absence, c(0L, 1L, 1L))
})

test_that("Chronic absence percentage calculated correctly", {
  clean <- make_student_clean("legacy", n = 3)
  clean$data$days_absent_total <- c(18, 36, 0)
  result <- student_transform(clean, add_gold_gains = FALSE,
                               add_kready_transitions = FALSE,
                               add_service_density = FALSE,
                               add_edeca_gains = FALSE,
                               school_days = 180)

  expect_equal(result$data$chronic_absence_pct, c(10, 20, 0))
})


# --- Service Density ---

test_that("n_services counts available binary service indicators", {
  clean <- make_student_clean("legacy", n = 3)
  # Set deterministic values
  clean$data$childcare_subsidy <- c(1L, 0L, 1L)
  clean$data$tanf <- c(0L, 0L, 1L)
  clean$data$wic <- c(1L, 1L, 0L)
  clean$data$free_reduced_lunch <- c(0L, 0L, 0L)
  clean$data$snap <- c(1L, 0L, 0L)
  clean$data$foster_care <- c(0L, 0L, 0L)
  clean$data$homeless <- c(0L, 0L, 0L)
  clean$data$head_start_dum <- c(0L, 1L, 0L)
  clean$data$center_care_dum <- c(0L, 0L, 1L)

  result <- student_transform(clean, add_gold_gains = FALSE,
                               add_kready_transitions = FALSE,
                               add_chronic_absence = FALSE,
                               add_edeca_gains = FALSE)

  expect_equal(result$data$n_services, c(3L, 2L, 3L))
})

test_that("risk_index counts risk indicators", {
  clean <- make_student_clean("legacy", n = 3)
  clean$data$poverty_dum <- c(1L, 0L, 1L)
  clean$data$single_parent <- c(1L, 0L, 0L)
  clean$data$english_learner <- c(0L, 0L, 1L)
  clean$data$homeless <- c(0L, 0L, 0L)
  clean$data$foster_care <- c(0L, 1L, 0L)

  result <- student_transform(clean, add_gold_gains = FALSE,
                               add_kready_transitions = FALSE,
                               add_chronic_absence = FALSE,
                               add_edeca_gains = FALSE)

  expect_equal(result$data$risk_index, c(2L, 1L, 2L))
})

test_that("Service density handles missing columns gracefully", {
  clean <- make_student_clean("legacy", n = 5)
  # Remove all service columns
  svc_cols <- c("childcare_subsidy", "tanf", "wic", "free_reduced_lunch",
                "snap", "foster_care", "homeless", "head_start_dum",
                "center_care_dum")
  for (col in svc_cols) clean$data[[col]] <- NULL
  risk_cols <- c("poverty_dum", "single_parent", "english_learner")
  for (col in risk_cols) clean$data[[col]] <- NULL

  result <- student_transform(clean, add_gold_gains = FALSE,
                               add_kready_transitions = FALSE,
                               add_chronic_absence = FALSE,
                               add_edeca_gains = FALSE)

  expect_true("n_services" %in% result$transform_log$skipped)
  expect_true("risk_index" %in% result$transform_log$skipped)
})


# --- eDECA Gains ---

test_that("eDECA gains compute T-score difference correctly", {
  clean <- make_student_clean("legacy", n = 5)
  clean$data$edeca_pre_initiative_tscore <- c(40, 50, 30, NA, 45)
  clean$data$edeca_post_initiative_tscore <- c(55, 60, 35, 50, NA)

  result <- student_transform(clean, add_gold_gains = FALSE,
                               add_kready_transitions = FALSE,
                               add_chronic_absence = FALSE,
                               add_service_density = FALSE)

  expected <- c(15, 10, 5, NA, NA)
  expect_equal(result$data$edeca_initiative_gain, expected)
})

test_that("eDECA gains are NA when either pre or post is NA", {
  clean <- make_student_clean("legacy", n = 3)
  clean$data$edeca_pre_tpf_tscore <- c(NA, 50, 40)
  clean$data$edeca_post_tpf_tscore <- c(60, NA, 55)

  result <- student_transform(clean, add_gold_gains = FALSE,
                               add_kready_transitions = FALSE,
                               add_chronic_absence = FALSE,
                               add_service_density = FALSE)

  expect_true(is.na(result$data$edeca_tpf_gain[1]))
  expect_true(is.na(result$data$edeca_tpf_gain[2]))
  expect_equal(result$data$edeca_tpf_gain[3], 15)
})


# --- Integration tests ---

test_that("Full pipeline: student_transform produces valid enriched data", {
  clean <- make_student_clean("legacy", n = 20)
  result <- student_transform(clean)

  # Should have all 27 derived columns
  expected_cols <- c(
    # GOLD gains (12)
    "gold_literacy_gain_raw", "gold_literacy_gain_scale",
    "gold_math_gain_raw", "gold_math_gain_scale",
    "gold_se_gain_raw", "gold_se_gain_scale",
    "gold_physical_gain_raw", "gold_physical_gain_scale",
    "gold_cognitive_gain_raw", "gold_cognitive_gain_scale",
    "gold_language_gain_raw", "gold_language_gain_scale",
    # K-readiness transitions (6)
    "gold_literacy_kready_improved", "gold_math_kready_improved",
    "gold_se_kready_improved", "gold_physical_kready_improved",
    "gold_cognitive_kready_improved", "gold_language_kready_improved",
    # Chronic absence (2)
    "chronic_absence", "chronic_absence_pct",
    # Service density (2)
    "n_services", "risk_index",
    # eDECA gains (5)
    "edeca_initiative_gain", "edeca_self_reg_gain",
    "edeca_attachment_gain", "edeca_tpf_gain", "edeca_behavior_gain"
  )

  for (col in expected_cols) {
    expect_true(col %in% names(result$data),
                info = paste("Missing derived column:", col))
  }
})

test_that("All flags can be toggled off (no new columns added)", {
  clean <- make_student_clean("legacy", n = 5)
  n_before <- ncol(clean$data)
  result <- student_transform(clean,
                               add_gold_gains = FALSE,
                               add_kready_transitions = FALSE,
                               add_chronic_absence = FALSE,
                               add_service_density = FALSE,
                               add_edeca_gains = FALSE)

  expect_equal(ncol(result$data), n_before)
  expect_equal(result$transform_log$n_cols_added, 0L)
})

test_that("student_transform returns same S3 class as input", {
  clean <- make_student_clean("legacy", n = 5)
  result <- student_transform(clean)
  expect_s3_class(result, "alprek_student_clean")
  expect_true(!is.null(result$transform_log))
  expect_true(!is.null(result$meta$transformed_at))
})

test_that("student_transform is idempotent (run twice = same result)", {
  clean <- make_student_clean("legacy", n = 10)
  result1 <- student_transform(clean)
  result2 <- student_transform(result1)

  expect_equal(ncol(result1$data), ncol(result2$data))
  # GOLD gain values should be the same
  expect_equal(result1$data$gold_literacy_gain_raw,
               result2$data$gold_literacy_gain_raw)
})

test_that("student_transform works with panel input", {
  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 4)
  result <- student_transform(fixtures$student_panel)

  expect_s3_class(result, "alprek_student_panel")
  expect_true("gold_literacy_gain_raw" %in% names(result$data))
  expect_true("chronic_absence" %in% names(result$data))
  expect_true("n_services" %in% names(result$data))
})

test_that("student_transform has transform_log with correct counts", {
  clean <- make_student_clean("legacy", n = 10)
  result <- student_transform(clean)

  log <- result$transform_log
  expect_true(is.list(log))
  expect_equal(log$gold_gains_added, 12L)
  expect_equal(log$kready_transitions_added, 6L)
  expect_equal(log$chronic_absence_added, 2L)
  expect_equal(log$service_density_added, 2L)
  expect_equal(log$edeca_gains_added, 5L)
  expect_equal(log$n_cols_added, 27L)
})
