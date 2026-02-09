# test-linkage-aggregate.R
# Tests for linkage_aggregate_students()

test_that("linkage_aggregate_students requires valid input", {
  expect_error(linkage_aggregate_students("not_panel"))
  expect_error(linkage_aggregate_students(list(data = data.frame())))
})

test_that("linkage_aggregate_students returns one row per classroom-year", {
  fixtures <- make_linkage_fixtures()
  agg <- linkage_aggregate_students(fixtures$student_panel)

  # Should have as many rows as unique classroom-year combinations
  student_df <- fixtures$student_panel$data
  expected_n <- nrow(unique(student_df[, c("school_year", "classroom_code")]))
  expect_equal(nrow(agg), expected_n)
})

test_that("linkage_aggregate_students computes n_children correctly", {
  fixtures <- make_linkage_fixtures(n_classrooms = 3, n_students_per = 4)
  agg <- linkage_aggregate_students(fixtures$student_panel)

  # Each classroom should have n_students_per students (except the one with none)
  # Only classrooms 1 and 2 have students (classroom 3 = last has no students)
  expect_true(all(agg$n_children == 4))
})

test_that("linkage_aggregate_students has required columns", {
  fixtures <- make_linkage_fixtures()
  agg <- linkage_aggregate_students(fixtures$student_panel)

  expected_cols <- c("school_year", "classroom_code", "n_children",
                     "pct_male", "pct_female", "pct_white", "pct_black",
                     "pct_poverty", "pct_iep", "mean_age",
                     "mean_days_absent", "mean_income_midpoint",
                     "n_gold_lit_fall", "n_gold_lit_spring",
                     "mean_gold_lit_fall", "mean_gold_lit_spring")
  for (col in expected_cols) {
    expect_true(col %in% names(agg),
                info = paste("Missing column:", col))
  }
})

test_that("linkage_aggregate_students percentages are in [0, 100]", {
  fixtures <- make_linkage_fixtures()
  agg <- linkage_aggregate_students(fixtures$student_panel)

  pct_cols <- grep("^pct_", names(agg), value = TRUE)
  for (col in pct_cols) {
    vals <- agg[[col]][!is.na(agg[[col]])]
    if (length(vals) > 0) {
      expect_true(all(vals >= 0 & vals <= 100),
                  info = paste(col, "out of range"))
    }
  }
})

test_that("linkage_aggregate_students works with linkage_student object", {
  fixtures <- make_linkage_fixtures()
  sc <- linkage_student_classroom(fixtures$student_panel, fixtures$classroom_panel)
  agg <- linkage_aggregate_students(sc)

  expect_true(is.data.frame(agg))
  expect_true("n_children" %in% names(agg))
})

test_that("linkage_aggregate_students gender percentages sum to ~100", {
  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 20)
  agg <- linkage_aggregate_students(fixtures$student_panel)

  gender_sum <- agg$pct_male + agg$pct_female
  # Should be close to 100 (may not be exactly 100 due to rounding)
  expect_true(all(abs(gender_sum - 100) < 1),
              info = "Gender percentages should sum to ~100%")
})

test_that("linkage_aggregate_students GOLD counts are non-negative integers", {
  fixtures <- make_linkage_fixtures()
  agg <- linkage_aggregate_students(fixtures$student_panel)

  count_cols <- grep("^n_gold_", names(agg), value = TRUE)
  for (col in count_cols) {
    vals <- agg[[col]][!is.na(agg[[col]])]
    if (length(vals) > 0) {
      expect_true(all(vals >= 0), info = paste(col, "has negative values"))
    }
  }
})


# --- Extended aggregate tests (transform-derived variables) ---

test_that("linkage_aggregate_students includes extended columns with transform data", {
  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 4)
  # Apply transform before aggregation
  enriched <- student_transform(fixtures$student_panel)
  agg <- linkage_aggregate_students(enriched)

  extended_cols <- c("mean_gold_lit_gain_raw", "mean_gold_math_gain_raw",
                     "mean_gold_se_gain_raw",
                     "pct_lit_kready_improved", "pct_math_kready_improved",
                     "pct_chronic_absence",
                     "mean_n_services", "mean_risk_index",
                     "pct_english_learner", "pct_foster_care",
                     "pct_homeless", "pct_military", "pct_migrant",
                     "mean_edeca_tpf_gain")
  for (col in extended_cols) {
    expect_true(col %in% names(agg),
                info = paste("Missing extended column:", col))
  }
})

test_that("linkage_aggregate_students extended aggregates are NA without transform", {
  fixtures <- make_linkage_fixtures()
  # Without transform, derived cols don't exist → should get NA
  agg <- linkage_aggregate_students(fixtures$student_panel)

  # These columns should exist but be NA (since gain cols don't exist in raw data)
  expect_true("mean_gold_lit_gain_raw" %in% names(agg))
  expect_true(all(is.na(agg$mean_gold_lit_gain_raw)))
})

test_that("linkage_aggregate_students extended pct columns in [0, 100]", {
  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 10)
  enriched <- student_transform(fixtures$student_panel)
  agg <- linkage_aggregate_students(enriched)

  ext_pct_cols <- c("pct_lit_kready_improved", "pct_math_kready_improved",
                    "pct_chronic_absence", "pct_english_learner",
                    "pct_foster_care", "pct_homeless",
                    "pct_military", "pct_migrant")
  for (col in ext_pct_cols) {
    if (col %in% names(agg)) {
      vals <- agg[[col]][!is.na(agg[[col]])]
      if (length(vals) > 0) {
        expect_true(all(vals >= 0 & vals <= 100),
                    info = paste(col, "out of [0, 100] range"))
      }
    }
  }
})

test_that("linkage_aggregate_students demographic pct columns work", {
  fixtures <- make_linkage_fixtures(n_classrooms = 3, n_students_per = 5)
  agg <- linkage_aggregate_students(fixtures$student_panel)

  # These come from student_clean (not transform), so should work without transform
  demo_cols <- c("pct_english_learner", "pct_foster_care", "pct_homeless",
                 "pct_military", "pct_migrant")
  for (col in demo_cols) {
    expect_true(col %in% names(agg),
                info = paste("Missing demographic column:", col))
  }
})
