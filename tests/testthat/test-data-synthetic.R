# tests/testthat/test-data-synthetic.R
# Tests for synthetic data generators

# ==========================================================================
# Budget Synthetic
# ==========================================================================

test_that("alprek_synthetic_budget returns correct S3 class", {
  budget <- alprek_synthetic_budget(n_classrooms = 5, n_years = 2, seed = 1)
  expect_s3_class(budget, "alprek_budget_panel")
  expect_true("data" %in% names(budget))
  expect_true("years" %in% names(budget))
  expect_true("n_years" %in% names(budget))
  expect_true("by_year" %in% names(budget))
})

test_that("alprek_synthetic_budget has correct dimensions", {
  budget <- alprek_synthetic_budget(n_classrooms = 10, n_years = 3, seed = 1)
  expect_equal(nrow(budget$data), 30)  # 10 * 3
  expect_equal(budget$n_years, 3)
  expect_equal(length(budget$years), 3)
})

test_that("alprek_synthetic_budget has required columns", {
  budget <- alprek_synthetic_budget(n_classrooms = 5, n_years = 1, seed = 1)
  required <- c("school_year", "year", "classroom_code", "delivery_type",
                 "osr_total", "other_total", "grand_total", "share_osr")
  for (col in required) {
    expect_true(col %in% names(budget$data),
                info = paste("Missing column:", col))
  }
})

test_that("alprek_synthetic_budget factor columns are correct", {
  budget <- alprek_synthetic_budget(n_classrooms = 5, n_years = 1, seed = 1)
  expect_true(is.factor(budget$data$school_year))
  expect_true(is.factor(budget$data$delivery_type))
  expect_true(is.factor(budget$data$delivery_type_binary))
})

test_that("alprek_synthetic_budget is reproducible with seed", {
  b1 <- alprek_synthetic_budget(n_classrooms = 5, n_years = 1, seed = 99)
  b2 <- alprek_synthetic_budget(n_classrooms = 5, n_years = 1, seed = 99)
  expect_identical(b1$data$grand_total, b2$data$grand_total)
  expect_identical(b1$data$classroom_code, b2$data$classroom_code)
})


# ==========================================================================
# Classroom Synthetic
# ==========================================================================

test_that("alprek_synthetic_classroom returns correct S3 class", {
  cr <- alprek_synthetic_classroom(n_classrooms = 5, n_years = 2, seed = 1)
  expect_s3_class(cr, "alprek_classroom_panel")
  expect_true("data" %in% names(cr))
  expect_true("years" %in% names(cr))
  expect_true("n_total" %in% names(cr))
  expect_true("imputation_log" %in% names(cr))
})

test_that("alprek_synthetic_classroom has correct dimensions", {
  cr <- alprek_synthetic_classroom(n_classrooms = 8, n_years = 2, seed = 1)
  expect_equal(nrow(cr$data), 16)  # 8 * 2
  expect_equal(cr$n_total, 16)
})

test_that("alprek_synthetic_classroom has required columns", {
  cr <- alprek_synthetic_classroom(n_classrooms = 5, n_years = 1, seed = 1)
  required <- c("classroom_code", "school_year", "delivery_type", "region",
                 "latitude", "longitude", "lead_tch_race", "lead_tch_gender",
                 "total_grant", "seat_count")
  for (col in required) {
    expect_true(col %in% names(cr$data),
                info = paste("Missing column:", col))
  }
})

test_that("alprek_synthetic_classroom factor columns are correct", {
  cr <- alprek_synthetic_classroom(n_classrooms = 5, n_years = 1, seed = 1)
  expect_true(is.factor(cr$data$delivery_type))
  expect_true(is.factor(cr$data$lead_tch_race))
  expect_true(is.factor(cr$data$lead_tch_gender))
  expect_true(is.factor(cr$data$lead_tch_degree_level))
})


# ==========================================================================
# Student Synthetic
# ==========================================================================

test_that("alprek_synthetic_student returns correct S3 class", {
  st <- alprek_synthetic_student(n_students = 20, n_classrooms = 5, n_years = 1, seed = 1)
  expect_s3_class(st, "alprek_student_panel")
  expect_true("data" %in% names(st))
  expect_true("years" %in% names(st))
  expect_true("n_total" %in% names(st))
  expect_true("n_unique_students" %in% names(st))
})

test_that("alprek_synthetic_student has correct dimensions", {
  st <- alprek_synthetic_student(n_students = 30, n_classrooms = 5, n_years = 2, seed = 1)
  expect_equal(nrow(st$data), 60)  # 30 * 2
  expect_equal(st$n_total, 60)
})

test_that("alprek_synthetic_student has demographic columns", {
  st <- alprek_synthetic_student(n_students = 10, n_classrooms = 3, n_years = 1, seed = 1)
  required <- c("adece_id", "gender", "race", "ethnicity", "dob", "age",
                 "poverty_dum", "english_learner", "single_parent")
  for (col in required) {
    expect_true(col %in% names(st$data),
                info = paste("Missing column:", col))
  }
})

test_that("alprek_synthetic_student has GOLD assessment columns", {
  st <- alprek_synthetic_student(n_students = 10, n_classrooms = 3, n_years = 1, seed = 1)
  domains <- c("literacy", "math", "se", "physical", "cognitive", "language")
  for (dom in domains) {
    expect_true(paste0("gold_", dom, "_fall_raw") %in% names(st$data),
                info = paste("Missing:", dom, "fall raw"))
    expect_true(paste0("gold_", dom, "_spring_raw") %in% names(st$data),
                info = paste("Missing:", dom, "spring raw"))
    expect_true(paste0("gold_", dom, "_gain_raw") %in% names(st$data),
                info = paste("Missing:", dom, "gain raw"))
    expect_true(paste0("gold_", dom, "_kready_improved") %in% names(st$data),
                info = paste("Missing:", dom, "kready_improved"))
  }
})

test_that("alprek_synthetic_student has eDECA columns", {
  st <- alprek_synthetic_student(n_students = 10, n_classrooms = 3, n_years = 1, seed = 1)
  constructs <- c("initiative", "self_reg", "attachment", "tpf", "behavior")
  for (ec in constructs) {
    expect_true(paste0("edeca_", ec, "_pre_tscore") %in% names(st$data),
                info = paste("Missing eDECA Pre:", ec))
    expect_true(paste0("edeca_", ec, "_gain") %in% names(st$data),
                info = paste("Missing eDECA gain:", ec))
  }
})

test_that("alprek_synthetic_student has derived columns", {
  st <- alprek_synthetic_student(n_students = 10, n_classrooms = 3, n_years = 1, seed = 1)
  derived <- c("chronic_absence", "chronic_absence_pct", "n_services", "risk_index")
  for (col in derived) {
    expect_true(col %in% names(st$data),
                info = paste("Missing derived:", col))
  }
})

test_that("alprek_synthetic_student factor types are correct", {
  st <- alprek_synthetic_student(n_students = 10, n_classrooms = 3, n_years = 1, seed = 1)
  expect_true(is.factor(st$data$gender))
  expect_true(is.factor(st$data$race))
  expect_true(is.factor(st$data$ethnicity))
  expect_true(is.factor(st$data$delivery_type))
  expect_true(is.factor(st$data$gold_literacy_fall_whe))
  expect_true(is.factor(st$data$gold_literacy_fall_kready))
})


# ==========================================================================
# Cross-Module Linkability
# ==========================================================================

test_that("all three generators share classroom codes with same seed", {
  seed <- 42
  nc <- 10
  budget <- alprek_synthetic_budget(n_classrooms = nc, n_years = 1, seed = seed)
  classroom <- alprek_synthetic_classroom(n_classrooms = nc, n_years = 1, seed = seed)
  student <- alprek_synthetic_student(n_students = 30, n_classrooms = nc, n_years = 1, seed = seed)

  budget_codes <- sort(unique(budget$data$classroom_code))
  classroom_codes <- sort(unique(classroom$data$classroom_code))

  expect_identical(budget_codes, classroom_codes)

  student_codes <- unique(student$data$classroom_code)
  expect_true(all(student_codes %in% classroom_codes))
})

test_that("synthetic data works with linkage_create_master", {
  skip_if_not_installed("ALprekDB")
  seed <- 42
  nc <- 8
  budget <- alprek_synthetic_budget(n_classrooms = nc, n_years = 1, seed = seed)
  classroom <- alprek_synthetic_classroom(n_classrooms = nc, n_years = 1, seed = seed)
  student <- alprek_synthetic_student(n_students = 30, n_classrooms = nc, n_years = 1, seed = seed)

  master <- linkage_create_master(budget, classroom, student)
  expect_s3_class(master, "alprek_linkage_master")
  expect_true(nrow(master$classroom_level) > 0)
  expect_true(nrow(master$student_level) > 0)
})
