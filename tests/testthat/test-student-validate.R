# tests/testthat/test-student-validate.R

test_that("student_validate requires correct input", {
  expect_error(student_validate(data.frame()), "alprek_student_clean")
})


test_that("student_validate runs on clean legacy data", {
  clean <- make_student_clean("legacy", n = 10)
  val <- student_validate(clean)

  expect_s3_class(val, "alprek_student_validation")
  expect_true(is.logical(val$passed))
  expect_true(is.numeric(val$n_errors))
  expect_true(is.numeric(val$n_warnings))
  expect_true(is.numeric(val$n_info))
  expect_s3_class(val$checks, "tbl_df")
})


test_that("student_validate has 12 checks", {
  clean <- make_student_clean("legacy", n = 10)
  val <- student_validate(clean)
  expect_equal(nrow(val$checks), 12)
})


test_that("validation checks have correct structure", {
  clean <- make_student_clean("legacy", n = 10)
  val <- student_validate(clean)

  expect_named(val$checks,
               c("check_name", "check_description", "status", "n_issues", "details"))

  # Status should be one of PASS, WARN, ERROR, INFO
  expect_true(all(val$checks$status %in% c("PASS", "WARN", "ERROR", "INFO")))
})


test_that("strict mode treats warnings as errors", {
  clean <- make_student_clean("legacy", n = 10)

  # Normal mode: warnings don't cause failure
  val_normal <- student_validate(clean, strict = FALSE)
  # Strict mode: warnings cause failure
  val_strict <- student_validate(clean, strict = TRUE)

  # If there are warnings, strict should fail
  if (val_normal$n_warnings > 0) {
    expect_true(val_normal$passed)
    expect_false(val_strict$passed)
  }
})


test_that("validation detects missing required columns", {
  clean <- make_student_clean("legacy", n = 10)
  # Remove a required column
  clean$data$gender <- NULL
  val <- student_validate(clean)

  required_check <- val$checks[val$checks$check_name == "required_columns", ]
  expect_equal(required_check$status, "ERROR")
})


test_that("validation detects duplicate students", {
  clean <- make_student_clean("legacy", n = 10)
  # Create duplicate
  clean$data$adece_id[2] <- clean$data$adece_id[1]
  clean$data$classroom_code[2] <- clean$data$classroom_code[1]
  val <- student_validate(clean)

  dup_check <- val$checks[val$checks$check_name == "student_unique", ]
  expect_gt(dup_check$n_issues, 0)
})


test_that("validation checks school_year consistency", {
  clean <- make_student_clean("legacy", n = 10)
  val <- student_validate(clean)

  sy_check <- val$checks[val$checks$check_name == "school_year_consistency", ]
  expect_equal(sy_check$status, "PASS")
})


test_that("print method for validation works", {
  clean <- make_student_clean("legacy", n = 10)
  val <- student_validate(clean)
  expect_output(print(val), "alprek_student_validation")
})
