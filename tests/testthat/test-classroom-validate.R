test_that("classroom_validate works on clean fixture", {
  clean <- make_classroom_clean("legacy", 5)
  val <- classroom_validate(clean)

  expect_s3_class(val, "alprek_classroom_validation")
  expect_true(is.logical(val$passed))
  expect_true(is.data.frame(val$checks))
  expect_gte(nrow(val$checks), 10)  # At least 10 checks
})

test_that("classroom_validate checks have expected columns", {
  clean <- make_classroom_clean("legacy", 5)
  val <- classroom_validate(clean)

  expect_true(all(c("check_name", "check_description", "status",
                     "n_issues", "details") %in% names(val$checks)))
})

test_that("classroom_validate has correct check names", {
  clean <- make_classroom_clean("legacy", 5)
  val <- classroom_validate(clean)

  expected_checks <- c("required_columns", "classroom_code_format",
                       "classroom_code_unique", "delivery_type_coverage",
                       "missing_rates", "grant_range",
                       "degree_classification_coverage", "experience_range",
                       "coordinate_range", "school_year_consistency")

  for (check in expected_checks) {
    expect_true(check %in% val$checks$check_name,
                info = paste("Missing check:", check))
  }
})

test_that("classroom_validate passes on good data", {
  clean <- make_classroom_clean("legacy", 5)
  val <- classroom_validate(clean)

  # Required columns should pass
  req_check <- val$checks[val$checks$check_name == "required_columns", ]
  expect_equal(req_check$status, "PASS")

  # School year consistency should pass
  sy_check <- val$checks[val$checks$check_name == "school_year_consistency", ]
  expect_equal(sy_check$status, "PASS")
})

test_that("classroom_validate accepts canonical classroom code shapes by source year", {
  cases <- list(
    "2021-2022" = list(format = "legacy", codes = c("901P900001.01", "967H900002.02")),
    "2024-2025" = list(format = "new", codes = c("901C900003.01", "967O900004.02")),
    "2025-2026" = list(format = "new", codes = c("901F900005.01", "967U900006.02"))
  )

  for (school_year in names(cases)) {
    clean <- make_classroom_clean(cases[[school_year]]$format,
                                  length(cases[[school_year]]$codes))
    clean$data$classroom_code <- cases[[school_year]]$codes
    clean$data$school_year <- school_year
    clean$meta$school_year <- school_year

    val <- classroom_validate(clean)
    code_check <- val$checks[val$checks$check_name == "classroom_code_format", ]

    expect_equal(code_check$status, "PASS", info = school_year)
    expect_equal(code_check$n_issues, 0, info = school_year)
  }
})

test_that("classroom_validate warns on non-canonical classroom code shapes", {
  clean <- make_classroom_clean("new", 7)
  clean$data$classroom_code <- c(
    "901P900001.01",
    "901P90001.01",
    "901P900001.1",
    "901P9000011",
    "91P900001.01",
    "901p900001.01",
    NA
  )

  val <- classroom_validate(clean)
  code_check <- val$checks[val$checks$check_name == "classroom_code_format", ]

  expect_equal(code_check$status, "WARN")
  expect_equal(code_check$n_issues, 5)
})

test_that("classroom_validate detects missing required columns", {
  clean <- make_classroom_clean("legacy", 3)
  clean$data$classroom_code <- NULL  # Remove required column

  val <- classroom_validate(clean)
  req_check <- val$checks[val$checks$check_name == "required_columns", ]
  expect_equal(req_check$status, "ERROR")
  expect_false(val$passed)
})

test_that("classroom_validate strict mode treats warnings as errors", {
  clean <- make_classroom_clean("legacy", 3)
  # Add an out-of-range coordinate to trigger a warning
  clean$data$latitude[1] <- 50.0  # Outside Alabama

  val_normal <- classroom_validate(clean, strict = FALSE)
  val_strict <- classroom_validate(clean, strict = TRUE)

  # Normal mode: should still pass (only warnings)
  if (val_normal$n_errors == 0 && val_normal$n_warnings > 0) {
    expect_true(val_normal$passed)
    expect_false(val_strict$passed)
  }
})

test_that("classroom_validate detects out-of-range grants", {
  clean <- make_classroom_clean("legacy", 3)
  clean$data$total_grant[1] <- -1000  # Negative

  val <- classroom_validate(clean)
  grant_check <- val$checks[val$checks$check_name == "grant_range", ]
  expect_equal(grant_check$status, "WARN")
})

test_that("classroom_validate detects out-of-range experience", {
  clean <- make_classroom_clean("legacy", 3)
  clean$data$lead_tch_osr_exp[1] <- 60  # > 50

  val <- classroom_validate(clean)
  exp_check <- val$checks[val$checks$check_name == "experience_range", ]
  expect_equal(exp_check$status, "WARN")
})

test_that("print method for validation works", {
  clean <- make_classroom_clean("legacy", 3)
  val <- classroom_validate(clean)
  expect_output(print(val), "alprek_classroom_validation")
  expect_output(print(val), "PASS")
})

test_that("classroom_validate rejects wrong input class", {
  expect_error(classroom_validate(list()), "alprek_classroom_clean")
})
