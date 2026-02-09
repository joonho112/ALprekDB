test_that("budget_validate requires alprek_budget_long input", {
  expect_error(
    budget_validate(data.frame(x = 1)),
    "alprek_budget_long"
  )
})

test_that("validation passes with clean synthetic data", {
  raw <- make_budget_raw("legacy")
  cleaned <- budget_clean(raw)
  val <- budget_validate(cleaned)

  expect_s3_class(val, "alprek_budget_validation")
  expect_true(val$passed)
  expect_equal(val$n_errors, 0)
})

test_that("validation returns correct check count", {
  raw <- make_budget_raw("new")
  cleaned <- budget_clean(raw)
  val <- budget_validate(cleaned)

  expect_equal(nrow(val$checks), 7)
  expect_true(all(c("check_name", "check_description", "status",
                     "n_issues", "details") %in% names(val$checks)))
})

test_that("strict mode treats warnings as failures", {
  raw <- make_budget_raw("legacy")
  cleaned <- budget_clean(raw)

  # Force a warning by modifying totals to have mismatches
  cleaned$totals$flag_total_mismatch[1] <- TRUE

  val_lenient <- budget_validate(cleaned, strict = FALSE)
  val_strict <- budget_validate(cleaned, strict = TRUE)

  # Lenient should pass (warnings OK), strict should fail
  expect_true(val_lenient$passed)
  expect_false(val_strict$passed)
})

test_that("print method works without error", {
  raw <- make_budget_raw("legacy")
  cleaned <- budget_clean(raw)
  val <- budget_validate(cleaned)
  expect_output(print(val), "alprek_budget_validation")
})
