test_that("detects legacy format from columns ending in 'From OSR Funds'", {
  df <- make_legacy_raw_df()
  expect_equal(budget_detect_format(df), "legacy")
})

test_that("detects new format from OSR + Proration columns", {
  df <- make_new_raw_df()
  expect_equal(budget_detect_format(df), "new")
})

test_that("errors on unrecognizable format", {
  df <- tibble::tibble(x = 1, y = 2, z = 3)
  expect_error(budget_detect_format(df), "Cannot detect budget format")
})

test_that("handles extra whitespace in column names", {
  df <- tibble::tibble(
    `Classroom Name` = "A",
    `Lead Teacher Salary  From OSR Funds` = 100  # double space
  )
  # This should NOT match because regex expects single space before "From OSR Funds"
  # But the pattern "From OSR Funds$" will still match
  expect_equal(budget_detect_format(df), "legacy")
})

test_that("legacy detection works with minimal columns", {
  df <- tibble::tibble(
    `Something From OSR Funds` = 1,
    `Another Column` = 2
  )
  expect_equal(budget_detect_format(df), "legacy")
})

test_that("new detection requires both OSR and Proration", {
  df_osr_only <- tibble::tibble(`Lead Teacher Salary OSR` = 1)
  expect_error(budget_detect_format(df_osr_only), "Cannot detect")

  df_pror_only <- tibble::tibble(`Proration Total` = 0)
  expect_error(budget_detect_format(df_pror_only), "Cannot detect")
})
