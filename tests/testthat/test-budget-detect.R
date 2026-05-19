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
    `Classroom Code` = "901P900001.01",
    `Lead Teacher Salary  From OSR Funds` = 100  # double space
  )
  # This should NOT match because regex expects single space before "From OSR Funds"
  # But the pattern "From OSR Funds$" will still match
  expect_equal(budget_detect_format(df), "legacy")
})

test_that("legacy detection rejects marker-only false positives", {
  df <- tibble::tibble(
    `Something From OSR Funds` = 1,
    `Another Column` = 2
  )
  expect_error(budget_detect_format(df), "required classroom-budget identifiers")
})

test_that("new detection requires both OSR and Proration", {
  df_osr_only <- tibble::tibble(`Lead Teacher Salary OSR` = 1)
  expect_error(budget_detect_format(df_osr_only), "Cannot detect")

  df_pror_only <- tibble::tibble(`Proration Total` = 0)
  expect_error(budget_detect_format(df_pror_only), "Cannot detect")
})

test_that("new detection rejects marker-only false positives", {
  df <- tibble::tibble(
    `OSR Grant Amount` = 1,
    `Proration Total` = 0
  )

  expect_error(budget_detect_format(df), "required classroom-budget identifiers")
})

test_that("new detection rejects Foundant-like request exports", {
  df <- tibble::tibble(
    `Application ID` = "A1",
    `Organization Name` = "Example",
    `OSR Requested Amount` = 1,
    `Proration Requested` = 0,
    `Grant Cycle` = "2024"
  )

  expect_error(budget_detect_format(df), "required classroom-budget identifiers")
})

test_that("budget_detect_format errors on ambiguous mixed markers", {
  df <- make_legacy_raw_df(1)
  df$`Proration Total` <- 0

  expect_error(budget_detect_format(df), "both legacy and new budget markers")
})
