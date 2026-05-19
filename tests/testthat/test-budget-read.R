test_that("budget_read errors on nonexistent file", {
  expect_error(budget_read("nonexistent.xlsx"), "File not found")
})

test_that("budget_read rejects Excel lock files before reading", {
  expect_error(
    budget_read("~$24-25 FCPK Budgets.xlsx"),
    "Excel lock/temp budget file"
  )
})

test_that("budget_read rejects noncanonical budget support exports", {
  expect_error(
    budget_read("Budget Requests Grant Applications Foundant 092724.xlsx"),
    "Budget request/Foundant export"
  )

  expect_error(
    budget_read("FCPK Classroom Budgets as of 12-5-24.xlsx"),
    "Dated/interim budget snapshot"
  )
})

test_that("budget_read gives specific message for missing 2025-2026 budget", {
  expect_error(
    budget_read("25-26 FCPK Budgets.xlsx"),
    "Canonical 2025-2026 budget source not found"
  )
})

test_that("alprek_infer_school_year works for full year format", {
  expect_equal(
    alprek_infer_school_year("rptClassBudgets 2021-2022.xlsx"),
    "2021-2022"
  )
  expect_equal(
    alprek_infer_school_year("rptClassBudgets 2023-2024.xlsx"),
    "2023-2024"
  )
})

test_that("alprek_infer_school_year works for short year format", {
  expect_equal(
    alprek_infer_school_year("24-25 FCPK Budgets.xlsx"),
    "2024-2025"
  )
  expect_equal(
    alprek_infer_school_year("25-26 FCPK Budgets.xlsx"),
    "2025-2026"
  )
  expect_equal(
    alprek_infer_school_year("21-22 Budget.xlsx"),
    "2021-2022"
  )
})

test_that("alprek_infer_school_year returns NA for unrecognizable names", {
  expect_true(is.na(alprek_infer_school_year("budget_data.xlsx")))
  expect_true(is.na(alprek_infer_school_year("report.xlsx")))
})

test_that("alprek_infer_school_year ignores non-school-year support dates", {
  expect_true(is.na(alprek_infer_school_year(
    "Budget Requests Grant Applications Foundant 092724.xlsx"
  )))
  expect_true(is.na(alprek_infer_school_year(
    "FCPK Classroom Budgets as of 12-5-24.xlsx"
  )))
  expect_true(is.na(alprek_infer_school_year(
    "Budget Requests Grant Applications Foundant 09-27-24.xlsx"
  )))
  expect_true(is.na(alprek_infer_school_year(
    "FCPK Classroom Budgets as of 12-05-24.xlsx"
  )))
  expect_true(is.na(alprek_infer_school_year("Budget Request 2024.xlsx")))
})

test_that("alprek_school_year_to_start converts correctly", {
  expect_equal(alprek_school_year_to_start("2024-2025"), 2024L)
  expect_equal(alprek_school_year_to_start("2021-2022"), 2021L)
})

test_that(".extract_year_from_bv extracts year from column name", {
  expect_equal(
    .extract_year_from_bv("Budget Version (latest approved for 2023-2024)"),
    "2023-2024"
  )
  expect_true(is.na(.extract_year_from_bv("Budget Version")))
})
